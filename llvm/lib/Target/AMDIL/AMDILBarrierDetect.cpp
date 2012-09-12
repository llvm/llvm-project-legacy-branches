//===-- AMDILBarrierDetect.cpp --------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "barrierdetect"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME 0
#endif
#include "AMDILAlgorithms.tpp"
#include "AMDILDevices.h"
#include "AMDILCompilerWarnings.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILSubtarget.h"
#include "AMDILTargetMachine.h"
#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

// The barrier detect pass determines if a barrier has been duplicated in the
// source program which can cause undefined behaviour if more than a single
// wavefront is executed in a group. This is because LLVM does not have an
// execution barrier and if this barrier function gets duplicated, undefined
// behaviour can occur. In order to work around this, we detect the duplicated
// barrier and then make the work-group execute in a single wavefront mode,
// essentially making the barrier a no-op.

namespace
{
class LLVM_LIBRARY_VISIBILITY AMDILBarrierDetect : public FunctionPass
{
TargetMachine &TM;
static char ID;
public:
  AMDILBarrierDetect(TargetMachine &TM, CodeGenOpt::Level OptLevel);
  ~AMDILBarrierDetect();
  const char *getPassName() const;
  bool runOnFunction(Function &F);
  bool doInitialization(Module &M);
  bool doFinalization(Module &M);
  void getAnalysisUsage(AnalysisUsage &AU) const;
private:
  bool detectBarrier(BasicBlock::iterator *BBI);
  bool mChanged;
  SmallVector<int64_t, DEFAULT_VEC_SLOTS> bVecMap;
  const AMDILSubtarget *mStm;

  // Constants used to define memory type.
  static const unsigned int LOCAL_MEM_FENCE = 1<<0;
  static const unsigned int GLOBAL_MEM_FENCE = 1<<1;
  static const unsigned int REGION_MEM_FENCE = 1<<2;
};
char AMDILBarrierDetect::ID = 0;
} // anonymouse namespace

namespace llvm
{
FunctionPass *
createAMDILBarrierDetect(TargetMachine &TM, CodeGenOpt::Level OptLevel)
{
  return new AMDILBarrierDetect(TM, OptLevel);
}
} // llvm namespace

AMDILBarrierDetect::AMDILBarrierDetect(TargetMachine &TM,
                                       CodeGenOpt::Level OptLevel)
  : FunctionPass(ID),
    TM(TM)
{
}
AMDILBarrierDetect::~AMDILBarrierDetect()
{
}
bool AMDILBarrierDetect::detectBarrier(BasicBlock::iterator *BBI)
{
  SmallVector<int64_t, DEFAULT_VEC_SLOTS>::iterator bIter;
  int64_t bID;
  Instruction *inst = (*BBI);
  CallInst *CI = dyn_cast<CallInst>(inst);

  if (!CI || !CI->getNumOperands()) {
    return false;
  }

  const Value *funcVal = CI->getOperand(CI->getNumOperands() - 1);

  if (!funcVal) {
    return false;
  }

  const StringRef& funcName = funcVal->getName();

  if (funcName.startswith("__amdil_gws")) {
    AMDILMachineFunctionInfo *MFI =
      getAnalysis<MachineFunctionAnalysis>().getMF()
      .getInfo<AMDILMachineFunctionInfo>();
    MFI->addMetadata(";memory:gws");
    return false;
  } else if (!funcName.startswith("barrier") &&
             !funcName.startswith("__amd_barrier")) {
    return false;
  }

  if (inst->getNumOperands() >= 3) {
    const Value *V = inst->getOperand(0);
    const ConstantInt *Cint = dyn_cast<ConstantInt>(V);
    bID = Cint->getSExtValue();
    bIter = std::find(bVecMap.begin(), bVecMap.end(), bID);
    if (bIter == bVecMap.end()) {
      bVecMap.push_back(bID);
    } else {
      if (mStm->device()->isSupported(AMDILDeviceInfo::BarrierDetect)) {
        AMDILMachineFunctionInfo *MFI =
          getAnalysis<MachineFunctionAnalysis>().getMF()
          .getInfo<AMDILMachineFunctionInfo>();
        MFI->addMetadata(";limitgroupsize");
        MFI->addErrorMsg(amd::CompilerWarningMessage[BAD_BARRIER_OPT]);
      }
    }
  }
  if (mStm->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    AMDILMachineFunctionInfo *MFI =
      getAnalysis<MachineFunctionAnalysis>().getMF()
      .getInfo<AMDILMachineFunctionInfo>();
    MFI->addErrorMsg(amd::CompilerWarningMessage[LIMIT_BARRIER]);
    MFI->addMetadata(";limitgroupsize");
    MFI->setUsesLDS();
  }

  const Value *V = inst->getOperand(inst->getNumOperands() - 2);
  const ConstantInt *Cint = dyn_cast<ConstantInt>(V);
  Function *iF = dyn_cast<Function>(inst->getOperand(inst->getNumOperands() - 1));

  Module *M = iF->getParent();
  bID = Cint->getSExtValue();
  if (bID > 0) {
    const char *name = "barrier";
    if (bID == GLOBAL_MEM_FENCE) {
      name = "barrierGlobal";
    } else if (bID == LOCAL_MEM_FENCE
               && mStm->device()->usesHardware(AMDILDeviceInfo::LocalMem)) {
      name = "barrierLocal";
    } else if (bID == REGION_MEM_FENCE
               && mStm->device()->usesHardware(AMDILDeviceInfo::RegionMem)) {
      name = "barrierRegion";
    }
    Function *nF =
      dyn_cast<Function>(M->getOrInsertFunction(name, iF->getFunctionType()));
    inst->setOperand(inst->getNumOperands() - 1, nF);
    return false;
  }

  return false;
}
bool AMDILBarrierDetect::runOnFunction(Function &MF)
{
  mChanged = false;
  bVecMap.clear();
  mStm = &TM.getSubtarget<AMDILSubtarget>();
  Function *F = &MF;
  safeNestedForEach(F->begin(), F->end(), F->begin()->begin(),
                    std::bind1st(
                      std::mem_fun(
                        &AMDILBarrierDetect::detectBarrier), this));
  return mChanged;
}
const char* AMDILBarrierDetect::getPassName() const
{
  return "AMDIL Barrier Detect Pass";
}
bool AMDILBarrierDetect::doInitialization(Module &M)
{
  return false;
}
bool AMDILBarrierDetect::doFinalization(Module &M)
{
  return false;
}
void AMDILBarrierDetect::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.addRequired<MachineFunctionAnalysis>();
  FunctionPass::getAnalysisUsage(AU);
  AU.setPreservesAll();
}
