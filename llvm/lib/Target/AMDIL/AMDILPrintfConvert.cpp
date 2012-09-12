//===-- AMDILPrintfConvert.cpp --------------------------------------------===//
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

#define DEBUG_TYPE "printfconvert"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME 0
#endif

#include "AMDILAlgorithms.tpp"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILModuleInfo.h"
#include "AMDILUtilityFunctions.h"
#include "AMDILTargetMachine.h"
#include "llvm/Instructions.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include <cstdio>
#include <string>
using namespace llvm;
namespace
{
class LLVM_LIBRARY_VISIBILITY AMDILPrintfConvert : public FunctionPass
{
public:
  TargetMachine &TM;
  static char ID;
  AMDILPrintfConvert(TargetMachine &tm, CodeGenOpt::Level OL);
  ~AMDILPrintfConvert();
  const char* getPassName() const;
  bool runOnFunction(Function &F);
  bool doInitialization(Module &M);
  bool doFinalization(Module &M);
  void getAnalysisUsage(AnalysisUsage &AU) const;
  static const char* getConversionSpecifiers(const std::string& fmt,
                                             size_t num_ops);
private:
  bool expandPrintf(BasicBlock::iterator *bbb);
  AMDILMachineFunctionInfo *mMFI;
  AMDILKernelManager *mKM;
  bool mChanged;
  SmallVector<int64_t, DEFAULT_VEC_SLOTS> bVecMap;
};
char AMDILPrintfConvert::ID = 0;
} // anonymouse namespace

namespace llvm
{
FunctionPass*
createAMDILPrintfConvert(TargetMachine &tm, CodeGenOpt::Level OL)
{
  return new AMDILPrintfConvert(tm, OL);
}
} // llvm namespace
AMDILPrintfConvert::AMDILPrintfConvert(TargetMachine &tm, CodeGenOpt::Level OL)
  : FunctionPass(ID), TM(tm)
{
}
AMDILPrintfConvert::~AMDILPrintfConvert()
{
}
bool
AMDILPrintfConvert::expandPrintf(BasicBlock::iterator *bbb)
{
  Instruction *inst = (*bbb);
  CallInst *CI = dyn_cast<CallInst>(inst);
  if (!CI) {
    return false;
  }
  int num_ops = CI->getNumOperands();
  if (!num_ops) {
    return false;
  }
  if (CI->getOperand(num_ops - 1)->getName() != "printf") {
    return false;
  }

  const char* opConvSpecifiers = NULL;
  Function *mF = inst->getParent()->getParent();
  uint64_t bytes = 0;
  mChanged = true;
  if (num_ops == 1) {
    ++(*bbb);
    Constant *newConst = ConstantInt::getSigned(CI->getType(), bytes);
    CI->replaceAllUsesWith(newConst);
    CI->eraseFromParent();
    return mChanged;
  }
  // Deal with the string here
  Value *op = CI->getOperand(0);
  ConstantExpr *GEPinst = dyn_cast<ConstantExpr>(op);
  if (GEPinst) {
    GlobalVariable *GVar
      = dyn_cast<GlobalVariable>(GEPinst->getOperand(0));
    std::string str = "unknown";
    if (GVar && GVar->hasInitializer()) {
      ConstantDataArray *CA
        = dyn_cast<ConstantDataArray>(GVar->getInitializer());
      str = (CA->isString() ? CA->getAsString() : "unknown");
      opConvSpecifiers = getConversionSpecifiers(str,num_ops - 2);
    }
    uint64_t id = (uint64_t)mMFI->addPrintfString(str,
                                                  getAnalysis<
                                                    MachineFunctionAnalysis>().
                                                  getMF()
                                                  .getMMI().getObjFileInfo<
                                                    AMDILModuleInfo>().
                                                  get_printf_offset());
    std::string name = "___dumpStringID";
    Function *nF = NULL;
    std::vector<Type*> types;
    types.push_back(Type::getInt32Ty(mF->getContext()));
    nF = mF->getParent()->getFunction(name);
    if (!nF) {
      nF = Function::Create(
        FunctionType::get(
          Type::getVoidTy(mF->getContext()), types, false),
        GlobalValue::ExternalLinkage,
        name, mF->getParent());
    }
    Constant *C = ConstantInt::get(
      Type::getInt32Ty(mF->getContext()), id, false);
    CallInst *nCI = CallInst::Create(nF, C);
    nCI->insertBefore(CI);
    bytes = strlen(str.data());
    for (uint32_t x = 1, y = num_ops - 1; x < y; ++x) {
      op = CI->getOperand(x);
      Type *oType = op->getType();
      uint32_t eleCount = getNumElements(oType);
      uint32_t eleSize = (uint32_t)GET_SCALAR_SIZE(oType);
      if (!eleSize) {
        // Default size is 32bits.
        eleSize = 32;
      }
      if (!eleCount) {
        // Default num elements is 1.
        eleCount = 1;
      }
      uint32_t totalSize = eleCount * eleSize;
      mMFI->addPrintfOperand(str, (x - 1),
                             (uint32_t)totalSize);
    }
  }
  for (uint32_t x = 1, y = num_ops - 1; x < y; ++x) {
    op = CI->getOperand(x);
    Type *oType = op->getType();
    if (oType->isFPOrFPVectorTy()
        && (oType->getTypeID() != Type::VectorTyID)) {
      Type *iType = NULL;
      if (oType->isFloatTy()) {
        iType = dyn_cast<Type>(
          Type::getInt32Ty(oType->getContext()));
      } else {
        iType = dyn_cast<Type>(
          Type::getInt64Ty(oType->getContext()));
      }
      op = new BitCastInst(op, iType, "printfBitCast", CI);
    } else if (oType->getTypeID() == Type::VectorTyID) {
      Type *iType = NULL;
      uint32_t eleCount = getNumElements(oType);
      uint32_t eleSize = (uint32_t)GET_SCALAR_SIZE(oType);
      uint32_t totalSize = eleCount * eleSize;
      if (eleCount == 3) {
        IntegerType *int32ty = Type::getInt32Ty(oType->getContext());
        Constant* indices[4] = {
          ConstantInt::get(int32ty, 0),
          ConstantInt::get(int32ty, 1),
          ConstantInt::get(int32ty, 2),
          ConstantInt::get(int32ty, 2)
        };
        Constant* mask = ConstantVector::get(indices);
        ShuffleVectorInst* shuffle = new ShuffleVectorInst(op, op, mask);
        shuffle->insertBefore(CI);
        op = shuffle;
        oType = op->getType();
        totalSize += eleSize;
      }
      switch (eleSize) {
      default:
        eleCount = totalSize / 64;
        iType = dyn_cast<Type>(
          Type::getInt64Ty(oType->getContext()));
        break;
      case 8:
        if (eleCount >= 8) {
          eleCount = totalSize / 64;
          iType = dyn_cast<Type>(
            Type::getInt64Ty(oType->getContext()));
        } else if (eleCount >= 3) {
          eleCount = 1;
          iType = dyn_cast<Type>(
            Type::getInt32Ty(oType->getContext()));
        } else {
          eleCount = 1;
          iType = dyn_cast<Type>(
            Type::getInt16Ty(oType->getContext()));
        }
        break;
      case 16:
        if (eleCount >= 3) {
          eleCount = totalSize / 64;
          iType = dyn_cast<Type>(
            Type::getInt64Ty(oType->getContext()));
        } else {
          eleCount = 1;
          iType = dyn_cast<Type>(
            Type::getInt32Ty(oType->getContext()));
        }
        break;
      }
      if (eleCount > 1) {
        iType = dyn_cast<Type>(
          VectorType::get(iType, eleCount));
      }
      op = new BitCastInst(op, iType, "printfBitCast", CI);
    }
    char buffer[256];
    uint32_t size = (uint32_t)GET_SCALAR_SIZE(oType);
    if (size) {
      sprintf(buffer, "___dumpBytes_v%db%u",
              1,
              (uint32_t)getNumElements(oType) * (uint32_t)size);
    } else {
      const PointerType *PT = dyn_cast<PointerType>(oType);
      if (opConvSpecifiers && opConvSpecifiers[x-1] == 's' &&
          PT->getAddressSpace() == 2 &&
          GET_SCALAR_SIZE(PT->getContainedType(0)) == 8 &&
          getNumElements(PT->getContainedType(0)) == 1) {
        op = new BitCastInst(op,
                             Type::getInt8PtrTy(oType->getContext(),
                                                AMDILAS::CONSTANT_ADDRESS),
                             "printfPtrCast", CI);

        sprintf(buffer, "___dumpBytes_v%dbs", 1);
      } else {
        op = new PtrToIntInst(op,
                              Type::getInt32Ty(oType->getContext()),
                              "printfPtrCast", CI);
        sprintf(buffer, "___dumpBytes_v1b32");
      }
    }
    std::vector<Type*> types;
    types.push_back(op->getType());
    std::string name = buffer;
    Function *nF = NULL;
    nF = mF->getParent()->getFunction(name);
    if (!nF) {
      nF = Function::Create(
        FunctionType::get(
          Type::getVoidTy(mF->getContext()), types, false),
        GlobalValue::ExternalLinkage,
        name, mF->getParent());
    }
    CallInst *nCI = CallInst::Create(nF, op);
    nCI->insertBefore(CI);
    bytes += (size - 4);
  }
  if (opConvSpecifiers != NULL) {
    delete[] opConvSpecifiers;
  }
  ++(*bbb);
  Constant *newConst = ConstantInt::getSigned(CI->getType(), bytes);
  CI->replaceAllUsesWith(newConst);
  CI->eraseFromParent();
  return mChanged;
}
bool
AMDILPrintfConvert::runOnFunction(Function &MF)
{
  mChanged = false;
  mKM = TM.getSubtarget<AMDILSubtarget>().getKernelManager();
  mMFI = getAnalysis<MachineFunctionAnalysis>().getMF()
         .getInfo<AMDILMachineFunctionInfo>();
  bVecMap.clear();
  safeNestedForEach(MF.begin(), MF.end(), MF.begin()->begin(),
                    std::bind1st(
                      std::mem_fun(
                        &AMDILPrintfConvert::expandPrintf), this));
  return mChanged;
}
const char*
AMDILPrintfConvert::getPassName() const
{
  return "AMDIL Printf Conversion Pass";
}
bool
AMDILPrintfConvert::doInitialization(Module &M)
{
  return false;
}
bool
AMDILPrintfConvert::doFinalization(Module &M)
{
  return false;
}
void
AMDILPrintfConvert::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.addRequired<MachineFunctionAnalysis>();
  FunctionPass::getAnalysisUsage(AU);
  AU.setPreservesAll();
}
const char*
AMDILPrintfConvert::getConversionSpecifiers(const std::string& fmt,
                                            size_t num_ops)
{
  static const char* convSpecifiers = "cdieEfgGaosuxXp";
  size_t curFmtSpecifierIdx = 0;
  size_t prevFmtSpecifierIdx = 0;
  size_t opIdx = 0;
  char* opConvSpecifiers = new char[num_ops];
  while ((curFmtSpecifierIdx =
            fmt.find_first_of(convSpecifiers,
                              curFmtSpecifierIdx)) != std::string::npos) {
    bool argDump = false;
    const std::string curFmt = fmt.substr(
      prevFmtSpecifierIdx,
      curFmtSpecifierIdx -
      prevFmtSpecifierIdx);
    size_t pTag = curFmt.find_last_of("%");
    if (pTag != std::string::npos) {
      argDump = true;
      while (pTag && curFmt[--pTag] == '%') {
        argDump = !argDump;
      }
    }
    if (argDump) {
      opConvSpecifiers[opIdx++] = fmt[curFmtSpecifierIdx];
    }
    prevFmtSpecifierIdx = ++curFmtSpecifierIdx;
  }
  if (opIdx == 0) {
    delete[] opConvSpecifiers;
    return NULL;
  }
  return opConvSpecifiers;
}
