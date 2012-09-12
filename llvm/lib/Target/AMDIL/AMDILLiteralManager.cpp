//===-- AMDILLiteralManager.cpp -------------------------------------------===//
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

#define DEBUG_TYPE "literal_manager"
#include "AMDIL.h"
#include "AMDILAlgorithms.tpp"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILSubtarget.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

// AMDIL Literal Manager traverses through all of the LOADCONST instructions and
// converts them from an immediate value to the literal index. The literal index
// is valid IL, but the immediate values are not. The Immediate values must be
// aggregated and declared for clarity and to reduce the number of literals that
// are used. It is also illegal to declare the same literal twice, so this keeps
// that from occuring.

namespace {
class AMDILLiteralManager : public MachineFunctionPass {
public:
  static char ID;
  AMDILLiteralManager(TargetMachine &tm, CodeGenOpt::Level OL);
  virtual const char *getPassName() const;

  bool runOnMachineFunction(MachineFunction &MF);
private:
  bool trackLiterals(MachineBasicBlock::iterator *bbb);
  TargetMachine &TM;
  const AMDILSubtarget *mSTM;
  AMDILKernelManager *mKM;
  AMDILMachineFunctionInfo *mMFI;
  int32_t mLitIdx;
  bool mChanged;
};
char AMDILLiteralManager::ID = 0;
}

namespace llvm {
FunctionPass *
createAMDILLiteralManager(TargetMachine &tm, CodeGenOpt::Level OL) {
  return new AMDILLiteralManager(tm, OL);
}
}

AMDILLiteralManager::AMDILLiteralManager(TargetMachine &tm,
                                         CodeGenOpt::Level OL)
  : MachineFunctionPass(ID),
    TM(tm) {
}
bool AMDILLiteralManager::runOnMachineFunction(MachineFunction &MF) {
  mChanged = false;
  DEBUG(MF.dump());
  mMFI = MF.getInfo<AMDILMachineFunctionInfo>();
  const AMDILTargetMachine *amdtm =
    reinterpret_cast<const AMDILTargetMachine *>(&TM);
  mSTM = dynamic_cast<const AMDILSubtarget *>(amdtm->getSubtargetImpl());
  mKM = const_cast<AMDILKernelManager *>(mSTM->getKernelManager());
  safeNestedForEach(MF.begin(), MF.end(), MF.begin()->begin(),
                    std::bind1st(std::mem_fun(&AMDILLiteralManager::
                                              trackLiterals), this));
  DEBUG(MF.dump());
  return mChanged;
}
bool AMDILLiteralManager::trackLiterals(MachineBasicBlock::iterator *bbb) {
  MachineInstr *MI = *bbb;
  uint32_t Opcode = MI->getOpcode();
  for (unsigned x = 0, y = MI->getNumOperands(); x < y; ++x) {
    MachineOperand &litOp = MI->getOperand(x);
    if ((!litOp.isImm() && !litOp.isFPImm())
        || isBypassedLiteral(MI, x)
        || isSkippedLiteral(MI, x)
        || !MI->getDesc().OpInfo) {
      continue;
    }
    /*
    assert(Opcode <= AMDIL::LOADCONSTf64 && Opcode >= AMDIL::LOADCONSTi8
        && "Found a loadconst instruction!");
        */
    uint32_t idx;
    if (litOp.isFPImm()) {
      const ConstantFP *fpVal = litOp.getFPImm();
      const APFloat &fp = fpVal->getValueAPF();
      const fltSemantics &fpSem = fpVal->getValueAPF().getSemantics();
      if (&fpSem == &APFloat::IEEEsingle) {
        idx = mMFI->addf32Literal(fpVal);
      } else if (&fpSem == &APFloat::IEEEdouble) {
        idx = mMFI->addf64Literal(fpVal);
      } else {
        assert(!"Found a case we don't handle!");
      }
    } else if (litOp.isImm()) {
      unsigned regClass = MI->getDesc().OpInfo[x].RegClass;
      if (regClass == ~0U) {
        regClass = getRegClassFromName(TM.getInstrInfo()->getName(Opcode));
      }
      int64_t immVal = litOp.getImm();
      switch (regClass) {
      default:
        idx = Opcode == AMDIL::LOADCONSTi64
              ? mMFI->addi64Literal(immVal)
              : mMFI->addi32Literal(static_cast<int>(immVal));
        break;
      case AMDIL::GPRI8RegClassID:
      case AMDIL::GPRV2I8RegClassID:
      case AMDIL::GPRV4I8RegClassID:
        idx = mMFI->addi32Literal(static_cast<int>(immVal), AMDIL::LOADCONSTi8);
        break;
      case AMDIL::GPRI16RegClassID:
      case AMDIL::GPRV2I16RegClassID:
      case AMDIL::GPRV4I16RegClassID:
        idx = mMFI->addi32Literal(static_cast<int>(immVal), AMDIL::LOADCONSTi16);
        break;
      case AMDIL::GPRI32RegClassID:
      case AMDIL::GPRV2I32RegClassID:
      case AMDIL::GPRV4I32RegClassID:
        idx = mMFI->addi32Literal(static_cast<int>(immVal));
        break;
      case AMDIL::GPRI64RegClassID:
      case AMDIL::GPRV2I64RegClassID:
        idx = mMFI->addi64Literal(immVal);
        break;
      }
    } else {
      assert(!"Should never hit here unless a new literal type was added!");
    }
    litOp.ChangeToImmediate(idx);
  }

  return false;
}
const char* AMDILLiteralManager::getPassName() const {
  return "AMDIL Literal Manager";
}
