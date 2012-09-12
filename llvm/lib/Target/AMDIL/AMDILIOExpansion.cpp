//===-- AMDILIOExpansion.cpp ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The AMDIL IO Expansion class expands pseudo IO instructions into a sequence
// of instructions that produces the correct results. These instructions are not
// expanded earlier in the pass because any pass before this can assume to be able to
// generate a load store instruction. So this pass can only have passes that execute
// after it if no load store instructions can be generated.
//
//===----------------------------------------------------------------------===//

#include "AMDILIOExpansion.h"
#include "AMDIL.h"
#include "AMDILDevices.h"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/DerivedTypes.h"
#include "llvm/GlobalValue.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Value.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/Support/DebugLoc.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegisterInfo.h"
using namespace llvm;

namespace llvm {
FunctionPass*
createAMDILIOExpansion(TargetMachine& TM, CodeGenOpt::Level OptLevel)
{
  return TM.getSubtarget<AMDILSubtarget>().device()->getIOExpansion();
}
}

AMDILIOExpansionImpl::AMDILIOExpansionImpl(MachineFunction& mf)
  : mDebug(DEBUGME), MF(mf), mBB(NULL),TM(MF.getTarget())
{
  mSTM = &TM.getSubtarget<AMDILSubtarget>();
  mKM = const_cast<AMDILKernelManager*>(mSTM->getKernelManager());
  mMFI = MF.getInfo<AMDILMachineFunctionInfo>();
  mTRI = TM.getRegisterInfo();
  mTII = TM.getInstrInfo();
}
bool
AMDILIOExpansionImpl::run()
{
  for (MachineFunction::iterator MFI = MF.begin(), MFE = MF.end();
       MFI != MFE; ++MFI) {
    MachineBasicBlock *MBB = MFI;
    for (MachineBasicBlock::iterator MBI = MBB->begin(), MBE = MBB->end();
         MBI != MBE; ++MBI) {
      MachineInstr *MI = MBI;
      if (isIOInstruction(MI)) {
        mBB = MBB;
        saveInst = false;
        expandIOInstruction(MI);
        if (!saveInst) {
          // erase returns the instruction after
          // and we want the instruction before
          MBI = MBB->erase(MI);
          --MBI;
        }
      }
    }
  }
  return false;
}
bool
AMDILIOExpansionImpl::isIOInstruction(MachineInstr *MI)
{
  if (!MI) {
    return false;
  }
  if (isPtrLoadInst(MI) || isPtrStoreInst(MI)) {
    return true;
  }
  return false;
}
void
AMDILIOExpansionImpl::expandIOInstruction(MachineInstr *MI)
{
  assert(isIOInstruction(MI) && "Must be an IO instruction to "
         "be passed to this function!");
  if (isPtrLoadInst(MI)) {
    if (isGlobalInst(MI)) {
      expandGlobalLoad(MI);
    } else if (isRegionInst(MI)) {
      expandRegionLoad(MI);
    } else if (isPrivateInst(MI)) {
      expandPrivateLoad(MI);
    } else if (isLocalInst(MI)) {
      expandLocalLoad(MI);
    } else if (isConstantInst(MI)) {
      if (isConstantPoolInst(MI)) {
        expandConstantPoolLoad(MI);
      } else {
        expandConstantLoad(MI);
      }
    } else {
      assert(!"Found an unsupported load instruction!");
    }
  } else if (isPtrStoreInst(MI)) {
    if (isGlobalInst(MI)) {
      expandGlobalStore(MI);
    } else if (isRegionInst(MI)) {
      expandRegionStore(MI);
    } else if (isPrivateInst(MI)) {
      expandPrivateStore(MI);
    } else if (isLocalInst(MI)) {
      expandLocalStore(MI);
    } else {
      assert(!"Found an unsupported load instruction!");
    }
  } else {
    assert(!"Found an unsupported IO instruction!");
  }
}
bool
AMDILIOExpansionImpl::isAddrCalcInstr(MachineInstr *MI)
{
  if (isPrivateInst(MI) && isPtrLoadInst(MI)) {
    // This section of code is a workaround for the problem of
    // globally scoped constant address variables. The problems
    // comes that although they are declared in the constant
    // address space, all variables must be allocated in the
    // private address space. So when there is a load from
    // the global address, it automatically goes into the private
    // address space. However, the data section is placed in the
    // constant address space so we need to check to see if our
    // load base address is a global variable or not. Only if it
    // is not a global variable can we do the address calculation
    // into the private memory ring.

    MachineMemOperand& memOp = (**MI->memoperands_begin());
    const Value *V = memOp.getValue();
    if (V) {
      const GlobalValue *GV = dyn_cast<GlobalVariable>(V);
      return mSTM->device()->usesSoftware(AMDILDeviceInfo::PrivateMem)
             && !(GV);
    } else {
      return false;
    }
  } else if (isConstantPoolInst(MI) && isPtrLoadInst(MI)) {
    return MI->getOperand(1).isReg();
  } else if (isPrivateInst(MI) && isPtrStoreInst(MI)) {
    return mSTM->device()->usesSoftware(AMDILDeviceInfo::PrivateMem);
  } else if (isLocalInst(MI) && (isPtrStoreInst(MI) || isPtrLoadInst(MI))) {
    return mSTM->device()->usesSoftware(AMDILDeviceInfo::LocalMem);
  }
  return false;
}
bool
AMDILIOExpansionImpl::isExtendLoad(MachineInstr *MI)
{
  return isSExtLoadInst(MI) || isZExtLoadInst(MI) || isAExtLoadInst(MI);
}
bool
AMDILIOExpansionImpl::isHardwareRegion(MachineInstr *MI)
{
  return (isRegionInst(MI) && (isPtrLoadInst(MI) || isPtrStoreInst(MI)) &&
          mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem));
}
bool
AMDILIOExpansionImpl::isHardwareLocal(MachineInstr *MI)
{
  return (isLocalInst(MI) && (isPtrLoadInst(MI) || isPtrStoreInst(MI)) &&
          mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem));
}
bool
AMDILIOExpansionImpl::isStaticCPLoad(MachineInstr *MI)
{
  if (isConstantPoolInst(MI) && isPtrLoadInst(MI)) {
    uint32_t x = 0;
    uint32_t num = MI->getNumOperands();
    for (x = 0; x < num; ++x) {
      if (MI->getOperand(x).isCPI()) {
        return true;
      }
    }
  }
  return false;
}
bool
AMDILIOExpansionImpl::isNbitType(Type *mType, uint32_t nBits, bool isScalar)
{
  if (!mType) {
    return false;
  }
  if (dyn_cast<PointerType>(mType)) {
    PointerType *PT = dyn_cast<PointerType>(mType);
    return isNbitType(PT->getElementType(), nBits);
  } else if (dyn_cast<StructType>(mType)) {
    return TM.getTargetData()->getTypeSizeInBits(mType) == nBits;
  } else if (dyn_cast<VectorType>(mType)) {
    VectorType *VT = dyn_cast<VectorType>(mType);
    size_t size = VT->getScalarSizeInBits();
    return (isScalar ?
            VT->getNumElements() * size == nBits : size == nBits);
  } else if (dyn_cast<ArrayType>(mType)) {
    ArrayType *AT = dyn_cast<ArrayType>(mType);
    size_t size = AT->getScalarSizeInBits();
    return (isScalar ?
            AT->getNumElements() * size == nBits : size == nBits);
  } else if (mType->isSized()) {
    return mType->getScalarSizeInBits() == nBits;
  } else {
    assert(0 && "Found a type that we don't know how to handle!");
    return false;
  }
}
bool
AMDILIOExpansionImpl::isHardwareInst(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curInst;
  getAsmPrinterFlags(MI, curInst);
  return curInst.bits.HardwareInst;
}
REG_PACKED_TYPE
AMDILIOExpansionImpl::getPackedID(MachineInstr *MI)
{
  if (isPackV2I8Inst(MI)) return PACK_V2I8;
  if (isPackV4I8Inst(MI)) return PACK_V4I8;
  if (isPackV2I16Inst(MI)) return PACK_V2I16;
  if (isPackV4I16Inst(MI)) return PACK_V4I16;
  if (isUnpackV2I8Inst(MI)) return UNPACK_V2I8;
  if (isUnpackV4I8Inst(MI)) return UNPACK_V4I8;
  if (isUnpackV2I16Inst(MI)) return UNPACK_V2I16;
  if (isUnpackV4I16Inst(MI)) return UNPACK_V4I16;
  return NO_PACKING;
}
uint32_t
AMDILIOExpansionImpl::getPointerID(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curInst;
  getAsmPrinterFlags(MI, curInst);
  return curInst.bits.ResourceID;
}
uint32_t
AMDILIOExpansionImpl::getShiftSize(MachineInstr *MI)
{
  switch(getPackedID(MI)) {
  default:
    return 0;
  case PACK_V2I8:
  case PACK_V4I8:
  case UNPACK_V2I8:
  case UNPACK_V4I8:
    return 1;
  case PACK_V2I16:
  case PACK_V4I16:
  case UNPACK_V2I16:
  case UNPACK_V4I16:
    return 2;
  }
  return 0;
}
uint32_t
AMDILIOExpansionImpl::getMemorySize(MachineInstr *MI)
{
  if (MI->memoperands_empty()) {
    return 4;
  }
  return (uint32_t)((*MI->memoperands_begin())->getSize());
}
void
AMDILIOExpansionImpl::expandLongExtend(MachineInstr *MI,
                                       uint32_t numComps,
                                       uint32_t size,
                                       bool signedShift,
                                       uint32_t &dataReg)
{
  DebugLoc DL = MI->getDebugLoc();
  switch(size) {
  default:
    assert(0 && "Found a case we don't handle!");
    break;
  case 8:
    if (numComps == 1) {
      expandLongExtendSub32(MI,
                            AMDIL::SHLi8i32rr,
                            AMDIL::SHRv2i32i32rr,
                            AMDIL::USHRi8i32rr,
                            24,
                            (24ULL | (31ULL << 32)),
                            24,
                            AMDIL::LCREATEi64rr,
                            signedShift,
                            false,
                            dataReg);
    } else if (numComps == 2) {
      expandLongExtendSub32(MI,
                            AMDIL::SHLv2i8i32rr,
                            AMDIL::SHRv4i32i32rr,
                            AMDIL::USHRv2i8i32rr,
                            24,
                            (24ULL | (31ULL << 32)),
                            24,
                            AMDIL::LCREATEv2i64rr,
                            signedShift,
                            true,
                            dataReg);
    } else {
      assert(0 && "Found a case we don't handle!");
    }
    break;
  case 16:
    if (numComps == 1) {
      expandLongExtendSub32(MI,
                            AMDIL::SHLi16i32rr,
                            AMDIL::SHRv2i32i32rr,
                            AMDIL::USHRi16i32rr,
                            16,
                            (16ULL | (31ULL << 32)),
                            16,
                            AMDIL::LCREATEi64rr,
                            signedShift,
                            false,
                            dataReg);
    } else if (numComps == 2) {
      expandLongExtendSub32(MI,
                            AMDIL::SHLv2i16i32rr,
                            AMDIL::SHRv4i32i32rr,
                            AMDIL::USHRv2i16i32rr,
                            16,
                            (16ULL | (31ULL << 32)),
                            16,
                            AMDIL::LCREATEv2i64rr,
                            signedShift,
                            true,
                            dataReg);
    } else {
      assert(0 && "Found a case we don't handle!");
    }
    break;
  case 32:
    if (numComps == 1) {
      MachineInstr *nMI = NULL;
      if (signedShift) {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), dataReg)
              .addReg(getCompReg(dataReg, sub_x_comp, sub_z_comp))
              .addImm(mMFI->addi64Literal((0ULL | (31ULL << 32))));
      } else {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEi64rr), dataReg)
              .addReg(dataReg)
              .addImm(mMFI->addi32Literal(0));
      }
    } else if (numComps == 2) {
      MachineInstr *nMI = NULL;
      if (signedShift) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRv2i32i32rr), AMDIL::Rxy1012)
        .addReg(getCompReg(dataReg, sub_xy_comp, sub_zw_comp))
        .addImm(mMFI->addi64Literal(31));
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEv2i64rr), dataReg)
              .addReg(dataReg)
              .addReg(AMDIL::Rxy1012);
      } else {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEv2i64rr), dataReg)
              .addReg(dataReg)
              .addImm(mMFI->addi32Literal(0));
      }
    } else {
      assert(0 && "Found a case we don't handle!");
    }
  };
}
void
AMDILIOExpansionImpl::expandLongExtendSub32(MachineInstr *MI,
                                            unsigned SHLop,
                                            unsigned SHRop,
                                            unsigned USHRop,
                                            unsigned SHLimm,
                                            uint64_t SHRimm,
                                            unsigned USHRimm,
                                            unsigned LCRop,
                                            bool signedShift,
                                            bool vec2,
                                            uint32_t &dataReg)
{
  MachineInstr *nMI = NULL;
  DebugLoc DL = MI->getDebugLoc();
  unsigned origReg = dataReg;
  BuildMI(*mBB, MI, DL, mTII->get(SHLop),
          (vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
  .addReg(dataReg)
  .addImm(mMFI->addi32Literal(SHLimm));
  dataReg = (vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011;
  if (signedShift) {
    BuildMI(*mBB, MI, DL, mTII->get(LCRop),
            (vec2) ? AMDIL::R1011 : AMDIL::Rxy1011)
    .addReg(dataReg).addReg(dataReg);
    dataReg = (vec2) ? AMDIL::R1011 : AMDIL::Rxy1011;
    nMI = BuildMI(*mBB, MI, DL, mTII->get(SHRop),
                  origReg).addReg(dataReg)
          .addImm(mMFI->addi64Literal(SHRimm));
  } else {
    BuildMI(*mBB, MI, DL, mTII->get(USHRop),
            dataReg).addReg(dataReg)
    .addImm(mMFI->addi32Literal(USHRimm));
    nMI = BuildMI(*mBB, MI, MI->getDebugLoc(), mTII->get(LCRop),
                  origReg)
          .addReg(dataReg)
          .addImm(mMFI->addi32Literal(0));
  }
}
void
AMDILIOExpansionImpl::expandIntegerExtend(MachineInstr *MI,
                                          unsigned SHLop,
                                          unsigned SHRop,
                                          unsigned offset,
                                          unsigned reg)
{
  DebugLoc DL = MI->getDebugLoc();
  offset = mMFI->addi32Literal(offset);
  BuildMI(*mBB, MI, DL,
          mTII->get(SHLop), reg)
  .addReg(reg).addImm(offset);
  BuildMI(*mBB, MI, DL,
          mTII->get(SHRop), reg)
  .addReg(reg).addImm(offset);
}
void
AMDILIOExpansionImpl::expandExtendLoad(MachineInstr *MI, uint32_t &dataReg)
{
  if (!isExtendLoad(MI)) {
    return;
  }
  Type *mType = NULL;
  if (!MI->memoperands_empty()) {
    MachineMemOperand *memOp = (*MI->memoperands_begin());
    const Value *moVal = (memOp) ? memOp->getValue() : NULL;
    mType = (moVal) ? moVal->getType() : NULL;
  }
  unsigned opcode = 0;
  DebugLoc DL = MI->getDebugLoc();
  if (isExtLoadInst(MI)) {
    switch(MI->getDesc().OpInfo[0].RegClass) {
    default:
      assert(0 && "Found an extending load that we don't handle!");
      break;
    case AMDIL::GPRI16RegClassID:
      if (!isHardwareLocal(MI)
          || mSTM->device()->usesSoftware(AMDILDeviceInfo::ByteLDSOps)) {
        opcode = isSExtLoadInst(MI) ? AMDIL::SHRi16i32rr : AMDIL::USHRi16i32rr;
        expandIntegerExtend(MI, AMDIL::SHLi16i32rr, opcode, 24, dataReg);
      }
      break;
    case AMDIL::GPRV2I16RegClassID:
      opcode =
        isSExtLoadInst(MI) ? AMDIL::SHRv2i16i32rr : AMDIL::USHRv2i16i32rr;
      expandIntegerExtend(MI, AMDIL::SHLv2i16i32rr, opcode, 24, dataReg);
      break;
    case AMDIL::GPRV4I8RegClassID:
      opcode = isSExtLoadInst(MI) ? AMDIL::SHRv4i8i32rr : AMDIL::USHRv4i8i32rr;
      expandIntegerExtend(MI, AMDIL::SHLv4i8i32rr, opcode, 24, dataReg);
      break;
    case AMDIL::GPRV4I16RegClassID:
      opcode =
        isSExtLoadInst(MI) ? AMDIL::SHRv4i16i32rr : AMDIL::USHRv4i16i32rr;
      expandIntegerExtend(MI, AMDIL::SHLv4i16i32rr, opcode, 24, dataReg);
      break;
    case AMDIL::GPRI32RegClassID:
      // We can be a i8 or i16 bit sign extended value
      if (isNbitType(mType, 8) || getMemorySize(MI) == 1) {
        opcode = isSExtLoadInst(MI) ? AMDIL::SHRi32i32rr : AMDIL::USHRi32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLi32i32rr, opcode, 24, dataReg);
      } else if (isNbitType(mType, 16) || getMemorySize(MI) == 2) {
        opcode = isSExtLoadInst(MI) ? AMDIL::SHRi32i32rr : AMDIL::USHRi32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLi32i32rr, opcode, 16, dataReg);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRV2I32RegClassID:
      // We can be a v2i8 or v2i16 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 2) {
        opcode =
          isSExtLoadInst(MI) ? AMDIL::SHRv2i32i32rr : AMDIL::USHRv2i32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLv2i32i32rr, opcode, 24, dataReg);
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 4) {
        opcode =
          isSExtLoadInst(MI) ? AMDIL::SHRv2i32i32rr : AMDIL::USHRv2i32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLv2i32i32rr, opcode, 16, dataReg);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRV4I32RegClassID:
      // We can be a v4i8 or v4i16 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 4) {
        opcode =
          isSExtLoadInst(MI) ? AMDIL::SHRv4i32i32rr : AMDIL::USHRv4i32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLv4i32i32rr, opcode, 24, dataReg);
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 8) {
        opcode =
          isSExtLoadInst(MI) ? AMDIL::SHRv4i32i32rr : AMDIL::USHRv4i32i32rr;
        expandIntegerExtend(MI, AMDIL::SHLv4i32i32rr, opcode, 16, dataReg);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRI64RegClassID:
      // We can be a i8, i16 or i32 bit sign extended value
      if (isNbitType(mType, 8) || getMemorySize(MI) == 1) {
        expandLongExtend(MI, 1, 8, isSExtLoadInst(MI), dataReg);
      } else if (isNbitType(mType, 16) || getMemorySize(MI) == 2) {
        expandLongExtend(MI, 1, 16, isSExtLoadInst(MI), dataReg);
      } else if (isNbitType(mType, 32) || getMemorySize(MI) == 4) {
        expandLongExtend(MI, 1, 32, isSExtLoadInst(MI), dataReg);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRV2I64RegClassID:
      // We can be a v2i8, v2i16 or v2i32 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 2) {
        expandLongExtend(MI, 2, 8, isSExtLoadInst(MI), dataReg);
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 4) {
        expandLongExtend(MI, 2, 16, isSExtLoadInst(MI), dataReg);
      } else if (isNbitType(mType, 32, false) || getMemorySize(MI) == 8) {
        expandLongExtend(MI, 2, 32, isSExtLoadInst(MI), dataReg);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRF32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOFf32r), dataReg)
      .addReg(dataReg);
      break;
    case AMDIL::GPRV2F32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOFv2f32r), dataReg)
      .addReg(dataReg);
      break;
    case AMDIL::GPRV4F32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOFv4f32r), dataReg)
      .addReg(dataReg);
      break;
    case AMDIL::GPRF64RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::FTODr), dataReg)
      .addReg(dataReg);
      break;
    case AMDIL::GPRV2F64RegClassID:
      if (mTRI->getSubReg(dataReg, sub_xy_comp)) {
        BuildMI(*mBB, MI, DL,
                mTII->get(AMDIL::FTODr), getCompReg(dataReg, sub_zw_comp))
        .addReg(getCompReg(dataReg, sub_y_comp));
        BuildMI(*mBB, MI, DL,
                mTII->get(AMDIL::FTODr), getCompReg(dataReg, sub_xy_comp))
        .addReg(getCompReg(dataReg, sub_x_comp));
      } else {
        BuildMI(*mBB, MI, DL,
                mTII->get(AMDIL::FTODr), getCompReg(dataReg, sub_xy_comp))
        .addReg(getCompReg(dataReg, sub_z_comp));
        BuildMI(*mBB, MI, DL,
                mTII->get(AMDIL::FTODr), getCompReg(dataReg, sub_zw_comp))
        .addReg(getCompReg(dataReg, sub_w_comp));
      }
      break;
    }
  }
}
void
AMDILIOExpansionImpl::expandTruncData(MachineInstr *MI, uint32_t &dataReg)
{
  if (!isTruncStoreInst(MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  switch (MI->getOpcode()) {
  default:
    MI->dump();
    assert(!"Found a trunc store instructions we don't handle!");
    break;
  case AMDIL::GLOBALTRUNCSTORE64i64i8r:  // case AMDIL::GLOBALTRUNCSTORE64i64i8i:
  case AMDIL::LOCALTRUNCSTORE64i64i8r:  // case AMDIL::LOCALTRUNCSTORE64i64i8i:
  case AMDIL::REGIONTRUNCSTORE64i64i8r:  // case AMDIL::REGIONTRUNCSTORE64i64i8i:
  case AMDIL::PRIVATETRUNCSTORE64i64i8r:  // case AMDIL::PRIVATETRUNCSTORE64i64i8i:
  case AMDIL::GLOBALTRUNCSTOREi64i8r:  // case AMDIL::GLOBALTRUNCSTOREi64i8i:
  case AMDIL::LOCALTRUNCSTOREi64i8r:  // case AMDIL::LOCALTRUNCSTOREi64i8i:
  case AMDIL::REGIONTRUNCSTOREi64i8r:  // case AMDIL::REGIONTRUNCSTOREi64i8i:
  case AMDIL::PRIVATETRUNCSTOREi64i8r:  // case AMDIL::PRIVATETRUNCSTOREi64i8i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOi64r), AMDIL::Rx1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rx1011;
  case AMDIL::GLOBALTRUNCSTORE64i16i8r:  // case AMDIL::GLOBALTRUNCSTORE64i16i8i:
  case AMDIL::LOCALTRUNCSTORE64i16i8r:  // case AMDIL::LOCALTRUNCSTORE64i16i8i:
  case AMDIL::REGIONTRUNCSTORE64i16i8r:  // case AMDIL::REGIONTRUNCSTORE64i16i8i:
  case AMDIL::PRIVATETRUNCSTORE64i16i8r:  // case AMDIL::PRIVATETRUNCSTORE64i16i8i:
  case AMDIL::GLOBALTRUNCSTOREi16i8r:  // case AMDIL::GLOBALTRUNCSTOREi16i8i:
  case AMDIL::LOCALTRUNCSTOREi16i8r:  // case AMDIL::LOCALTRUNCSTOREi16i8i:
  case AMDIL::REGIONTRUNCSTOREi16i8r:  // case AMDIL::REGIONTRUNCSTOREi16i8i:
  case AMDIL::PRIVATETRUNCSTOREi16i8r:  // case AMDIL::PRIVATETRUNCSTOREi16i8i:
  case AMDIL::GLOBALTRUNCSTORE64i32i8r:  // case AMDIL::GLOBALTRUNCSTORE64i32i8i:
  case AMDIL::LOCALTRUNCSTORE64i32i8r:  // case AMDIL::LOCALTRUNCSTORE64i32i8i:
  case AMDIL::REGIONTRUNCSTORE64i32i8r:  // case AMDIL::REGIONTRUNCSTORE64i32i8i:
  case AMDIL::PRIVATETRUNCSTORE64i32i8r:  // case AMDIL::PRIVATETRUNCSTORE64i32i8i:
  case AMDIL::GLOBALTRUNCSTOREi32i8r:  // case AMDIL::GLOBALTRUNCSTOREi32i8i:
  case AMDIL::LOCALTRUNCSTOREi32i8r:  // case AMDIL::LOCALTRUNCSTOREi32i8i:
  case AMDIL::REGIONTRUNCSTOREi32i8r:  // case AMDIL::REGIONTRUNCSTOREi32i8i:
  case AMDIL::PRIVATETRUNCSTOREi32i8r:  // case AMDIL::PRIVATETRUNCSTOREi32i8i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFF));
    dataReg = AMDIL::Rx1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v2i64i8r:  // case AMDIL::GLOBALTRUNCSTORE64v2i64i8i:
  case AMDIL::LOCALTRUNCSTORE64v2i64i8r:  // case AMDIL::LOCALTRUNCSTORE64v2i64i8i:
  case AMDIL::REGIONTRUNCSTORE64v2i64i8r:  // case AMDIL::REGIONTRUNCSTORE64v2i64i8i:
  case AMDIL::PRIVATETRUNCSTORE64v2i64i8r:  // case AMDIL::PRIVATETRUNCSTORE64v2i64i8i:
  case AMDIL::GLOBALTRUNCSTOREv2i64i8r:  // case AMDIL::GLOBALTRUNCSTOREv2i64i8i:
  case AMDIL::LOCALTRUNCSTOREv2i64i8r:  // case AMDIL::LOCALTRUNCSTOREv2i64i8i:
  case AMDIL::REGIONTRUNCSTOREv2i64i8r:  // case AMDIL::REGIONTRUNCSTOREv2i64i8i:
  case AMDIL::PRIVATETRUNCSTOREv2i64i8r:  // case AMDIL::PRIVATETRUNCSTOREv2i64i8i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rxy1011;
  case AMDIL::GLOBALTRUNCSTORE64v2i16i8r:  // case AMDIL::GLOBALTRUNCSTORE64v2i16i8i:
  case AMDIL::LOCALTRUNCSTORE64v2i16i8r:  // case AMDIL::LOCALTRUNCSTORE64v2i16i8i:
  case AMDIL::REGIONTRUNCSTORE64v2i16i8r:  // case AMDIL::REGIONTRUNCSTORE64v2i16i8i:
  case AMDIL::PRIVATETRUNCSTORE64v2i16i8r:  // case AMDIL::PRIVATETRUNCSTORE64v2i16i8i:
  case AMDIL::GLOBALTRUNCSTOREv2i16i8r:  // case AMDIL::GLOBALTRUNCSTOREv2i16i8i:
  case AMDIL::LOCALTRUNCSTOREv2i16i8r:  // case AMDIL::LOCALTRUNCSTOREv2i16i8i:
  case AMDIL::REGIONTRUNCSTOREv2i16i8r:  // case AMDIL::REGIONTRUNCSTOREv2i16i8i:
  case AMDIL::PRIVATETRUNCSTOREv2i16i8r:  // case AMDIL::PRIVATETRUNCSTOREv2i16i8i:
  case AMDIL::GLOBALTRUNCSTORE64v2i32i8r:  // case AMDIL::GLOBALTRUNCSTORE64v2i32i8i:
  case AMDIL::LOCALTRUNCSTORE64v2i32i8r:  // case AMDIL::LOCALTRUNCSTORE64v2i32i8i:
  case AMDIL::REGIONTRUNCSTORE64v2i32i8r:  // case AMDIL::REGIONTRUNCSTORE64v2i32i8i:
  case AMDIL::PRIVATETRUNCSTORE64v2i32i8r:  // case AMDIL::PRIVATETRUNCSTORE64v2i32i8i:
  case AMDIL::GLOBALTRUNCSTOREv2i32i8r:  // case AMDIL::GLOBALTRUNCSTOREv2i32i8i:
  case AMDIL::LOCALTRUNCSTOREv2i32i8r:  // case AMDIL::LOCALTRUNCSTOREv2i32i8i:
  case AMDIL::REGIONTRUNCSTOREv2i32i8r:  // case AMDIL::REGIONTRUNCSTOREv2i32i8i:
  case AMDIL::PRIVATETRUNCSTOREv2i32i8r:  // case AMDIL::PRIVATETRUNCSTOREv2i32i8i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDv2i32rr), AMDIL::Rxy1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFF));
    dataReg = AMDIL::Rxy1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v4i16i8r:  // case AMDIL::GLOBALTRUNCSTORE64v4i16i8i:
  case AMDIL::LOCALTRUNCSTORE64v4i16i8r:  // case AMDIL::LOCALTRUNCSTORE64v4i16i8i:
  case AMDIL::REGIONTRUNCSTORE64v4i16i8r:  // case AMDIL::REGIONTRUNCSTORE64v4i16i8i:
  case AMDIL::PRIVATETRUNCSTORE64v4i16i8r:  // case AMDIL::PRIVATETRUNCSTORE64v4i16i8i:
  case AMDIL::GLOBALTRUNCSTOREv4i16i8r:  // case AMDIL::GLOBALTRUNCSTOREv4i16i8i:
  case AMDIL::LOCALTRUNCSTOREv4i16i8r:  // case AMDIL::LOCALTRUNCSTOREv4i16i8i:
  case AMDIL::REGIONTRUNCSTOREv4i16i8r:  // case AMDIL::REGIONTRUNCSTOREv4i16i8i:
  case AMDIL::PRIVATETRUNCSTOREv4i16i8r:  // case AMDIL::PRIVATETRUNCSTOREv4i16i8i:
  case AMDIL::GLOBALTRUNCSTORE64v4i32i8r:  // case AMDIL::GLOBALTRUNCSTORE64v4i32i8i:
  case AMDIL::LOCALTRUNCSTORE64v4i32i8r:  // case AMDIL::LOCALTRUNCSTORE64v4i32i8i:
  case AMDIL::REGIONTRUNCSTORE64v4i32i8r:  // case AMDIL::REGIONTRUNCSTORE64v4i32i8i:
  case AMDIL::PRIVATETRUNCSTORE64v4i32i8r:  // case AMDIL::PRIVATETRUNCSTORE64v4i32i8i:
  case AMDIL::GLOBALTRUNCSTOREv4i32i8r:  // case AMDIL::GLOBALTRUNCSTOREv4i32i8i:
  case AMDIL::LOCALTRUNCSTOREv4i32i8r:  // case AMDIL::LOCALTRUNCSTOREv4i32i8i:
  case AMDIL::REGIONTRUNCSTOREv4i32i8r:  // case AMDIL::REGIONTRUNCSTOREv4i32i8i:
  case AMDIL::PRIVATETRUNCSTOREv4i32i8r:  // case AMDIL::PRIVATETRUNCSTOREv4i32i8i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDv4i32rr), AMDIL::R1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFF));
    dataReg = AMDIL::R1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64i64i16r:  // case AMDIL::GLOBALTRUNCSTORE64i64i16i:
  case AMDIL::LOCALTRUNCSTORE64i64i16r:  // case AMDIL::LOCALTRUNCSTORE64i64i16i:
  case AMDIL::REGIONTRUNCSTORE64i64i16r:  // case AMDIL::REGIONTRUNCSTORE64i64i16i:
  case AMDIL::PRIVATETRUNCSTORE64i64i16r:  // case AMDIL::PRIVATETRUNCSTORE64i64i16i:
  case AMDIL::GLOBALTRUNCSTOREi64i16r:  // case AMDIL::GLOBALTRUNCSTOREi64i16i:
  case AMDIL::LOCALTRUNCSTOREi64i16r:  // case AMDIL::LOCALTRUNCSTOREi64i16i:
  case AMDIL::REGIONTRUNCSTOREi64i16r:  // case AMDIL::REGIONTRUNCSTOREi64i16i:
  case AMDIL::PRIVATETRUNCSTOREi64i16r:  // case AMDIL::PRIVATETRUNCSTOREi64i16i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rxy1011;
  case AMDIL::GLOBALTRUNCSTORE64i32i16r:  // case AMDIL::GLOBALTRUNCSTORE64i32i16i:
  case AMDIL::LOCALTRUNCSTORE64i32i16r:  // case AMDIL::LOCALTRUNCSTORE64i32i16i:
  case AMDIL::REGIONTRUNCSTORE64i32i16r:  // case AMDIL::REGIONTRUNCSTORE64i32i16i:
  case AMDIL::PRIVATETRUNCSTORE64i32i16r:  // case AMDIL::PRIVATETRUNCSTORE64i32i16i:
  case AMDIL::GLOBALTRUNCSTOREi32i16r:  // case AMDIL::GLOBALTRUNCSTOREi32i16i:
  case AMDIL::LOCALTRUNCSTOREi32i16r:  // case AMDIL::LOCALTRUNCSTOREi32i16i:
  case AMDIL::REGIONTRUNCSTOREi32i16r:  // case AMDIL::REGIONTRUNCSTOREi32i16i:
  case AMDIL::PRIVATETRUNCSTOREi32i16r:  // case AMDIL::PRIVATETRUNCSTOREi32i16i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFFFF));
    dataReg = AMDIL::Rx1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v2i64i16r:  // case AMDIL::GLOBALTRUNCSTORE64v2i64i16i:
  case AMDIL::LOCALTRUNCSTORE64v2i64i16r:  // case AMDIL::LOCALTRUNCSTORE64v2i64i16i:
  case AMDIL::REGIONTRUNCSTORE64v2i64i16r:  // case AMDIL::REGIONTRUNCSTORE64v2i64i16i:
  case AMDIL::PRIVATETRUNCSTORE64v2i64i16r:  // case AMDIL::PRIVATETRUNCSTORE64v2i64i16i:
  case AMDIL::GLOBALTRUNCSTOREv2i64i16r:  // case AMDIL::GLOBALTRUNCSTOREv2i64i16i:
  case AMDIL::LOCALTRUNCSTOREv2i64i16r:  // case AMDIL::LOCALTRUNCSTOREv2i64i16i:
  case AMDIL::REGIONTRUNCSTOREv2i64i16r:  // case AMDIL::REGIONTRUNCSTOREv2i64i16i:
  case AMDIL::PRIVATETRUNCSTOREv2i64i16r:  // case AMDIL::PRIVATETRUNCSTOREv2i64i16i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rxy1011;
  case AMDIL::GLOBALTRUNCSTORE64v2i32i16r:  // case AMDIL::GLOBALTRUNCSTORE64v2i32i16i:
  case AMDIL::LOCALTRUNCSTORE64v2i32i16r:  // case AMDIL::LOCALTRUNCSTORE64v2i32i16i:
  case AMDIL::REGIONTRUNCSTORE64v2i32i16r:  // case AMDIL::REGIONTRUNCSTORE64v2i32i16i:
  case AMDIL::PRIVATETRUNCSTORE64v2i32i16r:  // case AMDIL::PRIVATETRUNCSTORE64v2i32i16i:
  case AMDIL::GLOBALTRUNCSTOREv2i32i16r:  // case AMDIL::GLOBALTRUNCSTOREv2i32i16i:
  case AMDIL::LOCALTRUNCSTOREv2i32i16r:  // case AMDIL::LOCALTRUNCSTOREv2i32i16i:
  case AMDIL::REGIONTRUNCSTOREv2i32i16r:  // case AMDIL::REGIONTRUNCSTOREv2i32i16i:
  case AMDIL::PRIVATETRUNCSTOREv2i32i16r:  // case AMDIL::PRIVATETRUNCSTOREv2i32i16i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDv2i32rr), AMDIL::Rxy1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFFFF));
    dataReg = AMDIL::Rxy1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v4i32i16r:  // case AMDIL::GLOBALTRUNCSTORE64v4i32i16i:
  case AMDIL::LOCALTRUNCSTORE64v4i32i16r:  // case AMDIL::LOCALTRUNCSTORE64v4i32i16i:
  case AMDIL::REGIONTRUNCSTORE64v4i32i16r:  // case AMDIL::REGIONTRUNCSTORE64v4i32i16i:
  case AMDIL::PRIVATETRUNCSTORE64v4i32i16r:  // case AMDIL::PRIVATETRUNCSTORE64v4i32i16i:
  case AMDIL::GLOBALTRUNCSTOREv4i32i16r:  // case AMDIL::GLOBALTRUNCSTOREv4i32i16i:
  case AMDIL::LOCALTRUNCSTOREv4i32i16r:  // case AMDIL::LOCALTRUNCSTOREv4i32i16i:
  case AMDIL::REGIONTRUNCSTOREv4i32i16r:  // case AMDIL::REGIONTRUNCSTOREv4i32i16i:
  case AMDIL::PRIVATETRUNCSTOREv4i32i16r:  // case AMDIL::PRIVATETRUNCSTOREv4i32i16i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::ANDv4i32rr), AMDIL::R1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFFFF));
    dataReg = AMDIL::R1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64i64i32r:  // case AMDIL::GLOBALTRUNCSTORE64i64i32i:
  case AMDIL::LOCALTRUNCSTORE64i64i32r:  // case AMDIL::LOCALTRUNCSTORE64i64i32i:
  case AMDIL::REGIONTRUNCSTORE64i64i32r:  // case AMDIL::REGIONTRUNCSTORE64i64i32i:
  case AMDIL::PRIVATETRUNCSTORE64i64i32r:  // case AMDIL::PRIVATETRUNCSTORE64i64i32i:
  case AMDIL::GLOBALTRUNCSTOREi64i32r:  // case AMDIL::GLOBALTRUNCSTOREi64i32i:
  case AMDIL::LOCALTRUNCSTOREi64i32r:  // case AMDIL::LOCALTRUNCSTOREi64i32i:
  case AMDIL::REGIONTRUNCSTOREi64i32r:  // case AMDIL::REGIONTRUNCSTOREi64i32i:
  case AMDIL::PRIVATETRUNCSTOREi64i32r:  // case AMDIL::PRIVATETRUNCSTOREi64i32i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOi64r), AMDIL::Rx1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rx1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v2i64i32r:  // case AMDIL::GLOBALTRUNCSTORE64v2i64i32i:
  case AMDIL::LOCALTRUNCSTORE64v2i64i32r:  // case AMDIL::LOCALTRUNCSTORE64v2i64i32i:
  case AMDIL::REGIONTRUNCSTORE64v2i64i32r:  // case AMDIL::REGIONTRUNCSTORE64v2i64i32i:
  case AMDIL::PRIVATETRUNCSTORE64v2i64i32r:  // case AMDIL::PRIVATETRUNCSTORE64v2i64i32i:
  case AMDIL::GLOBALTRUNCSTOREv2i64i32r:  // case AMDIL::GLOBALTRUNCSTOREv2i64i32i:
  case AMDIL::LOCALTRUNCSTOREv2i64i32r:  // case AMDIL::LOCALTRUNCSTOREv2i64i32i:
  case AMDIL::REGIONTRUNCSTOREv2i64i32r:  // case AMDIL::REGIONTRUNCSTOREv2i64i32i:
  case AMDIL::PRIVATETRUNCSTOREv2i64i32r:  // case AMDIL::PRIVATETRUNCSTOREv2i64i32i:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    dataReg = AMDIL::Rxy1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64f64f32r:  // case AMDIL::GLOBALTRUNCSTORE64f64f32i:
  case AMDIL::LOCALTRUNCSTORE64f64f32r:  // case AMDIL::LOCALTRUNCSTORE64f64f32i:
  case AMDIL::REGIONTRUNCSTORE64f64f32r:  // case AMDIL::REGIONTRUNCSTORE64f64f32i:
  case AMDIL::PRIVATETRUNCSTORE64f64f32r:  // case AMDIL::PRIVATETRUNCSTORE64f64f32i:
  case AMDIL::GLOBALTRUNCSTOREf64f32r:  // case AMDIL::GLOBALTRUNCSTOREf64f32i:
  case AMDIL::LOCALTRUNCSTOREf64f32r:  // case AMDIL::LOCALTRUNCSTOREf64f32i:
  case AMDIL::REGIONTRUNCSTOREf64f32r:  // case AMDIL::REGIONTRUNCSTOREf64f32i:
  case AMDIL::PRIVATETRUNCSTOREf64f32r:  // case AMDIL::PRIVATETRUNCSTOREf64f32i:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOFr),
            AMDIL::Rx1011).addReg(dataReg);
    dataReg = AMDIL::Rx1011;
    break;
  case AMDIL::GLOBALTRUNCSTORE64v2f64f32r:  // case AMDIL::GLOBALTRUNCSTORE64v2f64f32i:
  case AMDIL::LOCALTRUNCSTORE64v2f64f32r:  // case AMDIL::LOCALTRUNCSTORE64v2f64f32i:
  case AMDIL::REGIONTRUNCSTORE64v2f64f32r:  // case AMDIL::REGIONTRUNCSTORE64v2f64f32i:
  case AMDIL::PRIVATETRUNCSTORE64v2f64f32r:  // case AMDIL::PRIVATETRUNCSTORE64v2f64f32i:
  case AMDIL::GLOBALTRUNCSTOREv2f64f32r:  // case AMDIL::GLOBALTRUNCSTOREv2f64f32i:
  case AMDIL::LOCALTRUNCSTOREv2f64f32r:  // case AMDIL::LOCALTRUNCSTOREv2f64f32i:
  case AMDIL::REGIONTRUNCSTOREv2f64f32r:  // case AMDIL::REGIONTRUNCSTOREv2f64f32i:
  case AMDIL::PRIVATETRUNCSTOREv2f64f32r:  // case AMDIL::PRIVATETRUNCSTOREv2f64f32i:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOFr),
            AMDIL::Rx1011).addReg(getCompReg(dataReg, sub_xy_comp));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOFr),
            AMDIL::Ry1011).addReg(getCompReg(dataReg, sub_zw_comp));
    dataReg = AMDIL::Rxy1011;
    break;
  }
}
uint32_t
AMDILIOExpansionImpl::getPackedReg(uint32_t &dataReg, uint32_t id)
{
  switch (id) {
  default:
    return dataReg;
  case UNPACK_V2I8:
  case UNPACK_V2I16:
  case UNPACK_V4I8:
    return getCompReg(dataReg, sub_x_comp, sub_z_comp);
  case UNPACK_V4I16:
    return getCompReg(dataReg, sub_xy_comp, sub_zw_comp);
  }
}
void
AMDILIOExpansionImpl::expandAddressCalc(MachineInstr *MI, uint32_t &addyReg)
{
  if (!isAddrCalcInstr(MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  bool is64bit = is64bitLSOp(MI);
  uint32_t newReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  uint32_t addInst = (is64bit) ? AMDIL::ADDi64rr : AMDIL::ADDi32rr;
  if (isPrivateInst(MI) && (isPtrLoadInst(MI)
                            || (isPtrStoreInst(MI)
                                && mSTM->device()->usesSoftware(AMDILDeviceInfo
                                                                ::PrivateMem))))
  {
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            newReg).addReg(addyReg).addReg(AMDIL::T1);
    addyReg = newReg;
  } else if (isLocalInst(MI) && (isPtrStoreInst(MI) || isPtrLoadInst(MI))) {
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            newReg).addReg(addyReg).addReg(AMDIL::T2);
    addyReg = newReg;
  } else if (isConstantPoolInst(MI) && isPtrLoadInst(MI) &&
             MI->getOperand(1).isReg()) {
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            newReg).addReg(addyReg).addReg(AMDIL::SDP);
    addyReg = newReg;
  }
}
void
AMDILIOExpansionImpl::expandLoadStartCode(MachineInstr *MI, uint32_t &addyReg)
{
  DebugLoc DL = MI->getDebugLoc();
  bool is64bit = is64bitLSOp(MI);
  if (MI->getOperand(2).isReg()) {
    uint32_t newReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
    uint32_t addInst = (is64bit) ? AMDIL::ADDi64rr : AMDIL::ADDi32rr;
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            newReg).addReg(addyReg)
    .addReg(MI->getOperand(2).getReg());
    addyReg = newReg;
  }
  expandAddressCalc(MI, addyReg);
}
void
AMDILIOExpansionImpl::emitStaticCPLoad(MachineInstr* MI,
                                       int swizzle,
                                       int id,
                                       bool ExtFPLoad,
                                       uint32_t &dataReg)
{
  DebugLoc DL = MI->getDebugLoc();
  switch(swizzle) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(ExtFPLoad
                                    ? AMDIL::DTOFr : AMDIL::COPY),
            dataReg)
    .addImm(id);
    break;
  case 1:
  case 2:
  case 3:
    BuildMI(*mBB, MI, DL, mTII->get(ExtFPLoad
                                    ? AMDIL::DTOFr : AMDIL::COPY),
            AMDIL::Rx1001)
    .addImm(id);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VINSERTv4i32rr),
            dataReg)
    .addReg(dataReg)
    .addReg(AMDIL::Rx1001)
    .addImm(swizzle + 1);
    break;
  };
}
void
AMDILIOExpansionImpl::emitCPInst(MachineInstr* MI,
                                 const Constant* C,
                                 AMDILKernelManager* KM,
                                 int swizzle,
                                 bool ExtFPLoad,
                                 uint32_t &dataReg)
{
  if (const ConstantFP* CFP = dyn_cast<ConstantFP>(C)) {
    if (CFP->getType()->isFloatTy()) {
      uint32_t val = (uint32_t)(CFP->getValueAPF().bitcastToAPInt()
                                .getZExtValue());
      uint32_t id = mMFI->addi32Literal(val);
      if (!id) {
        const APFloat &APF = CFP->getValueAPF();
        union dtol_union {
          double d;
          uint64_t ul;
        } conv;
        if (&APF.getSemantics()
            == (const llvm::fltSemantics*)&APFloat::IEEEsingle) {
          float fval = APF.convertToFloat();
          conv.d = (double)fval;
        } else {
          conv.d = APF.convertToDouble();
        }
        id = mMFI->addi64Literal(conv.ul);
      }
      emitStaticCPLoad(MI, swizzle, id, ExtFPLoad, dataReg);
    } else {
      const APFloat &APF = CFP->getValueAPF();
      union ftol_union {
        double d;
        uint64_t ul;
      } conv;
      if (&APF.getSemantics()
          == (const llvm::fltSemantics*)&APFloat::IEEEsingle) {
        float fval = APF.convertToFloat();
        conv.d = (double)fval;
      } else {
        conv.d = APF.convertToDouble();
      }
      uint32_t id = mMFI->getLitIdx(conv.ul);
      if (!id) {
        id = mMFI->getLitIdx((uint32_t)conv.ul);
      }
      emitStaticCPLoad(MI, swizzle, id, ExtFPLoad, dataReg);
    }
  } else if (const ConstantInt* CI = dyn_cast<ConstantInt>(C)) {
    int64_t val = 0;
    if (CI) {
      val = CI->getSExtValue();
    }
    if (CI->getBitWidth() == 64) {
      emitStaticCPLoad(MI, swizzle, mMFI->addi64Literal(
                         val), ExtFPLoad, dataReg);
    } else {
      emitStaticCPLoad(MI, swizzle, mMFI->addi32Literal(
                         val), ExtFPLoad, dataReg);
    }
  } else if (const ConstantArray* CA = dyn_cast<ConstantArray>(C)) {
    uint32_t size = CA->getNumOperands();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CA->getOperand(0), KM, x, ExtFPLoad, dataReg);
    }
  } else if (const ConstantAggregateZero* CAZ
               = dyn_cast<ConstantAggregateZero>(C)) {
    if (CAZ->isNullValue()) {
      emitStaticCPLoad(MI, swizzle, mMFI->addi32Literal(0), ExtFPLoad, dataReg);
    }
  } else if (const ConstantStruct* CS = dyn_cast<ConstantStruct>(C)) {
    uint32_t size = CS->getNumOperands();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CS->getOperand(0), KM, x, ExtFPLoad, dataReg);
    }
  } else if (const ConstantVector* CV = dyn_cast<ConstantVector>(C)) {
    // TODO: Make this handle vectors natively up to the correct
    // size
    uint32_t size = CV->getNumOperands();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CV->getOperand(0), KM, x, ExtFPLoad, dataReg);
    }
  } else if (const ConstantDataVector* CV = dyn_cast<ConstantDataVector>(C)) {
    // TODO: Make this handle vectors natively up to the correct
    // size
    uint32_t size = CV->getNumElements();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CV->getElementAsConstant(0), KM, x, ExtFPLoad, dataReg);
    }
  } else {
    // TODO: Do we really need to handle ConstantPointerNull?
    // What about BlockAddress, ConstantExpr and Undef?
    // How would these even be generated by a valid CL program?
    assert(0 && "Found a constant type that I don't know how to handle");
  }
}
uint32_t
AMDILIOExpansionImpl::getCompReg(uint32_t reg,
                                 uint32_t subIdx0,
                                 uint32_t subIdx1)
{
  uint32_t subreg = mTRI->getSubReg(reg, subIdx0);
  if (!subreg) {
    subreg = mTRI->getSubReg(reg, subIdx1);
  }
  assert(subreg
         && "Found a case where the register does not have either sub-index!");
  // Just incase we hit this assert, lets as least use a valid register so
  // we don't have possible crashes in release mode.
  if (!subreg) subreg = reg;
  return subreg;
}
