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
using namespace llvm;

char AMDILIOExpansion::ID = 0;
namespace llvm
{
FunctionPass*
createAMDILIOExpansion(TargetMachine &TM, CodeGenOpt::Level OptLevel)
{
  return TM.getSubtarget<AMDILSubtarget>()
         .device()->getIOExpansion(TM, OptLevel);
}
}

AMDILIOExpansion::AMDILIOExpansion(TargetMachine &tm,
                                   CodeGenOpt::Level OptLevel) :
  MachineFunctionPass(ID), TM(tm)
{
  mSTM = &tm.getSubtarget<AMDILSubtarget>();
  mDebug = DEBUGME;
  mTII = tm.getInstrInfo();
  mKM = NULL;
}

AMDILIOExpansion::~AMDILIOExpansion()
{
}

bool
AMDILIOExpansion::runOnMachineFunction(MachineFunction &MF)
{
  mKM = const_cast<AMDILKernelManager*>(mSTM->getKernelManager());
  mMFI = MF.getInfo<AMDILMachineFunctionInfo>();
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
const char *AMDILIOExpansion::getPassName() const
{
  return "AMDIL Generic IO Expansion Pass";
}
bool
AMDILIOExpansion::isIOInstruction(MachineInstr *MI)
{
  if (!MI) {
    return false;
  }
  if (isLoadInst(TM, MI) || isStoreInst(TM, MI)) {
    return true;
  }
  return false;
}
void
AMDILIOExpansion::expandIOInstruction(MachineInstr *MI)
{
  assert(isIOInstruction(MI) && "Must be an IO instruction to "
         "be passed to this function!");
  if (isLoadInst(TM, MI)) {
    if (isGlobalInst(TM, MI)) {
      expandGlobalLoad(MI);
    } else if (isRegionInst(TM, MI)) {
      expandRegionLoad(MI);
    } else if (isPrivateInst(TM, MI)) {
      expandPrivateLoad(MI);
    } else if (isLocalInst(TM, MI)) {
      expandLocalLoad(MI);
    } else if (isConstantInst(TM, MI)) {
      if (isConstantPoolInst(TM, MI)) {
        expandConstantPoolLoad(MI);
      } else {
        expandConstantLoad(MI);
      }
    } else {
      assert(!"Found an unsupported load instruction!");
    }
  } else if (isStoreInst(TM, MI)) {
    if (isGlobalInst(TM, MI)) {
      expandGlobalStore(MI);
    } else if (isRegionInst(TM, MI)) {
      expandRegionStore(MI);
    } else if (isPrivateInst(TM, MI)) {
      expandPrivateStore(MI);
    } else if (isLocalInst(TM, MI)) {
      expandLocalStore(MI);
    } else {
      assert(!"Found an unsupported load instruction!");
    }
  } else {
    assert(!"Found an unsupported IO instruction!");
  }
}

bool
AMDILIOExpansion::isAddrCalcInstr(MachineInstr *MI)
{
  if (isPrivateInst(TM, MI) && isLoadInst(TM, MI)) {
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
  } else if (isConstantPoolInst(TM, MI) && isLoadInst(TM, MI)) {
    return MI->getOperand(1).isReg();
  } else if (isPrivateInst(TM, MI) && isStoreInst(TM, MI)) {
    return mSTM->device()->usesSoftware(AMDILDeviceInfo::PrivateMem);
  } else if (isLocalInst(TM, MI) && (isStoreInst(TM, MI) || isLoadInst(TM, MI))) {
    return mSTM->device()->usesSoftware(AMDILDeviceInfo::LocalMem);
  }
  return false;
}

bool
AMDILIOExpansion::isExtendLoad(MachineInstr *MI)
{
  return isSExtLoadInst(TM, MI) || isZExtLoadInst(TM, MI) || isAExtLoadInst(TM, MI);
}

bool
AMDILIOExpansion::isHardwareRegion(MachineInstr *MI)
{
  return (isRegionInst(TM, MI) && (isLoadInst(TM, MI) || isStoreInst(TM, MI)) &&
          mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem));
}
bool
AMDILIOExpansion::isHardwareLocal(MachineInstr *MI)
{
  return (isLocalInst(TM, MI) && (isLoadInst(TM, MI) || isStoreInst(TM, MI)) &&
          mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem));
}
bool
AMDILIOExpansion::isPackedData(MachineInstr *MI)
{
  switch(MI->getOpcode()) {
  default:
    if (isTruncStoreInst(TM, MI)) {
      switch (MI->getDesc().OpInfo[0].RegClass) {
      default:
        break;
      case AMDIL::GPRV2I64RegClassID:
      case AMDIL::GPRV2I32RegClassID:
        switch (getMemorySize(MI)) {
        case 2:
        case 4:
          return true;
        default:
          break;
        }
        break;
      case AMDIL::GPRV4I32RegClassID:
        switch (getMemorySize(MI)) {
        case 4:
        case 8:
          return true;
        default:
          break;
        }
        break;
      }
    }
    break;
    ExpandCaseToPackedTypes(AMDIL::CPOOLLOAD);
    ExpandCaseToPackedTypes(AMDIL::CPOOLSEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CPOOLZEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CPOOLAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::GLOBALLOAD);
    ExpandCaseToPackedTypes(AMDIL::GLOBALSEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::GLOBALZEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::GLOBALAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::LOCALLOAD);
    ExpandCaseToPackedTypes(AMDIL::LOCALSEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::LOCALZEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::LOCALAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::REGIONLOAD);
    ExpandCaseToPackedTypes(AMDIL::REGIONSEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::REGIONZEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::REGIONAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::PRIVATELOAD);
    ExpandCaseToPackedTypes(AMDIL::PRIVATESEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::PRIVATEZEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::PRIVATEAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTSEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTAEXTLOAD);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTZEXTLOAD);
    ExpandCaseToAllTruncTypes(AMDIL::GLOBALTRUNCSTORE)
    ExpandCaseToAllTruncTypes(AMDIL::PRIVATETRUNCSTORE);
    ExpandCaseToAllTruncTypes(AMDIL::LOCALTRUNCSTORE);
    ExpandCaseToAllTruncTypes(AMDIL::REGIONTRUNCSTORE);
    ExpandCaseToPackedTypes(AMDIL::GLOBALSTORE);
    ExpandCaseToPackedTypes(AMDIL::PRIVATESTORE);
    ExpandCaseToPackedTypes(AMDIL::LOCALSTORE);
    ExpandCaseToPackedTypes(AMDIL::REGIONSTORE);
    ExpandCaseToPackedTypes(AMDIL::CPOOLLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CPOOLSEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CPOOLZEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CPOOLAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::GLOBALLOAD64);
    ExpandCaseToPackedTypes(AMDIL::GLOBALSEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::GLOBALZEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::GLOBALAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::LOCALLOAD64);
    ExpandCaseToPackedTypes(AMDIL::LOCALSEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::LOCALZEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::LOCALAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::REGIONLOAD64);
    ExpandCaseToPackedTypes(AMDIL::REGIONSEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::REGIONZEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::REGIONAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::PRIVATELOAD64);
    ExpandCaseToPackedTypes(AMDIL::PRIVATESEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::PRIVATEZEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::PRIVATEAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTSEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTAEXTLOAD64);
    ExpandCaseToPackedTypes(AMDIL::CONSTANTZEXTLOAD64);
    ExpandCaseToAllTruncTypes(AMDIL::GLOBALTRUNCSTORE64)
    ExpandCaseToAllTruncTypes(AMDIL::PRIVATETRUNCSTORE64);
    ExpandCaseToAllTruncTypes(AMDIL::LOCALTRUNCSTORE64);
    ExpandCaseToAllTruncTypes(AMDIL::REGIONTRUNCSTORE64);
    ExpandCaseToPackedTypes(AMDIL::GLOBALSTORE64);
    ExpandCaseToPackedTypes(AMDIL::PRIVATESTORE64);
    ExpandCaseToPackedTypes(AMDIL::LOCALSTORE64);
    ExpandCaseToPackedTypes(AMDIL::REGIONSTORE64);
    return true;
  }
  return false;
}

bool
AMDILIOExpansion::isStaticCPLoad(MachineInstr *MI)
{
  if (isConstantPoolInst(TM, MI) && isLoadInst(TM, MI)) {
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
AMDILIOExpansion::isNbitType(Type *mType, uint32_t nBits, bool isScalar)
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
AMDILIOExpansion::isHardwareInst(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curInst;
  getAsmPrinterFlags(MI, curInst);
  return curInst.bits.HardwareInst;
}

uint32_t
AMDILIOExpansion::getDataReg(MachineInstr *MI)
{
  REG_PACKED_TYPE id = getPackedID(MI);
  switch (getMemorySize(MI)) {
  default:
    return AMDIL::R1011;
  case 4:
    if (id == UNPACK_V4I8
        || id == PACK_V4I8) {
      return AMDIL::R1011;
    } else if (id == UNPACK_V2I16
               || id == PACK_V2I16) {
      return AMDIL::Rxy1011;
    }
  case 2:
    if (id == UNPACK_V2I8
        || id == PACK_V2I8) {
      return AMDIL::Rxy1011;
    }
  case 1:
    return AMDIL::Rx1011;
  case 8:
    if (id == UNPACK_V4I16
        || id == PACK_V4I16) {
      return AMDIL::R1011;
    }
    return AMDIL::Rxy1011;
  }
}

REG_PACKED_TYPE
AMDILIOExpansion::getPackedID(MachineInstr *MI)
{
  switch (MI->getOpcode()) {
  default:
    break;
  case AMDIL::GLOBALTRUNCSTORE64_v2i64i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i64i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i64i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i64i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i64i8:
  case AMDIL::REGIONTRUNCSTORE_v2i64i8:
  case AMDIL::LOCALTRUNCSTORE_v2i64i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i64i8:
  case AMDIL::GLOBALTRUNCSTORE64_v2i32i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i32i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i32i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i32i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i32i8:
  case AMDIL::REGIONTRUNCSTORE_v2i32i8:
  case AMDIL::LOCALTRUNCSTORE_v2i32i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i32i8:
  case AMDIL::GLOBALTRUNCSTORE64_v2i16i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i16i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i16i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i16i8:
  case AMDIL::GLOBALSTORE64_v2i8:
  case AMDIL::LOCALSTORE64_v2i8:
  case AMDIL::REGIONSTORE64_v2i8:
  case AMDIL::PRIVATESTORE64_v2i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i16i8:
  case AMDIL::REGIONTRUNCSTORE_v2i16i8:
  case AMDIL::LOCALTRUNCSTORE_v2i16i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i16i8:
  case AMDIL::GLOBALSTORE_v2i8:
  case AMDIL::LOCALSTORE_v2i8:
  case AMDIL::REGIONSTORE_v2i8:
  case AMDIL::PRIVATESTORE_v2i8:
    return PACK_V2I8;
  case AMDIL::GLOBALTRUNCSTORE64_v4i32i8:
  case AMDIL::REGIONTRUNCSTORE64_v4i32i8:
  case AMDIL::LOCALTRUNCSTORE64_v4i32i8:
  case AMDIL::PRIVATETRUNCSTORE64_v4i32i8:
  case AMDIL::GLOBALTRUNCSTORE_v4i32i8:
  case AMDIL::REGIONTRUNCSTORE_v4i32i8:
  case AMDIL::LOCALTRUNCSTORE_v4i32i8:
  case AMDIL::PRIVATETRUNCSTORE_v4i32i8:
  case AMDIL::GLOBALTRUNCSTORE64_v4i16i8:
  case AMDIL::REGIONTRUNCSTORE64_v4i16i8:
  case AMDIL::LOCALTRUNCSTORE64_v4i16i8:
  case AMDIL::PRIVATETRUNCSTORE64_v4i16i8:
  case AMDIL::GLOBALSTORE64_v4i8:
  case AMDIL::LOCALSTORE64_v4i8:
  case AMDIL::REGIONSTORE64_v4i8:
  case AMDIL::PRIVATESTORE64_v4i8:
  case AMDIL::GLOBALTRUNCSTORE_v4i16i8:
  case AMDIL::REGIONTRUNCSTORE_v4i16i8:
  case AMDIL::LOCALTRUNCSTORE_v4i16i8:
  case AMDIL::PRIVATETRUNCSTORE_v4i16i8:
  case AMDIL::GLOBALSTORE_v4i8:
  case AMDIL::LOCALSTORE_v4i8:
  case AMDIL::REGIONSTORE_v4i8:
  case AMDIL::PRIVATESTORE_v4i8:
    return PACK_V4I8;
  case AMDIL::GLOBALTRUNCSTORE64_v2i64i16:
  case AMDIL::REGIONTRUNCSTORE64_v2i64i16:
  case AMDIL::LOCALTRUNCSTORE64_v2i64i16:
  case AMDIL::PRIVATETRUNCSTORE64_v2i64i16:
  case AMDIL::GLOBALTRUNCSTORE_v2i64i16:
  case AMDIL::REGIONTRUNCSTORE_v2i64i16:
  case AMDIL::LOCALTRUNCSTORE_v2i64i16:
  case AMDIL::PRIVATETRUNCSTORE_v2i64i16:
  case AMDIL::GLOBALTRUNCSTORE64_v2i32i16:
  case AMDIL::REGIONTRUNCSTORE64_v2i32i16:
  case AMDIL::LOCALTRUNCSTORE64_v2i32i16:
  case AMDIL::PRIVATETRUNCSTORE64_v2i32i16:
  case AMDIL::GLOBALSTORE64_v2i16:
  case AMDIL::LOCALSTORE64_v2i16:
  case AMDIL::REGIONSTORE64_v2i16:
  case AMDIL::PRIVATESTORE64_v2i16:
  case AMDIL::GLOBALTRUNCSTORE_v2i32i16:
  case AMDIL::REGIONTRUNCSTORE_v2i32i16:
  case AMDIL::LOCALTRUNCSTORE_v2i32i16:
  case AMDIL::PRIVATETRUNCSTORE_v2i32i16:
  case AMDIL::GLOBALSTORE_v2i16:
  case AMDIL::LOCALSTORE_v2i16:
  case AMDIL::REGIONSTORE_v2i16:
  case AMDIL::PRIVATESTORE_v2i16:
    return PACK_V2I16;
  case AMDIL::GLOBALTRUNCSTORE64_v4i32i16:
  case AMDIL::REGIONTRUNCSTORE64_v4i32i16:
  case AMDIL::LOCALTRUNCSTORE64_v4i32i16:
  case AMDIL::PRIVATETRUNCSTORE64_v4i32i16:
  case AMDIL::GLOBALSTORE64_v4i16:
  case AMDIL::LOCALSTORE64_v4i16:
  case AMDIL::REGIONSTORE64_v4i16:
  case AMDIL::PRIVATESTORE64_v4i16:
  case AMDIL::GLOBALTRUNCSTORE_v4i32i16:
  case AMDIL::REGIONTRUNCSTORE_v4i32i16:
  case AMDIL::LOCALTRUNCSTORE_v4i32i16:
  case AMDIL::PRIVATETRUNCSTORE_v4i32i16:
  case AMDIL::GLOBALSTORE_v4i16:
  case AMDIL::LOCALSTORE_v4i16:
  case AMDIL::REGIONSTORE_v4i16:
  case AMDIL::PRIVATESTORE_v4i16:
    return PACK_V4I16;

  case AMDIL::GLOBALLOAD64_v2i8:
  case AMDIL::GLOBALSEXTLOAD64_v2i8:
  case AMDIL::GLOBALAEXTLOAD64_v2i8:
  case AMDIL::GLOBALZEXTLOAD64_v2i8:
  case AMDIL::LOCALLOAD64_v2i8:
  case AMDIL::LOCALSEXTLOAD64_v2i8:
  case AMDIL::LOCALAEXTLOAD64_v2i8:
  case AMDIL::LOCALZEXTLOAD64_v2i8:
  case AMDIL::REGIONLOAD64_v2i8:
  case AMDIL::REGIONSEXTLOAD64_v2i8:
  case AMDIL::REGIONAEXTLOAD64_v2i8:
  case AMDIL::REGIONZEXTLOAD64_v2i8:
  case AMDIL::PRIVATELOAD64_v2i8:
  case AMDIL::PRIVATESEXTLOAD64_v2i8:
  case AMDIL::PRIVATEAEXTLOAD64_v2i8:
  case AMDIL::PRIVATEZEXTLOAD64_v2i8:
  case AMDIL::CONSTANTLOAD64_v2i8:
  case AMDIL::CONSTANTSEXTLOAD64_v2i8:
  case AMDIL::CONSTANTAEXTLOAD64_v2i8:
  case AMDIL::CONSTANTZEXTLOAD64_v2i8:
  case AMDIL::GLOBALLOAD_v2i8:
  case AMDIL::GLOBALSEXTLOAD_v2i8:
  case AMDIL::GLOBALAEXTLOAD_v2i8:
  case AMDIL::GLOBALZEXTLOAD_v2i8:
  case AMDIL::LOCALLOAD_v2i8:
  case AMDIL::LOCALSEXTLOAD_v2i8:
  case AMDIL::LOCALAEXTLOAD_v2i8:
  case AMDIL::LOCALZEXTLOAD_v2i8:
  case AMDIL::REGIONLOAD_v2i8:
  case AMDIL::REGIONSEXTLOAD_v2i8:
  case AMDIL::REGIONAEXTLOAD_v2i8:
  case AMDIL::REGIONZEXTLOAD_v2i8:
  case AMDIL::PRIVATELOAD_v2i8:
  case AMDIL::PRIVATESEXTLOAD_v2i8:
  case AMDIL::PRIVATEAEXTLOAD_v2i8:
  case AMDIL::PRIVATEZEXTLOAD_v2i8:
  case AMDIL::CONSTANTLOAD_v2i8:
  case AMDIL::CONSTANTSEXTLOAD_v2i8:
  case AMDIL::CONSTANTAEXTLOAD_v2i8:
  case AMDIL::CONSTANTZEXTLOAD_v2i8:
    return UNPACK_V2I8;

  case AMDIL::GLOBALLOAD64_v4i8:
  case AMDIL::GLOBALSEXTLOAD64_v4i8:
  case AMDIL::GLOBALAEXTLOAD64_v4i8:
  case AMDIL::GLOBALZEXTLOAD64_v4i8:
  case AMDIL::LOCALLOAD64_v4i8:
  case AMDIL::LOCALSEXTLOAD64_v4i8:
  case AMDIL::LOCALAEXTLOAD64_v4i8:
  case AMDIL::LOCALZEXTLOAD64_v4i8:
  case AMDIL::REGIONLOAD64_v4i8:
  case AMDIL::REGIONSEXTLOAD64_v4i8:
  case AMDIL::REGIONAEXTLOAD64_v4i8:
  case AMDIL::REGIONZEXTLOAD64_v4i8:
  case AMDIL::PRIVATELOAD64_v4i8:
  case AMDIL::PRIVATESEXTLOAD64_v4i8:
  case AMDIL::PRIVATEAEXTLOAD64_v4i8:
  case AMDIL::PRIVATEZEXTLOAD64_v4i8:
  case AMDIL::CONSTANTLOAD64_v4i8:
  case AMDIL::CONSTANTSEXTLOAD64_v4i8:
  case AMDIL::CONSTANTAEXTLOAD64_v4i8:
  case AMDIL::CONSTANTZEXTLOAD64_v4i8:
  case AMDIL::GLOBALLOAD_v4i8:
  case AMDIL::GLOBALSEXTLOAD_v4i8:
  case AMDIL::GLOBALAEXTLOAD_v4i8:
  case AMDIL::GLOBALZEXTLOAD_v4i8:
  case AMDIL::LOCALLOAD_v4i8:
  case AMDIL::LOCALSEXTLOAD_v4i8:
  case AMDIL::LOCALAEXTLOAD_v4i8:
  case AMDIL::LOCALZEXTLOAD_v4i8:
  case AMDIL::REGIONLOAD_v4i8:
  case AMDIL::REGIONSEXTLOAD_v4i8:
  case AMDIL::REGIONAEXTLOAD_v4i8:
  case AMDIL::REGIONZEXTLOAD_v4i8:
  case AMDIL::PRIVATELOAD_v4i8:
  case AMDIL::PRIVATESEXTLOAD_v4i8:
  case AMDIL::PRIVATEAEXTLOAD_v4i8:
  case AMDIL::PRIVATEZEXTLOAD_v4i8:
  case AMDIL::CONSTANTLOAD_v4i8:
  case AMDIL::CONSTANTSEXTLOAD_v4i8:
  case AMDIL::CONSTANTAEXTLOAD_v4i8:
  case AMDIL::CONSTANTZEXTLOAD_v4i8:
    return UNPACK_V4I8;

  case AMDIL::GLOBALLOAD64_v2i16:
  case AMDIL::GLOBALSEXTLOAD64_v2i16:
  case AMDIL::GLOBALAEXTLOAD64_v2i16:
  case AMDIL::GLOBALZEXTLOAD64_v2i16:
  case AMDIL::LOCALLOAD64_v2i16:
  case AMDIL::LOCALSEXTLOAD64_v2i16:
  case AMDIL::LOCALAEXTLOAD64_v2i16:
  case AMDIL::LOCALZEXTLOAD64_v2i16:
  case AMDIL::REGIONLOAD64_v2i16:
  case AMDIL::REGIONSEXTLOAD64_v2i16:
  case AMDIL::REGIONAEXTLOAD64_v2i16:
  case AMDIL::REGIONZEXTLOAD64_v2i16:
  case AMDIL::PRIVATELOAD64_v2i16:
  case AMDIL::PRIVATESEXTLOAD64_v2i16:
  case AMDIL::PRIVATEAEXTLOAD64_v2i16:
  case AMDIL::PRIVATEZEXTLOAD64_v2i16:
  case AMDIL::CONSTANTLOAD64_v2i16:
  case AMDIL::CONSTANTSEXTLOAD64_v2i16:
  case AMDIL::CONSTANTAEXTLOAD64_v2i16:
  case AMDIL::CONSTANTZEXTLOAD64_v2i16:
  case AMDIL::GLOBALLOAD_v2i16:
  case AMDIL::GLOBALSEXTLOAD_v2i16:
  case AMDIL::GLOBALAEXTLOAD_v2i16:
  case AMDIL::GLOBALZEXTLOAD_v2i16:
  case AMDIL::LOCALLOAD_v2i16:
  case AMDIL::LOCALSEXTLOAD_v2i16:
  case AMDIL::LOCALAEXTLOAD_v2i16:
  case AMDIL::LOCALZEXTLOAD_v2i16:
  case AMDIL::REGIONLOAD_v2i16:
  case AMDIL::REGIONSEXTLOAD_v2i16:
  case AMDIL::REGIONAEXTLOAD_v2i16:
  case AMDIL::REGIONZEXTLOAD_v2i16:
  case AMDIL::PRIVATELOAD_v2i16:
  case AMDIL::PRIVATESEXTLOAD_v2i16:
  case AMDIL::PRIVATEAEXTLOAD_v2i16:
  case AMDIL::PRIVATEZEXTLOAD_v2i16:
  case AMDIL::CONSTANTLOAD_v2i16:
  case AMDIL::CONSTANTSEXTLOAD_v2i16:
  case AMDIL::CONSTANTAEXTLOAD_v2i16:
  case AMDIL::CONSTANTZEXTLOAD_v2i16:
    return UNPACK_V2I16;

  case AMDIL::GLOBALLOAD64_v4i16:
  case AMDIL::GLOBALSEXTLOAD64_v4i16:
  case AMDIL::GLOBALAEXTLOAD64_v4i16:
  case AMDIL::GLOBALZEXTLOAD64_v4i16:
  case AMDIL::LOCALLOAD64_v4i16:
  case AMDIL::LOCALSEXTLOAD64_v4i16:
  case AMDIL::LOCALAEXTLOAD64_v4i16:
  case AMDIL::LOCALZEXTLOAD64_v4i16:
  case AMDIL::REGIONLOAD64_v4i16:
  case AMDIL::REGIONSEXTLOAD64_v4i16:
  case AMDIL::REGIONAEXTLOAD64_v4i16:
  case AMDIL::REGIONZEXTLOAD64_v4i16:
  case AMDIL::PRIVATELOAD64_v4i16:
  case AMDIL::PRIVATESEXTLOAD64_v4i16:
  case AMDIL::PRIVATEAEXTLOAD64_v4i16:
  case AMDIL::PRIVATEZEXTLOAD64_v4i16:
  case AMDIL::CONSTANTLOAD64_v4i16:
  case AMDIL::CONSTANTSEXTLOAD64_v4i16:
  case AMDIL::CONSTANTAEXTLOAD64_v4i16:
  case AMDIL::CONSTANTZEXTLOAD64_v4i16:
  case AMDIL::GLOBALLOAD_v4i16:
  case AMDIL::GLOBALSEXTLOAD_v4i16:
  case AMDIL::GLOBALAEXTLOAD_v4i16:
  case AMDIL::GLOBALZEXTLOAD_v4i16:
  case AMDIL::LOCALLOAD_v4i16:
  case AMDIL::LOCALSEXTLOAD_v4i16:
  case AMDIL::LOCALAEXTLOAD_v4i16:
  case AMDIL::LOCALZEXTLOAD_v4i16:
  case AMDIL::REGIONLOAD_v4i16:
  case AMDIL::REGIONSEXTLOAD_v4i16:
  case AMDIL::REGIONAEXTLOAD_v4i16:
  case AMDIL::REGIONZEXTLOAD_v4i16:
  case AMDIL::PRIVATELOAD_v4i16:
  case AMDIL::PRIVATESEXTLOAD_v4i16:
  case AMDIL::PRIVATEAEXTLOAD_v4i16:
  case AMDIL::PRIVATEZEXTLOAD_v4i16:
  case AMDIL::CONSTANTLOAD_v4i16:
  case AMDIL::CONSTANTSEXTLOAD_v4i16:
  case AMDIL::CONSTANTAEXTLOAD_v4i16:
  case AMDIL::CONSTANTZEXTLOAD_v4i16:
    return UNPACK_V4I16;
  };
  return NO_PACKING;
}

uint32_t
AMDILIOExpansion::getPointerID(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curInst;
  getAsmPrinterFlags(MI, curInst);
  return curInst.bits.ResourceID;
}

uint32_t
AMDILIOExpansion::getShiftSize(MachineInstr *MI)
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
AMDILIOExpansion::getMemorySize(MachineInstr *MI)
{
  if (MI->memoperands_empty()) {
    return 4;
  }
  return (uint32_t)((*MI->memoperands_begin())->getSize());
}

unsigned
AMDILIOExpansion::expandLongExtend(MachineInstr *MI,
                                   uint32_t numComps, uint32_t size, bool signedShift)
{
  DebugLoc DL = MI->getDebugLoc();
  switch(size) {
  default:
    assert(0 && "Found a case we don't handle!");
    break;
  case 8:
    if (numComps == 1) {
      return expandLongExtendSub32(MI, AMDIL::SHL_i8, AMDIL::SHRVEC_v2i32,
                                   AMDIL::USHRVEC_i8,
                                   24, (24ULL | (31ULL << 32)), 24, AMDIL::LCREATE, signedShift,
                                   false);
    } else if (numComps == 2) {
      return expandLongExtendSub32(MI, AMDIL::SHL_v2i8, AMDIL::SHRVEC_v4i32,
                                   AMDIL::USHRVEC_v2i8,
                                   24, (24ULL | (31ULL << 32)), 24, AMDIL::LCREATE_v2i64, signedShift,
                                   true);
    } else {
      assert(0 && "Found a case we don't handle!");
    }
    break;
  case 16:
    if (numComps == 1) {
      return expandLongExtendSub32(MI, AMDIL::SHL_i16, AMDIL::SHRVEC_v2i32,
                                   AMDIL::USHRVEC_i16,
                                   16, (16ULL | (31ULL << 32)), 16, AMDIL::LCREATE, signedShift,
                                   false);
    } else if (numComps == 2) {
      return expandLongExtendSub32(MI, AMDIL::SHL_v2i16, AMDIL::SHRVEC_v4i32,
                                   AMDIL::USHRVEC_v2i16,
                                   16, (16ULL | (31ULL << 32)), 16, AMDIL::LCREATE_v2i64, signedShift,
                                   true);
    } else {
      assert(0 && "Found a case we don't handle!");
    }
    break;
  case 32:
    if (numComps == 1) {
      MachineInstr *nMI = NULL;
      if (signedShift) {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRVEC_i32), AMDIL::Rxy1011)
              .addReg(AMDIL::Rx1011)
              .addImm(mMFI->addi64Literal((0ULL | (31ULL << 32))));
      } else {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATE), AMDIL::Rxy1011)
              .addReg(AMDIL::Rx1011)
              .addImm(mMFI->addi32Literal(0));
      }
      return nMI->getOperand(0).getReg();
    } else if (numComps == 2) {
      MachineInstr *nMI = NULL;
      if (signedShift) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRVEC_v2i32), AMDIL::Rxy1012)
        .addReg(AMDIL::Rxy1011)
        .addImm(mMFI->addi64Literal(31));
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATE_v2i64), AMDIL::R1011)
              .addReg(AMDIL::Rxy1011)
              .addReg(AMDIL::Rxy1012);
      } else {
        nMI = BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATE_v2i64), AMDIL::R1011)
              .addReg(AMDIL::Rxy1011)
              .addImm(mMFI->addi32Literal(0));
      }
      return nMI->getOperand(0).getReg();
    } else {
      assert(0 && "Found a case we don't handle!");
    }
  };
  return 0;
}
unsigned
AMDILIOExpansion::expandLongExtendSub32(MachineInstr *MI,
                                        unsigned SHLop, unsigned SHRop, unsigned USHRop,
                                        unsigned SHLimm, uint64_t SHRimm, unsigned USHRimm,
                                        unsigned LCRop, bool signedShift, bool vec2)
{
  MachineInstr *nMI = NULL;
  DebugLoc DL = MI->getDebugLoc();
  BuildMI(*mBB, MI, DL, mTII->get(SHLop),
          (vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
  .addReg((vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
  .addImm(mMFI->addi32Literal(SHLimm));
  if (signedShift) {
    BuildMI(*mBB, MI, DL, mTII->get(LCRop),
            (vec2) ? AMDIL::R1011 : AMDIL::Rxy1011)
    .addReg((vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
    .addReg(AMDIL::Rxy1011);
    nMI = BuildMI(*mBB, MI, DL, mTII->get(SHRop),
                  (vec2) ? AMDIL::R1011 : AMDIL::Rxy1011)
          .addReg((vec2) ? AMDIL::R1011 : AMDIL::Rxy1011)
          .addImm(mMFI->addi64Literal(SHRimm));
  } else {
    BuildMI(*mBB, MI, DL, mTII->get(USHRop),
            (vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
    .addReg((vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(USHRimm));
    nMI = BuildMI(*mBB, MI, MI->getDebugLoc(), mTII->get(LCRop),
                  (vec2) ? AMDIL::R1011 : AMDIL::Rxy1011)
          .addReg((vec2) ? AMDIL::Rxy1011 : AMDIL::Rx1011)
          .addImm(mMFI->addi32Literal(0));
  }
  return nMI->getOperand(0).getReg();
}

unsigned
AMDILIOExpansion::expandIntegerExtend(MachineInstr *MI, unsigned SHLop,
                                      unsigned SHRop, unsigned offset, unsigned reg)
{
  DebugLoc DL = MI->getDebugLoc();
  offset = mMFI->addi32Literal(offset);
  BuildMI(*mBB, MI, DL,
          mTII->get(SHLop), reg)
  .addReg(reg).addImm(offset);
  BuildMI(*mBB, MI, DL,
          mTII->get(SHRop), reg)
  .addReg(reg).addImm(offset);
  return reg;
}
unsigned
AMDILIOExpansion::expandExtendLoad(MachineInstr *MI)
{
  if (!isExtendLoad(MI)) {
    return 0;
  }
  Type *mType = NULL;
  if (!MI->memoperands_empty()) {
    MachineMemOperand *memOp = (*MI->memoperands_begin());
    const Value *moVal = (memOp) ? memOp->getValue() : NULL;
    mType = (moVal) ? moVal->getType() : NULL;
  }
  unsigned opcode = 0;
  DebugLoc DL = MI->getDebugLoc();
  if (isZExtLoadInst(TM, MI) || isAExtLoadInst(TM, MI) || isSExtLoadInst(TM, MI)) {
    switch(MI->getDesc().OpInfo[0].RegClass) {
    default:
      assert(0 && "Found an extending load that we don't handle!");
      break;
    case AMDIL::GPRI16RegClassID:
      if (!isHardwareLocal(MI)
          || mSTM->device()->usesSoftware(AMDILDeviceInfo::ByteLDSOps)) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_i16 : AMDIL::USHRVEC_i16;
        return expandIntegerExtend(MI, AMDIL::SHL_i16, opcode, 24, AMDIL::Rx1011);
      }
      break;
    case AMDIL::GPRV2I16RegClassID:
      opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v2i16 : AMDIL::USHRVEC_v2i16;
      return expandIntegerExtend(MI, AMDIL::SHL_v2i16, opcode, 24, AMDIL::Rxy1011);
      break;
    case AMDIL::GPRV4I8RegClassID:
      opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v4i8 : AMDIL::USHRVEC_v4i8;
      return expandIntegerExtend(MI, AMDIL::SHL_v4i8, opcode, 24, AMDIL::R1011);
      break;
    case AMDIL::GPRV4I16RegClassID:
      opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v4i16 : AMDIL::USHRVEC_v4i16;
      return expandIntegerExtend(MI, AMDIL::SHL_v4i16, opcode, 24, AMDIL::R1011);
      break;
    case AMDIL::GPRI32RegClassID:
      // We can be a i8 or i16 bit sign extended value
      if (isNbitType(mType, 8) || getMemorySize(MI) == 1) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_i32 : AMDIL::USHRVEC_i32;
        expandIntegerExtend(MI, AMDIL::SHL_i32, opcode, 24, AMDIL::Rx1011);
      } else if (isNbitType(mType, 16) || getMemorySize(MI) == 2) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_i32 : AMDIL::USHRVEC_i32;
        expandIntegerExtend(MI, AMDIL::SHL_i32, opcode, 16, AMDIL::Rx1011);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      return AMDIL::Rx1011;
      break;
    case AMDIL::GPRV2I32RegClassID:
      // We can be a v2i8 or v2i16 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 2) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v2i32 : AMDIL::USHRVEC_v2i32;
        expandIntegerExtend(MI, AMDIL::SHL_v2i32, opcode, 24, AMDIL::Rxy1011);
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 4) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v2i32 : AMDIL::USHRVEC_v2i32;
        expandIntegerExtend(MI, AMDIL::SHL_v2i32, opcode, 16, AMDIL::Rxy1011);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      return AMDIL::Rxy1011;
      break;
    case AMDIL::GPRV4I32RegClassID:
      // We can be a v4i8 or v4i16 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 4) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v4i32 : AMDIL::USHRVEC_v4i32;
        expandIntegerExtend(MI, AMDIL::SHL_v4i32, opcode, 24, AMDIL::R1011);
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 8) {
        opcode = isSExtLoadInst(TM, MI) ? AMDIL::SHRVEC_v4i32 : AMDIL::USHRVEC_v4i32;
        expandIntegerExtend(MI, AMDIL::SHL_v4i32, opcode, 16, AMDIL::R1011);
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      return AMDIL::R1011;
      break;
    case AMDIL::GPRI64RegClassID:
      // We can be a i8, i16 or i32 bit sign extended value
      if (isNbitType(mType, 8) || getMemorySize(MI) == 1) {
        return expandLongExtend(MI, 1, 8, isSExtLoadInst(TM, MI));
      } else if (isNbitType(mType, 16) || getMemorySize(MI) == 2) {
        return expandLongExtend(MI, 1, 16, isSExtLoadInst(TM, MI));
      } else if (isNbitType(mType, 32) || getMemorySize(MI) == 4) {
        return expandLongExtend(MI, 1, 32, isSExtLoadInst(TM, MI));
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRV2I64RegClassID:
      // We can be a v2i8, v2i16 or v2i32 bit sign extended value
      if (isNbitType(mType, 8, false) || getMemorySize(MI) == 2) {
        return expandLongExtend(MI, 2, 8, isSExtLoadInst(TM, MI));
      } else if (isNbitType(mType, 16, false) || getMemorySize(MI) == 4) {
        return expandLongExtend(MI, 2, 16, isSExtLoadInst(TM, MI));
      } else if (isNbitType(mType, 32, false) || getMemorySize(MI) == 8) {
        return expandLongExtend(MI, 2, 32, isSExtLoadInst(TM, MI));
      } else {
        assert(0 && "Found an extending load that we don't handle!");
      }
      break;
    case AMDIL::GPRF32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOF_f32), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1011);
      return AMDIL::Rx1011;
    case AMDIL::GPRV2F32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOF_v2f32), AMDIL::Rxy1011)
      .addReg(AMDIL::Rxy1011);
      return AMDIL::Rxy1011;
    case AMDIL::GPRV4F32RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::HTOF_v4f32), AMDIL::R1011)
      .addReg(AMDIL::R1011);
      return AMDIL::R1011;
    case AMDIL::GPRF64RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::FTOD), AMDIL::Rxy1011)
      .addReg(AMDIL::Rx1011);
      return AMDIL::Rxy1011;
    case AMDIL::GPRV2F64RegClassID:
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::FTOD), AMDIL::Rzw1011)
      .addReg(AMDIL::Ry1011);
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::FTOD), AMDIL::Rxy1011)
      .addReg(AMDIL::Rx1011);
      return AMDIL::R1011;
    }
  }
  return 0;
}

void
AMDILIOExpansion::expandTruncData(MachineInstr *MI)
{
  if (!isTruncStoreInst(TM, MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  switch (MI->getOpcode()) {
  default:
    MI->dump();
    assert(!"Found a trunc store instructions we don't handle!");
    break;
  case AMDIL::GLOBALTRUNCSTORE64_i64i8:
  case AMDIL::GLOBALTRUNCSTORE64_v2i64i8:
  case AMDIL::LOCALTRUNCSTORE64_i64i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i64i8:
  case AMDIL::REGIONTRUNCSTORE64_i64i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i64i8:
  case AMDIL::PRIVATETRUNCSTORE64_i64i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i64i8:
  case AMDIL::GLOBALTRUNCSTORE_i64i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i64i8:
  case AMDIL::LOCALTRUNCSTORE_i64i8:
  case AMDIL::LOCALTRUNCSTORE_v2i64i8:
  case AMDIL::REGIONTRUNCSTORE_i64i8:
  case AMDIL::REGIONTRUNCSTORE_v2i64i8:
  case AMDIL::PRIVATETRUNCSTORE_i64i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i64i8:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLO_v2i64), AMDIL::Rxy1011)
    .addReg(AMDIL::R1011);
  case AMDIL::GLOBALTRUNCSTORE64_i16i8:
  case AMDIL::GLOBALTRUNCSTORE64_v2i16i8:
  case AMDIL::GLOBALTRUNCSTORE64_v4i16i8:
  case AMDIL::LOCALTRUNCSTORE64_i16i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i16i8:
  case AMDIL::LOCALTRUNCSTORE64_v4i16i8:
  case AMDIL::REGIONTRUNCSTORE64_i16i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i16i8:
  case AMDIL::REGIONTRUNCSTORE64_v4i16i8:
  case AMDIL::PRIVATETRUNCSTORE64_i16i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i16i8:
  case AMDIL::PRIVATETRUNCSTORE64_v4i16i8:
  case AMDIL::GLOBALTRUNCSTORE_i16i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i16i8:
  case AMDIL::GLOBALTRUNCSTORE_v4i16i8:
  case AMDIL::LOCALTRUNCSTORE_i16i8:
  case AMDIL::LOCALTRUNCSTORE_v2i16i8:
  case AMDIL::LOCALTRUNCSTORE_v4i16i8:
  case AMDIL::REGIONTRUNCSTORE_i16i8:
  case AMDIL::REGIONTRUNCSTORE_v2i16i8:
  case AMDIL::REGIONTRUNCSTORE_v4i16i8:
  case AMDIL::PRIVATETRUNCSTORE_i16i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i16i8:
  case AMDIL::PRIVATETRUNCSTORE_v4i16i8:
  case AMDIL::GLOBALTRUNCSTORE64_i32i8:
  case AMDIL::GLOBALTRUNCSTORE64_v2i32i8:
  case AMDIL::GLOBALTRUNCSTORE64_v4i32i8:
  case AMDIL::LOCALTRUNCSTORE64_i32i8:
  case AMDIL::LOCALTRUNCSTORE64_v2i32i8:
  case AMDIL::LOCALTRUNCSTORE64_v4i32i8:
  case AMDIL::REGIONTRUNCSTORE64_i32i8:
  case AMDIL::REGIONTRUNCSTORE64_v2i32i8:
  case AMDIL::REGIONTRUNCSTORE64_v4i32i8:
  case AMDIL::PRIVATETRUNCSTORE64_i32i8:
  case AMDIL::PRIVATETRUNCSTORE64_v2i32i8:
  case AMDIL::PRIVATETRUNCSTORE64_v4i32i8:
  case AMDIL::GLOBALTRUNCSTORE_i32i8:
  case AMDIL::GLOBALTRUNCSTORE_v2i32i8:
  case AMDIL::GLOBALTRUNCSTORE_v4i32i8:
  case AMDIL::LOCALTRUNCSTORE_i32i8:
  case AMDIL::LOCALTRUNCSTORE_v2i32i8:
  case AMDIL::LOCALTRUNCSTORE_v4i32i8:
  case AMDIL::REGIONTRUNCSTORE_i32i8:
  case AMDIL::REGIONTRUNCSTORE_v2i32i8:
  case AMDIL::REGIONTRUNCSTORE_v4i32i8:
  case AMDIL::PRIVATETRUNCSTORE_i32i8:
  case AMDIL::PRIVATETRUNCSTORE_v2i32i8:
  case AMDIL::PRIVATETRUNCSTORE_v4i32i8:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::BINARY_AND_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi32Literal(0xFF));
    break;
  case AMDIL::GLOBALTRUNCSTORE64_i64i16:
  case AMDIL::GLOBALTRUNCSTORE64_v2i64i16:
  case AMDIL::LOCALTRUNCSTORE64_i64i16:
  case AMDIL::LOCALTRUNCSTORE64_v2i64i16:
  case AMDIL::REGIONTRUNCSTORE64_i64i16:
  case AMDIL::REGIONTRUNCSTORE64_v2i64i16:
  case AMDIL::PRIVATETRUNCSTORE64_i64i16:
  case AMDIL::PRIVATETRUNCSTORE64_v2i64i16:
  case AMDIL::GLOBALTRUNCSTORE_i64i16:
  case AMDIL::GLOBALTRUNCSTORE_v2i64i16:
  case AMDIL::LOCALTRUNCSTORE_i64i16:
  case AMDIL::LOCALTRUNCSTORE_v2i64i16:
  case AMDIL::REGIONTRUNCSTORE_i64i16:
  case AMDIL::REGIONTRUNCSTORE_v2i64i16:
  case AMDIL::PRIVATETRUNCSTORE_i64i16:
  case AMDIL::PRIVATETRUNCSTORE_v2i64i16:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLO_v2i64), AMDIL::Rxy1011)
    .addReg(AMDIL::R1011);
  case AMDIL::GLOBALTRUNCSTORE64_i32i16:
  case AMDIL::GLOBALTRUNCSTORE64_v2i32i16:
  case AMDIL::GLOBALTRUNCSTORE64_v4i32i16:
  case AMDIL::LOCALTRUNCSTORE64_i32i16:
  case AMDIL::LOCALTRUNCSTORE64_v2i32i16:
  case AMDIL::LOCALTRUNCSTORE64_v4i32i16:
  case AMDIL::REGIONTRUNCSTORE64_i32i16:
  case AMDIL::REGIONTRUNCSTORE64_v2i32i16:
  case AMDIL::REGIONTRUNCSTORE64_v4i32i16:
  case AMDIL::PRIVATETRUNCSTORE64_i32i16:
  case AMDIL::PRIVATETRUNCSTORE64_v2i32i16:
  case AMDIL::PRIVATETRUNCSTORE64_v4i32i16:
  case AMDIL::GLOBALTRUNCSTORE_i32i16:
  case AMDIL::GLOBALTRUNCSTORE_v2i32i16:
  case AMDIL::GLOBALTRUNCSTORE_v4i32i16:
  case AMDIL::LOCALTRUNCSTORE_i32i16:
  case AMDIL::LOCALTRUNCSTORE_v2i32i16:
  case AMDIL::LOCALTRUNCSTORE_v4i32i16:
  case AMDIL::REGIONTRUNCSTORE_i32i16:
  case AMDIL::REGIONTRUNCSTORE_v2i32i16:
  case AMDIL::REGIONTRUNCSTORE_v4i32i16:
  case AMDIL::PRIVATETRUNCSTORE_i32i16:
  case AMDIL::PRIVATETRUNCSTORE_v2i32i16:
  case AMDIL::PRIVATETRUNCSTORE_v4i32i16:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::BINARY_AND_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi32Literal(0xFFFF));
    break;
  case AMDIL::GLOBALTRUNCSTORE64_i64i32:
  case AMDIL::LOCALTRUNCSTORE64_i64i32:
  case AMDIL::REGIONTRUNCSTORE64_i64i32:
  case AMDIL::PRIVATETRUNCSTORE64_i64i32:
  case AMDIL::GLOBALTRUNCSTORE_i64i32:
  case AMDIL::LOCALTRUNCSTORE_i64i32:
  case AMDIL::REGIONTRUNCSTORE_i64i32:
  case AMDIL::PRIVATETRUNCSTORE_i64i32:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLO), AMDIL::Rx1011)
    .addReg(AMDIL::Rxy1011);
    break;
  case AMDIL::GLOBALTRUNCSTORE64_v2i64i32:
  case AMDIL::LOCALTRUNCSTORE64_v2i64i32:
  case AMDIL::REGIONTRUNCSTORE64_v2i64i32:
  case AMDIL::PRIVATETRUNCSTORE64_v2i64i32:
  case AMDIL::GLOBALTRUNCSTORE_v2i64i32:
  case AMDIL::LOCALTRUNCSTORE_v2i64i32:
  case AMDIL::REGIONTRUNCSTORE_v2i64i32:
  case AMDIL::PRIVATETRUNCSTORE_v2i64i32:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::LLO_v2i64), AMDIL::Rxy1011)
    .addReg(AMDIL::R1011);
    break;
  case AMDIL::GLOBALTRUNCSTORE64_f64f32:
  case AMDIL::LOCALTRUNCSTORE64_f64f32:
  case AMDIL::REGIONTRUNCSTORE64_f64f32:
  case AMDIL::PRIVATETRUNCSTORE64_f64f32:
  case AMDIL::GLOBALTRUNCSTORE_f64f32:
  case AMDIL::LOCALTRUNCSTORE_f64f32:
  case AMDIL::REGIONTRUNCSTORE_f64f32:
  case AMDIL::PRIVATETRUNCSTORE_f64f32:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOF),
            AMDIL::Rx1011).addReg(AMDIL::Rxy1011);
    break;
  case AMDIL::GLOBALTRUNCSTORE64_v2f64f32:
  case AMDIL::LOCALTRUNCSTORE64_v2f64f32:
  case AMDIL::REGIONTRUNCSTORE64_v2f64f32:
  case AMDIL::PRIVATETRUNCSTORE64_v2f64f32:
  case AMDIL::GLOBALTRUNCSTORE_v2f64f32:
  case AMDIL::LOCALTRUNCSTORE_v2f64f32:
  case AMDIL::REGIONTRUNCSTORE_v2f64f32:
  case AMDIL::PRIVATETRUNCSTORE_v2f64f32:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOF),
            AMDIL::Rx1011).addReg(AMDIL::Rxy1011);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DTOF),
            AMDIL::Ry1011).addReg(AMDIL::Rzw1011);
    break;
  }
}
void
AMDILIOExpansion::expandAddressCalc(MachineInstr *MI)
{
  if (!isAddrCalcInstr(MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  bool is64bit = is64bitLSOp(TM, MI);
  uint32_t addyReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  uint32_t addInst = (is64bit) ? AMDIL::LADD_i64 : AMDIL::ADD_i32;
  switch(MI->getOpcode()) {
    ExpandCaseToAllTruncTypes(AMDIL::PRIVATETRUNCSTORE)
    ExpandCaseToAllTruncTypes(AMDIL::PRIVATETRUNCSTORE64)
    ExpandCaseToAllTypes(AMDIL::PRIVATESTORE)
    ExpandCaseToAllTypes(AMDIL::PRIVATELOAD)
    ExpandCaseToAllTypes(AMDIL::PRIVATESEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::PRIVATEZEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::PRIVATEAEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::PRIVATESTORE64)
    ExpandCaseToAllTypes(AMDIL::PRIVATELOAD64)
    ExpandCaseToAllTypes(AMDIL::PRIVATESEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::PRIVATEZEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::PRIVATEAEXTLOAD64)
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            addyReg).addReg(addyReg).addReg(AMDIL::T1);
    break;
    ExpandCaseToAllTruncTypes(AMDIL::LOCALTRUNCSTORE)
    ExpandCaseToAllTypes(AMDIL::LOCALLOAD)
    ExpandCaseToAllTypes(AMDIL::LOCALSEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::LOCALZEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::LOCALAEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::LOCALSTORE)
    ExpandCaseToAllTruncTypes(AMDIL::LOCALTRUNCSTORE64)
    ExpandCaseToAllTypes(AMDIL::LOCALLOAD64)
    ExpandCaseToAllTypes(AMDIL::LOCALSEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::LOCALZEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::LOCALAEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::LOCALSTORE64)
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            addyReg).addReg(addyReg).addReg(AMDIL::T2);
    break;
    ExpandCaseToAllTypes(AMDIL::CPOOLLOAD)
    ExpandCaseToAllTypes(AMDIL::CPOOLSEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::CPOOLZEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::CPOOLAEXTLOAD)
    ExpandCaseToAllTypes(AMDIL::CPOOLLOAD64)
    ExpandCaseToAllTypes(AMDIL::CPOOLSEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::CPOOLZEXTLOAD64)
    ExpandCaseToAllTypes(AMDIL::CPOOLAEXTLOAD64)
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            addyReg).addReg(addyReg).addReg(AMDIL::SDP);
    break;
  default:
    return;
  }
}
void
AMDILIOExpansion::expandLoadStartCode(MachineInstr *MI)
{
  DebugLoc DL = MI->getDebugLoc();
  bool is64bit = is64bitLSOp(TM, MI);
  uint32_t addyReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  uint32_t addInst = (is64bit) ? AMDIL::LADD_i64 : AMDIL::ADD_i32;
  uint32_t moveInst = (is64bit) ? AMDIL::MOVE_i64 : AMDIL::MOVE_i32;
  if (MI->getOperand(2).isReg()) {
    BuildMI(*mBB, MI, DL, mTII->get(addInst),
            addyReg).addReg(MI->getOperand(1).getReg())
    .addReg(MI->getOperand(2).getReg());
  } else {
    BuildMI(*mBB, MI, DL, mTII->get(moveInst),
            addyReg).addReg(MI->getOperand(1).getReg());
  }
  MI->getOperand(1).setReg(addyReg);
  expandAddressCalc(MI);
}
void
AMDILIOExpansion::emitStaticCPLoad(MachineInstr* MI, int swizzle,
                                   int id, bool ExtFPLoad)
{
  DebugLoc DL = MI->getDebugLoc();
  switch(swizzle) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(ExtFPLoad
                                    ? AMDIL::DTOF : AMDIL::MOVE_i32),
            MI->getOperand(0).getReg())
    .addImm(id);
    break;
  case 1:
  case 2:
  case 3:
    BuildMI(*mBB, MI, DL, mTII->get(ExtFPLoad
                                    ? AMDIL::DTOF : AMDIL::MOVE_i32), AMDIL::Rx1001)
    .addImm(id);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VINSERT_v4i32),
            MI->getOperand(0).getReg())
    .addReg(MI->getOperand(0).getReg())
    .addReg(AMDIL::Rx1001)
    .addImm(swizzle + 1);
    break;
  };
}
void
AMDILIOExpansion::emitCPInst(MachineInstr* MI,
                             const Constant* C, AMDILKernelManager* KM, int swizzle, bool ExtFPLoad)
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
      emitStaticCPLoad(MI, swizzle, id, ExtFPLoad);
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
      uint32_t id = mMFI->getLongLits(conv.ul);
      if (!id) {
        id = mMFI->getIntLits((uint32_t)conv.ul);
      }
      emitStaticCPLoad(MI, swizzle, id, ExtFPLoad);
    }
  } else if (const ConstantInt* CI = dyn_cast<ConstantInt>(C)) {
    int64_t val = 0;
    if (CI) {
      val = CI->getSExtValue();
    }
    if (CI->getBitWidth() == 64) {
      emitStaticCPLoad(MI, swizzle, mMFI->addi64Literal(val), ExtFPLoad);
    } else {
      emitStaticCPLoad(MI, swizzle, mMFI->addi32Literal(val), ExtFPLoad);
    }
  } else if (const ConstantArray* CA = dyn_cast<ConstantArray>(C)) {
    uint32_t size = CA->getNumOperands();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CA->getOperand(0), KM, x, ExtFPLoad);
    }
  } else if (const ConstantAggregateZero* CAZ
             = dyn_cast<ConstantAggregateZero>(C)) {
    if (CAZ->isNullValue()) {
      emitStaticCPLoad(MI, swizzle, mMFI->addi32Literal(0), ExtFPLoad);
    }
  } else if (const ConstantStruct* CS = dyn_cast<ConstantStruct>(C)) {
    uint32_t size = CS->getNumOperands();
    assert(size < 5 && "Cannot handle a constant array where size > 4");
    if (size > 4) {
      size = 4;
    }
    for (uint32_t x = 0; x < size; ++x) {
      emitCPInst(MI, CS->getOperand(0), KM, x, ExtFPLoad);
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
      emitCPInst(MI, CV->getOperand(0), KM, x, ExtFPLoad);
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
      emitCPInst(MI, CV->getElementAsConstant(0), KM, x, ExtFPLoad);
    }
  } else {
    // TODO: Do we really need to handle ConstantPointerNull?
    // What about BlockAddress, ConstantExpr and Undef?
    // How would these even be generated by a valid CL program?
    assert(0 && "Found a constant type that I don't know how to handle");
  }
}

