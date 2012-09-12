//===-- AMDILEGIOExpansion.cpp --------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementation of IO expansion class for evergreen and NI devices.
//
//===----------------------------------------------------------------------===//

#include "AMDILIOExpansion.h"
#include "AMDILCompilerErrors.h"
#include "AMDILCompilerWarnings.h"
#include "AMDILDevices.h"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Value.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/Support/DebugLoc.h"
#include <cstdio>
using namespace llvm;

namespace llvm
{
extern void initializeAMDILEGIOExpansionPass(llvm::PassRegistry&);
}

char AMDILEGIOExpansion::ID = 0;
INITIALIZE_PASS(AMDILEGIOExpansion, "eg-io-expansion",
                "AMDIL EG/NI IO Expansion", false, false);

AMDILEGIOExpansion::AMDILEGIOExpansion()
  : MachineFunctionPass(ID)
{
  initializeAMDILEGIOExpansionPass(*PassRegistry::getPassRegistry());
}
const char *AMDILEGIOExpansion::getPassName() const
{
  return "AMDIL EG/NI IO Expansion Pass";
}
bool AMDILEGIOExpansion::runOnMachineFunction(MachineFunction& MF)
{
  AMDILEGIOExpansionImpl impl(MF);
  return impl.run();
}
bool
AMDILEGIOExpansionImpl::isIOInstruction(MachineInstr *MI)
{
  if (!MI) {
    return false;
  }
  if (isImageInst(MI)) {
    return true;
  }
  return AMDILIOExpansionImpl::isIOInstruction(MI);
}
void
AMDILEGIOExpansionImpl::expandIOInstruction(MachineInstr *MI)
{
  assert(isIOInstruction(MI) && "Must be an IO instruction to "
         "be passed to this function!");
  if (isReadImageInst(MI) || isImageTXLDInst(MI)) {
    expandImageLoad(mBB, MI);
  } else if (isWriteImageInst(MI)) {
    expandImageStore(mBB, MI);
  } else if (isImageInfoInst(MI)) {
    expandImageParam(mBB, MI);
  } else {
    AMDILIOExpansionImpl::expandIOInstruction(MI);
  }
}
bool
AMDILEGIOExpansionImpl::isCacheableOp(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curRes;
  getAsmPrinterFlags(MI, curRes);
  // We only support caching on UAV11 - JeffG
  if (curRes.bits.ResourceID == 11) {
    return curRes.bits.CacheableRead;
  } else {
    return false;
  }
}
bool
AMDILEGIOExpansionImpl::isArenaOp(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curRes;
  getAsmPrinterFlags(MI, curRes);
  return curRes.bits.ResourceID
         == mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID)
         || curRes.bits.ResourceID >= ARENA_SEGMENT_RESERVED_UAVS;
}
void
AMDILEGIOExpansionImpl::expandPackedData(MachineInstr *MI, uint32_t &dataReg)
{
  if (!isPackedInst(MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  uint32_t packedReg = getPackedReg(dataReg, getPackedID(MI));
  // If we have packed data, then the shift size is no longer
  // the same as the load size and we need to adjust accordingly
  switch(getPackedID(MI)) {
  default:
    break;
  case PACK_V2I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTi32rrrr), AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(8)).addImm(mMFI->addi32Literal(8))
    .addReg(getCompReg(dataReg, sub_y_comp, sub_w_comp))
    .addReg(getCompReg(dataReg, sub_x_comp, sub_z_comp));
    dataReg = AMDIL::Rx1011;
    break;
  case PACK_V4I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LHIv2i64r), AMDIL::Rxy1012)
    .addReg(dataReg);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTv2i32rrrr),
            AMDIL::Rxy1011)
    .addImm(mMFI->addi64Literal(8ULL | (8ULL << 32)))
    .addImm(mMFI->addi64Literal(8ULL | (8ULL << 32)))
    .addReg(AMDIL::Rxy1012).addReg(AMDIL::Rxy1011);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTi32rrrr), AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(16)).addImm(mMFI->addi32Literal(16))
    .addReg(AMDIL::Ry1011).addReg(AMDIL::Rx1011);
    dataReg = AMDIL::Rx1011;
    break;
  case PACK_V2I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTi32rrrr), AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(16)).addImm(mMFI->addi32Literal(16))
    .addReg(getCompReg(dataReg, sub_y_comp, sub_w_comp))
    .addReg(getCompReg(dataReg, sub_x_comp, sub_z_comp));
    dataReg = AMDIL::Rx1011;
    break;
  case PACK_V4I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LHIv2i64r), AMDIL::Rxy1012)
    .addReg(dataReg);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1011)
    .addReg(dataReg);
    BuildMI(*mBB, MI, DL, mTII->get(
              AMDIL::UBIT_INSERTv2i32rrrr), AMDIL::Rxy1011)
    .addImm(mMFI->addi64Literal(16ULL | (16ULL << 32)))
    .addImm(mMFI->addi64Literal(16ULL | (16ULL << 32)))
    .addReg(AMDIL::Rxy1012).addReg(AMDIL::Rxy1011);
    dataReg = AMDIL::Rxy1011;
    break;
  case UNPACK_V2I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_EXTRACTv2i32rrr), dataReg)
    .addImm(mMFI->addi32Literal(8))
    .addImm(mMFI->addi64Literal(8ULL << 32))
    .addReg(packedReg);
    break;
  case UNPACK_V4I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_EXTRACTv4i32rrr), dataReg)
    .addImm(mMFI->addi32Literal(8))
    .addImm(mMFI->addi128Literal(8ULL << 32, (16ULL | (24ULL << 32))))
    .addReg(packedReg);
    break;
  case UNPACK_V2I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_EXTRACTv2i32rrr), dataReg)
    .addImm(mMFI->addi32Literal(16))
    .addImm(mMFI->addi64Literal(16ULL << 32))
    .addReg(packedReg);
    break;
  case UNPACK_V4I16:
    AMDIL789IOExpansionImpl::expandPackedData(MI, dataReg);
    break;
  };
}
static bool
isAlignedInst(MachineInstr *MI)
{
  if (!MI->memoperands_empty()) {
    return ((*MI->memoperands_begin())->getAlignment()
            & ((*MI->memoperands_begin())->getSize() - 1)) == 0;
  }
  return true;
}
void
AMDILEGIOExpansionImpl::expandGlobalLoad(MachineInstr *MI)
{
  bool usesArena = isArenaOp(MI);
  bool cacheable = isCacheableOp(MI);
  bool aligned = isAlignedInst(MI);
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are generated before the current MI.
  expandLoadStartCode(MI, addyReg);
  expandArenaSetup(MI, addyReg);
  DebugLoc DL = MI->getDebugLoc();
  if (getMemorySize(MI) == 1) {
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi8), dataReg)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                   (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::EQv4i32rr), AMDIL::R1012)
      .addReg(AMDIL::R1008)
      .addImm(mMFI->addi32Literal(0));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1012)
      .addImm(mMFI->addi32Literal(0))
      .addImm(mMFI->addi32Literal(24));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Ry1012)
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rx1008);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Rz1012)
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1008);
      if (cacheable) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADCACHEDi32),
                AMDIL::Rx1011).addReg(AMDIL::Rx1010).addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADi32),
                AMDIL::Rx1011).addReg(AMDIL::Rx1010).addImm(ID);
      }
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rx1008)
      .addReg(AMDIL::Rx1011);
    }
  } else if (getMemorySize(MI) == 2) {
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi16), dataReg)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(1));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(0));
      if (cacheable) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADCACHEDi32),
                AMDIL::Rx1011).addReg(AMDIL::Rx1010).addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADi32),
                AMDIL::Rx1011).addReg(AMDIL::Rx1010).addImm(ID);
      }
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1008)
      .addReg(AMDIL::Rx1011);
    }
  } else if (getMemorySize(MI) == 4) {
    uint32_t opc = AMDIL::UAVRAW32LOADi32;
    if (usesArena) {
      opc = AMDIL::UAVARENA32LOADi32;
    } else if (cacheable) {
      opc = AMDIL::UAVRAW32LOADCACHEDi32;
    }
    BuildMI(*mBB, MI, DL, mTII->get(opc), getPackedReg(dataReg, getPackedID(MI)))
    .addReg(addyReg)
    .addImm(ID);
  } else if (getMemorySize(MI) == 8) {
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
              getCompReg(dataReg, sub_x_comp))
      .addReg(getCompReg(addyReg, sub_x_comp))
      .addImm(ID);
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::ArenaVectors)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
                getCompReg(dataReg, sub_y_comp))
        .addReg(getCompReg(addyReg, sub_y_comp))
        .addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32LOADi32), AMDIL::Rx1008)
        .addReg(AMDIL::Rx1007)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEi64rr), dataReg)
        .addReg(getCompReg(dataReg, sub_x_comp))
        .addReg(AMDIL::Rx1008);
      }
    } else {
      if (cacheable) {
        if (aligned) {
          BuildMI(*mBB, MI, DL, mTII->get(
                    AMDIL::UAVRAW32LOADCACHEDALIGNEDv2i32), AMDIL::Rxy1011)
          .addReg(addyReg).addImm(ID);
        } else {
          BuildMI(*mBB, MI, DL, mTII->get(
                    AMDIL::UAVRAW32LOADCACHEDv2i32), AMDIL::Rxy1011)
          .addReg(addyReg).addImm(ID);
        }
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVRAW32LOADv2i32), AMDIL::Rxy1011)
        .addReg(addyReg).addImm(ID);
      }
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::COPY),
              getPackedReg(dataReg, getPackedID(MI)))
      .addReg(AMDIL::Rxy1011);
    }
  } else {
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
              getCompReg(dataReg, sub_x_comp))
      .addReg(getCompReg(addyReg, sub_x_comp))
      .addImm(ID);
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::ArenaVectors)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
                getCompReg(dataReg, sub_y_comp))
        .addReg(getCompReg(addyReg, sub_y_comp))
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
                getCompReg(dataReg, sub_z_comp))
        .addReg(getCompReg(addyReg, sub_z_comp))
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32LOADi32),
                getCompReg(dataReg, sub_w_comp))
        .addReg(getCompReg(addyReg, sub_w_comp))
        .addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32LOADi32), AMDIL::Rx1008)
        .addReg(AMDIL::Rx1007)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEi64rr),
                getCompReg(dataReg, sub_xy_comp))
        .addReg(getCompReg(dataReg, sub_x_comp))
        .addReg(AMDIL::Rx1008);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(3);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32LOADi32), AMDIL::Rx1008)
        .addReg(AMDIL::Rx1007)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(4);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32LOADi32), AMDIL::Rx1006)
        .addReg(AMDIL::Rx1007)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEi64rr),
                getCompReg(dataReg, sub_zw_comp))
        .addReg(AMDIL::Rx1006)
        .addReg(AMDIL::Rx1008);
      }
    } else {
      if (cacheable) {
        if (aligned) {
          BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADCACHEDALIGNEDv4i32),
                  dataReg).addReg(addyReg).addImm(ID);
        } else {
          BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADCACHEDv4i32),
                  dataReg).addReg(addyReg).addImm(ID);
        }
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADv4i32),
                dataReg).addReg(addyReg).addImm(ID);
      }
    }
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDILEGIOExpansionImpl::expandRegionLoad(MachineInstr *MI)
{
  bool HWRegion = mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem);
  if (!mSTM->device()->isSupported(AMDILDeviceInfo::RegionMem)) {
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[REGION_MEMORY_ERROR]);
    return;
  }
  if (!HWRegion || !isHardwareRegion(MI)) {
    return expandGlobalLoad(MI);
  }
  if (!mMFI->usesGDS() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  DebugLoc DL = MI->getDebugLoc();
  unsigned mulOp = 0;
  uint32_t gID = getPointerID(MI);
  assert(gID && "Found a GDS load that was incorrectly marked as zero ID!\n");
  if (!gID) {
    gID = mSTM->device()->getResourceID(AMDILDevice::GDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are generated before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1010)
    .addReg(addyReg)
    .addImm(mMFI->addi128Literal(4ULL << 32, 8ULL | (12ULL << 32)));

    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_x_comp))
    .addReg(AMDIL::Rx1010)
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_y_comp))
    .addReg(AMDIL::Ry1010)
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_z_comp))
    .addReg(AMDIL::Rz1010)
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_w_comp))
    .addReg(AMDIL::Rw1010)
    .addImm(gID);
    break;
  case 1:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteGDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      mulOp = (mSTM->device()->usesSoftware(AMDILDeviceInfo::RegionMem))
              ? AMDIL::UMULi32rr : AMDIL::UMUL24i32rr;
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r), dataReg)
      .addReg(AMDIL::Rx1010)
      .addImm(gID);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rx1008)
      .addReg(dataReg);
    } else {
      if (isSWSExtLoadInst(MI)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi8r), dataReg)
        .addReg(addyReg)
        .addImm(gID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADu8r), dataReg)
        .addReg(addyReg)
        .addImm(gID);
      }
    }
    break;
  case 2:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteGDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      mulOp = (mSTM->device()->usesSoftware(AMDILDeviceInfo::RegionMem))
              ? AMDIL::UMULi32rr : AMDIL::UMUL24i32rr;
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r), dataReg)
      .addReg(AMDIL::Rx1010)
      .addImm(gID);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1008)
      .addReg(dataReg);
    } else {
      if (isSWSExtLoadInst(MI)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi16r), dataReg)
        .addReg(addyReg)
        .addImm(gID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADu16r), dataReg)
        .addReg(addyReg)
        .addImm(gID);
      }
    }
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getPackedReg(dataReg, getPackedID(MI)))
    .addReg(addyReg)
    .addImm(gID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv2i32rr), AMDIL::Rxy1010)
    .addReg(addyReg)
    .addImm(mMFI->addi64Literal(4ULL << 32));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_x_comp))
    .addReg(addyReg)
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_y_comp))
    .addReg(AMDIL::Ry1010)
    .addImm(gID);
    break;
  };
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDILEGIOExpansionImpl::expandLocalLoad(MachineInstr *MI)
{
  bool HWLocal = mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem);
  if (!HWLocal || !isHardwareLocal(MI)) {
    return expandGlobalLoad(MI);
  }
  if (!mMFI->usesLDS() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t lID = getPointerID(MI);
  assert(lID && "Found a LDS load that was incorrectly marked as zero ID!\n");
  if (!lID) {
    lID = mSTM->device()->getResourceID(AMDILDevice::LDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  unsigned mulOp = 0;
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are generated before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADv4i32r), dataReg)
    .addReg(addyReg)
    .addImm(lID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADv2i32r), AMDIL::Rxy1011)
    .addReg(addyReg)
    .addImm(lID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::COPY),
            getPackedReg(dataReg, getPackedID(MI)))
    .addReg(AMDIL::Rxy1011);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r),
            getPackedReg(dataReg, getPackedID(MI)))
    .addReg(addyReg)
    .addImm(lID);
    break;
  case 1:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteLDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      mulOp = (mSTM->device()->usesSoftware(AMDILDeviceInfo::LocalMem))
              ? AMDIL::UMULi32rr : AMDIL::UMUL24i32rr;
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r), dataReg)
      .addReg(addyReg)
      .addImm(lID);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rx1008)
      .addReg(dataReg);
    } else {
      if (isSWSExtLoadInst(MI)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi8r), dataReg)
        .addReg(addyReg)
        .addImm(lID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADu8r), dataReg)
        .addReg(addyReg)
        .addImm(lID);
      }
    }
    break;
  case 2:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteLDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      mulOp = (mSTM->device()->usesSoftware(AMDILDeviceInfo::LocalMem))
              ? AMDIL::UMULi32rr : AMDIL::UMUL24i32rr;
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0xFFFFFFFC));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r), dataReg)
      .addReg(AMDIL::Rx1010)
      .addImm(lID);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IBIT_EXTRACTi32rrr), dataReg)
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1008)
      .addReg(dataReg);
    } else {
      if (isSWSExtLoadInst(MI)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi16r), dataReg)
        .addReg(addyReg)
        .addImm(lID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADu16r), dataReg)
        .addReg(addyReg)
        .addImm(lID);
      }
    }
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDILEGIOExpansionImpl::expandGlobalStore(MachineInstr *MI)
{
  bool usesArena = isArenaOp(MI);
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = 0;
  if (MI->getOperand(0).isReg()) {
    dataReg = MI->getOperand(0).getReg();
  }
  // These instructions are expandted before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  expandArenaSetup(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
              getCompReg(addyReg, sub_x_comp))
      .addReg(getCompReg(dataReg, sub_x_comp))
      .addImm(ID);
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::ArenaVectors)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
                getCompReg(addyReg, sub_y_comp))
        .addReg(getCompReg(dataReg, sub_y_comp))
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
                getCompReg(addyReg, sub_z_comp))
        .addReg(getCompReg(dataReg, sub_z_comp))
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
                getCompReg(addyReg, sub_w_comp))
        .addReg(getCompReg(dataReg, sub_w_comp))
        .addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1008)
        .addReg(dataReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32STOREi32), AMDIL::Rx1007)
        .addReg(AMDIL::Rx1008)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(3);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1008)
        .addReg(dataReg)
        .addImm(3);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32STOREi32), AMDIL::Rx1007)
        .addReg(AMDIL::Rx1008)
        .addImm(ID);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(4);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1008)
        .addReg(dataReg)
        .addImm(4);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32STOREi32), AMDIL::Rx1007)
        .addReg(AMDIL::Rx1008)
        .addImm(ID);
      }
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREv4i32), AMDIL::MEM)
      .addReg(addyReg)
      .addReg(dataReg)
      .addImm(ID);
    }
    break;
  case 1:
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), dataReg)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0xFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi8), addyReg)
      .addReg(dataReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
      .addReg(addyReg)
      .addReg(dataReg)
      .addImm(ID);
    }
    break;
  case 2:
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), dataReg)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0xFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi16), addyReg)
      .addReg(dataReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
      .addReg(addyReg)
      .addReg(dataReg)
      .addImm(ID);
    }
    break;
  case 4:
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32), addyReg)
      .addReg(dataReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
      .addReg(addyReg)
      .addReg(dataReg)
      .addImm(ID);
    }
    break;
  case 8:
    if (usesArena) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
              getCompReg(addyReg, sub_x_comp, sub_z_comp))
      .addReg(getCompReg(dataReg, sub_x_comp, sub_z_comp))
      .addImm(ID);
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::ArenaVectors)) {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVARENA32STOREi32),
                getCompReg(addyReg, sub_y_comp, sub_w_comp))
        .addReg(getCompReg(dataReg, sub_y_comp, sub_w_comp))
        .addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1007)
        .addReg(addyReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(AMDIL::VEXTRACTv4i32r), AMDIL::Rx1008)
        .addReg(dataReg)
        .addImm(2);
        BuildMI(*mBB, MI, DL, mTII->get(
                  AMDIL::UAVARENA32STOREi32), AMDIL::Rx1007)
        .addReg(AMDIL::Rx1008)
        .addImm(ID);
      }
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREv2i32), AMDIL::MEMxy)
      .addReg(addyReg)
      .addReg(dataReg)
      .addImm(ID);
    }
    break;
  };
}
void
AMDILEGIOExpansionImpl::expandRegionStore(MachineInstr *MI)
{
  bool HWRegion = mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem);
  if (!HWRegion || !isHardwareRegion(MI)) {
    return expandGlobalStore(MI);
  }
  mKM->setOutputInst();
  if (!mMFI->usesGDS() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t gID = getPointerID(MI);
  assert(gID && "Found a GDS store that was incorrectly marked as zero ID!\n");
  if (!gID) {
    gID = mSTM->device()->getResourceID(AMDILDevice::GDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  unsigned mulOp = HWRegion ? AMDIL::UMUL24i32rr : AMDIL::UMUL24i32rr;
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are expandted before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  switch (getMemorySize(MI)) {
  default:

    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1010)
    .addReg(addyReg)
    .addImm(mMFI->addi128Literal(4ULL << 32, 8ULL | (12ULL << 32)));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rx1010)
    .addReg(getCompReg(dataReg, sub_x_comp))
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Ry1010)
    .addReg(getCompReg(dataReg, sub_y_comp))
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rz1010)
    .addReg(getCompReg(dataReg, sub_z_comp))
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rw1010)
    .addReg(getCompReg(dataReg, sub_w_comp))
    .addImm(gID);
    break;
  case 1:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteGDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0xFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1012)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                   (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1006)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1007)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(0xFFFFFF00))
      .addImm(mMFI->addi32Literal(0x00FFFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Ry1007)
      .addReg(AMDIL::Ry1008)
      .addReg(AMDIL::Rx1007)
      .addImm(mMFI->addi32Literal(0xFF00FFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rz1012)
      .addReg(AMDIL::Rz1008)
      .addReg(AMDIL::Rx1007)
      .addImm(mMFI->addi32Literal(0xFFFF00FF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLi32i32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1011)
      .addReg(AMDIL::Rx1007);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ATOM_R_MSKOR_NORET))
      .addReg(AMDIL::Rx1010)
      .addImm(mMFI->addi32Literal(0))
      .addReg(AMDIL::Rx1012)
      .addReg(AMDIL::Rx1011)
      .addImm(gID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi8r), addyReg)
      .addReg(dataReg)
      .addImm(gID);
    }
    break;
  case 2:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteGDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0x0000FFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(1));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1012)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(0x0000FFFF))
      .addImm(mMFI->addi32Literal(0xFFFF0000));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(0));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLi32i32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1011)
      .addReg(AMDIL::Rx1008);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ATOM_R_MSKOR_NORET))
      .addReg(AMDIL::Rx1010)
      .addImm(mMFI->addi32Literal(0))
      .addReg(AMDIL::Rx1012)
      .addReg(AMDIL::Rx1011)
      .addImm(gID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi16r), addyReg)
      .addReg(dataReg)
      .addImm(gID);
    }
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), addyReg)
    .addReg(dataReg)
    .addImm(gID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv2i32rr), AMDIL::Rxy1010)
    .addReg(addyReg)
    .addImm(mMFI->addi64Literal(4ULL << 32));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rx1010)
    .addReg(getCompReg(dataReg, sub_x_comp))
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Ry1010)
    .addReg(getCompReg(dataReg, sub_y_comp))
    .addImm(gID);
    break;
  };
}
void
AMDILEGIOExpansionImpl::expandLocalStore(MachineInstr *MI)
{
  bool HWLocal = mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem);
  if (!HWLocal || !isHardwareLocal(MI)) {
    return expandGlobalStore(MI);
  }
  DebugLoc DL = MI->getDebugLoc();
  if (!mMFI->usesLDS() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t lID = getPointerID(MI);
  assert(lID && "Found a LDS store that was incorrectly marked as zero ID!\n");
  if (!lID) {
    lID = mSTM->device()->getResourceID(AMDILDevice::LDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  unsigned mulOp = HWLocal ? AMDIL::UMUL24i32rr : AMDIL::UMUL24i32rr;
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are expandted before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREv4i32r), AMDIL::MEM)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(lID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREv2i32r), AMDIL::MEMxy)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(lID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREi32r), addyReg)
    .addReg(dataReg)
    .addImm(lID);
    break;
  case 1:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteLDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0xFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1012)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                   (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
      BuildMI(*mBB, MI, DL, mTII->get(mulOp), AMDIL::Rx1006)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(8));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1007)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(0xFFFFFF00))
      .addImm(mMFI->addi32Literal(0x00FFFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1007)
      .addReg(AMDIL::Ry1008)
      .addReg(AMDIL::Rx1007)
      .addImm(mMFI->addi32Literal(0xFF00FFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1012)
      .addReg(AMDIL::Rz1008)
      .addReg(AMDIL::Rx1007)
      .addImm(mMFI->addi32Literal(0xFFFF00FF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLi32i32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1011)
      .addReg(AMDIL::Rx1006);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ATOM_L_MSKOR_NORET))
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0))
      .addReg(AMDIL::Rx1012)
      .addReg(AMDIL::Rx1011)
      .addImm(lID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREi8r), addyReg)
      .addReg(dataReg)
      .addImm(lID);
    }
    break;
  case 2:
    if (!mSTM->device()->usesHardware(AMDILDeviceInfo::ByteLDSOps)) {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1011)
      .addReg(dataReg)
      .addImm(mMFI->addi32Literal(0x0000FFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(3));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(1));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1012)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(0x0000FFFF))
      .addImm(mMFI->addi32Literal(0xFFFF0000));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1008)
      .addReg(AMDIL::Rx1008)
      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(0));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLi32i32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1011)
      .addReg(AMDIL::Rx1008);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ATOM_L_MSKOR_NORET))
      .addReg(addyReg)
      .addImm(mMFI->addi32Literal(0))
      .addReg(AMDIL::Rx1012)
      .addReg(AMDIL::Rx1011)
      .addImm(lID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREi16r), addyReg)
      .addReg(dataReg)
      .addImm(lID);
    }
    break;
  }
}
void
AMDILEGIOExpansionImpl::expandArenaSetup(MachineInstr *MI, uint32_t &addyReg)
{
  if (!isArenaOp(MI)) {
    return;
  }
  const MCInstrDesc &TID = (MI->getDesc());
  const MCOperandInfo &TOI = TID.OpInfo[0];
  unsigned short RegClass = TOI.RegClass;
  DebugLoc DL = MI->getDebugLoc();
  switch (RegClass) {
  case AMDIL::GPRV4I16RegClassID:
  case AMDIL::GPRI64RegClassID:
  case AMDIL::GPRF64RegClassID:
  case AMDIL::GPRV2I32RegClassID:
  case AMDIL::GPRV2F32RegClassID:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv2i32rr), AMDIL::Rxy1010)
    .addReg(addyReg)

    .addImm(mMFI->addi64Literal(4ULL << 32));
    addyReg = AMDIL::Rxy1010;
    break;
  default:

    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1010)
    .addReg(addyReg)
    .addImm(mMFI->addi128Literal(4ULL << 32, 8ULL | (12ULL << 32)));
    addyReg = AMDIL::R1010;
    break;
  case AMDIL::GPRI8RegClassID:
  case AMDIL::GPRV2I8RegClassID:
  case AMDIL::GPRI16RegClassID:
  case AMDIL::GPRV2I16RegClassID:
  case AMDIL::GPRV4I8RegClassID:
  case AMDIL::GPRI32RegClassID:
  case AMDIL::GPRF32RegClassID:
    break;
  };
}
