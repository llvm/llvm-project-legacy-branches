//===-- AMDIL7XXIOExpansion.cpp -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementation of the IO Printing class for 7XX devices.
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
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/Support/DebugLoc.h"
#include <cstdio>

using namespace llvm;

namespace llvm
{
extern void initializeAMDIL7XXIOExpansionPass(PassRegistry&);
}

char AMDIL7XXIOExpansion::ID = 0;
INITIALIZE_PASS(AMDIL7XXIOExpansion, "7xx-io-expansion",
                "AMDIL 7XX IO Expansion", false, false);

AMDIL7XXIOExpansion::AMDIL7XXIOExpansion()
  : MachineFunctionPass(ID)
{
  initializeAMDIL7XXIOExpansionPass(*PassRegistry::getPassRegistry());
}
const char *AMDIL7XXIOExpansion::getPassName() const
{
  return "AMDIL 7XX IO Expansion Pass";
}
bool AMDIL7XXIOExpansion::runOnMachineFunction(MachineFunction& MF)
{
  AMDIL7XXIOExpansionImpl impl(MF);
  return impl.run();
}
void
AMDIL7XXIOExpansionImpl::expandGlobalLoad(MachineInstr *MI)
{
  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandLoadStartCode(MI, addyReg);
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  switch(getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADv4i32), dataReg)
    .addReg(addyReg)
    .addImm(ID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADi32), dataReg)
    .addReg(addyReg)
    .addImm(ID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADv2i32), AMDIL::Rxy1011)
    .addReg(addyReg)
    .addImm(ID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::COPY), dataReg)
    .addReg(AMDIL::Rxy1011);
    break;
  case 1:
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADi32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(ID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi8i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  case 2:
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32LOADi32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(ID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi16i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDIL7XXIOExpansionImpl::expandRegionLoad(MachineInstr *MI)
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
  uint32_t gID = getPointerID(MI);
  assert(gID && "Found a GDS load that was incorrectly marked as zero ID!\n");
  if (!gID) {
    gID = mSTM->device()->getResourceID(AMDILDevice::GDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }

  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1010)
    .addReg(addyReg)
    .addImm(mMFI->addi128Literal(1ULL << 32, 2ULL | (3ULL << 32)));
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UMULi32rr), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi32Literal(8));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(0xFFFFFFFC));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(gID);
    // The instruction would normally fit in right here so everything created
    // after this point needs to go into the afterInst vector.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  case 2:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UMULi32rr), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi32Literal(8));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(0xFFFFFFFC));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(gID);
    // The instruction would normally fit in right here so everything created
    // after this point needs to go into the afterInst vector.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r), dataReg)
    .addReg(addyReg)
    .addImm(gID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv2i32rr), AMDIL::Rxy1010)
    .addReg(addyReg)
    .addImm(mMFI->addi64Literal(1ULL << 32));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_x_comp, sub_z_comp))
    .addReg(AMDIL::Rx1010)
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32LOADi32r),
            getCompReg(dataReg, sub_y_comp, sub_w_comp))
    .addReg(AMDIL::Ry1010)
    .addImm(gID);
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDIL7XXIOExpansionImpl::expandLocalLoad(MachineInstr *MI)
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
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADv4i32r), dataReg)
    .addReg(addyReg)
    .addImm(lID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADv2i32r), dataReg)
    .addReg(addyReg)
    .addImm(lID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r), dataReg)
    .addReg(addyReg)
    .addImm(lID);
    break;
  case 1:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UMULi32rr), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi32Literal(8));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(0xFFFFFFFC));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(lID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  case 2:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UMULi32rr), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi32Literal(8));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(0xFFFFFFFC));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32LOADi32r), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1010)
    .addImm(lID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), dataReg)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008);
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDIL7XXIOExpansionImpl::expandGlobalStore(MachineInstr *MI)
{
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREv4i32), AMDIL::MEM)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 1:
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[BYTE_STORE_ERROR]);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 2:
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[BYTE_STORE_ERROR]);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UAVRAW32STOREv2i32), AMDIL::MEMxy)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  };
}
void
AMDIL7XXIOExpansionImpl::expandRegionStore(MachineInstr *MI)
{
  bool HWRegion = mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem);
  if (!mSTM->device()->isSupported(AMDILDeviceInfo::RegionMem)) {
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[REGION_MEMORY_ERROR]);
    return;
  }
  if (!HWRegion || !isHardwareRegion(MI)) {
    return expandGlobalStore(MI);
  }
  DebugLoc DL = MI->getDebugLoc();
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

  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1010)
    .addReg(addyReg)
    .addImm(mMFI->addi128Literal(1ULL << 32, 2ULL | (3ULL << 32)));
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
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[BYTE_STORE_ERROR]);
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UMULi32rr), AMDIL::Rx1006)
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
    .addReg(AMDIL::Rx1007);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rx1010)
    .addReg(AMDIL::Rx1011)
    .addImm(gID);
    break;
  case 2:
    mMFI->addErrorMsg(
      amd::CompilerErrorMessage[BYTE_STORE_ERROR]);
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rx1010)
    .addReg(AMDIL::Rx1011)
    .addImm(gID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), addyReg)
    .addReg(dataReg)
    .addImm(gID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv2i32rr), AMDIL::Rxy1010)
    .addReg(addyReg)
    .addImm(mMFI->addi64Literal(1ULL << 32));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Rx1010)
    .addReg(getCompReg(dataReg, sub_y_comp))
    .addImm(gID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::GDS32STOREi32r), AMDIL::Ry1010)
    .addReg(getCompReg(dataReg, sub_y_comp))
    .addImm(gID);
    break;
  };
}
void
AMDIL7XXIOExpansionImpl::expandLocalStore(MachineInstr *MI)
{
  bool HWLocal = mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem);
  if (!HWLocal || !isHardwareLocal(MI)) {
    return expandGlobalStore(MI);
  }
  uint32_t lID = getPointerID(MI);
  assert(lID && "Found a LDS store that was incorrectly marked as zero ID!\n");
  if (!lID) {
    lID = mSTM->device()->getResourceID(AMDILDevice::LDS_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  expandStoreSetupCode(MI, addyReg, dataReg);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LDS32STOREv4i32r), AMDIL::MEM)
  .addReg(addyReg)
  .addReg(dataReg)
  .addImm(lID);
}
