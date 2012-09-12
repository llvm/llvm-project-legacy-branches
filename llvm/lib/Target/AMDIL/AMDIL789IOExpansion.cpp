//===-- AMDIL789IOExpansion.cpp -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementation of the IO expansion class for 789 devices.
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

// This code produces the following pseudo-IL:
// cmov_logical r1006.x___, r1008.y, r1006.y, r1006.x
// cmov_logical r1006.x___, r1008.z, r1006.x, r1006.z
// cmov_logical $dst.x___, r1008.w, r1006.x, r1006.w
void
AMDIL789IOExpansionImpl::emitComponentExtract(MachineInstr *MI,
                                              unsigned src,
                                              unsigned dst,
                                              bool before)
{
  DebugLoc DL = MI->getDebugLoc();
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr),
          getCompReg(src, sub_x_comp))
  .addReg(AMDIL::Ry1008)
  .addReg(getCompReg(src, sub_y_comp))
  .addReg(getCompReg(src, sub_x_comp));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr),
          getCompReg(src, sub_x_comp))
  .addReg(AMDIL::Rz1008)
  .addReg(getCompReg(src, sub_z_comp))
  .addReg(getCompReg(src, sub_x_comp));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), dst)
  .addReg(AMDIL::Rw1008)
  .addReg(getCompReg(src, sub_w_comp))
  .addReg(getCompReg(src, sub_x_comp));
}
// We have a 128 bit load but a 8/16/32bit value, so we need to
// select the correct component and make sure that the correct
// bits are selected. For the 8 and 16 bit cases we need to
// extract from the component the correct bits and for 32 bits
// we just need to select the correct component.
void
AMDIL789IOExpansionImpl::emitDataLoadSelect(MachineInstr *MI,
                                            uint32_t &dataReg,
                                            uint32_t &addyReg)
{
  DebugLoc DL = MI->getDebugLoc();
  if (getMemorySize(MI) == 1) {
    emitComponentExtract(MI, AMDIL::R1011, AMDIL::Rx1011, false);
    // This produces the following pseudo-IL:
    // iand r1006.x___, addyReg.xxxx, l14.xxxx
    // iadd r1006, r1006.x, {0, -1, 2, 3}
    // ieq r1008, r1006, 0
    // ishr r1011, r1011.x, {0, 8, 16, 24}
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1006)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1006)
    .addReg(AMDIL::Rx1006)
    .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                 (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::EQv4i32rr), AMDIL::R1008)
    .addReg(AMDIL::R1006)
    .addImm(mMFI->addi32Literal(0));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRv4i32i32rr), AMDIL::R1011)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
    emitComponentExtract(MI, AMDIL::R1011, dataReg, false);
  } else if (getMemorySize(MI) == 2) {
    emitComponentExtract(MI, AMDIL::R1011, AMDIL::Rx1011, false);
    // This produces the following pseudo-IL:
    // ishr r1007.x___, addyReg.xxxx, 1
    // iand r1008.x___, r1007.xxxx, 1
    // ishr r1007.x___, r1011.xxxx, 16
    // cmov_logical r1011.x___, r1008.xxxx, r1007.xxxx, r1011.xxxx
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1007)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1007)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1007)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), dataReg)
    .addReg(AMDIL::Rx1008)
    .addReg(AMDIL::Rx1007)
    .addReg(AMDIL::Rx1011);
  } else {
    emitComponentExtract(MI, AMDIL::R1011, dataReg, false);
  }
}
// This function does address calculations modifications to load from a vector
// register type instead of a dword addressed load.
void
AMDIL789IOExpansionImpl::emitVectorAddressCalc(MachineInstr *MI, bool is32bit,
                                               bool needsSelect,
                                               uint32_t &addyReg)
{
  DebugLoc DL = MI->getDebugLoc();
  // This produces the following pseudo-IL:
  // ishr r1007.x___, r1010.xxxx, (is32bit) ? 2 : 3
  // iand r1008.x___, r1007.xxxx, (is32bit) ? 3 : 1
  // ishr r1007.x___, r1007.xxxx, (is32bit) ? 2 : 1
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1007)
  .addReg(addyReg)
  .addImm(mMFI->addi32Literal((is32bit) ? 0x2 : 3));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1008)
  .addReg(AMDIL::Rx1007)
  .addImm(mMFI->addi32Literal((is32bit) ? 3 : 1));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1007)
  .addReg(AMDIL::Rx1007)
  .addImm(mMFI->addi32Literal((is32bit) ? 2 : 1));
  if (needsSelect) {
    // If the component selection is required, the following
    // pseudo-IL is produced.
    // iadd r1008, r1008.x, (is32bit) ? {0, -1, -2, -3} : {0, 0, -1, -1}
    // ieq r1008, r1008, 0
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi128Literal((is32bit) ? 0xFFFFFFFFULL << 32 : 0ULL,
                                 (is32bit) ? 0xFFFFFFFEULL |
                                 (0xFFFFFFFDULL << 32) :
                                 -1ULL));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::EQv4i32rr), AMDIL::R1008)
    .addReg(AMDIL::R1008)
    .addImm(mMFI->addi32Literal(0));
  }
  addyReg = AMDIL::Rx1007;
}
// This function emits a switch statement and writes 32bit/64bit
// value to a 128bit vector register type.
void
AMDIL789IOExpansionImpl::emitVectorSwitchWrite(MachineInstr *MI,
                                               bool is32bit,
                                               uint32_t &addyReg,
                                               uint32_t &dataReg)
{
  uint32_t xID = getPointerID(MI);
  assert(
    xID && "Found a scratch store that was incorrectly marked as zero ID!\n");
  // This section generates the following pseudo-IL:
  // switch r1008.x
  // default
  //   mov x1[$addyReg.x].(is32bit) ? x___ : xy__, r1011.x{y}
  // break
  // case 1
  //   mov x1[$addyReg.x].(is32bit) ? _y__ : __zw, r1011.x{yxy}
  // break
  // if is32bit is true, case 2 and 3 are emitted.
  // case 2
  //   mov x1[$addyReg.x].__z_, r1011.x
  // break
  // case 3
  //   mov x1[$addyReg.x].___w, r1011.x
  // break
  // endswitch
  DebugLoc DL = MI->getDebugLoc();
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SWITCH))
  .addReg(AMDIL::Rx1008);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DEFAULT));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SCRATCH32STORE),
          (is32bit ? addyReg
           : (addyReg - AMDIL::Rx1) + AMDIL::Rxy1))
  .addReg(dataReg).addImm(xID);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(1);
  BuildMI(*mBB, MI, DL,
          mTII->get(AMDIL::SCRATCH32STORE),
          ( is32bit ? (addyReg - AMDIL::Rx1) + AMDIL::Ry1
            : (addyReg - AMDIL::Rx1) + AMDIL::Rzw1))
  .addReg(dataReg).addImm(xID);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  if (is32bit) {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(2);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32STORE),
            (addyReg - AMDIL::Rx1) + AMDIL::Rz1)
    .addReg(dataReg).addImm(xID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(3);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32STORE),
            (addyReg - AMDIL::Rx1) + AMDIL::Rw1)
    .addReg(dataReg).addImm(xID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  }
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ENDSWITCH));
}
void
AMDIL789IOExpansionImpl::expandPrivateLoad(MachineInstr *MI)
{
  bool HWPrivate = mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem);
  if (!HWPrivate || mSTM->device()->isSupported(AMDILDeviceInfo::PrivateUAV)) {
    return expandGlobalLoad(MI);
  }
  if (!mMFI->usesScratch() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t xID = getPointerID(MI);
  assert(
    xID && "Found a scratch load that was incorrectly marked as zero ID!\n");
  if (!xID) {
    xID = mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  uint32_t origReg = MI->getOperand(1).getReg();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    // Since the private register is a 128 bit aligned, we have to align the address
    // first, since our source address is 32bit aligned and then load the data.
    // This produces the following pseudo-IL:
    // ishr r1010.x___, r1010.xxxx, 4
    // mov r1011, x1[r1010.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32LOAD), dataReg)
    .addReg(AMDIL::Rx1010)
    .addImm(xID);
    break;
  case 1:
  case 2:
  case 4:
    emitVectorAddressCalc(MI, true, true, addyReg);
    // This produces the following pseudo-IL:
    // mov r1011, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32LOAD), AMDIL::R1011)
    .addReg(addyReg)
    .addImm(xID);
    // These instructions go after the current MI.
    emitDataLoadSelect(MI, dataReg, origReg);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, true, addyReg);
    // This produces the following pseudo-IL:
    // mov r1011, x1[r1007.x]
    // cmov_logical r1011.xy__, r1008.xxxx, r1011.xy, r1011.zw
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32LOAD), AMDIL::R1011)
    .addReg(addyReg)
    .addImm(xID);
    if (isExtLoadInst(MI)
        && MI->getDesc().OpInfo[0].RegClass
        == AMDIL::GPRV2F64RegClassID) {
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::SELECTi64rrr),
              getCompReg(dataReg, sub_xy_comp))
      .addReg(AMDIL::Rx1008)
      .addReg(AMDIL::Rxy1011)
      .addReg(AMDIL::Rzw1011);
    } else {
      BuildMI(*mBB, MI, DL,
              mTII->get(AMDIL::SELECTi64rrr), dataReg)
      .addReg(AMDIL::Rx1008)
      .addReg(AMDIL::Rxy1011)
      .addReg(AMDIL::Rzw1011);
    }
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDIL789IOExpansionImpl::expandConstantLoad(MachineInstr *MI)
{
  if (!isHardwareInst(MI) || MI->memoperands_empty()) {
    return expandGlobalLoad(MI);
  }
  uint32_t cID = getPointerID(MI);
  if (cID < 2) {
    return expandGlobalLoad(MI);
  }
  if (!mMFI->usesConstant() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }

  DebugLoc DL = MI->getDebugLoc();
  uint32_t origReg = MI->getOperand(1).getReg();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions go before the current MI.
  expandLoadStartCode(MI, addyReg);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CB32LOAD), dataReg)
    .addReg(AMDIL::Rx1010)
    .addImm(cID);
    break;
  case 1:
  case 2:
  case 4:
    emitVectorAddressCalc(MI, true, true, addyReg);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CB32LOAD), AMDIL::R1011)
    .addReg(addyReg)
    .addImm(cID);
    emitDataLoadSelect(MI, dataReg, origReg);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, true, addyReg);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CB32LOAD), AMDIL::R1011)
    .addReg(addyReg)
    .addImm(cID);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SELECTi64rrr), dataReg)
    .addReg(AMDIL::Rx1008)
    .addReg(AMDIL::Rxy1011)
    .addReg(AMDIL::Rzw1011);
    break;
  }
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDIL789IOExpansionImpl::expandConstantPoolLoad(MachineInstr *MI)
{
  if (!isStaticCPLoad(MI)) {
    return expandConstantLoad(MI);
  } else {
    uint32_t dataReg = MI->getOperand(0).getReg();
    uint32_t idx = MI->getOperand(1).getIndex();
    const MachineConstantPool *MCP = MI->getParent()->getParent()
                                     ->getConstantPool();
    const std::vector<MachineConstantPoolEntry> &consts
      = MCP->getConstants();
    const Constant *C = consts[idx].Val.ConstVal;
    emitCPInst(MI, C, mKM, 0, isExtendLoad(MI), dataReg);
  }
}
void
AMDIL789IOExpansionImpl::expandPrivateStore(MachineInstr *MI)
{
  bool HWPrivate = mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem);
  if (!HWPrivate || mSTM->device()->isSupported(AMDILDeviceInfo::PrivateUAV)) {
    return expandGlobalStore(MI);
  }
  if (!mMFI->usesScratch() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t xID = getPointerID(MI);
  assert(
    xID && "Found a scratch store that was incorrectly marked as zero ID!\n");
  if (!xID) {
    xID = mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  uint32_t dataReg = MI->getOperand(0).getReg();
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t origReg = addyReg;
  if (origReg == AMDIL::DFP) {
    BuildMI(*mBB, MI, DL, mTII->get(TargetOpcode::COPY), AMDIL::Rx1010).addReg(
      addyReg);
    addyReg = origReg = AMDIL::Rx1010;
  }
  // These instructions go before the current MI.
  expandStoreSetupCode(MI, addyReg, dataReg);
  switch (getMemorySize(MI)) {
  default:
    // This section generates the following pseudo-IL:
    // ishr r1010.x___, r1010.xxxx, 4
    // mov x1[r1010.x], r1011
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1010)
    .addReg(addyReg)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32STORE), AMDIL::Rx1010)
    .addReg(dataReg)
    .addImm(xID);
    break;
  case 1:
    emitVectorAddressCalc(MI, true, true, addyReg);
    // This section generates the following pseudo-IL:
    // mov r1002, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32LOAD), AMDIL::R1002)
    .addReg(addyReg)
    .addImm(xID);
    emitComponentExtract(MI, AMDIL::R1002, AMDIL::Rx1002, true);
    // This section generates the following pseudo-IL:
    // iand r1003.x, r1010.x, 3
    // iadd r1001, r1003.x, {0, -1, -2, -3}
    // ieq r1001, r1001, 0
    // ishr r1002, r1002.x, {0, 8, 16, 24}
    // cmov_logical r1002, r1001, r1011.x, r1002
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1003)
    .addReg(origReg)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADDv4i32rr), AMDIL::R1001)
    .addReg(AMDIL::Rx1003)
    .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                 (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::EQv4i32rr), AMDIL::R1001)
    .addReg(AMDIL::R1001)
    .addImm(mMFI->addi32Literal(0));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRv4i32i32rr), AMDIL::R1002)
    .addReg(AMDIL::Rx1002)
    .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTv4i32rrr), AMDIL::R1002)
    .addReg(AMDIL::R1001)
    .addReg(dataReg)
    .addReg(AMDIL::R1002);
    if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
      // This section generates the following pseudo-IL:
      // iand r1002, r1002, 0xFF
      // ishl r1002, r1002, {0, 8, 16, 24}
      // ior r1002.xy, r1002.xy, r1002.zw
      // ior r1011.x, r1002.x, r1002.y
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDv4i32rr), AMDIL::R1002)
      .addReg(AMDIL::R1002)
      .addImm(mMFI->addi32Literal(0xFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLv4i32i32rr), AMDIL::R1002)
      .addReg(AMDIL::R1002)
      .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i64rr), AMDIL::Rxy1002)
      .addReg(AMDIL::Rxy1002).addReg(AMDIL::Rzw1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Ry1002).addReg(AMDIL::Rx1002);
    } else {
      // This section generates the following pseudo-IL:
      // mov r1001.xy, r1002.yw
      // mov r1002.xy, r1002.xz
      // ubit_insert r1002.xy, 8, 8, r1001.xy, r1002.xy
      // ubit_insert r1011.x, 16, 16, r1002.y, r1002.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LHIv2i64r), AMDIL::Rxy1001)
      .addReg(AMDIL::R1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LLOv2i64r), AMDIL::Rxy1002)
      .addReg(AMDIL::R1002);
      BuildMI(*mBB, MI, DL, mTII->get(
                AMDIL::UBIT_INSERTv2i32rrrr), AMDIL::Rxy1002)
      .addImm(mMFI->addi32Literal(8))
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rxy1001)
      .addReg(AMDIL::Rxy1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTi32rrrr), AMDIL::Rx1011)
      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Ry1002)
      .addReg(AMDIL::Rx1002);
    }
    dataReg = AMDIL::Rx1011;
    emitVectorAddressCalc(MI, true, false, origReg);
    emitVectorSwitchWrite(MI, true, origReg, dataReg);
    break;
  case 2:
    emitVectorAddressCalc(MI, true, true, addyReg);
    // This section generates the following pseudo-IL:
    // mov r1002, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCH32LOAD), AMDIL::R1002)
    .addReg(addyReg)
    .addImm(xID);
    emitComponentExtract(MI, AMDIL::R1002, AMDIL::Rx1002, true);
    // This section generates the following pseudo-IL:
    // ishr r1003.x, $origReg, 1
    // iand r1003.x, r1003.x, 1
    // ishr r1001.x, r1002.x, 16
    // cmov_logical r1002.x, r1003.x, r1002.x, $origReg
    // cmov_logical r1001.x, r1003.x, $origReg, r1001.x
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1003)
    .addReg(origReg)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1003)
    .addReg(AMDIL::Rx1003)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRi32i32rr), AMDIL::Rx1001)
    .addReg(AMDIL::Rx1002)
    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1002)
    .addReg(AMDIL::Rx1003)
    .addReg(AMDIL::Rx1002)
    .addReg(dataReg);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SELECTi32rrr), AMDIL::Rx1001)
    .addReg(AMDIL::Rx1003)
    .addReg(dataReg)
    .addReg(AMDIL::Rx1001);
    if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
      // This section generates the following pseudo-IL:
      // iand r1002.x, r1002.x, 0xFFFF
      // iand r1001.x, r1001.x, 0xFFFF
      // ishl r1001.x, r1002.x, 16
      // ior r1011.x, r1002.x, r1001.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1002)
      .addReg(AMDIL::Rx1002)
      .addImm(mMFI->addi32Literal(0xFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDi32rr), AMDIL::Rx1001)
      .addReg(AMDIL::Rx1001)
      .addImm(mMFI->addi32Literal(0xFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLi32i32rr), AMDIL::Rx1001)
      .addReg(AMDIL::Rx1001)
      .addImm(mMFI->addi32Literal(16));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ORi32rr), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1002).addReg(AMDIL::Rx1001);
      dataReg = AMDIL::Rx1011;
    } else {
      // This section generates the following pseudo-IL:
      // ubit_insert r1011.x, 16, 16, r1001.x, r1002.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERTi32rrrr), AMDIL::Rx1011)

      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1001)
      .addReg(AMDIL::Rx1002);
      dataReg = AMDIL::Rx1011;
    }
    emitVectorAddressCalc(MI, true, false, origReg);
    emitVectorSwitchWrite(MI, true, origReg, dataReg);
    break;
  case 4:
    emitVectorAddressCalc(MI, true, false, addyReg);
    emitVectorSwitchWrite(MI, true, addyReg, dataReg);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, false, addyReg);
    emitVectorSwitchWrite(MI, false, addyReg, dataReg);
    break;
  };
}
void
AMDIL789IOExpansionImpl::expandStoreSetupCode(MachineInstr *MI,
                                              uint32_t &addyReg,
                                              uint32_t &dataReg)
{
  DebugLoc DL;
  bool is64bit = is64bitLSOp(MI);
  if (MI->getOperand(0).isUndef()) {
    BuildMI(*mBB, MI, DL, mTII->get(TargetOpcode::COPY), dataReg)
    .addImm(mMFI->addi32Literal(0));
  }
  expandTruncData(MI, dataReg);
  if (MI->getOperand(2).isReg()) {
    uint32_t newReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
    uint32_t addInst = (is64bit) ? AMDIL::ADDi64rr : AMDIL::ADDi32rr;
    BuildMI(*mBB, MI, DL, mTII->get(addInst), newReg)
    .addReg(addyReg)
    .addReg(MI->getOperand(2).getReg());
    addyReg = newReg;
  }
  expandAddressCalc(MI, addyReg);
  expandPackedData(MI, dataReg);
}
void
AMDIL789IOExpansionImpl::expandPackedData(MachineInstr *MI, uint32_t &dataReg)
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
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDv2i32rr), AMDIL::Rxy1011)
    .addReg(dataReg)
    .addImm(mMFI->addi64Literal(0xFFULL | (0xFFULL << 32)));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLv2i32i32rr), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011).addImm(mMFI->addi64Literal(8ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i32rr),
            getCompReg(dataReg, sub_x_comp))
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);
    break;
  case PACK_V4I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDv4i32rr), AMDIL::R1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLv4i32i32rr), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi128Literal(8ULL << 32, (16ULL | (24ULL << 32))));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i64rr), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011).addReg(AMDIL::Rzw1011);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i32rr),
            getCompReg(dataReg, sub_x_comp))
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);
    break;
  case PACK_V2I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDv2i32rr), AMDIL::Rxy1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFFFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLv2i32i32rr), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011)
    .addImm(mMFI->addi64Literal(16ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv2i32rr),
            getCompReg(dataReg, sub_x_comp))
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);
    break;
  case PACK_V4I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ANDv4i32rr), AMDIL::R1011)
    .addReg(dataReg)
    .addImm(mMFI->addi32Literal(0xFFFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHLv4i32i32rr), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi64Literal(16ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITORv4i16rr),
            getCompReg(dataReg, sub_xy_comp))
    .addReg(AMDIL::Rxy1011).addReg(AMDIL::Rzw1011);
    break;
  case UNPACK_V2I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRi32i32rr),
            getCompReg(dataReg, sub_y_comp))
    .addReg(packedReg)
    .addImm(mMFI->addi32Literal(8));
    break;
  case UNPACK_V4I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRv4i8i32rr), dataReg)
    .addReg(packedReg)
    .addImm(mMFI->addi128Literal(8ULL << 32, (16ULL | (24ULL << 32))));
    break;
  case UNPACK_V2I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRi32i32rr),
            getCompReg(dataReg, sub_y_comp))
    .addReg(packedReg)
    .addImm(mMFI->addi32Literal(16));
    break;
  case UNPACK_V4I16:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRv2i32i32rr), AMDIL::Rxy1012)
    .addReg(packedReg)
    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATEv2i64rr), dataReg)
    .addReg(packedReg).addReg(AMDIL::Rxy1012);
    break;
  };
}
