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
AMDIL789IOExpansion::AMDIL789IOExpansion(TargetMachine &tm,
    CodeGenOpt::Level OptLevel)
  : AMDILIOExpansion(tm, OptLevel)
{
}

AMDIL789IOExpansion::~AMDIL789IOExpansion()
{
}

const char *AMDIL789IOExpansion::getPassName() const
{
  return "AMDIL 789 IO Expansion Pass";
}
// This code produces the following pseudo-IL:
// cmov_logical r1006.x___, r1008.y, r1006.y, r1006.x
// cmov_logical r1006.x___, r1008.z, r1006.x, r1006.z
// cmov_logical $dst.x___, r1008.w, r1006.x, r1006.w
void
AMDIL789IOExpansion::emitComponentExtract(MachineInstr *MI,
    unsigned src, unsigned dst, bool before)
{
  DebugLoc DL = MI->getDebugLoc();
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32),
          (src - AMDIL::R1) + AMDIL::Rx1)
  .addReg(AMDIL::Ry1008)
  .addReg((src - AMDIL::R1) + AMDIL::Ry1)
  .addReg((src - AMDIL::R1) + AMDIL::Rx1);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32),
          (src - AMDIL::R1) + AMDIL::Rx1)
  .addReg(AMDIL::Rz1008)
  .addReg((src - AMDIL::R1) + AMDIL::Rz1)
  .addReg((src - AMDIL::R1) + AMDIL::Rx1);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32), dst)
  .addReg(AMDIL::Rw1008)
  .addReg((src - AMDIL::R1) + AMDIL::Rw1)
  .addReg((src - AMDIL::R1) + AMDIL::Rx1);
}
// We have a 128 bit load but a 8/16/32bit value, so we need to
// select the correct component and make sure that the correct
// bits are selected. For the 8 and 16 bit cases we need to
// extract from the component the correct bits and for 32 bits
// we just need to select the correct component.
void
AMDIL789IOExpansion::emitDataLoadSelect(MachineInstr *MI)
{
  DebugLoc DL = MI->getDebugLoc();
  emitComponentExtract(MI, AMDIL::R1011, AMDIL::Rx1011, false);
  if (getMemorySize(MI) == 1) {
    // This produces the following pseudo-IL:
    // iand r1006.x___, r1010.xxxx, l14.xxxx
    // iadd r1006, r1006.x, {0, -1, 2, 3}
    // ieq r1008, r1006, 0
    // ishr r1011, r1011.x, {0, 8, 16, 24}
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1006)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADD_v4i32), AMDIL::R1006)
    .addReg(AMDIL::Rx1006)
    .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                 (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IEQ_v4i32), AMDIL::R1008)
    .addReg(AMDIL::R1006)
    .addImm(mMFI->addi32Literal(0));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRVEC_v4i32), AMDIL::R1011)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
    emitComponentExtract(MI, AMDIL::R1011, AMDIL::Rx1011, false);
  } else if (getMemorySize(MI) == 2) {
    // This produces the following pseudo-IL:
    // ishr r1007.x___, r1010.xxxx, 1
    // iand r1008.x___, r1007.xxxx, 1
    // ishr r1007.x___, r1011.xxxx, 16
    // cmov_logical r1011.x___, r1008.xxxx, r1007.xxxx, r1011.xxxx
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1007)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1008)
    .addReg(AMDIL::Rx1007)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1007)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1008)
    .addReg(AMDIL::Rx1007)
    .addReg(AMDIL::Rx1011);
  }
}
// This function does address calculations modifications to load from a vector
// register type instead of a dword addressed load.
void
AMDIL789IOExpansion::emitVectorAddressCalc(MachineInstr *MI, bool is32bit, bool needsSelect)
{
  DebugLoc DL = MI->getDebugLoc();
  // This produces the following pseudo-IL:
  // ishr r1007.x___, r1010.xxxx, (is32bit) ? 2 : 3
  // iand r1008.x___, r1007.xxxx, (is32bit) ? 3 : 1
  // ishr r1007.x___, r1007.xxxx, (is32bit) ? 2 : 1
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1007)
  .addReg(AMDIL::Rx1010)
  .addImm(mMFI->addi32Literal((is32bit) ? 0x2 : 3));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1008)
  .addReg(AMDIL::Rx1007)
  .addImm(mMFI->addi32Literal((is32bit) ? 3 : 1));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1007)
  .addReg(AMDIL::Rx1007)
  .addImm(mMFI->addi32Literal((is32bit) ? 2 : 1));
  if (needsSelect) {
    // If the component selection is required, the following
    // pseudo-IL is produced.
    // iadd r1008, r1008.x, (is32bit) ? {0, -1, -2, -3} : {0, 0, -1, -1}
    // ieq r1008, r1008, 0
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADD_v4i32), AMDIL::R1008)
    .addReg(AMDIL::Rx1008)
    .addImm(mMFI->addi128Literal((is32bit) ? 0xFFFFFFFFULL << 32 : 0ULL,
                                 (is32bit) ? 0xFFFFFFFEULL | (0xFFFFFFFDULL << 32) :
                                 -1ULL));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IEQ_v4i32), AMDIL::R1008)
    .addReg(AMDIL::R1008)
    .addImm(mMFI->addi32Literal(0));
  }
}
// This function emits a switch statement and writes 32bit/64bit
// value to a 128bit vector register type.
void
AMDIL789IOExpansion::emitVectorSwitchWrite(MachineInstr *MI, bool is32bit)
{
  uint32_t xID = getPointerID(MI);
  assert(xID && "Found a scratch store that was incorrectly marked as zero ID!\n");
  // This section generates the following pseudo-IL:
  // switch r1008.x
  // default
  //   mov x1[r1007.x].(is32bit) ? x___ : xy__, r1011.x{y}
  // break
  // case 1
  //   mov x1[r1007.x].(is32bit) ? _y__ : __zw, r1011.x{yxy}
  // break
  // if is32bit is true, case 2 and 3 are emitted.
  // case 2
  //   mov x1[r1007.x].__z_, r1011.x
  // break
  // case 3
  //   mov x1[r1007.x].___w, r1011.x
  // break
  // endswitch
  DebugLoc DL = MI->getDebugLoc();
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SWITCH))
  .addReg(AMDIL::Rx1008);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::DEFAULT));
  BuildMI(*mBB, MI, DL,
          mTII->get(AMDIL::SCRATCHSTORE),
          (is32bit) ? AMDIL::Rx1007 : AMDIL::Rxy1007)
  .addReg((is32bit) ? AMDIL::Rx1011 : AMDIL::Rxy1011)
  .addImm(xID);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(1);
  BuildMI(*mBB, MI, DL,
          mTII->get(AMDIL::SCRATCHSTORE),
          (is32bit) ? AMDIL::Ry1007 : AMDIL::Rzw1007)
  .addReg(is32bit ? AMDIL::Rx1011 : AMDIL::Rxy1011)
  .addImm(xID);
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  if (is32bit) {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(2);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHSTORE), AMDIL::Rz1007)
    .addReg(AMDIL::Rx1011)
    .addImm(xID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CASE)).addImm(3);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHSTORE), AMDIL::Rw1007)
    .addReg(AMDIL::Rx1011)
    .addImm(xID);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BREAK));
  }
  BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ENDSWITCH));

}
void
AMDIL789IOExpansion::expandPrivateLoad(MachineInstr *MI)
{
  bool HWPrivate = mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem);
  if (!HWPrivate || mSTM->device()->isSupported(AMDILDeviceInfo::PrivateUAV)) {
    return expandGlobalLoad(MI);
  }
  if (!mMFI->usesScratch() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t xID = getPointerID(MI);
  assert(xID && "Found a scratch load that was incorrectly marked as zero ID!\n");
  if (!xID) {
    xID = mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  // These instructions go before the current MI.
  expandLoadStartCode(MI);
  switch (getMemorySize(MI)) {
  default:
    // Since the private register is a 128 bit aligned, we have to align the address
    // first, since our source address is 32bit aligned and then load the data.
    // This produces the following pseudo-IL:
    // ishr r1010.x___, r1010.xxxx, 4
    // mov r1011, x1[r1010.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHR_i32), AMDIL::Rx1010)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1010)
    .addImm(xID);
    break;
  case 1:
  case 2:
  case 4:
    emitVectorAddressCalc(MI, true, true);
    // This produces the following pseudo-IL:
    // mov r1011, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1007)
    .addImm(xID);
    // These instructions go after the current MI.
    emitDataLoadSelect(MI);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, true);
    // This produces the following pseudo-IL:
    // mov r1011, x1[r1007.x]
    // cmov_logical r1011.xy__, r1008.xxxx, r1011.xy, r1011.zw
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1007)
    .addImm(xID);
    // These instructions go after the current MI.
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CMOVLOG_i64), AMDIL::Rxy1011)
    .addReg(AMDIL::Rx1008)
    .addReg(AMDIL::Rxy1011)
    .addReg(AMDIL::Rzw1011);
    break;
  }
  unsigned dataReg;
  expandPackedData(MI);
  dataReg = expandExtendLoad(MI);
  if (!dataReg) {
    dataReg = getDataReg(MI);
  }
  BuildMI(*mBB, MI, MI->getDebugLoc(),
          mTII->get(getMoveInstFromID(
                      MI->getDesc().OpInfo[0].RegClass)))
  .addOperand(MI->getOperand(0))
  .addReg(dataReg);
  MI->getOperand(0).setReg(dataReg);
}


void
AMDIL789IOExpansion::expandConstantLoad(MachineInstr *MI)
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
  // These instructions go before the current MI.
  expandLoadStartCode(MI);
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHR_i32), AMDIL::Rx1010)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CBLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1010)
    .addImm(cID);
    break;
  case 1:
  case 2:
  case 4:
    emitVectorAddressCalc(MI, true, true);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CBLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1007)
    .addImm(cID);
    // These instructions go after the current MI.
    emitDataLoadSelect(MI);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, true);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CBLOAD), AMDIL::R1011)
    .addReg(AMDIL::Rx1007)
    .addImm(cID);
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::CMOVLOG_i64), AMDIL::Rxy1011)
    .addReg(AMDIL::Rx1008)
    .addReg(AMDIL::Rxy1011)
    .addReg(AMDIL::Rzw1011);
    break;
  }
  expandPackedData(MI);
  unsigned dataReg = expandExtendLoad(MI);
  if (!dataReg) {
    dataReg = getDataReg(MI);
  }
  BuildMI(*mBB, MI, MI->getDebugLoc(),
          mTII->get(getMoveInstFromID(
                      MI->getDesc().OpInfo[0].RegClass)))
  .addOperand(MI->getOperand(0))
  .addReg(dataReg);
  MI->getOperand(0).setReg(dataReg);
}

void
AMDIL789IOExpansion::expandConstantPoolLoad(MachineInstr *MI)
{
  if (!isStaticCPLoad(MI)) {
    return expandConstantLoad(MI);
  } else {
    uint32_t idx = MI->getOperand(1).getIndex();
    const MachineConstantPool *MCP = MI->getParent()->getParent()
                                     ->getConstantPool();
    const std::vector<MachineConstantPoolEntry> &consts
    = MCP->getConstants();
    const Constant *C = consts[idx].Val.ConstVal;
    emitCPInst(MI, C, mKM, 0, isExtendLoad(MI));
  }
}

void
AMDIL789IOExpansion::expandPrivateStore(MachineInstr *MI)
{
  bool HWPrivate = mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem);
  if (!HWPrivate || mSTM->device()->isSupported(AMDILDeviceInfo::PrivateUAV)) {
    return expandGlobalStore(MI);
  }
  if (!mMFI->usesScratch() && mMFI->isKernel()) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[MEMOP_NO_ALLOCATION]);
  }
  uint32_t xID = getPointerID(MI);
  assert(xID && "Found a scratch store that was incorrectly marked as zero ID!\n");
  if (!xID) {
    xID = mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID);
    mMFI->addErrorMsg(amd::CompilerWarningMessage[RECOVERABLE_ERROR]);
  }
  DebugLoc DL = MI->getDebugLoc();
  // These instructions go before the current MI.
  expandStoreSetupCode(MI);
  switch (getMemorySize(MI)) {
  default:
    // This section generates the following pseudo-IL:
    // ishr r1010.x___, r1010.xxxx, 4
    // mov x1[r1010.x], r1011
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SHR_i32), AMDIL::Rx1010)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(4));
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHSTORE), AMDIL::Rx1010)
    .addReg(AMDIL::R1011)
    .addImm(xID);
    break;
  case 1:
    emitVectorAddressCalc(MI, true, true);
    // This section generates the following pseudo-IL:
    // mov r1002, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHLOAD), AMDIL::R1002)
    .addReg(AMDIL::Rx1007)
    .addImm(xID);
    emitComponentExtract(MI, AMDIL::R1002, AMDIL::Rx1002, true);
    // This section generates the following pseudo-IL:
    // iand r1003.x, r1010.x, 3
    // iadd r1001, r1003.x, {0, -1, -2, -3}
    // ieq r1001, r1001, 0
    // ishr r1002, r1002.x, {0, 8, 16, 24}
    // cmov_logical r1002, r1001, r1011.x, r1002
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1003)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(3));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::ADD_v4i32), AMDIL::R1001)
    .addReg(AMDIL::Rx1003)
    .addImm(mMFI->addi128Literal(0xFFFFFFFFULL << 32,
                                 (0xFFFFFFFEULL | (0xFFFFFFFDULL << 32))));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::IEQ_v4i32), AMDIL::R1001)
    .addReg(AMDIL::R1001)
    .addImm(mMFI->addi32Literal(0));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHRVEC_v4i32), AMDIL::R1002)
    .addReg(AMDIL::Rx1002)
    .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_v4i32), AMDIL::R1002)
    .addReg(AMDIL::R1001)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::R1002);
    if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
      // This section generates the following pseudo-IL:
      // iand r1002, r1002, 0xFF
      // ishl r1002, r1002, {0, 8, 16, 24}
      // ior r1002.xy, r1002.xy, r1002.zw
      // ior r1011.x, r1002.x, r1002.y
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_v4i32), AMDIL::R1002)
      .addReg(AMDIL::R1002)
      .addImm(mMFI->addi32Literal(0xFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_v4i32), AMDIL::R1002)
      .addReg(AMDIL::R1002)
      .addImm(mMFI->addi128Literal(8ULL << 32, 16ULL | (24ULL << 32)));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i64), AMDIL::Rxy1002)
      .addReg(AMDIL::Rxy1002).addReg(AMDIL::Rzw1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i32), AMDIL::Rx1011)
      .addReg(AMDIL::Ry1002).addReg(AMDIL::Rx1002);
    } else {
      // This section generates the following pseudo-IL:
      // mov r1001.xy, r1002.yw
      // mov r1002.xy, r1002.xz
      // ubit_insert r1002.xy, 8, 8, r1001.xy, r1002.xy
      // ubit_insert r1011.x, 16, 16, r1002.y, r1002.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LHI_v2i64), AMDIL::Rxy1001)
      .addReg(AMDIL::R1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LLO_v2i64), AMDIL::Rxy1002)
      .addReg(AMDIL::R1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERT_v2i32), AMDIL::Rxy1002)
      .addImm(mMFI->addi32Literal(8))
      .addImm(mMFI->addi32Literal(8))
      .addReg(AMDIL::Rxy1001)
      .addReg(AMDIL::Rxy1002);
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERT_i32), AMDIL::Rx1011)
      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Ry1002)
      .addReg(AMDIL::Rx1002);
    }
    emitVectorAddressCalc(MI, true, false);
    emitVectorSwitchWrite(MI, true);
    break;
  case 2:
    emitVectorAddressCalc(MI, true, true);
    // This section generates the following pseudo-IL:
    // mov r1002, x1[r1007.x]
    BuildMI(*mBB, MI, DL,
            mTII->get(AMDIL::SCRATCHLOAD), AMDIL::R1002)
    .addReg(AMDIL::Rx1007)
    .addImm(xID);
    emitComponentExtract(MI, AMDIL::R1002, AMDIL::Rx1002, true);
    // This section generates the following pseudo-IL:
    // ishr r1003.x, r1010.x, 1
    // iand r1003.x, r1003.x, 1
    // ishr r1001.x, r1002.x, 16
    // cmov_logical r1002.x, r1003.x, r1002.x, r1011.x
    // cmov_logical r1001.x, r1003.x, r1011.x, r1001.x
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1003)
    .addReg(AMDIL::Rx1010)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1003)
    .addReg(AMDIL::Rx1003)
    .addImm(mMFI->addi32Literal(1));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHR_i32), AMDIL::Rx1001)
    .addReg(AMDIL::Rx1002)
    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32), AMDIL::Rx1002)
    .addReg(AMDIL::Rx1003)
    .addReg(AMDIL::Rx1002)
    .addReg(AMDIL::Rx1011);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::CMOVLOG_i32), AMDIL::Rx1001)
    .addReg(AMDIL::Rx1003)
    .addReg(AMDIL::Rx1011)
    .addReg(AMDIL::Rx1001);
    if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
      // This section generates the following pseudo-IL:
      // iand r1002.x, r1002.x, 0xFFFF
      // iand r1001.x, r1001.x, 0xFFFF
      // ishl r1001.x, r1002.x, 16
      // ior r1011.x, r1002.x, r1001.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1002)
      .addReg(AMDIL::Rx1002)
      .addImm(mMFI->addi32Literal(0xFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_i32), AMDIL::Rx1001)
      .addReg(AMDIL::Rx1001)
      .addImm(mMFI->addi32Literal(0xFFFF));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_i32), AMDIL::Rx1001)
      .addReg(AMDIL::Rx1001)
      .addImm(mMFI->addi32Literal(16));
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_OR_i32), AMDIL::Rx1011)
      .addReg(AMDIL::Rx1002).addReg(AMDIL::Rx1001);

    } else {
      // This section generates the following pseudo-IL:
      // ubit_insert r1011.x, 16, 16, r1001.x, r1002.x
      BuildMI(*mBB, MI, DL, mTII->get(AMDIL::UBIT_INSERT_i32), AMDIL::Rx1011)

      .addImm(mMFI->addi32Literal(16))
      .addImm(mMFI->addi32Literal(16))
      .addReg(AMDIL::Rx1001)
      .addReg(AMDIL::Rx1002);
    }
    emitVectorAddressCalc(MI, true, false);
    emitVectorSwitchWrite(MI, true);
    break;
  case 4:
    emitVectorAddressCalc(MI, true, false);
    emitVectorSwitchWrite(MI, true);
    break;
  case 8:
    emitVectorAddressCalc(MI, false, false);
    emitVectorSwitchWrite(MI, false);
    break;
  };
}
void
AMDIL789IOExpansion::expandStoreSetupCode(MachineInstr *MI)
{
  DebugLoc DL;
  bool is64bit = is64bitLSOp(TM, MI);
  uint32_t addyReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  uint32_t addInst = (is64bit) ? AMDIL::LADD_i64 : AMDIL::ADD_i32;
  uint32_t moveInst = (is64bit) ? AMDIL::MOVE_i64 : AMDIL::MOVE_i32;
  if (MI->getOperand(0).isUndef()) {
    BuildMI(*mBB, MI, DL, mTII->get(getMoveInstFromID(
                                      MI->getDesc().OpInfo[0].RegClass)), AMDIL::R1011)
    .addImm(mMFI->addi32Literal(0));
  } else {
    BuildMI(*mBB, MI, DL, mTII->get(getMoveInstFromID(
                                      MI->getDesc().OpInfo[0].RegClass)), AMDIL::R1011)
    .addReg(MI->getOperand(0).getReg());
  }
  expandTruncData(MI);
  if (MI->getOperand(2).isReg()) {
    BuildMI(*mBB, MI, DL, mTII->get(addInst), addyReg)
    .addReg(MI->getOperand(1).getReg())
    .addReg(MI->getOperand(2).getReg());
  } else {
    BuildMI(*mBB, MI, DL, mTII->get(moveInst), addyReg)
    .addReg(MI->getOperand(1).getReg());
  }
  expandAddressCalc(MI);
  expandPackedData(MI);
}


void
AMDIL789IOExpansion::expandPackedData(MachineInstr *MI)
{
  if (!isPackedData(MI)) {
    return;
  }
  DebugLoc DL = MI->getDebugLoc();
  // If we have packed data, then the shift size is no longer
  // the same as the load size and we need to adjust accordingly
  switch(getPackedID(MI)) {
  default:
    break;
  case PACK_V2I8: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_v2i32), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011)

    .addImm(mMFI->addi64Literal(0xFFULL | (0xFFULL << 32)));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_v2i32), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011).addImm(mMFI->addi64Literal(8ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);

  }
  break;
  case PACK_V4I8: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi32Literal(0xFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi128Literal(8ULL << 32, (16ULL | (24ULL << 32))));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i64), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011).addReg(AMDIL::Rzw1011);
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);

  }
  break;
  case PACK_V2I16: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_v2i32), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011)

    .addImm(mMFI->addi32Literal(0xFFFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_v2i32), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011)

    .addImm(mMFI->addi64Literal(16ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v2i32), AMDIL::Rx1011)
    .addReg(AMDIL::Rx1011).addReg(AMDIL::Ry1011);

  }
  break;
  case PACK_V4I16: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::BINARY_AND_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi32Literal(0xFFFF));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::SHL_v4i32), AMDIL::R1011)
    .addReg(AMDIL::R1011)
    .addImm(mMFI->addi64Literal(16ULL << 32));
    // TODO: HILO_BITOR can be removed and replaced with OR.
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::HILO_BITOR_v4i16), AMDIL::Rxy1011)
    .addReg(AMDIL::Rxy1011).addReg(AMDIL::Rzw1011);

  }
  break;
  case UNPACK_V2I8:
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRVEC_i32), AMDIL::Ry1011)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(8));
    break;
  case UNPACK_V4I8: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRVEC_v4i8), AMDIL::R1011)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi128Literal(8ULL << 32, (16ULL | (24ULL << 32))));
  }
  break;
  case UNPACK_V2I16: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRVEC_i32), AMDIL::Ry1011)
    .addReg(AMDIL::Rx1011)
    .addImm(mMFI->addi32Literal(16));
  }
  break;
  case UNPACK_V4I16: {
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::USHRVEC_v2i32), AMDIL::Rxy1012)
    .addReg(AMDIL::Rxy1011)

    .addImm(mMFI->addi32Literal(16));
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::LCREATE_v2i64), AMDIL::R1011)
    .addReg(AMDIL::Rxy1011).addReg(AMDIL::Rxy1012);
  }
  break;
  };
}
