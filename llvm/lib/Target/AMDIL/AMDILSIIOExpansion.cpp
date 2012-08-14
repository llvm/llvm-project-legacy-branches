//===-- AMDILSIIOExpansion.cpp --------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementation of the I/O expansion class for SI devices.
//
//===----------------------------------------------------------------------===//

#include "AMDILSIIOExpansion.h"
#include "AMDILCompilerErrors.h"
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
AMDILSIIOExpansion::AMDILSIIOExpansion(TargetMachine &tm,
                                       CodeGenOpt::Level OptLevel) : AMDILEGIOExpansion(tm, OptLevel)
{
}

AMDILSIIOExpansion::~AMDILSIIOExpansion()
{
}
const char *AMDILSIIOExpansion::getPassName() const
{
  return "AMDIL SI IO Expansion Pass";
}

bool
AMDILSIIOExpansion::isCacheableOp(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curRes;
  getAsmPrinterFlags(MI, curRes);
  return curRes.bits.CacheableRead;
}

bool
AMDILSIIOExpansion::isIOInstruction(TargetMachine &TM, MachineInstr *MI)
{
  if (!MI) {
    return false;
  }
  if (is64BitImageInst(TM, MI)) {
    return true;
  }
  return AMDILEGIOExpansion::isIOInstruction(MI);
}

void
AMDILSIIOExpansion::expandIOInstruction(TargetMachine &TM, MachineInstr *MI)
{
  assert(isIOInstruction(TM, MI) && "Must be an IO instruction to "
         "be passed to this function!");
  if (is64BitImageInst(TM, MI)) {
    if (isReadImageInst(TM, MI) || isImageTXLDInst(TM, MI)) {
      expandImageLoad(mBB, MI);
      return;
    }
    if (isWriteImageInst(TM, MI)) {
      expandImageStore(mBB, MI);
      return;
    }
    if (isImageInfoInst(TM, MI)) {
      expandImageParam(mBB, MI);
      return;
    }
  }
  AMDILEGIOExpansion::expandIOInstruction(MI);
}

void
AMDILSIIOExpansion::expandGlobalLoad(MachineInstr *MI)
{
  // These instructions are generated before the current MI.
  expandLoadStartCode(MI);
  DebugLoc DL = MI->getDebugLoc();
  uint32_t ID = getPointerID(MI);
  bool cacheable = isCacheableOp(MI);
  bool is64bit = is64bitLSOp(TM, MI);
  bool aligned = mSTM->calVersion() >= CAL_CACHED_ALIGNED_UAVS;
  mKM->setOutputInst();
  uint32_t addyReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  switch (getMemorySize(MI)) {
  default:
    if (cacheable) {
      if (aligned) {
        BuildMI(*mBB, MI, DL, mTII->get((is64bit) ?
                                        AMDIL::UAVRAWLOADCACHEDALIGNED64_v4i32
                                        : AMDIL::UAVRAWLOADCACHEDALIGNED_v4i32),
                AMDIL::R1011).addReg(addyReg).addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(
                  (is64bit) ? AMDIL::UAVRAWLOADCACHED64_v4i32 :
                  AMDIL::UAVRAWLOADCACHED_v4i32),
                AMDIL::R1011).addReg(addyReg).addImm(ID);
      }
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAWLOAD_v4i32 :
                AMDIL::UAVRAWLOAD_v4i32),
              AMDIL::R1011)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 1:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::UAVRAWLOADCACHED64_i8
                                          : AMDIL::UAVRAWLOADCACHED_i8) :
                                         ((is64bit) ? AMDIL::UAVRAWLOADCACHED64_u8
                                          : AMDIL::UAVRAWLOADCACHED_u8))),
                AMDIL::Rx1011)
        .addReg(addyReg)
        .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::UAVRAWLOAD64_i8 : AMDIL::UAVRAWLOAD_i8) :
                                         ((is64bit) ? AMDIL::UAVRAWLOAD64_u8 : AMDIL::UAVRAWLOAD_u8))),
                AMDIL::Rx1011)
        .addReg(addyReg)
        .addImm(ID);
    }
    break;
  case 2:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ?  ((is64bit) ? AMDIL::UAVRAWLOADCACHED64_i16
                                           : AMDIL::UAVRAWLOADCACHED_i16) :
                                         ((is64bit) ? AMDIL::UAVRAWLOADCACHED64_u16
                                          : AMDIL::UAVRAWLOADCACHED_u16))),
                AMDIL::Rx1011)
        .addReg(addyReg)
        .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::UAVRAWLOAD64_i16 : AMDIL::UAVRAWLOAD_i16) :
                                         ((is64bit) ? AMDIL::UAVRAWLOAD64_u16 : AMDIL::UAVRAWLOAD_u16))),
                AMDIL::Rx1011)
        .addReg(addyReg)
        .addImm(ID);
    }
    break;
  case 4:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAWLOADCACHED64_i32 : AMDIL::UAVRAWLOADCACHED_i32),
              AMDIL::Rx1011)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAWLOAD64_i32 : AMDIL::UAVRAWLOAD_i32),
              AMDIL::Rx1011)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 8:
    if (cacheable) {
      if (aligned) {
        BuildMI(*mBB, MI, DL, mTII->get(
                  (is64bit) ? AMDIL::UAVRAWLOADCACHEDALIGNED64_v2i32
                  : AMDIL::UAVRAWLOADCACHEDALIGNED_v2i32),
                AMDIL::Rxy1011).addReg(addyReg).addImm(ID);
      } else {
        BuildMI(*mBB, MI, DL, mTII->get(
                  (is64bit) ? AMDIL::UAVRAWLOADCACHED64_v2i32 : AMDIL::UAVRAWLOADCACHED_v2i32),
                AMDIL::Rxy1011).addReg(addyReg).addImm(ID);
      }
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAWLOAD64_v2i32 : AMDIL::UAVRAWLOAD_v2i32),
              AMDIL::Rxy1011)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  };
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
AMDILSIIOExpansion::expandGlobalStore(MachineInstr *MI)
{
  // These instructions are expandted before the current MI.
  AMDIL789IOExpansion::expandStoreSetupCode(MI);
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  bool is64bit = is64bitLSOp(TM, MI);
  DebugLoc DL = MI->getDebugLoc();
  uint32_t addyReg = (is64bit) ? AMDIL::Rxy1010 : AMDIL::Rx1010;
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAWSTORE64_v4i32 :
              AMDIL::UAVRAWSTORE_v4i32), AMDIL::MEM)
    .addReg(addyReg)
    .addReg(AMDIL::R1011)
    .addImm(ID);
    break;
  case 1:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAWSTORE64_i8 :
              AMDIL::UAVRAWSTORE_i8), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(AMDIL::Rx1011)
    .addImm(ID);
    break;
  case 2:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAWSTORE64_i16 :
              AMDIL::UAVRAWSTORE_i16), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(AMDIL::Rx1011)
    .addImm(ID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAWSTORE64_i32 :
              AMDIL::UAVRAWSTORE_i32), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(AMDIL::Rx1011)
    .addImm(ID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAWSTORE64_v2i32 :
              AMDIL::UAVRAWSTORE_v2i32), AMDIL::MEMxy)
    .addReg(addyReg)
    .addReg(AMDIL::Rxy1011)
    .addImm(ID);
    break;
  };
}
