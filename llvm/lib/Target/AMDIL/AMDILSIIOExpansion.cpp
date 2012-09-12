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

namespace llvm
{
extern void initializeAMDILSIIOExpansionPass(llvm::PassRegistry&);
}

char AMDILSIIOExpansion::ID = 0;
INITIALIZE_PASS(AMDILSIIOExpansion, "si-io-expansion",
                "AMDIL SI IO Expansion", false, false);

AMDILSIIOExpansion::AMDILSIIOExpansion()
  : MachineFunctionPass(ID)
{
  initializeAMDILSIIOExpansionPass(*PassRegistry::getPassRegistry());
}
const char *AMDILSIIOExpansion::getPassName() const
{
  return "AMDIL SI IO Expansion Pass";
}
bool AMDILSIIOExpansion::runOnMachineFunction(MachineFunction& MF)
{
  AMDILSIIOExpansionImpl impl(MF);
  return impl.run();
}
bool
AMDILSIIOExpansionImpl::isCacheableOp(MachineInstr *MI)
{
  AMDILAS::InstrResEnc curRes;
  getAsmPrinterFlags(MI, curRes);
  return curRes.bits.CacheableRead;
}
bool
AMDILSIIOExpansionImpl::isIOInstruction(MachineInstr *MI)
{
  if (!MI) {
    return false;
  }
  if (is64BitImageInst(MI)) {
    return true;
  }
  switch (MI->getOpcode()) {
  default:
    return AMDILEGIOExpansionImpl::isIOInstruction(MI);
  case AMDIL::ATOM_G_LOADi8:
  case AMDIL::ATOM_G_STOREi8:
  case AMDIL::ATOM64_G_LOADi8:
  case AMDIL::ATOM64_G_STOREi8:
  case AMDIL::ATOM_G_LOADi16:
  case AMDIL::ATOM_G_STOREi16:
  case AMDIL::ATOM64_G_LOADi16:
  case AMDIL::ATOM64_G_STOREi16:
  case AMDIL::ATOM_G_LOADi32:
  case AMDIL::ATOM_G_STOREi32:
  case AMDIL::ATOM64_G_LOADi32:
  case AMDIL::ATOM64_G_STOREi32:
  case AMDIL::ATOM_G_LOADv2i32:
  case AMDIL::ATOM_G_STOREv2i32:
  case AMDIL::ATOM64_G_LOADv2i32:
  case AMDIL::ATOM64_G_STOREv2i32:
  case AMDIL::ATOM_G_LOADv4i32:
  case AMDIL::ATOM_G_STOREv4i32:
  case AMDIL::ATOM64_G_LOADv4i32:
  case AMDIL::ATOM64_G_STOREv4i32:
    return false;
  }
  return AMDILEGIOExpansionImpl::isIOInstruction(MI);
}
void
AMDILSIIOExpansionImpl::expandIOInstruction(MachineInstr *MI)
{
  assert(isIOInstruction(MI) && "Must be an IO instruction to "
         "be passed to this function!");
  if (is64BitImageInst(MI)) {
    if (isReadImageInst(MI) || isImageTXLDInst(MI)) {
      expandImageLoad(mBB, MI);
      return;
    }
    if (isWriteImageInst(MI)) {
      expandImageStore(mBB, MI);
      return;
    }
    if (isImageInfoInst(MI)) {
      expandImageParam(mBB, MI);
      return;
    }
  }
  AMDILEGIOExpansionImpl::expandIOInstruction(MI);
}
void
AMDILSIIOExpansionImpl::expandGlobalLoad(MachineInstr *MI)
{
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  expandLoadStartCode(MI, addyReg);
  DebugLoc DL = MI->getDebugLoc();
  uint32_t ID = getPointerID(MI);
  bool cacheable = isCacheableOp(MI);
  bool is64bit = is64bitLSOp(MI);
  mKM->setOutputInst();
  switch (getMemorySize(MI)) {
  default:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get((is64bit) ?
                                      AMDIL::UAVRAW64LOADCACHEDALIGNEDv4i32
                                      : AMDIL::UAVRAW32LOADCACHEDALIGNEDv4i32),
              dataReg).addReg(addyReg).addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAW64LOADv4i32 :
                AMDIL::UAVRAW32LOADv4i32),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 1:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::
                                          UAVRAW64LOADCACHEDi8
                                          : AMDIL::UAVRAW32LOADCACHEDi8) :
                                       ((is64bit) ? AMDIL::UAVRAW64LOADCACHEDu8
                                        : AMDIL::UAVRAW32LOADCACHEDu8))),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::UAVRAW64LOADi8 :
                                          AMDIL::UAVRAW32LOADi8) :
                                       ((is64bit) ? AMDIL::UAVRAW64LOADu8 :
                                        AMDIL::UAVRAW32LOADu8))),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 2:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ?  ((is64bit) ? AMDIL::
                                           UAVRAW64LOADCACHEDi16
                                           : AMDIL::UAVRAW32LOADCACHEDi16) :
                                       ((is64bit) ? AMDIL::
                                        UAVRAW64LOADCACHEDu16
                                        : AMDIL::UAVRAW32LOADCACHEDu16))),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get((isSWSExtLoadInst(MI)
                                       ? ((is64bit) ? AMDIL::UAVRAW64LOADi16 :
                                          AMDIL::UAVRAW32LOADi16) :
                                       ((is64bit) ? AMDIL::UAVRAW64LOADu16 :
                                        AMDIL::UAVRAW32LOADu16))),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 4:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAW64LOADCACHEDi32 : AMDIL::
                UAVRAW32LOADCACHEDi32),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAW64LOADi32 : AMDIL::UAVRAW32LOADi32),
              dataReg)
      .addReg(addyReg)
      .addImm(ID);
    }
    break;
  case 8:
    if (cacheable) {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAW64LOADCACHEDALIGNEDv2i32
                : AMDIL::UAVRAW32LOADCACHEDALIGNEDv2i32),
              AMDIL::Rxy1011).addReg(addyReg).addImm(ID);
    } else {
      BuildMI(*mBB, MI, DL, mTII->get(
                (is64bit) ? AMDIL::UAVRAW64LOADv2i32 : AMDIL::UAVRAW32LOADv2i32),
              AMDIL::Rxy1011).addReg(addyReg).addImm(ID);
    }
    BuildMI(*mBB, MI, DL, mTII->get(AMDIL::COPY), dataReg)
    .addReg(AMDIL::Rxy1011);
    break;
  };
  expandPackedData(MI, dataReg);
  expandExtendLoad(MI, dataReg);
  MI->getOperand(0).setReg(dataReg);
}
void
AMDILSIIOExpansionImpl::expandGlobalStore(MachineInstr *MI)
{
  uint32_t addyReg = MI->getOperand(1).getReg();
  uint32_t dataReg = MI->getOperand(0).getReg();
  // These instructions are expandted before the current MI.
  AMDIL789IOExpansionImpl::expandStoreSetupCode(MI, addyReg, dataReg);
  uint32_t ID = getPointerID(MI);
  mKM->setOutputInst();
  bool is64bit = is64bitLSOp(MI);
  DebugLoc DL = MI->getDebugLoc();
  switch (getMemorySize(MI)) {
  default:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAW64STOREv4i32 :
              AMDIL::UAVRAW32STOREv4i32), AMDIL::MEM)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 1:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAW64STOREi8 :
              AMDIL::UAVRAW32STOREi8), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 2:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAW64STOREi16 :
              AMDIL::UAVRAW32STOREi16), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 4:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAW64STOREi32 :
              AMDIL::UAVRAW32STOREi32), AMDIL::MEMx)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  case 8:
    BuildMI(*mBB, MI, DL, mTII->get(
              (is64bit) ? AMDIL::UAVRAW64STOREv2i32 :
              AMDIL::UAVRAW32STOREv2i32), AMDIL::MEMxy)
    .addReg(addyReg)
    .addReg(dataReg)
    .addImm(ID);
    break;
  };
}
