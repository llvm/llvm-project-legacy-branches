//===-- AMDILRegisterInfo.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The LLVM Compiler Infrastructure This file is distributed under the
// University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "AMDILRegisterInfo.h"
#include "AMDIL.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

AMDILRegisterInfo::AMDILRegisterInfo(AMDILTargetMachine &tm,
                                     const TargetInstrInfo &tii)
  : AMDILGenRegisterInfo(0), // RA???
    TM(tm), TII(tii)
{
  baseOffset = 0;
  nextFuncOffset = 0;
}
const uint16_t*
AMDILRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const
{
  static const uint16_t CalleeSavedRegs[] = { 0 };
  // TODO: Does IL need to actually have any callee saved regs?
  // I don't think we do since we can just use sequential registers
  // Maybe this would be easier if every function call was inlined first
  // and then there would be no callee issues to deal with
  //TODO(getCalleeSavedRegs);
  return CalleeSavedRegs;
}
BitVector
AMDILRegisterInfo::getReservedRegs(const MachineFunction &MF) const
{
  BitVector Reserved(getNumRegs());
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();
  // Set the frame pointer, stack pointer, RA, Stack Data Pointer as reserved.
  if (TFI->hasFP(MF)) {
    Reserved.set(AMDIL::FP);
  }
  Reserved.set(AMDIL::SP);
  Reserved.set(AMDIL::SDP);
  Reserved.set(AMDIL::RA);

  // Set temps T1-T5 as reserved.
  Reserved.set(AMDIL::T1);
  Reserved.set(AMDIL::T2);
  Reserved.set(AMDIL::T3);
  Reserved.set(AMDIL::T4);
  Reserved.set(AMDIL::T5);

  // Set the mem register as reserved.
  Reserved.set(AMDIL::MEMx);
  Reserved.set(AMDIL::MEMxy);
  Reserved.set(AMDIL::MEM);

  // Set CFG1-CFG10 as reserved.
  Reserved.set(AMDIL::CFG1);
  Reserved.set(AMDIL::CFG2);
  Reserved.set(AMDIL::CFG3);
  Reserved.set(AMDIL::CFG4);
  Reserved.set(AMDIL::CFG5);
  Reserved.set(AMDIL::CFG6);
  Reserved.set(AMDIL::CFG7);
  Reserved.set(AMDIL::CFG8);
  Reserved.set(AMDIL::CFG9);
  Reserved.set(AMDIL::CFG10);

  // Set PRINTF register as reserved.
  Reserved.set(AMDIL::PRINTF);

  // Reserve the live-ins for the function.
  MachineBasicBlock::livein_iterator LII = MF.begin()->livein_begin();
  MachineBasicBlock::livein_iterator LIE = MF.begin()->livein_end();
  while (LII != LIE) {
    Reserved.set(*LII);
    ++LII;
  }
  return Reserved;
}
const TargetRegisterClass* const*
AMDILRegisterInfo::getCalleeSavedRegClasses(const MachineFunction *MF) const
{
  static const TargetRegisterClass * const CalleeSavedRegClasses[] = { 0 };
  // TODO: Keep in sync with getCalleeSavedRegs
  //TODO(getCalleeSavedRegClasses);
  return CalleeSavedRegClasses;
}
void
AMDILRegisterInfo::eliminateCallFramePseudoInstr(
  MachineFunction &MF,
  MachineBasicBlock &MBB,
  MachineBasicBlock::iterator I) const
{
  MBB.erase(I);
}
// For each frame index we find, we store the offset in the stack which is
// being pushed back into the global buffer. The offset into the stack where
// the value is stored is copied into a new register and the frame index is
// then replaced with that register.
void
AMDILRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj,
                                       RegScavenger *RS) const
{
  assert(SPAdj == 0 && "Unexpected");
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  unsigned int y = MI.getNumOperands();
  for (unsigned int x = 0; x < y; ++x) {
    if (!MI.getOperand(x).isFI()) {
      continue;
    }
    bool def = isPtrStoreInst(&MI);
    int FrameIndex = MI.getOperand(x).getIndex();
    int64_t Offset = MFI->getObjectOffset(FrameIndex);
    //int64_t Size = MF.getFrameInfo()->getObjectSize(FrameIndex);
    // An optimization is to only use the offsets if the size
    // is larger than 4, which means we are storing an array
    // instead of just a pointer. If we are size 4 then we can
    // just do register copies since we don't need to worry about
    // indexing dynamically
    // FIXME: This needs to embed the literals directly instead of
    // using DFP.
    unsigned reg = (def && !x) ? AMDIL::T5 : AMDIL::DFP;
    if (MI.getOpcode() != AMDIL::LOADFIi32) {
      MachineInstr *nMI = MF.CreateMachineInstr(
        TII.get(AMDIL::LOADFIi32), MI.getDebugLoc());
      nMI->addOperand(MachineOperand::CreateReg(reg, true));
      nMI->addOperand(
        MachineOperand::CreateImm(Offset));
      MI.getParent()->insert(II, nMI);
      if (MI.getOperand(x).isReg() == false)  {
        MI.getOperand(x).ChangeToRegister(
          nMI->getOperand(0).getReg(), false);
      } else {
        MI.getOperand(x).setReg(
          nMI->getOperand(0).getReg());
      }
    } else {
      MI.getOperand(1).ChangeToImmediate(Offset);
    }
  }
}
const TargetRegisterClass *
AMDILRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const
{
  assert(!Kind && "Unknown register class pointer specified!");
  return TM.getSubtargetImpl()->is64bit()
         ? &AMDIL::GPRI64RegClass
         : &AMDIL::GPRI32RegClass;
}
void
AMDILRegisterInfo::processFunctionBeforeFrameFinalized(
  MachineFunction &MF) const
{
  //TODO(processFunctionBeforeFrameFinalized);
  // Here we keep track of the amount of stack that the current function
  // uses so
  // that we can set the offset to the end of the stack and any other
  // function call
  // will not overwrite any stack variables.
  // baseOffset = nextFuncOffset;
  MachineFrameInfo *MFI = MF.getFrameInfo();

  for (uint32_t x = 0, y = MFI->getNumObjects(); x < y; ++x) {
    int64_t size = MFI->getObjectSize(x);
    if (!(size % 4) && size > 1) {
      nextFuncOffset += size;
    } else {
      nextFuncOffset += 16;
    }
  }
}
unsigned int
AMDILRegisterInfo::getRARegister() const
{
  return AMDIL::RA;
}
unsigned int
AMDILRegisterInfo::getFrameRegister(const MachineFunction &MF) const
{
  return AMDIL::FP;
}
unsigned int
AMDILRegisterInfo::getEHExceptionRegister() const
{
  assert(0 && "What is the exception register");
  return 0;
}
unsigned int
AMDILRegisterInfo::getEHHandlerRegister() const
{
  assert(0 && "What is the exception handler register");
  return 0;
}
int64_t
AMDILRegisterInfo::getStackSize() const
{
  return nextFuncOffset - baseOffset;
}
#define GET_REGINFO_MC_DESC
#define GET_REGINFO_TARGET_DESC
#include "AMDILGenRegisterInfo.inc"

