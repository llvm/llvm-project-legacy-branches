//===-- AMDILRegisterInfo.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the AMDIL implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#ifndef AMDILREGISTERINFO_H_
#define AMDILREGISTERINFO_H_

#include "AMDILLLVMPC.h"

#include "llvm/Target/TargetRegisterInfo.h"
#define GET_REGINFO_HEADER
#include "AMDILGenRegisterInfo.inc"

#include "llvm/Config/config.h"

// See header file for explanation

namespace llvm
{
class AMDILTargetMachine;
class TargetInstrInfo;
class Type;

/// DWARFFlavour - Flavour of dwarf regnumbers
///
namespace DWARFFlavour {
enum {
  AMDIL_Generic = 0
};
}

struct AMDILRegisterInfo : public AMDILGenRegisterInfo
{
  AMDILTargetMachine &TM;
  const TargetInstrInfo &TII;

  AMDILRegisterInfo(AMDILTargetMachine &tm, const TargetInstrInfo &tii);
  /// Code Generation virtual methods...
  const uint16_t*
  getCalleeSavedRegs(const MachineFunction *MF = 0) const;

  const TargetRegisterClass* const*
  getCalleeSavedRegClasses(
    const MachineFunction *MF = 0) const;

  virtual const TargetRegisterClass *
  getPointerRegClass(const MachineFunction &MF, unsigned Kind=0) const;

  BitVector
  getReservedRegs(const MachineFunction &MF) const;

  void
  eliminateCallFramePseudoInstr(
    MachineFunction &MF,
    MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const;
  void
  eliminateFrameIndex(MachineBasicBlock::iterator II,
                      int SPAdj,
                      RegScavenger *RS = NULL) const;

  void
  processFunctionBeforeFrameFinalized(MachineFunction &MF) const;

  // Debug information queries.
  unsigned int
  getRARegister() const;

  unsigned int
  getFrameRegister(const MachineFunction &MF) const;

  // Exception handling queries.
  unsigned int
  getEHExceptionRegister() const;
  unsigned int
  getEHHandlerRegister() const;

  int64_t
  getStackSize() const;
#if 0
  bool
  requiresRegisterScavenging(const MachineFunction&) const {
    return true;
  }
  bool
  requireFrameIndexScavenging(const MachineFunction&) const {
    return true;
  }
  bool
  requiresVirtualBaseRegisters(const MachineFunction&) const {
    return true;
  }
#endif

  private:
    mutable int64_t baseOffset;
    mutable int64_t nextFuncOffset;
};
} // end namespace llvm

#endif // AMDILREGISTERINFO_H_
