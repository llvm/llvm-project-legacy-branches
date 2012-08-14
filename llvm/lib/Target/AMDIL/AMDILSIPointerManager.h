//===-- AMDILSIPointerManager.h -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_SI_POINTER_MANAGER_H_
#define _AMDIL_SI_POINTER_MANAGER_H_
#include "AMDILPointerManager.h"
namespace llvm
{
class MachineFunction;

// The pointer manager for Southern Island
// devices. This pointer manager allocates and trackes
// cached memory, raw resources and
// whether multi-uav is utilized or not.
class AMDILSIPointerManager : public AMDILPointerManager
{
public:
  AMDILSIPointerManager(
    TargetMachine &tm,
    CodeGenOpt::Level OL);
  virtual ~AMDILSIPointerManager();
  virtual const char*
  getPassName() const;
  virtual bool
  runOnMachineFunction(MachineFunction &F);
private:
}; // class AMDILEGPointerManager
} // end llvm namespace
#endif // _AMDIL_SI_POINTER_MANAGER_H_
