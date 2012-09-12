//===-- AMDILPointerManager.h ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The AMDIL Pointer Manager is a class that does all the checking for
// different pointer characteristics. Pointers have attributes that need to be attached
// to them in order to correctly codegen them efficiently. This class will
// analyze the pointers of a function and then traverse the uses of the pointers and
// determine if a pointer can be cached, should belong in the arena, and what UAV it
// should belong to. There are seperate classes for each unique generation of
// devices. This pass only works in SSA form.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_POINTER_MANAGER_H_
#define _AMDIL_POINTER_MANAGER_H_

#undef DEBUG_TYPE
#undef DEBUGME
#define DEBUG_TYPE "PointerManager"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME (false)
#endif

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class MachineFunction;
class AnalysisUsage;
// The default pointer manager. This handles pointer
// resource allocation for default ID's only.
// There is no special processing.
class AMDILPointerManager : public MachineFunctionPass
{
public:
  AMDILPointerManager();
  virtual ~AMDILPointerManager() {
  };
  virtual const char*
  getPassName() const;
  virtual bool
  runOnMachineFunction(MachineFunction &F);
  virtual void
  getAnalysisUsage(AnalysisUsage &AU) const;
  static char ID;
private:
};   // class AMDILPointerManager

// The pointer manager for Evergreen and Northern Island
// devices. This pointer manager allocates and trackes
// cached memory, arena resources, raw resources and
// whether multi-uav is utilized or not.
class AMDILEGPointerManager : public MachineFunctionPass
{
public:
  AMDILEGPointerManager();
  virtual ~AMDILEGPointerManager() {
  };
  virtual const char*
  getPassName() const;
  virtual bool
  runOnMachineFunction(MachineFunction &F);
  virtual void
  getAnalysisUsage(AnalysisUsage &AU) const;
  static char ID;
};   // class AMDILEGPointerManager
} // end llvm namespace
#endif // _AMDIL_POINTER_MANAGER_H_
