//===-- AMDILSwizzleEncoder.h ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The AMDIL Swizzle Encoder is a class that encodes swizzle information in
// the machine operand target specific flag. This encoding can then be used to
// optimize the swizzles of a specific instruction to better pack the registers which
// will help allocation with SC.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_SWIZZLE_ENCODER_H_
#define _AMDIL_SWIZZLE_ENCODER_H_
#include "AMDIL.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetMachine.h"
#include <set>
#include <map>
namespace llvm {
struct AMDILRegisterInfo;
class AMDILSwizzleEncoder : public MachineFunctionPass
{
public:
  AMDILSwizzleEncoder(TargetMachine &tm, CodeGenOpt::Level OptLevel);
  const char* getPassName() const;
  bool runOnMachineFunction(MachineFunction &MF);
  static char ID;
private:
  bool mDebug;     ///< Flag to specify whether to dump debug information.
  CodeGenOpt::Level opt;
};   // class AMDILSwizzleEncoder
char AMDILSwizzleEncoder::ID = 0;
} // end llvm namespace
#endif // _AMDIL_SWIZZLE_ENCODER_H_
