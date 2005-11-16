//===-- CTargetMachine.h - TargetMachine for the C backend ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the TargetMachine that is used by the SSE C backend.
//
//===----------------------------------------------------------------------===//

#ifndef SSECTARGETMACHINE_H
#define SSECTARGETMACHINE_H

#include "CTargetMachine.h"

namespace llvm {

class IntrinsicLowering;

struct SSECTargetMachine : public CTargetMachine {
  SSECTargetMachine(const Module &M, IntrinsicLowering *IL, const std::string& name) :
    CTargetMachine(M, IL, "SSECBackend") {}

  // This is the only thing that actually does anything here.
  bool addPassesToEmitFile(PassManager &PM, std::ostream &Out,
			   CodeGenFileType FileType, bool Fast);

};

} // End llvm namespace


#endif
