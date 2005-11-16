//===-- CTargetMachine.h - TargetMachine for the C backend ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the TargetMachine that is used by the AltiVec C backend.
//
//===----------------------------------------------------------------------===//

#ifndef ALTIVECCTARGETMACHINE_H
#define ALTIVECCTARGETMACHINE_H

#include "CTargetMachine.h"

namespace llvm {

class IntrinsicLowering;

struct AltiVecCTargetMachine : public CTargetMachine {
  AltiVecCTargetMachine(const Module &M, IntrinsicLowering *IL, const std::string& name) :
    CTargetMachine(M, IL, "AltiVecCBackend") {}

  // This is the only thing that actually does anything here.
  bool addPassesToEmitFile(PassManager &PM, std::ostream &Out,
			   CodeGenFileType FileType, bool Fast);

};

} // End llvm namespace


#endif
