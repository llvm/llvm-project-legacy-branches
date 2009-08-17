//===- lib/MC/MCValue.cpp - MCValue implementation ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCValue.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

void MCValue::print(raw_ostream &OS) const {
  if (isAbsolute()) {
    OS << getConstant();
    return;
  }

  getSymA()->print(OS);

  if (getSymB()) {
    OS << " - "; 
    getSymB()->print(OS);
  }

  if (getConstant())
    OS << " + " << getConstant();
}

void MCValue::dump() const {
  print(errs());
}
