//===-- AMDILInstPrinter.h ------------------------------------------------===//
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

#ifndef AMDILMINSTPRINTER_H_
#define AMDILMINSTPRINTER_H_
#include "AMDILLLVMVersion.h"

#include "AMDILLLVMPC.h"

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {
class MCAsmInfo;
class MCInst;
class raw_ostream;
// FIXME: We will need to implement this class when we transition to use
//        MCStreamer.
class AMDILInstPrinter : public MCInstPrinter {
public:
  virtual ~AMDILInstPrinter();
  AMDILInstPrinter(const MCAsmInfo &MAI,
                   const MCInstrInfo &MII,
                   const MCRegisterInfo &MRI);
  virtual void printInst(const MCInst *MI, raw_ostream &OS, StringRef annot);
};
} // namespace llvm

#endif // AMDILMINSTPRINTER_H_
