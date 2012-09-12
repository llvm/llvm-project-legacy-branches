//===-- AMDILInstPrinter.cpp ----------------------------------------------===//
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

#include "AMDILInstPrinter.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
using namespace llvm;
AMDILInstPrinter::AMDILInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                                   const MCRegisterInfo &MRI)
  : MCInstPrinter(MAI, MII, MRI)
{
}
void
AMDILInstPrinter::printInst(const MCInst *MI, raw_ostream &OS, StringRef annot)
{
  llvm_unreachable("unsupported");
}
AMDILInstPrinter::~AMDILInstPrinter()
{
}
