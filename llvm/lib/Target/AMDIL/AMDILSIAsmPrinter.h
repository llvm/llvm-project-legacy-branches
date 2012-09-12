//===-- AMDILSIAsmPrinter.h -----------------------------------------------===//
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

#ifndef _AMDIL_SI_ASM_PRINTER_H_
#define _AMDIL_SI_ASM_PRINTER_H_
#include "AMDILAsmPrinter.h"

namespace llvm
{
class LLVM_LIBRARY_VISIBILITY AMDILSIAsmPrinter : public AMDILAsmPrinter
{
public:
  //
  // Constructor for the AMDIL SI specific AsmPrinter class.
  // Interface is defined by LLVM proper and should reference
  // there for more information.
  //
  AMDILSIAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS);

  //
  // Destructor for the EG Asm Printer class that deletes
  // all of the allocated memory
  //
  virtual ~AMDILSIAsmPrinter();

  void
  EmitInstruction(const MachineInstr *MI);

  //
  // @param F MachineFunction to print the assembly for
  // @brief parse the specified machine function and print
  // out the assembly for all the instructions in the function
  //
  bool
  runOnMachineFunction(MachineFunction &F);

protected:
  //
  // @param MI Machine instruction to emit the macro code for
  //
  // Emits a fully functional macro function that uses the argument
  // registers as the macro arguments.
  //
  virtual void
  emitMacroFunc(const MachineInstr *MI, OSTREAM_TYPE &O);
};   // AMDILSIAsmPrinter
} // end of llvm namespace
#endif // _AMDIL_SI_ASM_PRINTER_H_
