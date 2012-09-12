//===-- AMDIL7XXAsmPrinter.h ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Asm Printer class for 7XX generation of cards. This class handles all of
// the items that are unique to these devices that must be handles by the
// AsmPrinter.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_7XX_ASM_PRINTER_H_
#define _AMDIL_7XX_ASM_PRINTER_H_
#include "AMDILAsmPrinter.h"

namespace llvm
{
class LLVM_LIBRARY_VISIBILITY AMDIL7XXAsmPrinter : public AMDILAsmPrinter
{
public:
  //
  // Constructor for the AMDIL 7XX specific AsmPrinter class.
  // Interface is defined by LLVM proper and should reference
  // there for more information.
  //
  AMDIL7XXAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS);

  //
  // Destructor for the 7XX Asm Printer class that deletes
  // all of the allocated memory
  //
  virtual ~AMDIL7XXAsmPrinter();

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
};   // AMDIL7XXAsmPrinter
} // end of llvm namespace
#endif // AMDIL_7XX_ASM_PRINTER_H_
