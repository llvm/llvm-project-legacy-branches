//===-- AMDILLLVMVersion.h ------------------------------------------------===//
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

#ifndef _AMDIL_LLVM_VERSION_H_
#define _AMDIL_LLVM_VERSION_H_

// Macro that expands into the correct type for output streams
#define OSTREAM_TYPE llvm::raw_ostream

// AMDILAsmPrinter.cpp macros
#define AMDIL_ASM_PRINTER_ARGUMENTS TargetMachine& TM, MCStreamer &Streamer
#define ASM_PRINTER_ARGUMENTS TM, Streamer

#endif // _AMDIL_LLVM_VERSION_H_
