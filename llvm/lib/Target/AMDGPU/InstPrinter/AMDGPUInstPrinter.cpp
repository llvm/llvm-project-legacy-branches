//===-- AMDGPUInstPrinter.cpp - AMDGPU MC Inst -> ASM ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "AMDGPUInstPrinter.h"
#include "MCTargetDesc/AMDGPUMCTargetDesc.h"
#include "llvm/MC/MCInst.h"

using namespace llvm;

void AMDGPUInstPrinter::printInst(const MCInst *MI, raw_ostream &OS,
                             StringRef Annot) {
  printInstruction(MI, OS);

  printAnnotation(OS, Annot);
}

void AMDGPUInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &O) {

  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    switch (Op.getReg()) {
    // This is the default predicate state, so we don't need to print it.
    case AMDGPU::PRED_SEL_OFF: break;
    default: O << getRegisterName(Op.getReg()); break;
    }
  } else if (Op.isImm()) {
    O << Op.getImm();
  } else if (Op.isFPImm()) {
    O << Op.getFPImm();
  } else {
    assert(!"unknown operand type in printOperand");
  }
}

void AMDGPUInstPrinter::printMemOperand(const MCInst *MI, unsigned OpNo,
                                        raw_ostream &O) {
  printOperand(MI, OpNo, O);
}

#include "AMDGPUGenAsmWriter.inc"
