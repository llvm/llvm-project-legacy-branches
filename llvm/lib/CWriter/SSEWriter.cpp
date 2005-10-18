//===-- Writer.cpp - Library for converting LLVM code to C ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This library converts LLVM code to C code with SSE extensions.
//
//===----------------------------------------------------------------------===//

#include "SSECTargetMachine.h"
#include "SSEWriter.h"

using namespace llvm;

namespace {
  // Register the target.
  RegisterTarget<SSECTargetMachine> X("sse-c", "  SSE C backend");
}

static void badValue(Value *V) {
  std::cerr << "SSE C Backend can't handle this value!\n";
  std::cerr << *V << "\n";
  abort();
}

void SSECWriter::printConstant(Constant *CPV) {
  if (!isa<FixedVectorType>(CPV->getType())) {
    CWriter::printConstant(CPV);
    return;
  }
  if (const ConstantExpr *CE = dyn_cast<ConstantExpr>(CPV)) {
    switch (CE->getOpcode()) {
    case Instruction::Cast:
      Out << "((";
      printType(Out, CPV->getType());
      Out << ") (";
      printConstant(CE->getOperand(0));
      Out << "))";
      return;
    default:
      badValue(CPV);
    }
  }
  // FIXME:  Add support for literal vector constants here
  //
  Out << "((";
  printType(Out, CPV->getType());
  Out << ") (";
  for (unsigned i = 0; i < CPV->getNumOperands()-1; ++i) {
    printConstant(CPV->getOperand(i));
    Out << ", ";
  }
  printConstant(CPV->getOperand(CPV->getNumOperands()-1));
  Out << "))";
  return;

}

void SSECWriter::visitCastInst (CastInst &I) {
  if (!isa<FixedVectorType>(I.getType()) ||
      isa<FixedVectorType>(I.getOperand(0)->getType()))
    return CWriter::visitCastInst(I);
  std::cerr << I;
  std::cerr << "SSE C Backend cannot handle this cast!\n";
}

void SSECWriter::visitBinaryOperator(Instruction &I) {
  if (!isa<FixedVectorType>(I.getType()))
    return CWriter::visitBinaryOperator(I);
  switch(I.getOpcode()) {
  case Instruction::Add:
    Out << "_mm_add_epi16(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ")";
    break;
  case Instruction::Mul:
    Out << "_mm_mullo_epi16(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ")";
    break;
  case Instruction::And:
    Out << "_mm_and_si128(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ")";
    break;
  default:
    std::cerr << "SSE C Backend can't handle this instruction:\n" << I;
    exit(1);
  }
}

std::ostream &SSECWriter::printFixedVectorType(std::ostream &Out,
					       const FixedVectorType *Ty,
					       const std::string &VariableName) {
  if (Ty->isIntegralVector()) {
    Out << "__m128i " << VariableName;
  } else {
    std::cerr << "SSE C Backend cannot handle type!\n" << Ty->getDescription() << "\n";
    exit(1);
  }
  return Out;
}


//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool SSECTargetMachine::addPassesToEmitFile(PassManager &PM, std::ostream &o,
					    CodeGenFileType FileType) {
  if (FileType != TargetMachine::AssemblyFile) return true;
  // Add lowervectors pass here to lower variable-length vectors, which we can't handle
  PM.add(createLowerGCPass());
  PM.add(createLowerAllocationsPass(true));
  PM.add(createLowerInvokePass());
  PM.add(new CBackendNameAllUsedStructs());
  PM.add(new SSECWriter(o, getIntrinsicLowering()));
  return false;
}
