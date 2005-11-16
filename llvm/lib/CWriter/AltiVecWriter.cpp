//===-- Writer.cpp - Library for converting LLVM code to C ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This library converts LLVM code to C code with AltiVec extensions.
//
//===----------------------------------------------------------------------===//

#include "AltiVecCTargetMachine.h"
#include "AltiVecWriter.h"

using namespace llvm;

namespace {
  // Register the target.
  RegisterTarget<AltiVecCTargetMachine> X("altivec-c", "  AltiVec C backend");
}

static void badValue(Value *V) {
  std::cerr << "AltiVec C Backend can't handle this value!\n";
  std::cerr << *V << "\n";
  abort();
}

void AltiVecCWriter::printConstant(Constant *CPV) {
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

void AltiVecCWriter::visitCastInst (CastInst &I) {
  if (!isa<FixedVectorType>(I.getType()) ||
      isa<FixedVectorType>(I.getOperand(0)->getType()))
    return CWriter::visitCastInst(I);
  if (!isa<Constant>(I.getOperand(0))) {
    std::cerr << I;
    std::cerr << "AltiVec C Backend can handle only cast of constant to vector!\n";
    exit(1);
  }
  Out << '(';
  printType(Out, I.getType());
  Out << ") (";
  writeOperand(I.getOperand(0));
  Out << ')';
}

void AltiVecCWriter::visitVImmInst (VImmInst &VL) {
  if (!isa<Constant>(VL.getOperand(0))) {
    std::cerr << VL;
    std::cerr << "AltiVec C Backend can handle only vimm of constant!\n";
    exit(1);
  }
  Out << '(';
  printType(Out, VL.getType());
  Out << ") (";
  writeOperand(VL.getOperand(0));
  Out << ')';
}

void AltiVecCWriter::visitBinaryOperator(Instruction &I) {
  if (!isa<FixedVectorType>(I.getType()))
    return CWriter::visitBinaryOperator(I);
  switch(I.getOpcode()) {
  case Instruction::Add:
    // Check for mladd pattern
    //
    if (BinaryOperator *BO = dyn_cast<BinaryOperator>(I.getOperand(0))) {
      if (BO->getOpcode() == Instruction::Mul) {
	Out << "vec_mladd(";
	writeOperand(BO->getOperand(0));
	Out << ",";
	writeOperand(BO->getOperand(1));
	Out << ",";
	writeOperand(I.getOperand(1));
	Out << ")";
	break;
      }
    }
    Out << "vec_add(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ")";
    break;
  case Instruction::Mul:
    Out << "vec_mladd(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ",(vector short)(0))";
    break;
  case Instruction::And:
    Out << "vec_and(";
    writeOperand(I.getOperand(0));
    Out << ",";
    writeOperand(I.getOperand(1));
    Out << ")";
    break;
  default:
    std::cerr << "AltiVec C Backend can't handle this instruction:\n" << I;
    exit(1);
  }
}

std::ostream &AltiVecCWriter::printFixedVectorType(std::ostream &Out,
						   const FixedVectorType *Ty,
						   const std::string &VariableName) {
  unsigned numElements = Ty->getNumElements();
  switch(Ty->getElementType()->getTypeID()) {
  case(Type::SByteTyID):
    if (numElements != 16) {
      std::cerr << "Vector of sbyte must have 16 elements!\n";
      exit(1);
    }
    Out << "vector signed char " << VariableName;
    break;
  case (Type::UByteTyID):
    if (numElements != 16) {
      std::cerr << "Vector of ubyte must have 16 elements!\n";
      exit(1);
    }
    Out << "vector unsigned char " << VariableName;
    break;
  case(Type::ShortTyID):
    if (numElements != 8) {
      std::cerr << "Vector of short must have 8 elements!\n";
      exit(1);
    }
    Out << "vector signed short " << VariableName;
    break;
  case(Type::UShortTyID):
    if (numElements != 8) {
      std::cerr << "Vector of ushort must have 8 elements!\n";
      exit(1);
    }
    Out << "vector unsigned short " << VariableName;
    break;
  case(Type::BoolTyID):
    switch(numElements) {
    case 8:
      Out << "vector bool short ";
      break;
    default:
      std::cerr << "AltiVec C Backend cannot handle boolean vector with "
		<< numElements << " elements!\n";
      break;
    }
    Out << VariableName;
    break;
  default:
    std::cerr << "AltiVec C Backend cannot handle " << Ty->getDescription() << "!\n";
    exit(1);
    break;
  }
  return Out;
}


//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool AltiVecCTargetMachine::addPassesToEmitFile(PassManager &PM, std::ostream &o,
						CodeGenFileType FileType, bool Fast) {
  if (FileType != TargetMachine::AssemblyFile) return true;
  // Add lowervectors pass here to lower variable-length vectors, which we can't handle
  PM.add(createLowerGCPass());
  PM.add(createLowerAllocationsPass(true));
  PM.add(createLowerInvokePass());
  PM.add(new CBackendNameAllUsedStructs());
  PM.add(new AltiVecCWriter(o, getIntrinsicLowering()));
  return false;
}

