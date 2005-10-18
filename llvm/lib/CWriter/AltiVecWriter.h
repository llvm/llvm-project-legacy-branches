//===---------------- Writer.h - C backend interface ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file exposes the interface that is common to all C backends.
//
//===----------------------------------------------------------------------===//

#ifndef ALTIVECCWRITER_H
#define ALTIVECCWRITER_H

#include "Writer.h"

using namespace llvm;

namespace llvm {
  /// AltiVecCWriter - This class is the main chunk of code that
  /// converts an LLVM module to an AltiVec/C translation unit.
  class AltiVecCWriter : public CWriter {
    void printConstant(Constant *CPV);
    void visitCastInst(CastInst &I);
    void visitVImmInst(VImmInst &I);
    void visitBinaryOperator(Instruction &I);

  public:
    AltiVecCWriter(std::ostream &o, IntrinsicLowering &il) : 
      CWriter(o,il) {}

    void writeValueName(Value *V) {
      std::string name = Mang->getValueName(V);
      if (name.substr(0,8) == "altivec_") {
	unsigned pos = name.find("_", 8);
	if (pos == std::string::npos) {
	  std::cerr << "Bad syntax for AltiVec intrinsic " << name << "\n";
	  exit(1);
	}
	// C name is vec_xxx
	Out << name.substr(4, pos-4);
      } else {
	CWriter::writeValueName(V);
      }
    }

    bool printDeclarationFor(Function *F) {
      if (F->getName().substr(0, 7) == "altivec")
	return false;
      return CWriter::printDeclarationFor(F);
    }
    virtual const char *getPassName() const { return "AltiVec C backend"; }

    std::ostream &printFixedVectorType(std::ostream &Out,
				       const FixedVectorType *Ty,
				       const std::string &VariableName = "");

  };
}



#endif

