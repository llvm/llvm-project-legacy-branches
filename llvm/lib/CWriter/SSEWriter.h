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

#ifndef SSECWRITER_H
#define SSECWRITER_H

#include "Writer.h"

using namespace llvm;

namespace llvm {
  /// SSECWriter - This class is the main chunk of code that
  /// converts an LLVM module to an SSE/C translation unit.
  class SSECWriter : public CWriter {
    void printConstant(Constant *CPV);
    void visitCastInst (CastInst &I);
    void visitBinaryOperator(Instruction &I);

  public:
    SSECWriter(std::ostream &o, IntrinsicLowering &il) : 
      CWriter(o,il) {}

    // Provide declaration for SSE intrinsics
    //
    bool doInitialization(Module &M) {
      Out << "#include \"SSE.h\"\n";
      return CWriter::doInitialization(M);
    }

    bool printDeclarationFor(Function *F) {
      if (F->getName().substr(0, 4) == "_mm_")
	return false;
      return CWriter::printDeclarationFor(F);
    }
    virtual const char *getPassName() const { return "SSE C backend"; }

    std::ostream &printFixedVectorType(std::ostream &Out,
				       const FixedVectorType *Ty,
				       const std::string &VariableName = "");

  };
}



#endif

