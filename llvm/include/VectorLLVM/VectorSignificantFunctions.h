//===-- llvm/VectorLLVM/Instrinsics.h - LLVM Vector Significant Function Handling ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a set of enums which allow processing of
// significant functions for Vector-LLVM.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VECTOR_SIGNIFICANT_FUNCTIONS_H
#define LLVM_VECTOR_SIGNIFICANT_FUNCTIONS_H

#include <string.h>

namespace llvm {

/// VectorSignificantFunctions Namespace - This namespace contains an
/// enum with a value for every vector significant function known by
/// LLVM.
///
namespace VectorSignificantFunctions {

#define NUM_NAMES 17

  std::string names[NUM_NAMES] = {
    "vimm",
    "fixed_vimm",
    "vgather",
    "vscatter",
    "load",
    "store",
    "vselect", 
    "extract", 
    "combine", 
    "fixed_combine",
    "extractelement", 
    "combineelement",
    "constant",
    "fixed_permute",
    // Older forms
    "vload", 
    "vloadi", 
    "vstore"
  };

  enum ID {
    not_significant = 0,   // Must be zero
    vimm,
    fixed_vimm,
    vgather,
    vscatter,
    load,
    store,
    vselect,
    extract, 
    combine, 
    fixed_combine,
    extractelement, 
    combineelement,
    constant,
    fixed_permute,
    // Older forms
    vload, 
    vloadi, 
    vstore
  };

  ID getID(std::string name) {
    for (unsigned i = 0; i < NUM_NAMES; ++i) {
      if (name.substr(0, 5) == "vllvm") {
	if (name.substr(6, names[i].length()+1) == names[i] + '_')
	  return ID(i+1);
      }
      if (name.substr(0, names[i].length()+2) == '_'+names[i]+'_')
	return ID(i+1);
    }
    return not_significant;
  }

  /// Return true if the specified call represents a properly declared
  /// vector significant function
  ///
  bool isProperlyDeclared(Function *F) {
    std::string name = F->getName();
    const FunctionType *FTy = F->getFunctionType();
    const Type *RetTy = FTy->getReturnType();
    unsigned numParams = FTy->getNumParams();
    bool isVarArg = FTy->isVarArg();
    switch(getID(name)) {
    case not_significant:
      return true;
    case vgather:
    case vload: {
      // A vload function must be a varargs function with 1 argument:
      // the place to load from
      //
      if (numParams != 1 || !isVarArg)
	return false;
      const PointerType *PointerTy = 
	dyn_cast<PointerType>(FTy->getParamType(0));
      if (!PointerTy || RetTy != PointerTy->getElementType())
	return false;
      break;
    }
    case load: {
      // A fixed_vload function must have precisely 3 arguments: a
      // pointer, a vector length, and an index.  The pointer must
      // point to the return value type.
      //
      if (numParams != 3 || isVarArg)
	return false;
      const PointerType *PointerTy = 
	dyn_cast<PointerType>(FTy->getParamType(0));
      if (!PointerTy || RetTy != PointerTy->getElementType())
	return false;
      if (!FTy->getParamType(1)->isIntegral())
	return false;
      if (!FTy->getParamType(2)->isIntegral())
	return false;
      break;
    }
    case vloadi:
      // A vloadi function must have precisely 2 arguments: a scalar
      // and a length.  It must return the type of its first argument.
      //
      if (numParams != 2 || isVarArg)
	return false;
      if (RetTy != FTy->getParamType(0))
	return false;
      if (!FTy->getParamType(0)->isIntegral())
	return false;
      break;
    case vimm:
    case fixed_vimm:{
      // A fixed_vimm must have precisely 2 arguments: a scalar and a
      // vector length.  It must return the type of its first
      // argument.
      //
      if (numParams != 2 || isVarArg)
	return false;
      if (RetTy != FTy->getParamType(0))
	return false;
      if (!FTy->getParamType(0)->isIntegral())
	return false;
      break;
    }
    case vscatter:
    case vstore:
      // A vstore function must be a varargs function with 2
      // arguments: the value to store and the place to store it
      //
      if (numParams != 2 || !isVarArg)
	return false;
      if (!isa<PointerType>(FTy->getParamType(1)))
	return false;
      if (FTy->getParamType(0) != cast<PointerType>(FTy->getParamType(1))->getElementType())
	return false;
      break;
    case store: {
      // A store function must have precisely 3 arguments: a value to
      // store, the place to store it, and an index.  The pointer must
      // point to the value type.
      //
      if (numParams != 3 || isVarArg)
	return false;
      const PointerType *PointerTy = 
	dyn_cast<PointerType>(FTy->getParamType(1));
      if (!PointerTy || FTy->getParamType(0) != PointerTy->getElementType())
	return false;
      if (!FTy->getParamType(2)->isIntegral())
	return false;
      break;
    }
    case vselect:
      // A vselect function must have precisely 3 arguments: an int
      // (really a boolean) and two values of the same type
      //
      if (numParams != 3 || isVarArg)
	return false;
      if (FTy->getParamType(0) != Type::IntTy)
	return false;
      if (FTy->getParamType(1) != FTy->getParamType(2))
	return false;
      break;
    case extract:
      // An extract function must have precisely 4 arguments: a value
      // and 3 unsigned ints
      //
      if (numParams != 4 || isVarArg)
	return false;
      if (FTy->getParamType(1) != Type::UIntTy)
	return false;
      if (FTy->getParamType(2) != Type::UIntTy)
	return false;
      if (FTy->getParamType(3) != Type::UIntTy)
	return false;
      break;
    case combine:
      // A combine function must have precisely 4 arguments: two
      // values of the same type and two unsigned ints
      //
      if (numParams != 4 || isVarArg)
	return false;
      if (RetTy != FTy->getParamType(0))
	return false;
      if (FTy->getParamType(0) != FTy->getParamType(1))
	return false;
      if (FTy->getParamType(2) != Type::UIntTy)
	return false;
      if (FTy->getParamType(3) != Type::UIntTy)
	return false;
      break;
    case fixed_combine:
      // A fixed_combine must have precisely 6 arguments: a value, a
      // vector length, another value of the same type as the first,
      // another vector length, and two unsigned ints.
      //
      if (numParams != 6 || isVarArg)
	return false;
      if (RetTy != FTy->getParamType(0))
	return false;
      if (FTy->getParamType(0) != FTy->getParamType(2))
	return false;
      if (FTy->getParamType(1) != Type::UIntTy)
	return false;
      if (FTy->getParamType(3) != Type::UIntTy)
	return false;
      if (FTy->getParamType(4) != Type::UIntTy)
	return false;
      if (FTy->getParamType(5) != Type::UIntTy)
	return false;
      break;
    case extractelement:
      // An extractelement function must have precisely 2 arguments: a
      // value and an unsigned int
      //
      if (numParams != 2 || isVarArg)
	return false;
      if (FTy->getParamType(1) != Type::UIntTy)
	return false;
      break;
    case combineelement:
      // A combineelement function must have precisely 3 arguments:
      // two values of the same type and two unsigned ints
      //
      if (numParams != 3 || isVarArg)
	return false;
      if (FTy->getParamType(0) != FTy->getParamType(1))
	return false;
      if (FTy->getParamType(2) != Type::UIntTy)
	return false;
      break;
    case constant:
      // A constant function must be a varargs function with one
      // argument.  The return type must be the same as the type of
      // that argument
      //
      if (numParams != 1 || !isVarArg)
	return false;
      if (RetTy != FTy->getParamType(0))
	return false;
      break;
    case fixed_permute:
      // A fixed permute function must have four arguments: a vector,
      // a length, and index vector, and a length
      if (numParams != 4 || isVarArg)
	return false;
      if (FTy->getParamType(0) != FTy->getParamType(2))
	return false;
      if (FTy->getParamType(1) != Type::UIntTy ||
	  FTy->getParamType(3) != Type::UIntTy)
	return false;
      break;
      //
      // Newer Forms
      //
    default:
      std::cerr << "Unknown vector significant function " << name << "!\n";
      exit(1);
    }
    return true;
  }

} // End VectorSignificantFunctions namespace

} // End llvm namespace

#endif
