//===- Utils.h - Utilities for vector code generation -----------*- C++ -*-===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef PROJECTS_VECTOR_INCLUDE_UTILS_H
#define PROJECTS_VECTOR_INCLUDE_UTILS_H

#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/hash_set"
#include <sstream>
#include <iostream>

using namespace llvm;

namespace VectorUtils {



  //===----------------------------------------------------------------------===//
  //          Helper functions for managing register and label names
  //===----------------------------------------------------------------------===//

  /// Counter for temporary variable names
  ///
  static unsigned tmpCount = 0;

  /// Counter for loop labels
  ///
  static unsigned loopCount = 0;

  /// Counter for conditionals
  ///
  static unsigned condCount = 0;

  static std::string next(std::string prefix, unsigned suffix) {
    std::ostringstream os;
    os << prefix << "." << suffix;
    return os.str();
  }

  static std::string nextTmp() {
    return next("tmp", tmpCount++);
  }

  static std::string nextHeader() {
    return next("loop_header", loopCount++);
  }

  static std::string nextBody() {
    return next("loop_body", loopCount-1);
  }

  static std::string nextExit() {
    return next("loop_exit", loopCount-1);
  }

  static std::string nextIfTrue() {
    return next("if_true", condCount++);
  }

  static std::string nextIfFalse() {
    return next("if_false", condCount);
  }


  //===----------------------------------------------------------------------===//
  //          Helper functions for identifying vector stuff
  //===----------------------------------------------------------------------===//

  static bool recursiveContainsVector(const Type* Ty,
					      hash_set<const Type*> &seenTys) {
    if (seenTys.count(Ty))
      return false;
    seenTys.insert(Ty);
    if (isa<VectorType>(Ty)) 
      return true;
    else if (const SequentialType *SequentialTy = dyn_cast<SequentialType>(Ty)) {
      return recursiveContainsVector(SequentialTy->getElementType(),
					     seenTys);
    } else if (const StructType *StructTy = dyn_cast<StructType>(Ty)) {
      for (StructType::element_iterator I = StructTy->element_begin(),
	     E = StructTy->element_end(); I != E; ++I) {
	if (recursiveContainsVector(*I, seenTys)) return true;
      }
      return false;
    } else if (const FunctionType *FunctionTy = dyn_cast<FunctionType>(Ty)) {
      if (recursiveContainsVector(FunctionTy->getReturnType(),
					  seenTys))
	return true;
      for (FunctionType::param_iterator I = FunctionTy->param_begin(),
	   E = FunctionTy->param_end(); I != E; ++I)
	if (recursiveContainsVector(*I, seenTys)) return true;
      return false;
    }
    return false;
  }

  static bool containsVector(const Type *Ty) {
    hash_set<const Type*> seenTys;
    return recursiveContainsVector(Ty, seenTys);
  }

  static bool containsVector(Instruction *inst) {
    if (containsVector(inst->getType())) return true;
    for (User::op_iterator I = inst->op_begin(), E = inst->op_end();
	 I != E; ++I)
      if (containsVector((*I)->getType()))
	return true;
    return false;
  }


  //===----------------------------------------------------------------------===//
  //          Very useful for identifying intrinsics
  //===----------------------------------------------------------------------===//

  static bool isFunctionContaining(Value *Val, const std::string &s) {
    if (CallInst *CI = dyn_cast<CallInst>(Val)) {
      if (Function *F = CI->getCalledFunction()) {
	if (F->getName().find(s) != std::string::npos) {
	  return true;
	}
      }
    }
    return false;
  }

  //===----------------------------------------------------------------------===//
  //          Helper functions for array computations
  //===----------------------------------------------------------------------===//

  /// Compute an index into a flattened (one-dimensional) array
  /// corresponding to the given indices for a possibly
  /// multidimensional array.
  ///
  static Instruction *computeFlattenedPointer(VMemoryInst *VI, std::vector<Value*> indices,
					      Instruction *before) {
    unsigned numLevels = VI->getNumIndices() >> 2;
    Value *indexValue = 
      BinaryOperator::create(Instruction::Mul, indices[0],
			     VI->getMultiplier(0), nextTmp(), before);
    for (unsigned i = 1; i < numLevels; ++i) {
      Instruction *mul = 
	BinaryOperator::create(Instruction::Mul, indices[i], 
			       VI->getMultiplier(i), nextTmp(), before);
      indexValue =
	BinaryOperator::create(Instruction::Add, indexValue, mul,
			       nextTmp(), before);
    }

    std::vector<Value*> indexVector;
    indexVector.push_back(indexValue);
    return new GetElementPtrInst(VI->getPointerOperand(), indexVector, 
				 "ptr", before);
  }

  /// Compute the length of an array in a single dimension, given the
  /// lower bound, upper bound, and stride.  Type is LongTy.
  ///
  static Value *computeSingleDimensionLength(Value *lowerBound, Value *upperBound,
					     Value *stride, Instruction *before) {
    ConstantSInt *longZero = ConstantSInt::get(Type::LongTy, 0);
    ConstantSInt *longOne = ConstantSInt::get(Type::LongTy, 1);
    ConstantSInt *longNegOne = ConstantSInt::get(Type::LongTy, -1);
    Value *length;
    length = (lowerBound == longZero) ? upperBound :
      BinaryOperator::create(Instruction::Sub, upperBound, lowerBound, 
			     nextTmp(), before);
    length = BinaryOperator::create(Instruction::Add, length, stride, 
				    nextTmp(), before);
    if (stride != longOne)
      length = BinaryOperator::create(Instruction::Div, length, stride,
				      nextTmp(), before);
    return length;
  }

  /// Given a vector instruction (vload or vstore), generate code to
  /// compute the length of the (flattened) array slice defined by the
  /// indices.  Type is UIntTy.
  ///
  static Value *computeIndexedLength(VMemoryInst *VI, 
				     Instruction *before = 0,
				     std::string name = "tmp") {
    if (!before)
      before = VI;

    unsigned numTriples = VI->getNumIndices() >> 2;
    Value *oldVal = 0, *newVal = 0, *length = 0;

    for (unsigned i = 0; i < numTriples; ++i) {
      Value *start = VI->getLowerBound(i);
      Value *end = VI->getUpperBound(i);
      Value *stride = VI->getStride(i);
      if (start != end) {
	newVal = computeSingleDimensionLength(start, end, stride, before);
	if (oldVal)
	  newVal = BinaryOperator::create(Instruction::Mul, oldVal, newVal,
					  nextTmp(), before);
	oldVal = newVal;
      }
    }
    if (newVal)
      length = new CastInst(newVal, Type::UIntTy, name, before);
    else 
      length = ConstantUInt::get(Type::UIntTy, 1);

    return length;
  }

  /// Given two lengths, generate code to ensure that they are equal;
  /// abort if not
  ///
  static void ensureEquality(Instruction *before, Value *V1, Value *V2) {
    // Set up the basic blocks
    //
    BasicBlock *initial = before->getParent();
    BasicBlock *ifFalse = initial->splitBasicBlock(before, nextIfFalse());
    BasicBlock *ifTrue = ifFalse->splitBasicBlock(before, nextIfTrue());
    Instruction *cond = 
      BinaryOperator::create(Instruction::SetEQ, V1, V2,
			     nextTmp(), initial->getTerminator());
    BasicBlock::iterator oldInst(initial->getTerminator());
    Instruction *newInst = new BranchInst(ifTrue, ifFalse, cond);
    ReplaceInstWithInst(initial->getInstList(), oldInst, newInst);

    // Generate code to abort
    //
    std::vector<const Type*> formalArgs;
    std::vector<Value*> args;

    FunctionType *FType = FunctionType::get(Type::VoidTy, formalArgs, false);
    Function *F = initial->getParent();
    Module *M = F->getParent();
    Function *abort = M->getOrInsertFunction("abort", FType);
    Instruction *call = new CallInst(abort, args, "", ifFalse->getTerminator());

    // Generate code to print the error message
    //
    formalArgs.push_back(PointerType::get(Type::SByteTy));
    FType = FunctionType::get(Type::IntTy, formalArgs, true);
    Function *printf = M->getOrInsertFunction("printf", FType);
    
    std::string msg = "unequal vector lengths " + V1->getName() + " and "
      + V2->getName() + " in function " + F->getName() + "\n";
    Constant *arr = ConstantArray::get(msg);
    GlobalVariable *GV = new GlobalVariable(arr->getType(), true,
					    GlobalValue::InternalLinkage, 
					    arr, "error_str", M);
    std::vector<Constant*> idx;
    idx.push_back(ConstantUInt::get(Type::ULongTy, 0));
    idx.push_back(ConstantUInt::get(Type::ULongTy, 0));
    Constant *gep = ConstantExpr::getGetElementPtr(GV, idx);

    args.push_back(gep);
    new CallInst(printf, args, "", call);
  }

  //===----------------------------------------------------------------------===//
  //          Nicer CallInst constructor interface
  //===----------------------------------------------------------------------===//

  static CallInst *getCallInst(const Type* RetTy, const std::string& FName, std::vector<Value*>& args,
			       const std::string& Name, Instruction *before) {
    std::vector<const Type*> formalArgs;
    for (std::vector<Value*>::iterator I = args.begin(), E = args.end(); I != E; ++I) {
      formalArgs.push_back((*I)->getType());
    }
    FunctionType *FType = FunctionType::get(RetTy, formalArgs, false);
    Module *M = before->getParent()->getParent()->getParent();
    Function *F = M->getOrInsertFunction(FName, FType);
    return new CallInst(F, args, Name, before);
  }

  static CallInst *getCallInst(const Type* RetTy, const std::string& FName, Value *arg1,
			       const std::string& Name, Instruction *before) {
    std::vector<Value*> args;
    args.push_back(arg1);
    return getCallInst(RetTy, FName, args, Name, before);
  }

  static CallInst *getCallInst(const Type* RetTy, const std::string& FName, Value *arg1, Value *arg2,
			       const std::string& Name, Instruction *before) {
    std::vector<Value*> args;
    args.push_back(arg1);
    args.push_back(arg2);
    return getCallInst(RetTy, FName, args, Name, before);
  }

  static CallInst *getCallInst(const Type* RetTy, const std::string& FName, Value *arg1, Value *arg2, Value *arg3,
			       const std::string& Name, Instruction *before) {
    std::vector<Value*> args;
    args.push_back(arg1);
    args.push_back(arg2);
    args.push_back(arg3);
    return getCallInst(RetTy, FName, args, Name, before);
  }

}

#endif
