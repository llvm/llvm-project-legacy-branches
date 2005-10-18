//===- Alloca2Realloc.cpp - Replace allocas with realloc inside loops -----===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file replaces alloca instructions with calls to the C library
// function realloc inside loops, in order to conserve memory and
// prevent stack overflow.  The replacement is done only if the
// pointer resulting from the alloca is never stored to memory or
// passed as an argument to a function.  In this case, the replacement
// is safe: because LLVM is in SSA form, any subsequent execution of
// the same alloca must overwrite the old pointer.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "alloca2realloc"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/BasicBlock.h"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

namespace {


  //===----------------------------------------------------------------------===//
  //                          Class definitions
  //===----------------------------------------------------------------------===//

  class Alloca2Realloc : public FunctionPass {

    Function *ReallocFunc;
    bool changed;
    Function *function;
    LoopInfo *LI;

  public:
    /// This transformation requires natural loop information
    ///
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<LoopInfo>();
    }
    virtual bool doInitialization(Module &M);
    virtual bool runOnFunction(Function &F);
    
  private:
    void visitLoop(Loop*);
    bool processAlloca(AllocaInst*);
    bool isSafe(Value*);
  }; 

  RegisterOpt<Alloca2Realloc> X("alloca2realloc",
				"Replace alloca with realloc inside loops");


  //===----------------------------------------------------------------------===//
  //                     Alloca2Realloc implementation
  //===----------------------------------------------------------------------===//

  bool Alloca2Realloc::doInitialization(Module &M) {
    const Type *SBPTy = PointerType::get(Type::SByteTy);
    ReallocFunc = M.getNamedFunction("realloc");

    if (ReallocFunc == 0)
      ReallocFunc = M.getOrInsertFunction("realloc", SBPTy, SBPTy, Type::UIntTy, 0);

    return true;
  }

  bool Alloca2Realloc::runOnFunction(Function &F) {
    changed = false;
    function = &F;
    assert(ReallocFunc && "Pass not initialized!");

    // Get loop information
    //
    LI = &getAnalysis<LoopInfo>();

    // Process each top-level loop
    //
    for (LoopInfo::iterator I = LI->begin(), E = LI->end(); I != E; ++I) {
      visitLoop(*I);
    }
    return changed;
  }

  void Alloca2Realloc::visitLoop(Loop *L) {
    // Recurse through all subloops before we process this loop...
    //
    for (Loop::iterator I = L->begin(), E = L->end(); I != E; ++I) {
      visitLoop(*I);
    }

    // Now do this loop
    //
    for (std::vector<BasicBlock*>::const_iterator BI = L->getBlocks().begin(),
	   BE = L->getBlocks().end(); BI != BE; ++BI) {
      BasicBlock *BB = *BI;
      if (LI->getLoopFor(BB) == L) {       // Ignore blocks in subloops...
	BasicBlock::InstListType &BBIL = BB->getInstList();
	for (BasicBlock::iterator II = BB->begin(), IE = BB->end(); II != IE; ++II) {
	  if (AllocaInst *AI = dyn_cast<AllocaInst>(II)) {
	    if (processAlloca(AI)) {
	      II = --BBIL.erase(II);         // remove and delete the alloca instruction
	      changed = true;
	    }
	  }
	}
      }
    }

  }

  // Process an alloca instruction.  Return true if the alloca was
  // replaced with a realloc, false otherwise.
  //
  bool Alloca2Realloc::processAlloca(AllocaInst *AI) {
    DEBUG(std::cerr << "Processing " << *AI);
    if (!isSafe(AI)) {
      DEBUG(std::cerr << "Instruction is not safe to replace!\n");
      return false;
    }
    DEBUG(std::cerr << "Replacing instruction\n");
    Instruction *before = &(function->getEntryBlock().front());
    Value *loadStorePtr = new AllocaInst(AI->getType(), 0, "loadstore_ptr", before);
    new StoreInst(Constant::getNullValue(AI->getType()), loadStorePtr, before);
    
    const FunctionType *ReallocFTy = ReallocFunc->getFunctionType();

    // Create the vector of arguments to realloc
    //
    Value *reallocPtr = new LoadInst(loadStorePtr, "realloc_ptr", AI);
    if (reallocPtr->getType() != ReallocFTy->getParamType(0)) {
      reallocPtr = new CastInst(reallocPtr, ReallocFTy->getParamType(0), "cast", AI);
    }

    Value *reallocSize = 
      BinaryOperator::create(Instruction::Mul, AI->getArraySize(),
			     ConstantUInt::get(Type::UIntTy,
					       AI->getAllocatedType()->getPrimitiveSize()),
			     "size", AI);
    if (reallocSize->getType() != ReallocFTy->getParamType(1)) {
      reallocSize = new CastInst(reallocSize, ReallocFTy->getParamType(1), "cast", AI);
    }

    std::vector<Value*> ReallocArgs;
    ReallocArgs.push_back(reallocPtr);
    ReallocArgs.push_back(reallocSize);

    // Create the call to realloc
    //
    CallInst *call = new CallInst(ReallocFunc, ReallocArgs, AI->getName(), AI);

    // Create a cast instruction to convert to the right type...
    //
    Value *newVal = call;
    if (call->getType() == Type::VoidTy)
      newVal = Constant::getNullValue(AI->getType());
    else if (call->getType() != AI->getType())
      newVal = new CastInst(call, AI->getType(), "cast", AI);

    // Replace all uses of the old malloc inst with the cast inst
    //
    AI->replaceAllUsesWith(newVal);

    // Insert a free instruction before every return
    //
    for (Function::iterator FI = function->begin(), FE = function->end(); 
	 FI != FE; ++FI) {
      if (ReturnInst *RI = dyn_cast<ReturnInst>(FI->getTerminator())) {
	LoadInst *freePtr = new LoadInst(loadStorePtr, "free_ptr", RI);
	new FreeInst(freePtr, RI);
      }
    }

    return true;
  }

  // Return true if the given value is never used in a store or call
  // instruction, false otherwise
  //
  bool Alloca2Realloc::isSafe(Value *V) {
    for (Value::use_iterator I = V->use_begin(), E = V->use_end();
	 I != E; ++I) {
      if (isa<CallInst>(*I)) {
	DEBUG(std::cerr << "Followed chain of uses to " << **I);
	return false;
      }
      StoreInst *SI = dyn_cast<StoreInst>(*I);
      if (SI && V == SI->getOperand(0)) {
	DEBUG(std::cerr << "Followed chain of uses to " << *SI);
	return false;
      }
      if (!isa<LoadInst>(*I) && !isSafe(*I))
	return false;
    }
    return true;
  }

}
