//===- PbrInjector.cpp - Convert hand-parallel code to use `pbr'  ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass converts hand-annotated parallel code regions to use `pbr'
//
// Due to optimizations that run before this pass, at the end of it, there may
// be multiple calls to %llvm.join() for a given result of pbr. This is invalid
// LLVM pbr code, and a pass should run after it to `canonicalize' the usage of
// %llvm.join to push them down to a commonly-dominated block (creating it if
// necessary).
//
// This will be done by a subsequent pass, PbrCanonicalize.
//
//===----------------------------------------------------------------------===//

#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Type.h"
#include <vector>
using namespace llvm;

namespace {

  struct Parallelize : public FunctionPass {
    Parallelize() {}
    virtual bool runOnFunction(Function &F);    
  private:
    Function* getPbrFunc(Module *M);
    Function* getJoinFunc(Module *M);
    Function* getJoinIntrinsic(Module *M);
  };

  RegisterOpt<Parallelize> X("pbr-inject", "Convert `parallel' code to `pbr'");

} // End anonymous namespace 


/// runOnFunction - convert uses of the parallelizer function to uses of pbr
///
bool Parallelize::runOnFunction(Function &F) {
  bool Changed = false;
  Function *ParallelFn = getPbrFunc(F.getParent());
  if (ParallelFn->use_begin() == ParallelFn->use_end())
    return Changed;

  std::vector<User*> Users(ParallelFn->use_begin(), ParallelFn->use_end());
  for (std::vector<User*>::iterator u = Users.begin(), e = Users.end();
       u != e; ++u) {
    // Check for the pattern as follows:
    // %a = call  sbyte* %__llvm_pbr()
    // %b = seteq int %a, 0
    // br %b, %label_1, %label_2
    //
    // Rewrite to:
    // pbr %label_1, %label_2
    if (CallInst *CI = dyn_cast<CallInst>(*u)) {
      if (SetCondInst *SCI = dyn_cast<SetCondInst>(*CI->use_begin())) {
        if (BranchInst *BI = dyn_cast<BranchInst>(*SCI->use_begin())) {
          // Rewrite as above
          BasicBlock *BB = BI->getParent();
          ParaBrInst *pbr = 
            new ParaBrInst(BI->getSuccessor(0), BI->getSuccessor(1), BB);

          // Before we erase the call, add a join instruction to match the `pbr'
          CI->replaceAllUsesWith(pbr);

          // Delete condition and branch instructions
          BB->getInstList().erase(BI);
          BB->getInstList().erase(SCI);
          BB->getInstList().erase(CI);
          Changed = true;
        }
      }
    }
  }

  Function *JoinFn = getJoinFunc(F.getParent());
  if (JoinFn->use_begin() == JoinFn->use_end())
    return Changed;

  std::vector<User*> JoinCalls(JoinFn->use_begin(), JoinFn->use_end());
  Function *Join = getJoinIntrinsic(F.getParent());
  for (std::vector<User*>::iterator u = JoinCalls.begin(), e = JoinCalls.end();
       u != e; ++u) {
    // Convert calls to __llvm_join(%x = __llvm_pbr()) to
    // uses of the %llvm.join intrinsic
    if (CallInst *CJ = dyn_cast<CallInst>(*u)) {
      if (CJ->getCalledFunction() == JoinFn) {
        new CallInst(Join, CJ->getOperand(1), "", CJ);
        CJ->getParent()->getInstList().erase(CJ);
      }
    }
  }

  return Changed;
}

/// getPbrFunc - find the function in the Module that marks parallel regions
///
Function* Parallelize::getPbrFunc(Module *M) {
  return M->getOrInsertFunction("__llvm_pbr", PointerType::get(Type::SByteTy),
                                0);
}

Function* Parallelize::getJoinFunc(Module *M) {
  return M->getOrInsertFunction("__llvm_join", Type::VoidTy, 
                                PointerType::get(Type::SByteTy), 0);
}

Function* Parallelize::getJoinIntrinsic(Module *M) {
  return M->getOrInsertFunction("llvm.join", Type::VoidTy,
                                PointerType::get(Type::SByteTy), 0);
}
