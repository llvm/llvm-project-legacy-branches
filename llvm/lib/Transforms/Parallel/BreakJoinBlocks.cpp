//===- BreakJoinBlocks.cpp - Break blocks at calls to join ----------===//
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

#include "llvm/BasicBlock.h"
#include "llvm/Function.h"
#include "llvm/iOther.h"
#include "llvm/iTerminators.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/FunctionUtils.h"
using namespace llvm;

namespace {

  /// BreakJoin - 
  ///
  struct BreakJoin : public FunctionPass {
  public:
    BreakJoin() {}
    bool runOnFunction(Function &F);
  private:
    CallInst *findJoin(BasicBlock *BB);
  };

  RegisterOpt<BreakJoin> X("break-joins", "Break BasicBlocks at joins");

} // End anonymous namespace

bool BreakJoin::runOnFunction(Function &F) {
  bool Changed = false;
  std::vector<BasicBlock*> Worklist;
  for (Function::iterator i = F.begin(), e = F.end(); i != e; ++i)
    Worklist.push_back(i);

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.back();
    Worklist.pop_back();
    if (CallInst *CI = findJoin(BB)) {
      // Break away everything after the join
      BasicBlock::iterator Next(CI); ++Next;
      if (!isa<TerminatorInst>(Next) || isa<ReturnInst>(Next)) {
        BasicBlock *newBB = 
          BB->splitBasicBlock(Next, BB->getName() + "_postsplit");
        if (newBB) Worklist.push_back(newBB);
      }

      // Break away the join from anything before it
      BasicBlock::iterator II(CI);
      if (II != BB->begin()) {
        BasicBlock *newBB = BB->splitBasicBlock(II, BB->getName() +"_presplit");
        if (newBB) Worklist.push_back(newBB);
      }
    }
  }

  return Changed;
}

CallInst* BreakJoin::findJoin(BasicBlock *BB) {
  for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i)
    if (CallInst *CI = dyn_cast<CallInst>(i))
      if (CI->getCalledFunction()->getName() == "llvm.join" ||
          CI->getCalledFunction()->getName() == "__llvm_thread_join") 
        return CI;

  return 0;
}
