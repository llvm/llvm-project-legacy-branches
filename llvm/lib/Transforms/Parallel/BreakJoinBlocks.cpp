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

#include "llvm/iOther.h"
#include "llvm/Function.h"
#include "llvm/BasicBlock.h"
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
      BasicBlock::iterator II(CI);
      if (II != BB->begin()) {
        BasicBlock *newBB = BB->splitBasicBlock(II, BB->getName() + "_split");
        if (newBB) Worklist.push_back(newBB);
      }
    }
  }

  return Changed;
}

CallInst* BreakJoin::findJoin(BasicBlock *BB) {
  for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i)
    if (CallInst *CI = dyn_cast<CallInst>(i))
      if (CI->getCalledFunction()->getName() == "llvm.join")
        return CI;

  return 0;
}
