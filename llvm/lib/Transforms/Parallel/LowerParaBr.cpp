//===- LowerParaBr.cpp - Eliminate Pbr instructions: straighten code ------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// The LowerParaBr transformation converts parallel code regions utilizing `pbr'
// and `join' into sequential code for use in cases where parallelization is too
// fine-grained to benefit from concurrent execution, or to fall back on
// sequential execution when future transformations cannot yet handle
// parallelism, or to convert code that has already become re-written to use a
// thread library.
//
//===----------------------------------------------------------------------===//

#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/iOther.h"
#include "llvm/iTerminators.h"
#include "llvm/Pass.h"
#include "llvm/Type.h"
#include "llvm/Analysis/ParallelInfo.h"
#include <vector>
using namespace llvm;

namespace {

  /// LowerParaBr - Convert parallel code regions to sequential code.
  ///
  class LowerParaBr : public FunctionPass {

  public:
    LowerParaBr() {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<ParallelInfo>();
    }
    
    bool runOnFunction(Function &F);

  private:
    void straightenSequence(ParallelSeq *PS);
  };

  RegisterOpt<LowerParaBr> X("lowerpbr", "Lower pbr to sequential code");
}

/// runOnFunction - Lower all parallel sequences to straight-line code
///
bool LowerParaBr::runOnFunction(Function &F) {
  bool Changed = false;

  ParallelInfo &PI = getAnalysis<ParallelInfo>();

  // Convert parallel regions to sequential code
  for (ParallelInfo::iterator i = PI.begin(), e = PI.end(); i != e; ++i) {
    ParallelSeq *PS = *i;
    // Recursively straighten parallel sequences
    straightenSequence(PS);
    Changed = true;
  }

  return Changed;
}

/// straightenSequence - Recursively process parallel sequences
///
void LowerParaBr::straightenSequence(ParallelSeq *PS) {
  ParallelRegion *PR = 0, *PrevPR = 0;
  for (ParallelSeq::riterator i = PS->rbegin(),
         e = PS->rend(); i != e; ++i)
  {
    PrevPR = PR;
    PR = *i;
    for (ParallelRegion::seqiterator ci = PR->seqbegin(), ce = PR->seqend(); 
         ci != ce; ++ci) 
      straightenSequence(*ci);

    // Stitch previous region to the current one by rewriting the last branch
    if (PrevPR) {
      std::vector<BasicBlock*> &PrevBlocks = PrevPR->getBlocks(),
        &PRBlocks = PR->getBlocks();
      BasicBlock *PrevLastBB = PrevBlocks.back(), *PRFirstBB = PRBlocks[0];
      TerminatorInst *TI = PrevLastBB->getTerminator();
      new BranchInst(PRFirstBB, TI);
      PrevLastBB->getInstList().erase(TI);
    }
  }

  // Remove the call to llvm.join()
  ParaBrInst *pbr = dyn_cast<ParaBrInst>(PS->getHeader()->getTerminator());
  assert(pbr && "pbr not a terminator of ParallelSeq header!");
  std::vector<User*> Users(pbr->use_begin(), pbr->use_end());
  for (std::vector<User*>::iterator use = Users.begin(), ue = Users.end();
       use != ue; ++use)
    if (Instruction *Inst = dyn_cast<Instruction>(*use))
      Inst->getParent()->getInstList().erase(Inst);

  // Rewrite pbr as a regular branch
  new BranchInst(pbr->getSuccessor(0), pbr);
  pbr->getParent()->getInstList().erase(pbr);
}
