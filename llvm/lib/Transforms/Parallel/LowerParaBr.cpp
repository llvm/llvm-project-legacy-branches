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
    Function* getJoinIntrinsic(Module *M);    
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

static bool contains(std::vector<BasicBlock*> haystack, BasicBlock *needle) {
  return std::find(haystack.begin(), haystack.end(), needle) != haystack.end();
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

    // Stitch previous region to the current one by all branches to the join
    // block instead branch to the first block of the second region
    if (PrevPR) {
      std::vector<BasicBlock*> &PrevJoinBlocks = PrevPR->getJoinBlocks(),
        &PRBlocks = PR->getBlocks();
      BasicBlock *PRFirstBB = PRBlocks[0];
      for (std::vector<BasicBlock*>::iterator j = PrevJoinBlocks.begin(),
             je = PrevJoinBlocks.end(); j != je; ++j) {
        std::vector<User*> JUsers((*j)->use_begin(), (*j)->use_end());
        for (std::vector<User*>::iterator u = JUsers.begin(), ue = JUsers.end();
             u != ue; ++u)
          if (Instruction *I = dyn_cast<Instruction>(*u))
            if (contains(PR->getBlocks(), I->getParent()))
              I->replaceUsesOfWith(*j, PRFirstBB);
      }
    }
  }

  Module *M = PS->getHeader()->getParent()->getParent();
  Function *JoinIntr = getJoinIntrinsic(M);
  std::vector<User*> Users(JoinIntr->use_begin(), JoinIntr->use_end());
  // Remove the calls to llvm.join()
  for (std::vector<User*>::iterator use = Users.begin(), ue = Users.end();
       use != ue; ++use)
    if (CallInst *CI = dyn_cast<CallInst>(*use))
      if (contains(PR->getJoinBlocks(), CI->getParent()) || 
          contains(PrevPR->getJoinBlocks(), CI->getParent()))
        CI->getParent()->getInstList().erase(CI);
          
  // Remove pbr
  ParaBrInst *pbr = dyn_cast<ParaBrInst>(PS->getHeader()->getTerminator());
  //assert(pbr && "Terminator of ParaSeq header is not a pbr");
  if (pbr) {
    new BranchInst(pbr->getSuccessor(0), pbr);
    pbr->getParent()->getInstList().erase(pbr);
  }
}

Function* LowerParaBr::getJoinIntrinsic(Module *M) {
  return M->getOrInsertFunction("llvm.join", Type::VoidTy,
                                PointerType::get(Type::SByteTy), 0);
}
