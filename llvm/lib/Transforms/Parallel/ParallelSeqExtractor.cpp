//===- PSeqExtractor.cpp - Pull parallel seqs into new functions ----------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file implements the interface to tear out parallel seqs into a new
// function, replacing it with a call to the new function.
//
//===----------------------------------------------------------------------===//

#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/BasicBlock.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/ParallelInfo.h"
#include "llvm/Transforms/Utils/FunctionUtils.h"
#include "llvm/Support/CFG.h"
#include "Support/Debug.h"
#include <vector>
using namespace llvm;

namespace {

  /// PSeqExtractor - 
  ///
  /// FIXME: This should be a Pass, but Passes currently cannot require
  /// FunctionPasses.
  ///
  struct PSeqExtractor : public FunctionPass {
  public:
    PSeqExtractor() {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<DominatorSet>();
      AU.addRequired<ParallelInfo>();
    }
    
    bool runOnFunction(Function &F);
  private:
    bool containsThreadCall(BasicBlock *BB);
  };

  // FIXME: need to specify threshold level.. ie, up to 2 nested pseqs become
  // new functions, then everything else sequential
  RegisterOpt<PSeqExtractor>
  X("pseq-extract", "Extract parallel seqs into new functions");

} // End anonymous namespace

static bool contains(std::vector<BasicBlock*> &haystack,
                     BasicBlock *needle) {
  return std::find(haystack.begin(), haystack.end(), needle) != haystack.end();
}

static void intersect(std::vector<BasicBlock*> &a,
                      std::vector<BasicBlock*> &b,
                      std::vector<BasicBlock*> &intersection) {
  for (unsigned i = 0, e = a.size(); i != e; ++i)
    for (unsigned j = 0, f = b.size(); j != f; ++j)
      if (a[i] == b[j])
        intersection.push_back(a[i]);
}

static BasicBlock* clone(BasicBlock *BB) {
  BasicBlock *newBB = new BasicBlock(BB->getName(), BB->getParent());
  for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i)
    newBB->getInstList().push_back(i->clone());
  return newBB;
}

/// runOnFunction - 
///
bool PSeqExtractor::runOnFunction(Function &F) {
  bool Changed = false;
  DEBUG(std::cerr << "Running on: " << F.getName() << "\n");
  ParallelInfo &PI = getAnalysis<ParallelInfo>();
  DominatorSet &DS = getAnalysis<DominatorSet>();

#if 0
  // If there is shared code in the ParallelRegions, make copies
  // 1. find intersecting code region
  for (ParallelInfo::iterator i = PI.begin(), e = PI.end(); i != e; ++i) {
    ParallelSeq *PS = *i;
    ParallelRegion* PR[2];
    unsigned i=0, e=0;
    for (ParallelSeq::riterator r = PS->rbegin(), re = PS->rend(); 
         r!=re; ++r, ++i) PR[i] = *r;
    std::vector<BasicBlock*> Blocks0(PR[0]->begin(), PR[0]->end());
    std::vector<BasicBlock*> Blocks1(PR[1]->begin(), PR[1]->end());
    std::vector<BasicBlock*> Intersection;
    intersect(Blocks0, Blocks1, Intersection);

    // 2. for each basic block in shared code:
    //      clone block
    //      all incoming/outgoing edges to/from region0 stay,
    //      all incoming/outgoing edges to/from region1 redirected to/from clone
    for (i = 0, e = Intersection.size(); i != e; ++i) {
      BasicBlock *I = Intersection[i];
      BasicBlock *newBB = clone(I);
      for (pred_iterator p = pred_begin(I), pe = pred_end(I); p != pe; ++p) {
        BasicBlock *pred = *p;
        if (contains(Blocks1, pred))
          pred->getTerminator()->replaceUsesOfWith(I, newBB);
      }
      for (succ_iterator s = succ_begin(newBB), se = succ_end(newBB); s != se;
           ++s) {
        BasicBlock *succ = *s;
        if (contains(Blocks1, succ)) {
          BasicBlock *newSucc = clone(succ);
          newBB->getTerminator()->replaceUsesOfWith(succ, newSucc);
        }
      }
    }
  }
#endif

  // extract each code region to a new function
  for (ParallelInfo::iterator i = PI.begin(), e = PI.end(); i != e; ++i) {
    ParallelSeq *PS = *i;
    for (ParallelSeq::riterator r = PS->rbegin(), re = PS->rend(); r!=re; ++r) {
      Changed = true;
      ParallelRegion *PR = *r;
      std::vector<BasicBlock*> RegionBlocks(PR->begin(), PR->end());
      if (RegionBlocks.empty()) continue;
      if (containsThreadCall(RegionBlocks[0])) continue;
      ExtractCodeRegion(DS, RegionBlocks, true /* aggregate args */);
      for (std::vector<BasicBlock*>::iterator BBr = RegionBlocks.begin(),
             BBre = RegionBlocks.end(); BBr != BBre; ++BBr)
        PR->removeBasicBlock(*BBr);
    }
  }

  return Changed;
}

bool PSeqExtractor::containsThreadCall(BasicBlock *BB) {
  static const std::string llvmThreadFn("__llvm_thread_start");
  for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i)
    if (CallInst *CI = dyn_cast<CallInst>(i))
      if (CI->getCalledFunction()->getName() == llvmThreadFn)
        return true;

  return false;
}
