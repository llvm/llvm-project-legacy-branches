//===- ParallelInfo.cpp - Parallel Sequence Calculator --------------------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file defines the ParallelInfo class that is used to identify parallel
// code blocks and code sequences within the CFG. It should make transformations
// and parallel code generation easier.
//
//===----------------------------------------------------------------------===//

#include "llvm/iOther.h"
#include "llvm/iTerminators.h"
#include "llvm/Analysis/ParallelInfo.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/CFG.h"
#include "Support/Debug.h"
#include "Support/DepthFirstIterator.h"
#include "Support/StringExtras.h"
#include <algorithm>
#include <vector>

namespace llvm {

static RegisterAnalysis<ParallelInfo>
X("paraseq", "Parallel Sequence Construction", true);

//===----------------------------------------------------------------------===//
// ParallelRegion implementation
//

bool ParallelRegion::contains(const BasicBlock *BB) {
  return std::find(Blocks.begin(), Blocks.end(), BB) != Blocks.end();
}

void ParallelRegion::removeBasicBlock(BasicBlock *BB) {
  iterator BBe = Blocks.end();
  iterator BBi = std::find(Blocks.begin(), BBe, BB);
  assert(BBi != BBe && "Attempting to delete block not in region!");
  Blocks.erase(BBi);
}

void ParallelRegion::print(std::ostream &os) {
  for (const_iterator i = const_begin(), e = const_end(); i != e; ++i) {
    os << (*i)->getName() << " ";
  }
}

ParallelRegion* ParallelRegion::discoverRegion(BasicBlock *pbrBlock,
                                               BasicBlock *begin,
                                               BasicBlock *end) {
  ParallelRegion *PR = new ParallelRegion();
  
  // accumulate all the basic blocks in the subtree following to the first
  // join worklist algorithm. assumption: no shared code between parallel
  // regions.
  std::vector<BasicBlock*> Worklist;
  DEBUG(std::cerr << "begin: " << begin->getName() << "\n");
  Worklist.push_back(begin);

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.back();
    DEBUG(std::cerr << "processing: " << BB->getName() << "\n");
    Worklist.pop_back();
    PR->addBasicBlock(BB);

    TerminatorInst *TI = BB->getTerminator();
    // Add all successors to the worklist
    if (BranchInst *BI = dyn_cast<BranchInst>(TI)) {
      for (unsigned i = 0, e = BI->getNumSuccessors(); i != e; ++i) {
        BasicBlock *succ = BI->getSuccessor(i);
        DEBUG(std::cerr << "successor: " << succ->getName() << "\n");
        if (succ != pbrBlock && succ != end && !PR->contains(succ) &&
            std::find(Worklist.begin(), Worklist.end(), succ) == Worklist.end())
          Worklist.push_back(succ);
      }
    } else if (SwitchInst *SI = dyn_cast<SwitchInst>(TI)) {
      assert(0 && "switch not handled within parallel region");
    } else if (ReturnInst *RI = dyn_cast<ReturnInst>(TI)) {
      assert(0 && "return not handled within parallel region");
    } else if (ParaBrInst *PB = dyn_cast<ParaBrInst>(TI)) {
      assert(0 && "pbr not handled within parallel region");
    } else {
      assert(0 && "<unknown terminator> not handled within parallel region");
    }
  } 

  return PR;
}


//===----------------------------------------------------------------------===//
// ParallelSeq implementation
//

/// contains - Return true of the specified basic block is in this loop
///
bool ParallelSeq::contains(const BasicBlock *BB) const {
  for (region_iterator i = region_begin(), e = region_end(); i != e; ++i)
    if ((*i)->contains(BB))
      return true;
  return false;
}

void ParallelSeq::print(std::ostream &os, unsigned Depth) const {
  unsigned region = 0;
  if (SeqHeader)
    os << "header: " << SeqHeader->getName() << "\n";
  for (region_iterator i = region_begin(), e = region_end(); i != e;
       ++i, ++region)
  {
    os << utostr(region) << " ";
    (*i)->print(os);
    os << "\n";
  }
}

void ParallelSeq::dump() const {
  print(std::cerr);
}


//===----------------------------------------------------------------------===//
// ParallelInfo implementation
//

bool ParallelInfo::runOnFunction(Function &) {
  Calculate(getAnalysis<DominatorSet>());
  return false;
}

void ParallelInfo::Calculate(const DominatorSet &DS) {
  BasicBlock *RootNode = DS.getRoot();

  for (df_iterator<BasicBlock*> NI = df_begin(RootNode),
	 NE = df_end(RootNode); NI != NE; ++NI)
    if (ParallelSeq *PS = ConsiderParallelSeq(*NI, DS))
      TopLevelSeqs.push_back(PS);
}

ParallelSeq*
ParallelInfo::ConsiderParallelSeq(BasicBlock *BB, const DominatorSet &DS) {
  // if this block has a pbr
  bool bbHasPbr = false;
  ParaBrInst *PBr = 0;
  for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i) {
    if ((PBr = dyn_cast<ParaBrInst>(i))) {
      bbHasPbr = true;
      break;
    }
  }

  if (!bbHasPbr) return 0;

  // process each successor tree separately into regions, and combine them
  // into a parallel sequence

  // The last instruction of the pbr BasicBlock is the pbr that spawned the
  // regionStart, so find the join instruction correlated to it and hence the
  // end basic block for the region.
  Value::use_iterator join = PBr->use_begin();
  CallInst *call = dyn_cast<CallInst>(*join);
  // FIXME: make sure the call is to "llvm.join" intrinsic
  assert(call && "Value returned by pbr used in something not a call to join!");
  BasicBlock *end = call->getParent();

  ParallelRegion *PR0 =
    ParallelRegion::discoverRegion(PBr->getParent(), PBr->getSuccessor(0), end);
  DEBUG(std::cerr << "PR0: "; PR0->print(std::cerr); std::cerr << "\n";);
  ParallelRegion *PR1 =
    ParallelRegion::discoverRegion(PBr->getParent(), PBr->getSuccessor(1), end);
  DEBUG(std::cerr << "PR1: "; PR1->print(std::cerr); std::cerr << "\n";);

  // construct a parallel sequence
  ParallelSeq *PS = new ParallelSeq(PR0, PR1);

  return PS;
}
  

void ParallelInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<DominatorSet>();
}

void ParallelInfo::print(std::ostream &OS) const {
  for (unsigned i = 0; i < TopLevelSeqs.size(); ++i)
    TopLevelSeqs[i]->print(OS);
}

} // End llvm namespace
