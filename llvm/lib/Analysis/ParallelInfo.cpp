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
#include "llvm/iPHINode.h"
#include "llvm/iTerminators.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/ParallelInfo.h"
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

ParallelRegion::~ParallelRegion() {
  for (std::vector<ParallelSeq*>::iterator i = Children.begin(),
         e = Children.end(); i != e; ++i)
    (*i)->setParent(0);

  // recursively destroy contained parallel sequences?
}

void ParallelRegion::addChildSeq(ParallelSeq *PS) { 
  Children.push_back(PS); 
  PS->setParent(this);
}

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
  for (const_iterator i = const_begin(), e = const_end(); i != e; ++i)
    os << (*i)->getName() << " ";
}

static bool findPbr(Value *V, ParaBrInst *pbr) {
  if (V == pbr)
    return true;
  else if (PHINode *phi = dyn_cast<PHINode>(V)) {
    bool Found = false;
    for (unsigned i = 0, e = phi->getNumIncomingValues(); i != e; ++i)
      if (findPbr(phi->getIncomingValue(i), pbr))
        return true;
    return false;
  } else
    return false;
}

ParallelRegion* ParallelRegion::discoverRegion(BasicBlock *pbrBlock,
                                               BasicBlock *begin) {
  ParallelRegion *PR = new ParallelRegion();
  
  // accumulate all the basic blocks in the subtree following to the first
  // join worklist algorithm. assumption: no shared code between parallel
  // regions.
  std::vector<BasicBlock*> Worklist;
  DEBUG(std::cerr << "begin: " << begin->getName() << "\n");
  Worklist.push_back(begin);
  ParaBrInst *pbr = dyn_cast<ParaBrInst>(pbrBlock->getTerminator());

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.back();
    DEBUG(std::cerr << "processing: " << BB->getName() << "\n");
    Worklist.pop_back();

    // if this block contains a join to the pbr which we've begun from, then
    // stop processing this block
    bool done = false;
    for (BasicBlock::iterator i = BB->begin(), e = BB->end(); i != e; ++i)
      if (CallInst *CI = dyn_cast<CallInst>(i))
        if (CI->getCalledFunction()->getName() == "llvm.join" &&
            findPbr(CI->getOperand(1), pbr)) {
          done = true;
          break;
        }
    if (done) continue;

    PR->addBasicBlock(BB);
    TerminatorInst *TI = BB->getTerminator();
    // Add all successors to the worklist
    if (BranchInst *BI = dyn_cast<BranchInst>(TI)) {
      for (unsigned i = 0, e = BI->getNumSuccessors(); i != e; ++i) {
        BasicBlock *succ = BI->getSuccessor(i);
        DEBUG(std::cerr << "successor: " << succ->getName() << "\n");
        if (succ != pbrBlock && !PR->contains(succ) &&
            std::find(Worklist.begin(), Worklist.end(), succ) == Worklist.end())
          Worklist.push_back(succ);
      }
    } else if (SwitchInst *SI = dyn_cast<SwitchInst>(TI)) {
      for (unsigned i = 0, e = SI->getNumCases(); i != e; ++i) {
        BasicBlock *succ = SI->getSuccessor(i);
        DEBUG(std::cerr << "successor: " << succ->getName() << "\n");
        if (succ != pbrBlock && !PR->contains(succ) &&
            std::find(Worklist.begin(), Worklist.end(), succ) == Worklist.end())
          Worklist.push_back(succ);
      }
    } else if (ReturnInst *RI = dyn_cast<ReturnInst>(TI)) {
      assert(0 && "return not handled within parallel region");
    } else if (ParaBrInst *PB = dyn_cast<ParaBrInst>(TI)) {
      BasicBlock *pbrBB = PB->getParent();
      ParallelSeq *PS =  
        new ParallelSeq(discoverRegion(pbrBB, PB->getSuccessor(0)),
                        discoverRegion(pbrBB, PB->getSuccessor(1)),
                        pbrBB);
      PR->addChildSeq(PS);
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
  for (const_riterator i = const_rbegin(), e = const_rend(); i != e; ++i)
    if ((*i)->contains(BB))
      return true;
  return false;
}

void ParallelSeq::print(std::ostream &os, unsigned Depth) const {
  unsigned region = 0;
  if (SeqHeader)
    os << "header: " << SeqHeader->getName() << "\n";
  for (const_riterator i = const_rbegin(), e = const_rend(); i != e;
       ++i, ++region) {
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

bool ParallelInfo::runOnFunction(Function &F) {
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
  ParaBrInst *PBr = dyn_cast<ParaBrInst>(BB->getTerminator());
  if (!PBr) return 0;

  // process each successor tree separately into regions, and combine them
  // into a parallel sequence
  ParallelRegion *PR0 =
    ParallelRegion::discoverRegion(PBr->getParent(), PBr->getSuccessor(0));
  DEBUG(std::cerr << "PR0: "; PR0->print(std::cerr); std::cerr << "\n";);
  ParallelRegion *PR1 =
    ParallelRegion::discoverRegion(PBr->getParent(), PBr->getSuccessor(1));
  DEBUG(std::cerr << "PR1: "; PR1->print(std::cerr); std::cerr << "\n";);

  // construct a parallel sequence
  ParallelSeq *PS = new ParallelSeq(PR0, PR1, BB);

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
