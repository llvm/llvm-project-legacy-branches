//===- llvm/Analysis/ParallelInfo.h - Parallel Region Analyzer --*- C++ -*-===//
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

#ifndef LLVM_ANALYSIS_PARALLEL_INFO_H
#define LLVM_ANALYSIS_PARALLEL_INFO_H

#include "llvm/Pass.h"
#include "llvm/Analysis/Dominators.h"
#include <iostream>
#include <vector>

namespace llvm {

class ParallelSeq;

/// ParallelRegion class - Defines a parallel region, one of several that could
/// execute in parallel.
///
class ParallelRegion {
  ParallelSeq *parent;
  std::vector<BasicBlock*> Blocks;

public:
  typedef std::vector<BasicBlock*>::iterator             iterator;
  typedef std::vector<BasicBlock*>::const_iterator const_iterator;
  iterator             begin()       { return Blocks.begin(); }
  iterator               end()       { return Blocks.end();   }
  const_iterator const_begin() const { return Blocks.begin(); }
  const_iterator   const_end() const { return Blocks.end();   }

  ParallelRegion(ParallelSeq *PS) : parent(PS) {}
  ParallelRegion(BasicBlock *BB) : parent(0) { Blocks.push_back(BB); }
  ParallelRegion() : parent(0) {}

  static ParallelRegion* discoverRegion(BasicBlock *pbrBlock,
                                        BasicBlock *begin, BasicBlock *end);
  void addBasicBlock(BasicBlock *BB) { Blocks.push_back(BB); }
  void removeBasicBlock(BasicBlock *BB);
  bool contains(const BasicBlock *BB);

  ParallelSeq *getParent() { return parent; }

  void print(std::ostream &os);
  void dump() { print(std::cerr); }

};


/// ParallelSeq class - Defines a parallel sequence, i.e., where code splits
/// into two threads of control, ending with their synchronization, if it exists
/// before the end of the function.
///
class ParallelSeq { 
  ParallelSeq *ParentSeq;
  BasicBlock *SeqHeader;
  std::vector<ParallelSeq*> ParaSubSeqs; // Other parallel sequences contained
  std::vector<ParallelRegion*> Regions;  // Parallel code regions
  std::vector<BasicBlock*> JoinBlocks;   // Blocks containing "join", if any

  ParallelSeq(ParallelSeq &);                        // DO NOT IMPLEMENT
  const ParallelSeq &operator=(const ParallelSeq &); // DO NOT IMPLEMENT

public:
  /// contains - Return true of the specified basic block is in this sequence
  ///
  bool contains(const BasicBlock *BB) const;
  
  /// iterator/begin/end - Return the seqs contained entirely within this seq.
  ///
  typedef std::vector<ParallelSeq*>::const_iterator iterator;
  iterator begin() const { return ParaSubSeqs.begin(); }
  iterator end() const { return ParaSubSeqs.end(); }

  /// region_iterator/region_begin/region_end - Return the regions contained
  /// within this parallel sequence.
  ///
  typedef std::vector<ParallelRegion*>::const_iterator region_iterator;
  region_iterator region_begin() const { return Regions.begin(); }
  region_iterator region_end()   const { return Regions.end();   }

  /// getJoinBlocks - Return all of the join/synchronization blocks where the
  /// threads of this sequence become one again.
  ///
  const std::vector<BasicBlock*> &getJoinBlocks() const { return JoinBlocks; }

  /// getHeader - Return the header for this parallel sequence
  ///
  BasicBlock *getHeader() const { return SeqHeader; }

  void print(std::ostream &O, unsigned Depth = 0) const;
  void dump() const;

private:
  friend class ParallelInfo;
  inline ParallelSeq(ParallelRegion *PR0, ParallelRegion *PR1) 
    : ParentSeq(0), SeqHeader(0) {
    Regions.push_back(PR0);
    Regions.push_back(PR1);
  }
};


//===----------------------------------------------------------------------===//
/// ParallelInfo - This class builds and contains all of the parallel sequences
/// in the specified function.
///
class ParallelInfo : public FunctionPass {
  std::vector<ParallelSeq*> TopLevelSeqs;
  friend class ParallelSeq;

  /// getAnalysisUsage - Requires dominator sets
  ///
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;

  /// runOnFunction - Calculate the parallel region information.
  ///
  virtual bool runOnFunction(Function &F);

  void print(std::ostream &O) const;

  void Calculate(const DominatorSet &DS);
  ParallelSeq* ConsiderParallelSeq(BasicBlock *BB, const DominatorSet &DS);

public:

  /// iterator/begin/end - The interface to the top-level parallel sequences in
  /// the current function.
  ///
  typedef std::vector<ParallelSeq*>::const_iterator iterator;
  iterator begin() const { return TopLevelSeqs.begin(); }
  iterator end() const { return TopLevelSeqs.end(); }

};

#if 0
// Allow clients to walk the list of nested parallel sequences...
template <> struct GraphTraits<const Loop*> {
  typedef const Loop NodeType;
  typedef std::vector<Loop*>::const_iterator ChildIteratorType;

  static NodeType *getEntryNode(const Loop *L) { return L; }
  static inline ChildIteratorType child_begin(NodeType *N) { 
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) { 
    return N->end();
  }
};

template <> struct GraphTraits<Loop*> {
  typedef Loop NodeType;
  typedef std::vector<Loop*>::const_iterator ChildIteratorType;

  static NodeType *getEntryNode(Loop *L) { return L; }
  static inline ChildIteratorType child_begin(NodeType *N) { 
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) { 
    return N->end();
  }
};
#endif

} // End llvm namespace

#endif
