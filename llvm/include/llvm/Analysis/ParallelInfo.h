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
  std::vector<ParallelSeq*> Children;
  std::vector<BasicBlock*> Blocks;

public:
  ParallelRegion(ParallelSeq *PS) : parent(PS) {}
  ParallelRegion(BasicBlock *BB) : parent(0) { Blocks.push_back(BB); }
  ParallelRegion() : parent(0) {}
  ~ParallelRegion();

  static ParallelRegion* discoverRegion(BasicBlock *pbrBlock,
                                        BasicBlock *begin);

  typedef std::vector<BasicBlock*>::iterator             iterator;
  typedef std::vector<BasicBlock*>::const_iterator const_iterator;
  iterator             begin()       { return Blocks.begin(); }
  iterator               end()       { return Blocks.end();   }
  const_iterator const_begin() const { return Blocks.begin(); }
  const_iterator   const_end() const { return Blocks.end();   }

  typedef std::vector<ParallelSeq*>::iterator             seqiterator;
  typedef std::vector<ParallelSeq*>::const_iterator const_seqiterator;  
  seqiterator             seqbegin()       { return Children.begin(); }
  seqiterator               seqend()       { return Children.end();   }
  const_seqiterator const_seqbegin() const { return Children.begin(); }
  const_seqiterator   const_seqend() const { return Children.end();   }

  void addBasicBlock(BasicBlock *BB) { Blocks.push_back(BB); }
  void removeBasicBlock(BasicBlock *BB);
  bool contains(const BasicBlock *BB);
  void addChildSeq(ParallelSeq *PS);
  std::vector<BasicBlock*> &getBlocks() { return Blocks; }

  ParallelSeq *getParent() { return parent; }

  void print(std::ostream &os);
  void dump() { print(std::cerr); }
};


/// ParallelSeq class - Defines a parallel sequence, i.e., where code splits
/// into two threads of control, ending with their synchronization, if it exists
/// before the end of the function.
///
class ParallelSeq { 
  ParallelRegion *parent;
  BasicBlock *SeqHeader;
  std::vector<ParallelRegion*> Regions;  // Parallel code regions
  std::vector<BasicBlock*> JoinBlocks;   // Blocks containing "join", if any

  ParallelSeq(ParallelSeq &);                        // DO NOT IMPLEMENT
  const ParallelSeq &operator=(const ParallelSeq &); // DO NOT IMPLEMENT

public:
  ParallelSeq(ParallelRegion *PR0, ParallelRegion *PR1, BasicBlock *SeqHd = 0) 
    : parent(0), SeqHeader(SeqHd) {
    Regions.push_back(PR0);
    Regions.push_back(PR1);
  }

  /// contains - Return true of the specified basic block is in this sequence
  ///
  bool contains(const BasicBlock *BB) const;
  
  /// region_iterator/region_begin/region_end - Return the regions contained
  /// within this parallel sequence.
  ///
  typedef std::vector<ParallelRegion*>::iterator             riterator;
  typedef std::vector<ParallelRegion*>::const_iterator const_riterator;
  riterator             rbegin()       { return Regions.begin(); }
  riterator             rend()         { return Regions.end();   }
  const_riterator const_rbegin() const { return Regions.begin(); }
  const_riterator const_rend()   const { return Regions.end();   }

  /// getJoinBlocks - Return all of the join/synchronization blocks where the
  /// threads of this sequence become one again.
  ///
  const std::vector<BasicBlock*> &getJoinBlocks() const { return JoinBlocks; }

  /// getHeader - Return the header for this parallel sequence
  ///
  BasicBlock *getHeader() const { return SeqHeader; }

  void setParent(ParallelRegion *PR) { parent = PR; }

  void print(std::ostream &O, unsigned Depth = 0) const;
  void dump() const;

private:
  friend class ParallelInfo;
};


//===----------------------------------------------------------------------===//
/// ParallelInfo - This class builds and contains all of the parallel sequences
/// in the specified function.
///
class ParallelInfo : public FunctionPass {
  std::vector<ParallelSeq*> TopLevelSeqs;
  friend class ParallelSeq;

  void print(std::ostream &O) const;

  ParallelSeq* ConsiderParallelSeq(BasicBlock *BB, const DominatorSet &DS);

public:

  /// runOnFunction - Calculate the parallel region information.
  ///
  virtual bool runOnFunction(Function &F);

  /// getAnalysisUsage - Requires dominator sets
  ///
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;

  void Calculate(const DominatorSet &DS);

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
