//===- PCallToThreads.cpp - Convert parallel calls to pthreads ------------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// Convert parallel function calls to threaded code.
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

  /// PCallToThreads - 
  ///
  /// FIXME: This should be a Pass, but Passes currently cannot require
  /// FunctionPasses.
  ///
  struct PCallToThreads : public FunctionPass {
    Type *startTy;

  public:
    PCallToThreads() {
      // void*(*start_routine)(void *) is the goal type
      std::vector<const Type*> ArgTypes;
      Type *VoidPtr = PointerType::get(Type::SByteTy);
      ArgTypes.push_back(VoidPtr);
      FunctionType *FT = FunctionType::get(VoidPtr, ArgTypes, false);
      startTy = PointerType::get(FT);
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<ParallelInfo>();
    }
    
    bool runOnFunction(Function &F);

  private:
    Function* getFuncThreadStart(Module &M);
    Function* getFuncThreadJoin(Module &M);
  };

  RegisterOpt<PCallToThreads>
  X("pcall-thread", "Convert parallel calls to thread-based code");

} // End anonymous namespace

/// runOnFunction - 
///
bool PCallToThreads::runOnFunction(Function &F) {
  bool Changed = false;
  ParallelInfo &PI = getAnalysis<ParallelInfo>();

  // Convert parallel calls to pthread_create() invocations
  for (ParallelInfo::iterator i = PI.begin(), e = PI.end(); i != e; ++i) {
    ParallelSeq *PS = *i;
    std::vector<Value*> JoinValues;
    for (ParallelSeq::region_iterator r = PS->region_begin(),
           re = PS->region_end(); r != re; ++r) {
      ParallelRegion *PR = *r;
      std::vector<BasicBlock*> RegionBlocks(PR->begin(), PR->end());

      // Ensure that there is only one block in this region
      assert(RegionBlocks.size() == 1 && "Parallel region has > 1 BB");

      // Within the single block, the only code that should be there is a call
      // and an unconditional branch to the join point
      BasicBlock* BB = RegionBlocks[0];
      BasicBlock::iterator Instrs = BB->begin();
      CallInst *OldCall = dyn_cast<CallInst>(Instrs);
      assert(OldCall && "First instr is not a call!");
      
      // Replace call with __llvm_thread_create
      Function *ThCreate = getFuncThreadStart(*F.getParent());
      assert(OldCall->getNumOperands() == 2 && 
             "Can only threadify calls with one argument!");

      TerminatorInst *TI = BB->getTerminator();
      CastInst *funcPtr = new CastInst(OldCall->getOperand(0), startTy,
                                       "cast_ptr", TI);
      CastInst *funcVal = new CastInst(OldCall->getOperand(1),
                                       PointerType::get(Type::SByteTy),
                                       "cast_val", TI);
      std::vector<Value*> Args;
      Args.push_back(funcPtr);
      Args.push_back(funcVal);
      CallInst *ThCreateCall = new CallInst(ThCreate, Args, "threadCall", TI);

      OldCall->getParent()->getInstList().erase(OldCall);

      Changed = true;
    }

    // Convert llvm.join() intrinsic to __llvm_thread_join() calls
    // Get join function call position/bb
    ParaBrInst *Pbr = dyn_cast<ParaBrInst>(PS->getHeader()->getTerminator());
    assert(Pbr && "Terminator of parallel sequence header is not a Pbr!");
    
    std::vector<User*> Users(Pbr->use_begin(), Pbr->use_end());
    assert(Users.size() == 1 && "Must have unique user of Pbr");
    CallInst *JoinCall = cast<CallInst>(Users[0]);
    assert(JoinCall && "Pbr result used in something other than call!");
    
    Function *ThJoin = getFuncThreadJoin(*F.getParent());
    for (std::vector<Value*>::iterator i = JoinValues.begin(), 
           e = JoinValues.end(); i != e; ++i)
      CallInst *Join = new CallInst(ThJoin, *i, "join", JoinCall);

    if (JoinValues.size() > 0)
      JoinCall->getParent()->getInstList().erase(JoinCall);
  }

  return Changed;
}

/// getFuncThreadStart -
///
Function* PCallToThreads::getFuncThreadStart(Module &M) {
  // int __llvm_thread_create(void*(*)(void*), void*);
  return M.getOrInsertFunction("__llvm_thread_start", Type::IntTy,
                               startTy, PointerType::get(Type::SByteTy), 0);
}

/// getFuncThreadJoin -
///
Function* PCallToThreads::getFuncThreadJoin(Module &M) {
  // void __llvm_thread_join(int);
  return M.getOrInsertFunction("__llvm_thread_join", Type::VoidTy, Type::IntTy, 
                               0);
}
