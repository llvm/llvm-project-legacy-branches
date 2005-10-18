//===---------------- Writer.h - C backend interface ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file exposes the interface that is common to all C backends.
//
//===----------------------------------------------------------------------===//

#ifndef CWRITER_H
#define CWRITER_H

#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/SymbolTable.h"
#include "llvm/Intrinsics.h"
#include "llvm/Analysis/ConstantsScanner.h"
#include "llvm/Analysis/FindUsedTypes.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Target/TargetMachineRegistry.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/Support/Mangler.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Config/config.h"
#include <algorithm>
#include <iostream>
#include <sstream>

using namespace llvm;

namespace llvm {
  /// NameAllUsedStructs - This pass inserts names for any unnamed structure
  /// types that are used by the program.
  ///
  class CBackendNameAllUsedStructs : public ModulePass {
    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<FindUsedTypes>();
    }

    virtual const char *getPassName() const {
      return "C backend type canonicalizer";
    }

    virtual bool runOnModule(Module &M);
  };

  /// CWriter - This class is the main chunk of code that converts an LLVM
  /// module to a C translation unit.
  class CWriter : public FunctionPass, public InstVisitor<CWriter> {
  protected:
    std::ostream &Out;
    IntrinsicLowering &IL;
    Mangler *Mang;
    LoopInfo *LI;
    const Module *TheModule;
    std::map<const Type *, std::string> TypeNames;

    std::map<const ConstantFP *, unsigned> FPConstantMap;
  public:
    CWriter(std::ostream &o, IntrinsicLowering &il) : Out(o), IL(il) {}

    virtual const char *getPassName() const { return "C backend"; }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<LoopInfo>();
      AU.setPreservesAll();
    }

    virtual bool printDeclarationFor(Function *F);
    virtual bool doInitialization(Module &M);

    bool runOnFunction(Function &F) {
      LI = &getAnalysis<LoopInfo>();

      // Get rid of intrinsics we can't handle.
      lowerIntrinsics(F);

      // Output all floating point constants that cannot be printed accurately.
      printFloatingPointConstants(F);

      // Ensure that no local symbols conflict with global symbols.
      F.renameLocalSymbols();

      printFunction(F);
      FPConstantMap.clear();
      return false;
    }

    virtual bool doFinalization(Module &M) {
      // Free memory...
      delete Mang;
      TypeNames.clear();
      return false;
    }

    std::ostream &printType(std::ostream &Out, const Type *Ty,
                            const std::string &VariableName = "",
                            bool IgnoreName = false);

    // Methods for printing vector types; these can be overridden by
    // target-specific subclasses (AltiVec, SSE, etc.)
    //
    virtual std::ostream &printFixedVectorType(std::ostream &Out,
					       const FixedVectorType *Ty,
					       const std::string &VariableName = "") {
      // FIXME:  Insert lowering so this never happens
      //
      std::cerr << "Backend cannot handle vector types!\n";
      exit(1);
    }

    virtual std::ostream &printVectorType(std::ostream &Out,
					  const VectorType *Ty,
					  const std::string &VariableName = "") {
      // FIXME:  Insert lowering so this never happens
      // 
      std::cerr << "Backend cannot handle vector types!\n";
      exit(1);
    }

    virtual void writeValueName(Value *V);
    void writeOperand(Value *Operand);
    void writeOperandInternal(Value *Operand);

  protected:
    void lowerIntrinsics(Function &F);

    bool nameAllUsedStructureTypes(Module &M);
    void printModule(Module *M);
    void printModuleTypes(const SymbolTable &ST);
    void printContainedStructs(const Type *Ty, std::set<const StructType *> &);
    void printFloatingPointConstants(Function &F);
    void printFunctionSignature(const Function *F, bool Prototype);

    void printFunction(Function &);
    void printBasicBlock(BasicBlock *BB);
    void printLoop(Loop *L);

    virtual void printConstant(Constant *CPV);
    void printConstantArray(ConstantArray *CPA);

    // isInlinableInst - Attempt to inline instructions into their uses to build
    // trees as much as possible.  To do this, we have to consistently decide
    // what is acceptable to inline, so that variable declarations don't get
    // printed and an extra copy of the expr is not emitted.
    //
    static bool isInlinableInst(const Instruction &I) {
      // Always inline setcc instructions, even if they are shared by multiple
      // expressions.  GCC generates horrible code if we don't.
      if (isa<SetCondInst>(I)) return true;

      // Must be an expression, must be used exactly once.  If it is dead, we
      // emit it inline where it would go.
      if (I.getType() == Type::VoidTy || !I.hasOneUse() ||
          isa<TerminatorInst>(I) || isa<CallInst>(I) || isa<PHINode>(I) ||
          isa<LoadInst>(I) || isa<VAArgInst>(I))
        // Don't inline a load across a store or other bad things!
        return false;

      // Only inline instruction if its use is in the same BB as the inst.
      return I.getParent() == cast<Instruction>(I.use_back())->getParent();
   }

    static const bool isSafeUse(const Value *user, const Value *use) {
      if (isa<LoadInst>(user))
	return true;
      if (const StoreInst *SI = dyn_cast<StoreInst>(user))
	return (use == SI->getOperand(1));
      if (isa<CastInst>(user)) {
	return isSafeUser(user);
      }
      return false;
    }

    static const bool isSafeUser(const Value *user) {
      for (User::use_const_iterator I = user->use_begin(),
	     E = user->use_end(); I != E; ++I)
	if (!isSafeUse(*I, user))
	  return false;
      return true;
    }

    // isDirectAlloca - Define fixed sized allocas in the entry block
    // as direct variables which are accessed with the & operator.
    // This causes GCC to generate significantly better code than to
    // emit alloca calls directly.
    //
    static const AllocaInst *isDirectAlloca(const Value *V) {
      const AllocaInst *AI = dyn_cast<AllocaInst>(V);
      if (!AI) return false;
      if (AI->isArrayAllocation())
        return 0;   // FIXME: we can also inline fixed size array allocas!
      if (AI->getParent() != &AI->getParent()->getParent()->getEntryBlock())
	if (!isSafeUser(AI))
	  return 0;
      return AI;
    }

    // Instruction visitation functions
    friend class InstVisitor<CWriter>;

    void visitReturnInst(ReturnInst &I);
    void visitBranchInst(BranchInst &I);
    void visitSwitchInst(SwitchInst &I);
    void visitInvokeInst(InvokeInst &I) {
      assert(0 && "Lowerinvoke pass didn't work!");
    }

    void visitUnwindInst(UnwindInst &I) {
      assert(0 && "Lowerinvoke pass didn't work!");
    }
    void visitUnreachableInst(UnreachableInst &I);

    void visitPHINode(PHINode &I);
    virtual void visitBinaryOperator(Instruction &I);

    virtual void visitCastInst (CastInst &I);
    virtual void visitVImmInst(VImmInst &VL) { visitInstruction(VL); }
    void visitSelectInst(SelectInst &I);
    void visitCallInst (CallInst &I);
    void visitShiftInst(ShiftInst &I) { visitBinaryOperator(I); }

    void visitMallocInst(MallocInst &I);
    void visitAllocaInst(AllocaInst &I);
    void visitFreeInst  (FreeInst   &I);
    void visitLoadInst  (LoadInst   &I);
    void visitStoreInst (StoreInst  &I);
    void visitGetElementPtrInst(GetElementPtrInst &I);
    //void visitVANextInst(VANextInst &I);
    void visitVAArgInst (VAArgInst &I);

    void visitInstruction(Instruction &I) {
      std::cerr << "C Writer does not know about " << I;
      abort();
    }

    void outputLValue(Instruction *I) {
      Out << "  " << Mang->getValueName(I) << " = ";
    }

    bool isGotoCodeNecessary(BasicBlock *From, BasicBlock *To);
    void printPHICopiesForSuccessor(BasicBlock *CurBlock,
                                    BasicBlock *Successor, unsigned Indent);
    void printBranchToBlock(BasicBlock *CurBlock, BasicBlock *SuccBlock,
                            unsigned Indent);
    void printIndexingExpression(Value *Ptr, gep_type_iterator I,
                                 gep_type_iterator E);
  };
}



#endif

