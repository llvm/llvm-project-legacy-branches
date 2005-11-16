//===- LowerVectors.cpp - Lower vector operations -------------------------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file lowers vector operations (such as vgather, vscatter, and
// vector arithmetic) to iterated scalar operations.  This pass does
// NOT generate efficient code; it is intended for testing and
// debugging of Vector-LLVM.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "lowervectors"

#include <sstream>

#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Type.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/hash_map"
#include "llvm/ADT/hash_set"
#include "llvm/Transforms/Scalar.h"
#include "VectorLLVM/Utils.h"

using namespace llvm;

namespace {

  //===----------------------------------------------------------------------===//
  //                          Class definitions
  //===----------------------------------------------------------------------===//

  class LowerVectors : public FunctionPass, public InstVisitor<LowerVectors> {

  public:
    bool doInitialization(Module &M);
    bool runOnFunction(Function &F);
    void visitVGatherInst(VGatherInst&);
    void visitVScatterInst(VScatterInst&);
    void visitCastInst(CastInst&);
    void visitBinaryOperator(BinaryOperator&);
    void visitVImmInst(VImmInst&);
    void visitShiftInst(ShiftInst&);
    void visitVSelectInst(VSelectInst&);
    void visitExtractInst(ExtractInst&);
    void visitCombineInst(CombineInst&);
    void visitExtractElementInst(ExtractElementInst&);
    void visitCombineElementInst(CombineElementInst&);
    void visitPHINode(PHINode&);
    void visitMallocInst(MallocInst&);
    void visitFreeInst(FreeInst&);
    void visitStoreInst(StoreInst&);
    void visitLoadInst(LoadInst&);
    void visitGetElementPtrInst(GetElementPtrInst&);
    void visitInstruction(Instruction& I) {
      std::cerr << "LowerVectors class can't handle instruction " << I << "!\n";
      exit(1);
    }

    static Function *ReallocFunc;

  private:
    void lowerInstruction(Instruction&);
    //void lowerInstructionToLoop(Instruction*,Value*);
  }; 

  RegisterOpt<LowerVectors> X("lowervectors", 
			      "Lower vector operations to iterated scalar operations");

  class VMemoryInstLowering {
  protected:
    BasicBlock *constructLoop(VMemoryInst*,BasicBlock*,std::vector<Value*>,Value*);
    void constructInnerLoop(VMemoryInst*,BasicBlock*,BasicBlock*,BasicBlock*,
			    BasicBlock*,std::vector<Value*>,Value*);
    virtual void constructLoopBody(VMemoryInst*,BasicBlock*,
				   std::vector<Value*>,Value*) = 0;
  };

  class VGatherLowering : public VMemoryInstLowering {
  public:
    VGatherLowering(VGatherInst *VL) { lowerVGather(VL); }
    void lowerVGather(VGatherInst*);

  private:
    void constructLoopBody(VMemoryInst*,BasicBlock*,
			   std::vector<Value*>,Value*);
  };

  class VScatterLowering : public VMemoryInstLowering {
  public:
    VScatterLowering(VScatterInst *VS) { lowerVScatter(VS); }
    void lowerVScatter(VScatterInst*);

  private:
    void constructLoopBody(VMemoryInst*,BasicBlock*,
			   std::vector<Value*>,Value*);
  };

  class InstructionLowering : public InstVisitor<InstructionLowering> {
    friend class LowerVectors;

    Value *vector;
    Value *result, *vectorIndex;
    std::vector<Value*> idx;
    BasicBlock *body;

  public:
    InstructionLowering(Instruction *I, Value *length, Value *ptr=0) 
    { lowerInstruction (I, length, ptr); }
    void lowerInstruction(Instruction*,Value*,Value*);
    void visitBinaryOperator(BinaryOperator&);
    void visitVSelectInst(VSelectInst&);
    void visitCastInst(CastInst&);
    void visitShiftInst(ShiftInst&);
    void visitExtractInst(ExtractInst&);
    void visitCombineInst(CombineInst&);
    void visitVImmInst(VImmInst&);
    void visitLoadInst(LoadInst&);
    void visitStoreInst(StoreInst&);
    void visitInstruction(Instruction& I) {
      std::cerr << "InstructionLowering class can't handle instruction " << I << "!\n";
      exit(1);
    }
  };


  //===----------------------------------------------------------------------===//
  //                        Global data for this module
  //===----------------------------------------------------------------------===//

  /// Map from each vector value to the in-memory array that has been
  /// allocated to hold the vector.
  ///
  hash_map<Value*,Value*> loweringMap;

  /// Map from each vector value to its length.  If a vector value
  /// does not appear in this map, then it has not yet been lowered.
  ///
  hash_map<Value*,Value*> lengthMap;


  //===----------------------------------------------------------------------===//
  //                     Helper functions for managing vectors
  //===----------------------------------------------------------------------===//

  /// Initialize all {T*,uint} instances in a lowered type
  /// corresponding to a [vector of T] that would be created by a
  /// malloc or alloca with nulls
  ///
  void initializeVectors(Value *ptr, const Type* originalAllocatedTy,
			 Instruction *before) {
    if (isa<VectorType>(originalAllocatedTy)) {
      for (unsigned i = 0; i < 2; ++i) {
	std::vector<Value*> Idx;
	Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
	Idx.push_back(ConstantUInt::get(Type::UIntTy, i));
	GetElementPtrInst *GEP =
	  new GetElementPtrInst(ptr, Idx, "gep", before);
	const PointerType *pointerTy = cast<PointerType>(GEP->getType());
	StoreInst *store =
	  new StoreInst(Constant::getNullValue(pointerTy->getElementType()),
			GEP, before);
      }
    } else if (isa<PointerType>(originalAllocatedTy)) {
      return;
    } else {
      std::cerr << "Can't yet handle this type!\n";
      exit(1);
    }
  }

  /// Initialize all {T*,uint} instances in a lowered type
  /// corresponding to a [vector of T] that would be created by a
  /// malloc or alloca with nulls
  ///
  void freeVectors(Value *ptr, const Type* originalAllocatedTy,
		   Instruction *before) {
    if (isa<VectorType>(originalAllocatedTy)) {
	std::vector<Value*> Idx;
	Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
	Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
	GetElementPtrInst *GEP =
	  new GetElementPtrInst(ptr, Idx, "gep", before);
	LoadInst *load =
	  new LoadInst(GEP, "load", before);
	FreeInst *free =
	  new FreeInst(load, before);
    } else if (isa<PointerType>(originalAllocatedTy)) {
      return;
    } else {
      std::cerr << "Can't yet handle this type!\n";
      exit(1);
    }
  }

  /// Add the specified <vector, length> pair to the lengthMap,
  /// replacing dummy uses if necessary
  ///
  void setLength(Value *key, Value *newValue) {
    Value*& oldValue = lengthMap[key];
    if (oldValue) {
      oldValue->replaceAllUsesWith(newValue);
      delete oldValue;
    }
    oldValue = newValue;
  }

  /// Given a vector type value, look in the lengthMap to get its length.
  ///
  Value *getLength(Value *key) {
    if (const FixedVectorType *VT = dyn_cast<FixedVectorType>(key->getType()))
      return ConstantUInt::get(Type::UIntTy, VT->getNumElements());
    Value*& V = lengthMap[key];
    if (!V) {
      V = new Argument(Type::UIntTy);
      DEBUG(std::cerr << "Creating dummy length " << *V << "\n");
    }
    return V;
  }

  /// Add the specified pair to the loweringMap, replacing dummy uses
  /// if necessary
  ///
  void setLoweredValue(Value *key, Value *newValue, Value *length = 0) {
    Value*& oldValue = loweringMap[key];
    if (oldValue) {
      oldValue->replaceAllUsesWith(newValue);
      delete oldValue;
    }
    oldValue = newValue;
    if (length)
      setLength(key, length);
  }
  
  /// Lower an arbitary type of a pointer or an object stored in
  /// memory.  Lower a vector-derived type by replacing all instances
  /// of [vector of T] with {T*,uint}, with some special rules for
  /// vector-derived function types.  "Lower" a non-vector-derived
  /// type to the same type.
  ///
  const Type* getLoweredMemoryType(const Type* Ty) {
    if (const FixedVectorType *VT = dyn_cast<FixedVectorType>(Ty)) {
      return VT->getElementType();
    }
    if (const VectorType *VectorTy = dyn_cast<VectorType>(Ty)) {
      std::vector<const Type*> Params;
      Params.push_back(PointerType::get(VectorTy->getElementType()));
      Params.push_back(Type::UIntTy);
      return StructType::get(Params);
    } else if (Ty->isPrimitiveType()) {
      return Ty;
    } else if (const PointerType *PT = dyn_cast<PointerType>(Ty)) {
      return PointerType::get(getLoweredMemoryType(PT->getElementType()));
    }
    return Ty; // For now
  }

  /// Lower the type of a first-class object stored in a virtual
  /// register.  Lower [vector of T] to T*, but lower [vector of T]*
  /// to {T*,uint}*.  "Lower" a non-vector-derived type to the same
  /// type.
  ///
  const Type* getLoweredRegisterType(const Type* Ty) {
    assert (Ty->isFirstClassType() &&
	    "getLoweredRegisterType() should be called only on first-class types!");
    if (const VectorType *VectorTy = dyn_cast<VectorType>(Ty)) {
      return PointerType::get(VectorTy->getElementType());
    } else if (isa<PointerType>(Ty)) {
      return getLoweredMemoryType(Ty);
    }
    return Ty;
  }

  /// Given a value, return the corresponding lowered value.
  /// Otherwise, look in the loweringMap to get the corresponding
  /// scalar value.  If there is no corresponding value, we create a
  /// dummy value that will be filled in later when the operand is
  /// lowered.
  ///
  Value *getLoweredValue(Value *key) {
    if (!VectorUtils::containsVector(key->getType()) &&
	!isa<VScatterInst>(key)) {
      return key;
    }
    Value*& value = loweringMap[key];
    if (!value) {
      value = new Argument(getLoweredRegisterType(key->getType()));
      DEBUG(std::cerr << "Creating dummy lowered value " << *value << "\n");
    }
    return value;
  }

  /// Get a single lowered operand of an instruction.  Each operand is
  /// a value loaded from the specified position of the array that
  /// stores the vector elements.
  ///
  Value* getOp(Instruction* I, unsigned n, std::vector<Value*>& idx, 
	       Instruction *insertBefore) {
    Value *op = I->getOperand(n);
    Value *loweredOp = getLoweredValue(op);
    Value *ptr = new GetElementPtrInst(loweredOp, idx, "ptr",
				       insertBefore);
    return new LoadInst(ptr, "load", insertBefore);
  }

  /// Get the first n lowered operands of an instruction.  
  ///
  void getOps(Instruction* I, Value* ops[], unsigned n, 
	      std::vector<Value*>& idx, Instruction *insertBefore) {
    for (unsigned i = 0; i < n; ++i)
      ops[i] = getOp(I, i, idx, insertBefore);
  }

  /// Allocate a vector.  We use realloc in case the allocation
  /// happens inside a loop; because LLVM is in SSA form, this is
  /// guaranteed to be correct.  For each possible allocation, we
  /// store a null pointer on the stack.  When the allocation is
  /// actually done, we store the pointer there.  At each exit point
  /// of the function, for each allocation, we call free on the
  /// (possibly null) pointer.
  ///
  /// In the case of a store instruction, we use the
  /// previously-allocated pointer, so we don't need to create a new
  /// pointer, and we don't need to add the free instruction.
  ///
  Instruction *allocateVector(const Type* Ty, Value* ArraySize, 
			      const std::string& Name, Instruction *InsertBefore,
			      Value *ptr=0) {
    Function *function = InsertBefore->getParent()->getParent();
    Instruction *front = &(function->getEntryBlock().front());
    const Type *PointerTy = PointerType::get(Ty);

    Value *loadStorePtr = ptr;
    if (!ptr) {
      loadStorePtr = new AllocaInst(PointerType::get(Ty), 0, "loadstore_ptr", front);
      new StoreInst(Constant::getNullValue(PointerType::get(Ty)), loadStorePtr, front);
    }
		  
    const FunctionType *ReallocFTy = LowerVectors::ReallocFunc->getFunctionType();
    
    // Create the vector of arguments to realloc
    //
    Value *reallocPtr = new LoadInst(loadStorePtr, "realloc_ptr", InsertBefore);
    if (reallocPtr->getType() != ReallocFTy->getParamType(0)) {
      reallocPtr = new CastInst(reallocPtr, ReallocFTy->getParamType(0), "cast", InsertBefore);
    }

    Value *reallocSize = 
      BinaryOperator::create(Instruction::Mul, ArraySize,
			     ConstantUInt::get(Type::UIntTy,
					       Ty->getPrimitiveSize()),
			     "size", InsertBefore);
    if (reallocSize->getType() != ReallocFTy->getParamType(1)) {
      reallocSize = new CastInst(reallocSize, ReallocFTy->getParamType(1), 
				 "cast", InsertBefore);
    }

    std::vector<Value*> ReallocArgs;
    ReallocArgs.push_back(reallocPtr);
    ReallocArgs.push_back(reallocSize);

    // Create the call to realloc
    //
    CallInst *call = new CallInst(LowerVectors::ReallocFunc, 
				  ReallocArgs, Name, InsertBefore);

    if (!ptr) {
      // Insert a free instruction before every return
      //
      for (Function::iterator FI = function->begin(), FE = function->end(); 
	   FI != FE; ++FI) {
	if (ReturnInst *RI = dyn_cast<ReturnInst>(FI->getTerminator())) {
	  LoadInst *freePtr = new LoadInst(loadStorePtr, "free_ptr", RI);
	  new FreeInst(freePtr, RI);
	}
      }
    }
      
    // Create a cast instruction if necessary to convert to the right
    // type
    //
    Instruction *result = call;
    if (call->getType() != PointerTy)
      result = new CastInst(call, PointerTy, "cast", InsertBefore);
    
    // Store the pointer to the allocated memory in the load-store pointer
    //
    new StoreInst(result, loadStorePtr, InsertBefore);

    return result;
  } 

  /// Test whether a given instruction is a vector instruction that we
  /// need to lower
  ///
  bool isVectorInstruction(Instruction *I) {
    if (isa<VMemoryInst>(I)) return true;
    if (isa<FreeInst>(I) || isa<StoreInst>(I))
      return (getLoweredMemoryType(I->getOperand(0)->getType()) 
	      != I->getOperand(0)->getType());
    return getLoweredMemoryType(I->getType()) != I->getType();
  }
      

  //===----------------------------------------------------------------------===//
  //                     LowerVectors implementation
  //===----------------------------------------------------------------------===//

  Function *LowerVectors::ReallocFunc = 0;

  bool LowerVectors::doInitialization(Module &M) {
    const Type *SBPTy = PointerType::get(Type::SByteTy);
    ReallocFunc = M.getNamedFunction("realloc");

    if (ReallocFunc == 0)
      ReallocFunc = M.getOrInsertFunction("realloc", SBPTy, SBPTy, Type::UIntTy, 0);

    return false;
  }

  bool LowerVectors::runOnFunction(Function &F) {
    DEBUG(std::cerr << "\nrunOnFunction(" << F.getName() << ")\n");
    
    std::vector<Instruction*> instList;
    loweringMap.clear();
    lengthMap.clear();
    
    // Find all vector instructions and push them on a list
    //
    for (Function::iterator FI = F.begin(), FE = F.end(); 
	 FI != FE; ++FI)
      for (BasicBlock::iterator BI = FI->begin(), BE = FI->end(); 
	   BI != BE; ++BI)
	//if (isVectorInstruction(BI)) {
	if (VectorUtils::containsVector(BI)) {
	  instList.push_back(BI);
	}

    // Lower each one
    //
    for (std::vector<Instruction*>::const_iterator I = instList.begin();
	 I != instList.end(); ++I) {
	lowerInstruction(**I);
    }

    // Remove each one
    //
    for (std::vector<Instruction*>::const_iterator I = instList.begin();
	 I != instList.end(); ++I) {
      (*I)->dropAllReferences();
    }
    
    for (std::vector<Instruction*>::const_iterator I = instList.begin();
	 I != instList.end(); ++I) {
      DEBUG(std::cerr << "Removing instruction " << **I);
      (*I)->getParent()->getInstList().erase(*I);
    }

    // Program was changed iff we processed something on the list
    //
    return instList.size() > 0;
  }

  /// Lower a single instruction
  ///
  void LowerVectors::lowerInstruction(Instruction &I) {

    // Check the operands
    //
    for (User::const_op_iterator OI = I.op_begin(), 
	   OE = I.op_end(); OI != OE; ++OI) {
      if (isa<VectorType>((*OI)->getType())) {
	if (!isa<Instruction>(*OI))
	  // For a correct Vector-LLVM program, this should never
	  // occur
	  //
	  assert(0 && "All operands of vector instructions must be instructions for this pass to work!");
      }
    }
    
    // Now process the instruction itself
    //
    DEBUG(std::cerr << "Lowering instruction " << I);
    visit(I);

  }

  void LowerVectors::visitVGatherInst(VGatherInst &VL) {
    VGatherLowering lower(&VL);
  }

  void LowerVectors::visitVScatterInst(VScatterInst &VS) {
    VScatterLowering lower(&VS);
  }    

  void LowerVectors::visitVImmInst(VImmInst &VL) {
    InstructionLowering lower(&VL, VL.getOperand(1));
  }

  void LowerVectors::visitCastInst(CastInst &CI) {
    if (isa<PointerType>(CI.getType())) {
      CastInst *Cast = 
	new CastInst(getLoweredValue(CI.getOperand(0)),
		     getLoweredMemoryType(CI.getType()),
		     "cast", &CI);
      setLoweredValue(&CI, Cast);
    } else {
      InstructionLowering lower(&CI, getLength(CI.getOperand(0)));
    }
  }

  void LowerVectors::visitGetElementPtrInst(GetElementPtrInst &GEP) {
    std::vector<Value*> idx;
    for (User::op_iterator I = GEP.idx_begin(), E = GEP.idx_end();
	 I != E; ++I) {
      idx.push_back(*I);
    }
    GetElementPtrInst *NewGEP = 
      new GetElementPtrInst(getLoweredValue(GEP.getPointerOperand()),
			    idx, "gep", &GEP);
    setLoweredValue(&GEP, NewGEP);
  }

  void LowerVectors::visitBinaryOperator(BinaryOperator &BO) {
    Value *op0 = BO.getOperand(0);
    Value *op1 = BO.getOperand(1);
    Value *length0 = getLength(op0);
    Value *length1 = getLength(op1);
    VectorUtils::ensureEquality(&BO, length0, length1);
    InstructionLowering lower(&BO, length0);
  }

  void LowerVectors::visitShiftInst(ShiftInst &SI) {
    Value *op0 = SI.getOperand(0);
    Value *length = getLength(op0);
    InstructionLowering lower(&SI, length);
  }

  void LowerVectors::visitVSelectInst(VSelectInst &SI) {
    Value *op0 = SI.getOperand(0);
    Value *op1 = SI.getOperand(1);
    Value *op2 = SI.getOperand(2);
    Value *loweredOp0 = getLoweredValue(op0);
    Value *loweredOp1 = getLoweredValue(op1);
    Value *loweredOp2 = getLoweredValue(op2);
    Value *length0 = getLength(op0);
    Value *length1 = getLength(op1);
    Value *length2 = getLength(op2);
    VectorUtils::ensureEquality(&SI, length0, length1);
    VectorUtils::ensureEquality(&SI, length1, length2);
    InstructionLowering lower(&SI, length0);
  }

  void LowerVectors::visitExtractInst(ExtractInst &EI) {
    Value *length = EI.getOperand(3);
    InstructionLowering lower(&EI, length);
  }

  void LowerVectors::visitCombineInst(CombineInst &CI) {
    Value *length = getLength(CI.getOperand(0));
    InstructionLowering lower(&CI, length);
  }

  void LowerVectors::visitExtractElementInst(ExtractElementInst &EI) {
    std::vector<Value*> idx;
    idx.push_back(EI.getOperand(1));
    Value *element = getOp(&EI, 0, idx, &EI);
    EI.replaceAllUsesWith(element);
  }

  void LowerVectors::visitCombineElementInst(CombineElementInst &CI) {
    Value *base = getLoweredValue(CI.getOperand(0));
    Value *element = CI.getOperand(1);
    std::vector<Value*> idx;
    idx.push_back(CI.getOperand(2));
    Value *ptr = new GetElementPtrInst(base, idx, "ptr", &CI);
    // According to the relaxed semantics, this is correct
    //
    new StoreInst(element, ptr, &CI);
    setLoweredValue(&CI, base, getLength(CI.getOperand(0)));
  }

  void LowerVectors::visitPHINode(PHINode &PN) {
    const VectorType *Ty = dyn_cast<VectorType>(PN.getType());
    assert(Ty && "Instruction must be of vector type!");
    PHINode *vectorPHI = 
      new PHINode(getLoweredRegisterType(PN.getIncomingValue(0)->getType()),
		  "phi", &PN);
    PHINode *lengthPHI = new PHINode(Type::UIntTy, "phi", vectorPHI);
    for(unsigned i = 0, e = PN.getNumIncomingValues();
	i < e; ++i) {
      Value *V = PN.getIncomingValue(i);
      BasicBlock *BB = PN.getIncomingBlock(i);
      vectorPHI->addIncoming(getLoweredValue(V), BB);
      lengthPHI->addIncoming(getLength(V), BB);
    }
    setLoweredValue(&PN, vectorPHI, lengthPHI);
  }

  void LowerVectors::visitMallocInst(MallocInst &MI) {
    const Type* Ty = MI.getAllocatedType();
    const Type* loweredTy = getLoweredMemoryType(Ty);
    MallocInst *malloc = 
      new MallocInst(loweredTy, MI.getArraySize(), "malloc", &MI);
    initializeVectors(malloc, Ty, &MI);
    setLoweredValue(&MI, malloc);
  }

  void LowerVectors::visitFreeInst(FreeInst &FI) {
    Value *ptr = FI.getOperand(0);
    Value *loweredPtr = getLoweredValue(ptr);
    const Type* originalAllocatedTy =
      cast<PointerType>(ptr->getType())->getElementType();
    freeVectors(loweredPtr, originalAllocatedTy, &FI);
    FreeInst *free =
      new FreeInst(loweredPtr, &FI);
    setLoweredValue(&FI, free);
  }

  void LowerVectors::visitStoreInst(StoreInst &SI) {
    const Type *Ty = SI.getOperand(0)->getType();
    if (const FixedVectorType *VT = dyn_cast<FixedVectorType>(SI.getOperand(0)->getType())) {
      Value *length = getLength(SI.getOperand(0));//ConstantUInt::get(Type::UIntTy, VT->getNumElements());
      Value *ptr = getLoweredValue(SI.getOperand(1));
      InstructionLowering lower(&SI, length, ptr);
    } else if (isa<VectorType>(SI.getOperand(0)->getType())) {
      std::vector<Value*> Idx;
      Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
      Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
      Value *ptr = getLoweredValue(SI.getOperand(1));
      GetElementPtrInst *GEP =
	new GetElementPtrInst(ptr, Idx, "gep", &SI);
      InstructionLowering lower(&SI, getLength(SI.getOperand(0)), GEP);
      Idx.pop_back();
      Idx.push_back(ConstantUInt::get(Type::UIntTy, 1));
      GEP = new GetElementPtrInst(ptr, Idx, "gep", &SI);
      new StoreInst(getLength(SI.getOperand(0)), GEP, &SI);
    } else {
      Value *val = getLoweredValue(SI.getOperand(0));
      Value *ptr = getLoweredValue(SI.getOperand(1));
      StoreInst *store =
	new StoreInst(val, ptr, &SI);
      setLoweredValue(&SI, store);
    }
  }

  void LowerVectors::visitLoadInst(LoadInst &LI) {
    const Type *Ty = cast<PointerType>(LI.getOperand(0)->getType())->getElementType();
    if (isa<FixedVectorType>(Ty)) {
      setLoweredValue(&LI, getLoweredValue(LI.getOperand(0)));
    } else if (isa<VectorType>(Ty)) {
      std::vector<Value*> Idx;
      Idx.push_back(ConstantUInt::get(Type::UIntTy, 0));
      Idx.push_back(ConstantUInt::get(Type::UIntTy, 1));
      Value *ptr = getLoweredValue(LI.getOperand(0));
      GetElementPtrInst *GEP =
	new GetElementPtrInst(ptr, Idx, "gep", &LI);
      LoadInst *load =
	new LoadInst(GEP, "load", &LI);
      InstructionLowering lower(&LI, load);
    } else {
      LoadInst *load =
	new LoadInst(getLoweredValue(LI.getOperand(0)), "load", &LI);
      setLoweredValue(&LI, load);
    }
  }


  //===----------------------------------------------------------------------===//
  //                     InstructionLowering implementation
  //===----------------------------------------------------------------------===//

  /// Lower an instruction to a loop
  ///
  void InstructionLowering::lowerInstruction(Instruction *I, Value *length,
					     Value *ptr = 0) {
    const Type *Ty = 
      isa<StoreInst>(I) ? I->getOperand(0)->getType() : I->getType();
    const VectorType *VectorTy = dyn_cast<VectorType>(Ty);
    assert(VectorTy && "Instruction must be of vector type!");
    const Type *elementType = VectorTy->getElementType();
    if (isa<FixedVectorType>(Ty)) {
      if (isa<StoreInst>(I)) {
	vector = ptr;
      } else {
	vector = new AllocaInst(VectorTy->getElementType(), length, "vector", 
				&(I->getParent()->getParent()->getEntryBlock().front()));
      }
    } else {
      vector = allocateVector(elementType, length, "vector", I, ptr);
    }
    setLoweredValue(I, vector, length);

    // Set up the loop index
    //
    Instruction *vectorIndexPtr = 
      new AllocaInst(Type::UIntTy, ConstantUInt::get(Type::UIntTy, 1),
		     vector->getName() + ".index", I);
    new StoreInst(Constant::getNullValue(Type::UIntTy), vectorIndexPtr, I);

    // Create the basic blocks of the loop
    //
    BasicBlock *predecessor = I->getParent();
    BasicBlock *header = 
      I->getParent()->splitBasicBlock(I, "loop_header");
    body = 
      header->splitBasicBlock(I, "loop_body");
    BasicBlock *exit = 
      body->splitBasicBlock(I, "loop_exit");

    // Add the correct branch instructions
    //
    body->getTerminator()->setSuccessor(0, header);
    vectorIndex = new LoadInst(vectorIndexPtr, "index", 
				      header->getTerminator());
    Instruction *cond = 
      BinaryOperator::create(Instruction::SetLT, vectorIndex, length,
			     "setlt", header->getTerminator());
    BasicBlock::iterator oldInst(header->getTerminator());
    Instruction *newInst = new BranchInst(body, exit, cond);
    ReplaceInstWithInst(header->getInstList(), oldInst, newInst);

    // Perform the operation in the body of the loop
    //
    idx.clear();
    idx.push_back(vectorIndex);
    visit(I);
    Instruction *GEP = new GetElementPtrInst(vector, idx, "GEP", 
					     body->getTerminator());
    new StoreInst(result, GEP, body->getTerminator());

    // Increment the loop counter
    //
    Value *incr = BinaryOperator::create(Instruction::Add, vectorIndex, 
					 ConstantUInt::get(Type::UIntTy, 1),
					 "incr", body->getTerminator());
    new StoreInst(incr, vectorIndexPtr, body->getTerminator());
  }

  void InstructionLowering::visitBinaryOperator(BinaryOperator &BO) {
    Value *ops[2];
    getOps(&BO, ops, 2, idx, body->getTerminator());
    Instruction::BinaryOps loweredOpcode = BO.getOpcode();
    if (SetCondInst *SC = dyn_cast<SetCondInst>(&BO))
      loweredOpcode = SC->getScalarOpcode();
    result = BinaryOperator::create(loweredOpcode,
				    ops[0], ops[1], "binop",
				    body->getTerminator());
  }

  void InstructionLowering::visitVSelectInst(VSelectInst &SI) {
    Value *ops[3];
    getOps(&SI, ops, 3, idx, body->getTerminator());
    result = new SelectInst(ops[0], ops[1], ops[2], "select", 
			    body->getTerminator());
  }

  void InstructionLowering::visitCastInst(CastInst &CI) {
    const VectorType *VT = dyn_cast<VectorType>(CI.getType());
    assert(VT && "Cast instruction must have vector type!");
    const Type* destTy = VT->getElementType();
    Value *loweredOp = getOp(&CI, 0, idx, body->getTerminator());
    result = new CastInst(loweredOp, destTy, "cast", body->getTerminator());
  }

  void InstructionLowering::visitShiftInst(ShiftInst &SI) {
    Value *loweredOp = getOp(&SI, 0, idx, body->getTerminator());
    result = new ShiftInst(cast<ShiftInst>(SI).getOpcode(), loweredOp,
			   SI.getOperand(1), "shift",
			   body->getTerminator());
  }

  void InstructionLowering::visitExtractInst(ExtractInst &EI) {
    Value *mul = BinaryOperator::create(Instruction::Mul, vectorIndex,
					EI.getOperand(2),
					"mul", body->getTerminator());
    Value *add = BinaryOperator::create(Instruction::Add, 
					EI.getOperand(1), mul,
					"add", body->getTerminator());
    std::vector<Value*> idx2;
    idx2.push_back(add);
    result = getOp(&EI, 0, idx2, body->getTerminator());
  }

  void InstructionLowering::visitCombineInst(CombineInst &CI) {
    // Here we are generating code for
    //
    //   %tmp = combine v1, v2, start, stride
    //
    // First we compute secondIndex, the index into v2.  If start <=
    // vectorIndex < start + stride * getLength(v2), then
    // secondIndex = (vectorIndex - start) / stride.  Otherwise, we
    // won't use the value from the second vector, so we set
    // secondIndex = 0 (a safe value that won't cause an illegal
    // load).
    //
    Value *secondLength = 
      getLength(CI.getOperand(1));
    Instruction *mul =
      BinaryOperator::create(Instruction::Mul, CI.getOperand(3),
			     secondLength,
			     "mul", body->getTerminator());
    Instruction *add =
      BinaryOperator::create(Instruction::Add, CI.getOperand(2),
			     mul,
			     "add", body->getTerminator());
    Instruction *compare1 =
      BinaryOperator::create(Instruction::SetGE, vectorIndex,
			     CI.getOperand(2),
			     "compare", body->getTerminator());
    Instruction *compare2 =
      BinaryOperator::create(Instruction::SetLT, vectorIndex,
			     add,
			     "compare", body->getTerminator());
    Instruction *inRange =
      BinaryOperator::create(Instruction::And, compare1,
			     compare2,
			     "inRange", body->getTerminator());
    Instruction *sub =
      BinaryOperator::create(Instruction::Sub, vectorIndex,
			     CI.getOperand(2),
			     "secondIndex", body->getTerminator());
    Instruction *div =
      BinaryOperator::create(Instruction::Div, sub,
			     CI.getOperand(3),
			     "secondIndex", body->getTerminator());
    Value *secondIndex =
      new SelectInst(inRange, div,
		     ConstantUInt::get(Type::UIntTy, 0),
		     "select", body->getTerminator());
    
    // Get the value out of v1 at vectorIndex and the value out of
    // v2 at secondIndex
    //
    Value *firstValue = getOp(&CI, 0, idx, body->getTerminator());
    std::vector<Value*> idx2;
    idx2.push_back(secondIndex);
    Value *secondValue = getOp(&CI, 1, idx2, body->getTerminator());
    
    // Use the second value if vectorIndex is in range and
    // (vectorIndex - start) is 0 mod the stride; otherwise use the
    // first value
    //
    Value *select = 
      new SelectInst(inRange, secondValue, firstValue,
		     "select", body->getTerminator());
    Value *rem =
      BinaryOperator::create(Instruction::Rem, sub,
			     CI.getOperand(3), "rem", 
			     body->getTerminator());
    Value *compare =
      BinaryOperator::create(Instruction::SetEQ, rem,
			     ConstantUInt::get(Type::UIntTy, 0),
			     "compare", body->getTerminator());
    result =
      new SelectInst(compare, select, firstValue,
		     "select", body->getTerminator());
  }

  void InstructionLowering::visitVImmInst(VImmInst &VL) {
    result = VL.getOperand(0);
  }

  void InstructionLowering::visitLoadInst(LoadInst &LI) {
    Value *ptr = getLoweredValue(LI.getOperand(0));
    std::vector<Value*> idx2;
    idx2.push_back(ConstantUInt::get(Type::UIntTy, 0));
    idx2.push_back(ConstantUInt::get(Type::UIntTy, 0));
    GetElementPtrInst *GEP = 
      new GetElementPtrInst(ptr, idx2, "gep", body->getTerminator());
    LoadInst *load =
      new LoadInst(GEP, "load", body->getTerminator());
    GEP =
      new GetElementPtrInst(load, idx, "gep", body->getTerminator());
    result = new LoadInst(GEP, "load", body->getTerminator());
  }

  void InstructionLowering::visitStoreInst(StoreInst &SI) {
    result = getOp(&SI, 0, idx, body->getTerminator());
  }


  //===----------------------------------------------------------------------===//
  //                     VMemoryInstLowering implementation
  //===----------------------------------------------------------------------===//

  /// Construct a loop over the array elements defined by the indices
  /// of a vgather or vscatter instruction.  The resulting loop has
  /// one nest for each set of four indices.  This function calls the
  /// template function constructLoopBody(), which is defined
  /// differently for vgather and vscatter.
  ///
  BasicBlock* VMemoryInstLowering::constructLoop(VMemoryInst *VI, BasicBlock *oldHeader,
						std::vector<Value*> arrayIndices, 
						Value* vectorIndex) {
    BasicBlock *lastBB = 0;
    unsigned loopLevel = arrayIndices.size();
    if (VI->getNumIndices() == 4 * loopLevel) {
      // This is the innermost loop nest; construct the loop body
      //
      BasicBlock *body = 
	oldHeader->getTerminator()->getSuccessor(0);
      body->setName("loop_body");
      constructLoopBody(VI, body, arrayIndices, vectorIndex);
      // Increment the vector index if there is one
      //
      if (vectorIndex) {
	Value *load = new LoadInst(vectorIndex, "load", body->getTerminator());
	Value *add = BinaryOperator::create(Instruction::Add, load, 
					    ConstantSInt::get(Type::LongTy, 1),
					    "add", body->getTerminator());
	new StoreInst(add, vectorIndex, body->getTerminator());
      }
      lastBB = body;
    } else {
      // We need another loop nest, so go ahead and split this loop
      // into header, body, and exit basic blocks and recursively
      // construct the nest
      //
      BasicBlock *header =
	oldHeader->getTerminator()->getSuccessor(0);
      header->setName("loop_header");
      BasicBlock *bodyBegin = 
	header->splitBasicBlock(header->begin());
      BasicBlock *exit = 
	bodyBegin->splitBasicBlock(bodyBegin->begin(), "loop_exit");
      bodyBegin->getTerminator()->setSuccessor(0, header);
      constructInnerLoop(VI, oldHeader, header, bodyBegin, exit,
			 arrayIndices, vectorIndex);
      lastBB = exit;
    }
    return lastBB;
  }

  /// Construct an inner loop, recursively calling constructLoop
  ///
  void VMemoryInstLowering::constructInnerLoop(VMemoryInst *VI, BasicBlock *oldHeader, 
					      BasicBlock *header, BasicBlock *bodyBegin, 
					      BasicBlock *exit, std::vector<Value*> arrayIndices, 
					      Value *vectorIndex) {
    // Fill in the loop header
    //
    unsigned loopLevel = arrayIndices.size();
    PHINode *arrayPHI = new PHINode(Type::LongTy, "phi",
				    header->getTerminator());
    arrayPHI->addIncoming(VI->getLowerBound(loopLevel), oldHeader);

    // Fill in the body or split it to make a new loop
    //
    arrayIndices.push_back(arrayPHI);
    BasicBlock *bodyEnd = constructLoop(VI, header, arrayIndices, vectorIndex);

    // Fill in the increment and branch instructions at the end of the
    // loop
    //
    pred_iterator pred = pred_begin(bodyEnd);
    BasicBlock::iterator I = (*pred)->begin();
    Instruction *arrayIncr = 
      BinaryOperator::create(Instruction::Add, arrayPHI, VI->getStride(loopLevel),
			     "add", bodyEnd->getTerminator());
    arrayPHI->addIncoming(arrayIncr, bodyEnd);
    Instruction *cond1 = 
      BinaryOperator::create(Instruction::SetLE, arrayPHI, VI->getUpperBound(loopLevel),
			     "setle", header->getTerminator());
    Instruction *cond2 =
      BinaryOperator::create(Instruction::SetGE, arrayPHI, VI->getUpperBound(loopLevel),
			     "setge", header->getTerminator());
    Instruction *positiveStride = 
      BinaryOperator::create(Instruction::SetGE, VI->getStride(loopLevel),
			     ConstantSInt::get(Type::LongTy, 0),
			     "setge", header->getTerminator());
    Instruction *select = 
      new SelectInst(positiveStride, cond1, cond2, "select", header->getTerminator());
    BasicBlock::iterator oldInst(header->getTerminator());
    Instruction *newInst = new BranchInst(bodyBegin, exit, select);
    ReplaceInstWithInst(header->getInstList(), oldInst, newInst);
  }

  
  //===----------------------------------------------------------------------===//
  //                     VGatherLowering implementation
  //===----------------------------------------------------------------------===//

  /// Lower a vgather instruction: Create an array to hold the vector
  /// contents, then copy the indexed memory locations to that array
  ///
  void VGatherLowering::lowerVGather(VGatherInst *VL) {

    // Allocate the array.
    //
    Instruction *firstInst = VL;
    std::string name = VL->getName();
    Value *length = 
      VectorUtils::computeIndexedLength(VL, VL, name + std::string(".length"));
    VL->setName(name + std::string(".vector"));
    Instruction *vector = allocateVector(VL->getElementType(), length, 
					 name, firstInst);
    setLoweredValue(VL, vector, length);

    // Generate a loop to copy the contents
    //
    BasicBlock *header = VL->getParent();
    Value *vectorIndex = new AllocaInst(Type::LongTy, ConstantUInt::get(Type::UIntTy, 1),
					name + ".index", VL);
    new StoreInst(Constant::getNullValue(Type::LongTy), vectorIndex, VL);
    header->splitBasicBlock(VL);
    std::vector<Value*> arrayIndices;
    constructLoop(VL, header, arrayIndices, vectorIndex);

  }
  
  /// Generate code to take elements from the indexed memory locations
  /// and put them into the array that holds the vector
  ///
  void VGatherLowering::constructLoopBody(VMemoryInst *VI, BasicBlock *body,
					std::vector<Value*> arrayIndices, 
					Value* vectorIndexPtr) {
    Instruction *arrayPtr =
      VectorUtils::computeFlattenedPointer(VI, arrayIndices,
					   body->getTerminator());
    Instruction *load = 
      new LoadInst(arrayPtr, "load", body->getTerminator());

    Value *vectorIndex = new LoadInst(vectorIndexPtr, "index", body->getTerminator());
    std::vector<Value*> vectorIndices;
    vectorIndices.push_back(vectorIndex);
    Instruction *vectorPtr = 
      new GetElementPtrInst(getLoweredValue(VI), vectorIndices, "ptr",
			    body->getTerminator());
    new StoreInst(load, vectorPtr, body->getTerminator());
  }


  //===----------------------------------------------------------------------===//
  //                     VScatterLowering implementation
  //===----------------------------------------------------------------------===//

  /// Lower a vscatter instruction.  Find the array that holds the
  /// vector contents, then copy that array to the indexed memory
  /// locations.
  ///
  void VScatterLowering::lowerVScatter(VScatterInst *VS) {
    Value *V = getLoweredValue(VS->getOperand(0));
    setLoweredValue(VS, V);

    // Make sure the vector length agrees with the indexed array slice
    //
    Value *indexedLength = 
      VectorUtils::computeIndexedLength(VS, VS,
					VS->getPointerOperand()->getName() 
					+ std::string(".length"));
    Value *operandLength = getLength(VS->getOperand(0));
    VectorUtils::ensureEquality(VS, indexedLength, operandLength);

    std::vector<Value*> arrayIndices;
    BasicBlock *header = VS->getParent();
    Value *vectorIndex = 0;

    // Create a vector index to track the indexed position in the
    // vector.
    //
    vectorIndex = new AllocaInst(Type::LongTy, ConstantUInt::get(Type::UIntTy, 1),
				 V->getName() + ".index", VS);
    new StoreInst(Constant::getNullValue(Type::LongTy), vectorIndex, VS);

    header->splitBasicBlock(VS);
    constructLoop(VS, header, arrayIndices, vectorIndex);
  }
  
  /// Generate code to take elements from the array allocated for the
  /// vector and put them into the indexed memory locations
  ///
  void VScatterLowering::constructLoopBody(VMemoryInst *VI, BasicBlock *body,
					   std::vector<Value*> arrayIndices, 
					   Value* vectorIndexPtr) {
    Value *elt;
    // Copy vector array at indexed position to destination array.
    //
    Value *vectorIndex = new LoadInst(vectorIndexPtr, "index",
				      body->getTerminator());
    std::vector<Value*> vectorIndices;
    vectorIndices.push_back(vectorIndex);
    Instruction *vectorPtr =
      new GetElementPtrInst(getLoweredValue(VI), vectorIndices, "ptr",
			    body->getTerminator());
    elt = new LoadInst(vectorPtr, "load", body->getTerminator());
    Instruction *arrayPtr =
      VectorUtils::computeFlattenedPointer(VI, arrayIndices,
    					   body->getTerminator());
    new StoreInst(elt, arrayPtr, body->getTerminator());
  }

}

FunctionPass *llvm::createLowerVectorsPass() { return new LowerVectors(); }

