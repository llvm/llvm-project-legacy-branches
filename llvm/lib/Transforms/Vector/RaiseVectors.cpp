//===- RaiseVectors.cpp - Raise significant functions to Vector-LLVM ------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file raises the Vector-C significant functions to Vector-LLVM.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "raisevectors"

#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Type.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/hash_map"
#include "llvm/ADT/hash_set"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/InstVisitor.h"
#include "VectorLLVM/VectorSignificantFunctions.h"
#include "VectorLLVM/Utils.h"

using namespace llvm;

namespace {


  //===----------------------------------------------------------------------===//
  //                          Class definitions
  //===----------------------------------------------------------------------===//

  class RaiseVectors : public FunctionPass, public InstVisitor<RaiseVectors> {

  public:
    virtual bool doInitialization(Module &M);
    virtual bool runOnFunction(Function &F);
    void visitCallInst(CallInst&);
    void visitBinaryOperator(BinaryOperator&);
    void visitCastInst(CastInst&);
    void visitShiftInst(ShiftInst&);
    void visitSelectInst(SelectInst&);
    void visitPHINode(PHINode&);
    void visitInstruction(Instruction& I) {
      std::cerr << "RaiseVectors:  Unhandled instruction " << I;
      exit(1);
    }
    
  private:
    /// Map from original value to vector instruction
    ///
    hash_map<Value*,Value*> raisingMap;
    
    /// Worklist of instructions to raise
    ///
    std::vector<Instruction*> workList;
    
    /// Set of instructions that we have raised
    ///
    hash_set<Instruction*> raisedInstructions;

    unsigned getVectorLength(Instruction*);
    const Type *getRaisedType(const Type*,unsigned);
    void setRaisedValue(Instruction*,Value*);
    Value *getRaisedValue(Value*,unsigned);
    Value *getRaisedOperand(Instruction*,unsigned);
    bool addDefsToWorklist(Function&);
    void addUsesToWorklist(Instruction*);
    void raiseInstructions();
    void deleteRaisedInstructions();

  }; 

  RegisterOpt<RaiseVectors> X("raisevectors",
			      "Raise Vector-C significant functions to Vector-LLVM");


  //===----------------------------------------------------------------------===//
  //                     RaiseVectors implementation
  //===----------------------------------------------------------------------===//

  /// llvm-gcc is very permissive about function declarations --
  /// undeclared functions are treated as int(...).  Here we require
  /// and check that the user has properly declared all Vector-C
  /// significant functions.
  ///
  bool RaiseVectors::doInitialization(Module &M) {
    for (Module::iterator I = M.begin(), E = M.end();
	 I != E; ++I) {
      if (!VectorSignificantFunctions::isProperlyDeclared(I)) {
	std::cerr << "Significant function " << I->getName() 
		  << " was not declared or was improperly declared.\n";
	exit(1);
      }
    }
    return false;
  }

  /// Main function called by PassManager
  ///
  bool RaiseVectors::runOnFunction(Function &F) {
    DEBUG(std::cerr << "\nrunOnFunction(" << F.getName() << ")\n");

    raisingMap.clear();
    workList.clear();
    raisedInstructions.clear();
    bool changed = addDefsToWorklist(F);

    if (changed) {
      raiseInstructions();
      deleteRaisedInstructions();
    }

    return changed;

  }

  /// Add all vector definitions to the work list
  ///
  bool RaiseVectors::addDefsToWorklist(Function& F) {
    bool defFound = false;
    for (Function::iterator FI = F.begin(), FE = F.end(); 
	 FI != FE; ++FI) {
      for (BasicBlock::iterator BI = FI->begin(), BE = FI->end(); 
	   BI != BE; ++BI) {
	if (CallInst *CI = dyn_cast<CallInst>(BI)) {
	  if (Function* F = CI->getCalledFunction()) {
	    VectorSignificantFunctions::ID id = 
	      VectorSignificantFunctions::getID(F->getName());
	    if (id == VectorSignificantFunctions::vload ||
		id == VectorSignificantFunctions::vgather ||
		id == VectorSignificantFunctions::vloadi ||
		id == VectorSignificantFunctions::vimm ||
		id == VectorSignificantFunctions::fixed_vimm ||
		id == VectorSignificantFunctions::load ||
		id == VectorSignificantFunctions::constant) {
	      workList.push_back(CI);
	      defFound = true;
	    }
	  }
	}
      }
    }
    return defFound;

  }

  /// Raise all instructions on the worklist.
  ///
  void RaiseVectors::raiseInstructions() {
    // Visit all the instructions to be raised.  As uses are
    // encountered, they are added to the worklist.
    //
    while (workList.size() > 0) {
      Instruction *I = workList.back();
      workList.pop_back();
      if(raisedInstructions.insert(I).second) {
	DEBUG(std::cerr << "Raising " << *I);
	visit(*I);
	DEBUG(if (raisingMap[I]) {std::cerr << "Raised value is " << *raisingMap[I];});
      }
    }
    // Check for leftover dummy values indicating the program
    // attempted to combine a scalar with a vector
    //
    for (hash_map<Value*,Value*>::iterator I = raisingMap.begin(),
	   E = raisingMap.end(); I != E; ++I) {
      if (I->second && isa<Argument>(I->second)) {
	std::cerr << "Value was never raised!\n";
	std::cerr << *(I->first) << "\n";
	std::cerr << "This is because you used a scalar value in a vector operation.\n";
	std::cerr << "Use vimm to promote scalars to vectors "
		  << "before combining them with vectors.\n";
	exit(1);
      }
    }
  }

  /// Delete all instructions that we have raised.
  ///
  void RaiseVectors::deleteRaisedInstructions() {

    for (hash_set<Instruction*>::iterator I = raisedInstructions.begin(),
	   E = raisedInstructions.end(); I != E; ++I) {
      DEBUG(std::cerr << "Dropping all references from " << **I);
      (*I)->dropAllReferences();
    }

    for (hash_set<Instruction*>::iterator I = raisedInstructions.begin(),
	   E = raisedInstructions.end(); I != E; ++I) {
      (*I)->getParent()->getInstList().erase(*I);
    }
  }

  /// Raise a significant function call
  ///
  void RaiseVectors::visitCallInst(CallInst &CI) {
    Function *F = CI.getCalledFunction();
    if (!F) {
      std::cerr << "Can't handle indirect function call " << CI;
      exit(1);
    }
    std::string name = F->getName();
    Value *raisedValue;
    switch(VectorSignificantFunctions::getID(name)) {
    case VectorSignificantFunctions::vload:
    case VectorSignificantFunctions::vgather: {
      std::vector<Value*> idx;
      for (unsigned i = 2; i < CI.getNumOperands(); ++i) {
	CastInst *castInst = 
	  new CastInst(CI.getOperand(i), Type::LongTy, "cast", &CI);
	idx.push_back(castInst);
      }
      raisedValue = new VGatherInst(CI.getOperand(1), 
				    idx, "vgather", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::load: {
      ConstantUInt *UIntVal = dyn_cast<ConstantUInt>(CI.getOperand(2));
      assert(UIntVal && "Vector length must be a constant UInt!"); 
      const PointerType *PointerTy = 
	dyn_cast<PointerType>(CI.getOperand(1)->getType());
      assert(PointerTy && "Pointer operand must be pointer type!");
      CastInst *cast = 
	new CastInst(CI.getOperand(1), 
		     PointerType::get(FixedVectorType::get(PointerTy->getElementType(),
							   UIntVal->getValue())),
		     "cast", &CI);
      std::vector<Value*> Idx;
      Idx.push_back(CI.getOperand(3));
      GetElementPtrInst *GEP =
	new GetElementPtrInst(cast, Idx, "gep", &CI);
      raisedValue = new LoadInst(GEP, "load", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::vimm:
    case VectorSignificantFunctions::vloadi: {
      raisedValue = new VImmInst(CI.getOperand(1), CI.getOperand(2), false,
				 "vimm", &CI);
      const VectorType *VT = VectorType::get(CI.getOperand(1)->getType());
      if (raisedValue->getType() != VT)
      	raisedValue = new CastInst(raisedValue, VT, "cast", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::constant: {
      std::vector<Constant*> elements;
      for (unsigned i = 1; i < CI.getNumOperands(); ++i) {
	Constant *C = dyn_cast<Constant>(CI.getOperand(i));
	assert(C && "Operands of constant must be constants!");
	elements.push_back(ConstantExpr::getCast(C, CI.getType()));
      }
      raisedValue = ConstantVector::get(elements);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::fixed_vimm: {
      ConstantUInt *UIntVal = dyn_cast<ConstantUInt>(CI.getOperand(2));
      assert(UIntVal && "Vector length must be a constant UInt!"); 
      raisedValue = new VImmInst(CI.getOperand(1), CI.getOperand(2),
				 true, "vimm", &CI);
      const FixedVectorType *VT = FixedVectorType::get(CI.getType(), UIntVal->getValue());
      if (raisedValue->getType() != VT)
	raisedValue = new CastInst(raisedValue, VT, "cast", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::vstore:
    case VectorSignificantFunctions::vscatter: {
      std::vector<Value*> idx;
      for (unsigned i = 3; i < CI.getNumOperands(); ++i) {
	CastInst *castInst = 
	  new CastInst(CI.getOperand(i), Type::LongTy, "cast", &CI);
	idx.push_back(castInst);
      }
      Value *raisedOp = getRaisedValue(CI.getOperand(1), (unsigned) 0);
      Value *ptr = CI.getOperand(2);
      raisedValue = new VScatterInst(raisedOp, ptr, idx, &CI);
      break;
    }
    case VectorSignificantFunctions::store: {
      unsigned length = getVectorLength(&CI);
      Value *op1 = getRaisedValue(CI.getOperand(1), length);
      const PointerType *PointerTy = 
	dyn_cast<PointerType>(CI.getOperand(2)->getType());
      assert(PointerTy && "Pointer operand must be pointer type!");
      CastInst *cast = 
	new CastInst(CI.getOperand(2), 
		     PointerType::get(FixedVectorType::get(PointerTy->getElementType(),
							   length)),
		     "cast", &CI);
      std::vector<Value*> Idx;
      Idx.push_back(CI.getOperand(3));
      GetElementPtrInst *GEP =
	new GetElementPtrInst(cast, Idx, "gep", &CI);
      raisedValue = new StoreInst(op1, GEP, &CI);
      break;
    }
    case VectorSignificantFunctions::vselect: {
      unsigned numArgs = 3;
      unsigned length = getVectorLength(&CI);
      assert((CI.getNumOperands() == numArgs+1) && 
	     "Wrong number of arguments to _select!");
      CastInst *cast = dyn_cast<CastInst>(CI.getOperand(1));
      assert(cast && "First operand of vselect must be cast!");
      assert(cast->getOperand(0)->getType() == Type::BoolTy &&
	     "First operand of vselect must be cast of bool to int!");
      Value *raisedArgs[numArgs];
      raisedArgs[0] = getRaisedValue(cast->getOperand(0), length);
      for (unsigned i = 1; i < numArgs; ++i) {
	Value *arg = CI.getOperand(i+1); 
	raisedArgs[i] = getRaisedValue(arg, length);
      }
      raisedValue = 
	new VSelectInst(raisedArgs[0], raisedArgs[1], raisedArgs[2], "vselect", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::extract: {
      Value *raisedOp = getRaisedOperand(&CI, 1);
      raisedValue = new ExtractInst(raisedOp, CI.getOperand(2),
				    CI.getOperand(3), CI.getOperand(4),
				    "extract", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::combine: {
      unsigned length = getVectorLength(&CI);
      Value *raisedOp1 = getRaisedValue(CI.getOperand(1), length);
      Value *raisedOp2 = getRaisedValue(CI.getOperand(2), length);
      raisedValue = new CombineInst(raisedOp1, raisedOp2,
				    CI.getOperand(3), CI.getOperand(4),
				    "combine", &CI);
      addUsesToWorklist(&CI);

      break;
    }
    case VectorSignificantFunctions::fixed_combine: {
      ConstantUInt *op2 = dyn_cast<ConstantUInt>(CI.getOperand(2));
      ConstantUInt *op4 = dyn_cast<ConstantUInt>(CI.getOperand(4));
      assert((op2 && op4) && "Vector length operands to fixed_combine must be constant uints!");
      unsigned length1 = op2->getValue();
      unsigned length2 = op4->getValue();
      Value *raisedOp1 = getRaisedValue(CI.getOperand(1), length1);
      Value *raisedOp2 = getRaisedValue(CI.getOperand(3), length2);
      raisedValue = new CombineInst(raisedOp1, raisedOp2,
				    CI.getOperand(5), CI.getOperand(6),
				    "combine", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::fixed_permute: {
      ConstantUInt *op2 = dyn_cast<ConstantUInt>(CI.getOperand(2));
      ConstantUInt *op4 = dyn_cast<ConstantUInt>(CI.getOperand(4));
      assert((op2 && op4) && "Vector length operands to fixed_combine must be constant uints!");
      unsigned length1 = op2->getValue();
      unsigned length2 = op4->getValue();
      Value *raisedOp1 = getRaisedValue(CI.getOperand(1), length1);
      Value *raisedOp2 = getRaisedValue(CI.getOperand(3), length2);
      raisedValue = VectorUtils::getCallInst(raisedOp2->getType(), "vllvm_permute_" + 
					     cast<FixedVectorType>(raisedOp2->getType())->getElementType()->getDescription(),
					     raisedOp1, raisedOp2, "permute", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    case VectorSignificantFunctions::extractelement: {
      Value *raisedOp = getRaisedOperand(&CI, 1);
      raisedValue = 
	new ExtractElementInst(raisedOp, CI.getOperand(2),
			       "extractelement", &CI);
      CI.replaceAllUsesWith(raisedValue);
      break;
    }
    case VectorSignificantFunctions::combineelement: {
      Value *raisedOp1 = getRaisedOperand(&CI, 1);
      raisedValue = new CombineElementInst(raisedOp1, CI.getOperand(2),
					   CI.getOperand(3), "combineelement", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    default:
      if (name.substr(0, 7) == "vectorc") {
	name.erase(0, 7);
	name = "vllvm" + name;
      }
      else if (name.substr(0, 5) == "vllvm") {
	name += "_vector";
      } else {
	std::cerr << "Can't handle instruction " << CI;
	exit(1);
      }
      unsigned length = getVectorLength(&CI);
      std::vector<const Type*> formalArgs;
      std::vector<Value*> args;
      for (unsigned i = 1; i < CI.getNumOperands(); ++i) {
	Value *op = CI.getOperand(i);
	if (isa<Constant>(op)) {
	  formalArgs.push_back(op->getType());
	  args.push_back(op);
	} else {
	  formalArgs.push_back(getRaisedType(CI.getOperand(i)->getType(), length));
	  args.push_back(getRaisedOperand(&CI, i));
	}
      }
      FunctionType *FType = 
	FunctionType::get(getRaisedType(F->getReturnType(), length), formalArgs, false);
      Module *M = CI.getParent()->getParent()->getParent();
      Function *func = M->getOrInsertFunction(name, FType);
      raisedValue = new CallInst(func, args, "func", &CI);
      addUsesToWorklist(&CI);
      break;
    }
    setRaisedValue(&CI, raisedValue);
  }

  /// Raise a binary operator
  ///
  void RaiseVectors::visitBinaryOperator(BinaryOperator &BO) {
    unsigned length = getVectorLength(&BO);
    Value *newOp[2];
    for (unsigned i = 0; i < 2; ++i) {
      newOp[i] = getRaisedValue(BO.getOperand(i), length);
    }
    Instruction::BinaryOps raisedOp;
    if (SetCondInst *SI = dyn_cast<SetCondInst>(&BO))
      raisedOp = SI->getVectorOpcode();
    else raisedOp = BO.getOpcode();
    Instruction *raisedValue = 
      BinaryOperator::create(raisedOp, newOp[0], newOp[1], "binop", &BO);
    setRaisedValue(&BO, raisedValue);
    addUsesToWorklist(&BO);
  }

  /// Raise a cast instruction
  ///
  void RaiseVectors::visitCastInst(CastInst &CI) {
    // Don't raise the cast if it's a cast of a bool to an int to get
    // it into a vselect significant function
    //
    if (CI.hasOneUse()) {
      User *use = *CI.use_begin();
      if (CallInst *I = dyn_cast<CallInst>(use)) {
	if (Function *F = I->getCalledFunction()) {
	  if (VectorSignificantFunctions::getID(F->getName()) ==
	      VectorSignificantFunctions::vselect)
	    return;
	}
      }
    }
    unsigned length = getVectorLength(&CI);
    Value *raisedOp = getRaisedValue(CI.getOperand(0), length);
    Instruction *raisedValue =
      new CastInst(raisedOp, getRaisedType(CI.getType(), length), "cast", &CI);
    setRaisedValue(&CI, raisedValue);
    addUsesToWorklist(&CI);
  }

  /// Raise a shift instruction
  ///
  void RaiseVectors::visitShiftInst(ShiftInst &SI) {
    Value *raisedOp = getRaisedOperand(&SI, 0);
    Instruction *raisedValue =
      new ShiftInst(SI.getOpcode(), raisedOp, 
		    SI.getOperand(1), "shift", &SI);
    setRaisedValue(&SI, raisedValue);
    addUsesToWorklist(&SI);
  }

  /// Raise a select instruction
  ///
  void RaiseVectors::visitSelectInst(SelectInst &SI) {
    Value *newOp[2];
    unsigned length = getVectorLength(&SI);
    for (unsigned i = 0; i < 2; ++i) {
      newOp[i] = getRaisedValue(SI.getOperand(i+1), length);
    }
    Instruction *raisedValue =
      new SelectInst(SI.getOperand(0), newOp[0], newOp[1],
		     "select", &SI);
    setRaisedValue(&SI, raisedValue);
    addUsesToWorklist(&SI);
  }

  /// Raise a phi node
  ///
  void RaiseVectors::visitPHINode(PHINode &PN) {
    unsigned length = getVectorLength(&PN);
    const VectorType *VT = 0;
    if (length)
      VT = FixedVectorType::get(PN.getType(), length);
    else
      VT = VectorType::get(PN.getType());
    PHINode *raisedValue = new PHINode(VT, "phi", &PN);
    for (unsigned i = 0; i < PN.getNumIncomingValues(); ++i)
      raisedValue->addIncoming(getRaisedValue(PN.getIncomingValue(i), length),
			       PN.getIncomingBlock(i));
    setRaisedValue(&PN, raisedValue);
    addUsesToWorklist(&PN);
  }

  /// Get the raised type for a given scalar type or pointer to scalar
  /// type and vector length.  Vector length of 0 means a non-fixed
  /// length vector.
  ///
  const Type *RaiseVectors::getRaisedType(const Type* Ty, unsigned length) {
    unsigned i = 0;
    while (isa<PointerType>(Ty)) {
      Ty = cast<PointerType>(Ty)->getElementType();
      ++i;
    }
    const Type *result = 
      (length == 0) ? VectorType::get(Ty) : FixedVectorType::get(Ty, length);
    while (i-- > 0) {
      result = PointerType::get(result);
    }
    return result;
  }

  /// Get the (fixed) vector length of a raised instruction.  Return 0
  /// if the vector is not a fixed-length vector.
  ///
  unsigned RaiseVectors::getVectorLength(Instruction *I) {
    for (User::op_iterator OI = I->op_begin(), OE = I->op_end();
	 OI != OE; ++OI) {
      if (Value *val = raisingMap[*OI]) {
	const Type *Ty = val->getType();
	while (isa<PointerType>(Ty))
	  Ty = cast<PointerType>(Ty)->getElementType();
	if (const FixedVectorType *VT = dyn_cast<FixedVectorType>(Ty))
	  return VT->getNumElements();
	return 0;
      }
    }
    assert(0 && "Instruction has no raised operands!");
  }

  /// Add the specified pair to the raising map, replacing dummy uses
  /// if necessary
  ///
  void RaiseVectors::setRaisedValue(Instruction *key, Value *newValue) {
    if (!newValue)
      return;
    Value*& oldValue = raisingMap[key];
    if (oldValue) {
      if (oldValue->getType() != newValue->getType())
	newValue = new CastInst(oldValue, newValue->getType(), "cast", key);
      oldValue->replaceAllUsesWith(newValue);
      delete oldValue;
    }
    oldValue = newValue;
  }

  /// Get the value for the specified key from the raising map.  If no
  /// value is there, we haven't raised the value yet, so create a
  /// dummy value with the appropriate vector length and replace it
  /// when the value is raised.
  ///
  Value *RaiseVectors::getRaisedValue(Value *key, unsigned length) {
    const Type *Ty = getRaisedType(key->getType(), length);
    Value*& Val = raisingMap[key];
    if (!Val) {
      Val = Ty ? new Argument(Ty) : new Argument(VectorType::get(key->getType()));
      DEBUG(std::cerr << "Created dummy value " << *Val << "\n");
    }
    return Val;
  }

  /// Get the raised operand of an instruction
  ///
  Value *RaiseVectors::getRaisedOperand(Instruction *I, unsigned i) {
    Value *op = I->getOperand(i);
    unsigned length = getVectorLength(I);
    return getRaisedValue(op, length);
  }

  /// Add all uses of an instruction to the worklist
  ///
  void RaiseVectors::addUsesToWorklist(Instruction *I) {
    DEBUG(std::cerr << "Adding uses of " << *I);
    for (Value::use_iterator UI = I->use_begin(), UE = I->use_end();
	 UI != UE; ++UI) {
      if (Instruction *II = dyn_cast<Instruction>(*UI)) {
	DEBUG(std::cerr << "Adding " << *II);
	workList.push_back(II);
      }
    }
  }

}
