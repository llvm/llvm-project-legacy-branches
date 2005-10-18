//===- AltiVec.cpp - Raise significant functions to Vector-LLVM ------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file takes blocked Vector-LLVM code and puts it in a form that
// can be passed to the AltiVec C Backend.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "altivec"

#include "VectorLLVM/Utils.h"
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
#include "VectorLLVM/Utils.h"

using namespace llvm;

namespace {


  //===----------------------------------------------------------------------===//
  //                          Class definitions
  //===----------------------------------------------------------------------===//

  class AltiVec : public FunctionPass, public InstVisitor<AltiVec> {

  public:
    bool runOnFunction(Function &F);
    void visitCastInst(CastInst &);
    void visitVImmInst(VImmInst &);
    void visitExtractInst(ExtractInst &);
    void visitCombineInst(CombineInst &);
    void visitVSelectInst(VSelectInst &);
    void visitShiftInst(ShiftInst &);
    void visitMul(BinaryOperator &BO);
    void visitSub(BinaryOperator &BO);
    void visitSetCondInst(SetCondInst &BO);
    void visitCallInst(CallInst &);
    void visitInstruction(Instruction& I) {}
    
  private:
    bool changed;
    hash_set<Instruction*> instructionsToDelete;

    void deleteInstructions() {
      for (hash_set<Instruction*>::iterator I = instructionsToDelete.begin(),
	     E = instructionsToDelete.end(); I != E; ++I) {
	(*I)->dropAllReferences();
      }
      for (hash_set<Instruction*>::iterator I = instructionsToDelete.begin(),
	     E = instructionsToDelete.end(); I != E; ++I) {
	(*I)->getParent()->getInstList().erase(*I);
      }
    }
  }; 

  RegisterOpt<AltiVec> X("altivec",
			 "AltiVec code generation pre-pass");

  //===----------------------------------------------------------------------===//
  //                     Helper functions
  //===----------------------------------------------------------------------===//

  /// Check whether the type is one that AltiVec can handle; if not,
  /// it must be lowered later.
  ///
  bool isProperType(const VectorType *VT) {
    // Only fixed vector types are allowed
    //
    const FixedVectorType *FVT = dyn_cast<FixedVectorType>(VT);
    if (!FVT) return false;
    switch(VT->getElementType()->getTypeID()) {
    case (Type::IntTyID):
    case (Type::UIntTyID):
      return FVT->getNumElements() == 4;
    case (Type::ShortTyID):
    case (Type::UShortTyID):
      return FVT->getNumElements() == 8;
    case (Type::SByteTyID):
    case (Type::UByteTyID):
      return FVT->getNumElements() == 16;
    default:
      return false;
    }
  }

  std::string getAltiVecName(const std::string& baseName, const VectorType *VT) {
    return "altivec_" + baseName + "_" + VT->getElementType()->getDescription();
  }


  //===----------------------------------------------------------------------===//
  //                     AltiVec implementation
  //===----------------------------------------------------------------------===//

  /// Main function called by PassManager
  ///
  bool AltiVec::runOnFunction(Function &F) {
    instructionsToDelete.clear();
    changed = false;
    for (Function::iterator FI = F.begin(), FE = F.end(); 
	 FI != FE; ++FI)
      for (BasicBlock::iterator BI = FI->begin(), BE = FI->end(); 
	   BI != BE; ++BI)
	if (!instructionsToDelete.count(BI)) {
	  DEBUG(std::cerr << "Visiting instruction " << *BI);
	  visit(*BI);
	}
    if (changed) deleteInstructions();
    return changed;
  }

  void AltiVec::visitVImmInst(VImmInst &VL) {
    if (!VL.hasOneUse()) return;
    CastInst *cast = dyn_cast<CastInst>(*VL.use_begin());
    if (!cast) return;
    const VectorType *VT = dyn_cast<VectorType>(cast->getType());
    // We need only worry about a cast of a non-constant scalar to a
    // vector; the AltiVec C Backend can handle the other cases
    // directly.
    //
    if (!VT || !isProperType(VT) ||
	isa<Constant>(VL.getOperand(0)))
      return;
    // We need to create a new vector on the stack, store the scalar
    // value into it, and splat the value into a vector register
    //
    AllocaInst *vectorPtr = new AllocaInst(VT, 0, "alloca", &VL);
    Value *element = VL.getOperand(0); 
    if (element->getType() != VT->getElementType())
      element = new CastInst(element, VT->getElementType(), "cast", &VL);
    CastInst *scalarPtr = new CastInst(vectorPtr, PointerType::get(VT->getElementType()),
				       "cast", &VL);
    StoreInst *store = new StoreInst(element, scalarPtr, &VL);
    LoadInst *vector = new LoadInst(vectorPtr, "load", &VL);
    CallInst *call = VectorUtils::getCallInst(VT, getAltiVecName("splat", VT),
					      vector, ConstantUInt::get(Type::UByteTy, 0),
					      "splat", &VL);
    cast->replaceAllUsesWith(call);
    instructionsToDelete.insert(cast);
    instructionsToDelete.insert(&VL);
    changed = true;
  }

  void AltiVec::visitCastInst(CastInst &CI) {
    // We need only worry about a cast of a non-constant scalar to a
    // vector; the AltiVec C Backend can handle the other cases
    // directly.
    //
    const VectorType *VT = dyn_cast<VectorType>(CI.getType());
    if (!VT || !isProperType(VT) ||
	isa<VectorType>(CI.getOperand(0)->getType()) ||
	isa<Constant>(CI.getOperand(0)))
      return;
    // We need to create a new vector on the stack, store the scalar
    // value into it, and splat the value into a vector register
    //
    AllocaInst *vectorPtr = new AllocaInst(VT, 0, "alloca", &CI);
    Value *element = CI.getOperand(0); 
    if (element->getType() != VT->getElementType())
      element = new CastInst(element, VT->getElementType(), "cast", &CI);
    CastInst *scalarPtr = new CastInst(vectorPtr, PointerType::get(VT->getElementType()),
				       "cast", &CI);
    StoreInst *store = new StoreInst(element, scalarPtr, &CI);
    LoadInst *vector = new LoadInst(vectorPtr, "load", &CI);
    CallInst *call = VectorUtils::getCallInst(VT, getAltiVecName("splat", VT),
					      vector, ConstantUInt::get(Type::UByteTy, 0),
					      "splat", &CI);
    CI.replaceAllUsesWith(call);
    instructionsToDelete.insert(&CI);
    changed = true;
  }
  
  // Check whether an extract instruction should be turned into
  // altivec_unpack
  //
  void AltiVec::visitExtractInst(ExtractInst &EI) {
    Value *v = EI.getOperand(0);
    ConstantUInt *start = dyn_cast<ConstantUInt>(EI.getOperand(1));
    ConstantUInt *stride = dyn_cast<ConstantUInt>(EI.getOperand(2));
    ConstantUInt *len = dyn_cast<ConstantUInt>(EI.getOperand(3));
    if (!start || !stride || !len) return;
    if (stride->getValue() != 1 || len->getValue() != 8) return;
    std::string baseName;
    if (start->getValue() == 0)
      baseName = "unpackh";
    else if (start->getValue() == 8)
      baseName = "unpackl";
    else return;
    const FixedVectorType *VT = dyn_cast<FixedVectorType>(v->getType());
    if (VT == FixedVectorType::get(Type::UByteTy, 16)) {
      if (!EI.hasOneUse()) return;
      CastInst *cast = dyn_cast<CastInst>(*EI.use_begin());
      if (!cast) return;
      const FixedVectorType *argTy = FixedVectorType::get(Type::SByteTy, 16);
      const FixedVectorType *retTy = FixedVectorType::get(Type::ShortTy, 8);
      if (cast->getType() != FixedVectorType::get(Type::ShortTy, 8)) {
	CastInst *cast2 = new CastInst(&EI, FixedVectorType::get(Type::ShortTy, 8), "cast", cast);
	cast->setOperand(0, cast2);
	cast = cast2;
      }
      CastInst *arg = new CastInst(v, argTy, "cast", &EI);
      CallInst *call = VectorUtils::getCallInst(retTy, getAltiVecName(baseName, argTy),
						arg, "unpack", &EI);
						
      BinaryOperator *andInst = 
	BinaryOperator::create(Instruction::And, call,
			       ConstantExpr::getCast(ConstantSInt::get(Type::ShortTy, 0xFF), retTy), 
			       "and", &EI);
      cast->replaceAllUsesWith(andInst);
      instructionsToDelete.insert(cast);
      instructionsToDelete.insert(&EI);
      changed = true;
    }
  }

  void AltiVec::visitCombineInst(CombineInst &CI) {
    CombineInst *combine1 = cast<CombineInst>(&CI);
    Value *v1 = CI.getOperand(0);
    Value *v2 = CI.getOperand(1);
    // If the destination is a combine instruction, do nothing; if
    // necessary, we'll handle the first combine instruction in the
    // series.
    //
    if (isa<CombineInst>(v1))
      return;
    // We must have two fixed-vector operands
    //
    const FixedVectorType *VT1 = dyn_cast<FixedVectorType>(v1->getType());
    if (!VT1) return;
    const FixedVectorType *VT2 = dyn_cast<FixedVectorType>(v2->getType());
    if (!VT2) return;
    if (VT1->getNumElements() != 2*VT2->getNumElements())
      return;
    // This combine must have exactly one use, and it must be a
    // combine whose operand 0 is this combine.  The types must work
    // out properly.
    //
    if (!CI.hasOneUse()) return;
    CombineInst* combine2 = dyn_cast<CombineInst>(*CI.use_begin());
    if (!combine2) return;
    if (&CI != combine2->getOperand(0)) return;
    if (combine2->getOperand(1)->getType() != VT2) return;
    if (combine2->hasOneUse()) {
      // Check for a vec_pack or vec_perm pattern
      //
      Instruction *use = dyn_cast<Instruction>(*combine2->use_begin());
      if (!use) return;
      const FixedVectorType *VT = dyn_cast<FixedVectorType>(use->getType());
      if (!VT || !isProperType(VT)) return;
      std::string baseName;
      CallInst *call = 0;
      if (isa<CastInst>(use)) {
	baseName = "pack";
	call = VectorUtils::getCallInst(VT, getAltiVecName(baseName, VT1),
					v2, combine2->getOperand(1),
					"pack", &CI);
      }
      else if (VectorUtils::isFunctionContaining(use, "saturate")) {
	baseName = "packsu";
	call = VectorUtils::getCallInst(VT, getAltiVecName(baseName, VT1),
					v2, combine2->getOperand(1),
					"pack", &CI);
      }
      else if (VectorUtils::isFunctionContaining(use, "permute")) {
	call = VectorUtils::getCallInst(VT, getAltiVecName("perm", VT),
					CI.getOperand(1), combine2->getOperand(1),
					cast<CallInst>(use)->getOperand(2),
					"perm", &CI);
      } 
      else {
	return;
      }

      use->replaceAllUsesWith(call);
      instructionsToDelete.insert(use);
      instructionsToDelete.insert(combine2);
      instructionsToDelete.insert(&CI);
      Instruction *op0 = dyn_cast<Instruction>(CI.getOperand(0));
      if (op0)
	instructionsToDelete.insert(op0);
      changed = true;
    } else if (combine2->hasNUses(2)) {
      Value::use_iterator I = combine2->use_begin();
      ExtractInst *extract0 = dyn_cast<ExtractInst>(*I++);
      ExtractInst *extract1 = dyn_cast<ExtractInst>(*I);
      assert(extract0 && extract1);
      CallInst *mergeh = VectorUtils::getCallInst(VT2, getAltiVecName("mergeh", VT2),
						  combine1->getOperand(1), combine2->getOperand(1),
						  "mergeh", extract0);
      CallInst *mergel = VectorUtils::getCallInst(VT2, getAltiVecName("mergel", VT2),
						  combine1->getOperand(1), combine2->getOperand(1),
						  "mergel", extract0);
      if (cast<ConstantUInt>(extract0->getOperand(1))->getValue() == 0) {
	extract0->replaceAllUsesWith(mergeh);
	extract1->replaceAllUsesWith(mergel);
      } else {
	extract0->replaceAllUsesWith(mergel);
	extract1->replaceAllUsesWith(mergeh);
      }
      instructionsToDelete.insert(combine1);
      instructionsToDelete.insert(combine2);
      instructionsToDelete.insert(extract0);
      instructionsToDelete.insert(extract1);
      changed = true;
    }
  }

  void AltiVec::visitVSelectInst(VSelectInst &VI) {
    const FixedVectorType *VT = dyn_cast<FixedVectorType>(VI.getType());
    if (!VT || !isProperType(VT)) return;
    CallInst *call = VectorUtils::getCallInst(VT, getAltiVecName("sel", VT),
					      VI.getOperand(2), VI.getOperand(1),
					      VI.getOperand(0), "sel", &VI);
    VI.replaceAllUsesWith(call);
    instructionsToDelete.insert(&VI);
    changed = true;
  }

  void AltiVec::visitSetCondInst(SetCondInst &BO) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(BO.getOperand(0)->getType());
    if (!VT || !isProperType(VT)) return;
    std::string name;
    switch(BO.getOpcode()) {
    case Instruction::VSetGT:
      name = "cmpgt";
      break;
    default:
      assert(0 && "Unknown VSetCC opcode!");
    }
    CallInst *call = VectorUtils::getCallInst(BO.getType(), getAltiVecName(name, VT),
					      BO.getOperand(0), BO.getOperand(1),
					      "cmp", &BO);
    BO.replaceAllUsesWith(call);
    instructionsToDelete.insert(&BO);
    changed = true;
  }

  void AltiVec::visitSub(BinaryOperator &BO) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(BO.getOperand(0)->getType());
    if (!VT) return;
    CallInst *sub = VectorUtils::getCallInst(VT, getAltiVecName("sub", VT),
					     BO.getOperand(0), BO.getOperand(1),
					     "sub", &BO);
    BO.replaceAllUsesWith(sub);
    instructionsToDelete.insert(&BO);
    changed = true;
  }

  void AltiVec::visitShiftInst(ShiftInst &SI) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(SI.getOperand(0)->getType());
    if (!VT) return;
    std::string shortName;
    if (SI.getOpcode() == Instruction::Shr) {
      if (VT->getElementType()->isSigned())
	shortName = "sra";
      else
	shortName = "srl";
    } else {
      shortName = "sll";
    }
    CastInst *cast = new CastInst(SI.getOperand(1), FixedVectorType::get(Type::UShortTy, 8), "cast", &SI);
    CallInst *shift = VectorUtils::getCallInst(VT, getAltiVecName(shortName, VT),
					     SI.getOperand(0), cast,
					     "shift", &SI);
    SI.replaceAllUsesWith(shift);
    instructionsToDelete.insert(&SI);
    changed = true;
  }

  void AltiVec::visitMul(BinaryOperator &BO) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(BO.getOperand(0)->getType());
    if (!VT || !BO.hasOneUse())
      return;
    Instruction *use = dyn_cast<Instruction>(*BO.use_begin());
    if (!use) return;
    switch (use->getOpcode()) {
    case Instruction::Add: {
      // Check for mradds pattern
      //
      BinaryOperator *add = cast<BinaryOperator>(use);
      CastInst *mulCast0 = dyn_cast<CastInst>(BO.getOperand(0));
      CastInst *mulCast1 = dyn_cast<CastInst>(BO.getOperand(1));
      CastInst *addCast0 = 0, *addCast2 = 0, *shrCast = 0;
      VImmInst *VImm = 0;
      ShiftInst *shr = 0;
      CallInst *adds;
      unsigned offset = 0, shamt = 0;
      if (&BO == add->getOperand(0))
	addCast0 = dyn_cast<CastInst>(add->getOperand(1));
      else
	addCast0 = dyn_cast<CastInst>(add->getOperand(0));
      if (addCast0)
	VImm = dyn_cast<VImmInst>(addCast0->getOperand(0));
      if (VImm) {
	if (ConstantSInt *C = dyn_cast<ConstantSInt>(VImm->getOperand(0)))
	  offset = C->getValue();
	else if (ConstantUInt *C = dyn_cast<ConstantUInt>(VImm->getOperand(0)))
	  offset = C->getValue();
      }
      if (add->hasOneUse()) {
	shr = dyn_cast<ShiftInst>(*add->use_begin()); 
      }
      if (shr && shr->hasOneUse()) {
	if (ConstantUInt *C = dyn_cast<ConstantUInt>(shr->getOperand(1)))
	  shamt = C->getValue();
	shrCast = dyn_cast<CastInst>(*shr->use_begin());
      }
      if (shrCast && shrCast->hasOneUse()) {
	adds = dyn_cast<CallInst>(*shrCast->use_begin());
	Function *F = adds->getCalledFunction();
	if (!F || F->getName().substr(0, 10) != "vllvm_adds")
	    adds = 0;
      }
      if (mulCast0 && mulCast1 && addCast0 && VImm &&
	  offset == 16384 && shrCast && shr && shamt == 15 && 
	  adds) {
	VT = cast<FixedVectorType>(adds->getType());
	CallInst *mradds = VectorUtils::getCallInst(VT, getAltiVecName("mradds", VT),
						    new CastInst(mulCast0->getOperand(0), VT, "cast", adds), 
						    new CastInst(mulCast1->getOperand(0), VT, "cast", adds),
						    (shrCast == adds->getOperand(1)) ? adds->getOperand(2) : adds->getOperand(1),
						    "mradds", adds);
	adds->replaceAllUsesWith(mradds);
	instructionsToDelete.insert(&BO);
	instructionsToDelete.insert(add);
	instructionsToDelete.insert(shr);
	instructionsToDelete.insert(mulCast0);
	instructionsToDelete.insert(mulCast1);
	instructionsToDelete.insert(addCast0);
	instructionsToDelete.insert(VImm);
	instructionsToDelete.insert(shrCast);
	instructionsToDelete.insert(adds);
	changed = true;
	return;
      }    
      // Check for mladd pattern
      //
      CallInst *call = VectorUtils::getCallInst(VT, getAltiVecName("mladd", VT),
						BO.getOperand(0), BO.getOperand(1),
						(use->getOperand(0) == &BO) ? use->getOperand(1) : use->getOperand(0),
						"mladd", use);
      use->replaceAllUsesWith(call);
      instructionsToDelete.insert(&BO);
      instructionsToDelete.insert(use);
      changed = true;
      break;
    }
    case Instruction::Shr: {
      // Check for madds pattern
      //
      if (!use->hasOneUse())
	return;
      ConstantUInt *shamt = dyn_cast<ConstantUInt>(use->getOperand(1));
      if (!shamt || (shamt->getValue() != 15))
	return;
      CastInst *op0 = dyn_cast<CastInst>(BO.getOperand(0));
      CastInst *op1 = dyn_cast<CastInst>(BO.getOperand(1));
      CastInst *use2 = dyn_cast<CastInst>(*use->use_begin());
      VT = dyn_cast<FixedVectorType>(op0->getOperand(0)->getType());
      if (op0 && op1 && use2 && VT) {
	CallInst *madds =
	  VectorUtils::getCallInst(VT, getAltiVecName("madds", VT),
				   op0->getOperand(0), op1->getOperand(0),
				   ConstantExpr::getCast(ConstantSInt::get(Type::IntTy, 0), VT),
				   "madds", &BO);
	use2->replaceAllUsesWith(madds);
	instructionsToDelete.insert(&BO);
	instructionsToDelete.insert(use);
	instructionsToDelete.insert(op0);
	instructionsToDelete.insert(op1);
	instructionsToDelete.insert(use2);
	changed = true;
      }
      break;
    }
    default:
      break;
    }
  }

  void AltiVec::visitCallInst(CallInst &CI) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(CI.getType());
    if (!VT || !isProperType(VT)) return;
    Function *callee = CI.getCalledFunction();
    if (!callee) return;
    std::string calleeName = callee->getName();
    std::string prefix = calleeName.substr(0, 6);
    if (prefix != "vllvm_") return;
    unsigned pos = calleeName.find("_", 6);
    if (pos == std::string::npos) {
      std::cerr << "Bad syntax for Vector-LLVM intrinsic " << calleeName << "\n";
      exit(1);
    }
    std::string shortName = calleeName.substr(6, pos-6);
    if (shortName == "saturate") {
      return;
    } else {
      std::vector<Value*> args;
      for (unsigned i = 1; i < CI.getNumOperands(); ++i)
	args.push_back(CI.getOperand(i));
      CallInst *call = VectorUtils::getCallInst(VT, getAltiVecName(shortName, VT),
						args, "vec_func", &CI);
      CI.replaceAllUsesWith(call);
      instructionsToDelete.insert(&CI);
    }
    changed = true;
  }

}
