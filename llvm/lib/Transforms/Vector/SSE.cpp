//===- SSE.cpp - Raise significant functions to Vector-LLVM ------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file takes blocked Vector-LLVM code and puts it in a form that
// can be passed to the SSE C Backend.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "SSE"

#include <sstream>
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

using namespace llvm;

namespace {


  //===----------------------------------------------------------------------===//
  //                          Class definitions
  //===----------------------------------------------------------------------===//

  class SSE : public FunctionPass, public InstVisitor<SSE> {

  public:
    bool runOnFunction(Function &F);
    void visitCastInst(CastInst &);
    void visitVImmInst(VImmInst &);
    void visitCombineInst(CombineInst &);
    void visitVSelectInst(VSelectInst &);
    void visitAdd(BinaryOperator &);
    void visitMul(BinaryOperator &);
    void visitSetCondInst(SetCondInst &);
    void visitSub(BinaryOperator &);
    void visitCallInst(CallInst &);
    void visitShiftInst(ShiftInst &);
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
    void addComposeConstant(BinaryOperator&,Value*,Value*);
  }; 

  RegisterOpt<SSE> X("sse",
		     "SSE code generation pre-pass");


  //===----------------------------------------------------------------------===//
  //                     Helper functions
  //===----------------------------------------------------------------------===//

  static unsigned getVectorSize(const Type* Ty) {
    return 128 / (8 * Ty->getPrimitiveSize());
  }

  /// Check whether the type is one that SSE can handle; if not,
  /// it must be lowered later.
  ///
  bool isProperType(const VectorType *VT) {
    // Only fixed vector types are allowed
    //
    const FixedVectorType *FVT = dyn_cast<FixedVectorType>(VT);
    if (!FVT) return false;
    // Vector size must be appropriate
    //
    return (FVT->getNumElements() == getVectorSize(FVT->getElementType()));
  }

  static std::string getSSESuffix(const FixedVectorType *VecTy) {
    const Type *ElTy = VecTy->getElementType();
    std::ostringstream os;
    if (ElTy->isIntegral()) {
      if (ElTy->isSigned())
	os << "epi" << 8*ElTy->getPrimitiveSize();
      else
	os << "epu" << 8*ElTy->getPrimitiveSize();
    } else {
      std::cerr << "Can't yet handle this type!\n"
		<< VecTy->getDescription() << "\n";
      exit(1);
    }
    return os.str();
  }

  static std::string getSSEName(std::string baseName, const FixedVectorType *VecTy) {
    return "_mm_" + baseName + "_" + getSSESuffix(VecTy);
  }

  static bool isCall(Value *V, const std::string &name) {
    if (!V) return false;
    if (CallInst *CI = dyn_cast<CallInst>(V))
      if (Function *F = CI->getCalledFunction())
	if (F->getName().substr(0, name.length()) == name)
	  return true;
    return false;
  }

  static bool isMMCall(Value *V, const std::string &name) {
    return isCall(V, "_mm_" + name);
  }

  static bool isComposeIntrinsic(Value *V) {
    if (CallInst *CI = dyn_cast<CallInst>(V)) {
      if (Function *F = CI->getCalledFunction()) {
	if (F->getName().substr(0, 7) == "compose")
	  return true;
      }
    }
    return false;
  }

  static bool isFullCompose(CallInst *CI) {
    if (Function *F = CI->getCalledFunction()) {
      if (F && F->getName().substr(0, 11) == "fullCompose")
	return true;
    }
    return false;
  }


  //===----------------------------------------------------------------------===//
  //                     SSE implementation
  //===----------------------------------------------------------------------===//

  /// Main function called by PassManager
  ///
  bool SSE::runOnFunction(Function &F) {
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

  void SSE::visitVImmInst(VImmInst &VL) {
    const FixedVectorType *VT = dyn_cast<FixedVectorType>(VL.getType());
    assert(VT && "Vimm must be fixed vector type!\n");
    CallInst *call = VectorUtils::getCallInst(VT, getSSEName("splat", VT),
					      VL.getOperand(0), "splat", &VL);
    VL.replaceAllUsesWith(call);
    instructionsToDelete.insert(&VL);
    changed = true;
  }

  void SSE::visitCastInst(CastInst &CI) {
    const VectorType *VT = dyn_cast<VectorType>(CI.getType());
    if (!VT || !isProperType(VT)) 
      return;
    if (isa<VectorType>(CI.getOperand(0)->getType())) {
      CallInst *op0 = dyn_cast<CallInst>(CI.getOperand(0));
      if (op0 && isComposeIntrinsic(op0) && (CI.getType() == op0->getOperand(1)->getType())) {
	CallInst *Or = VectorUtils::getCallInst(VT, "_mm_or_si128", op0->getOperand(1),
						op0->getOperand(2), "or", &CI);
	CI.replaceAllUsesWith(Or);
	instructionsToDelete.insert(&CI);
	instructionsToDelete.insert(op0);
	changed = true;
      } else if (op0 && isFullCompose(op0)) {
	if (const FixedVectorType *LongVT = dyn_cast<FixedVectorType>(op0->getType())) { 
	  CallInst *Pack = VectorUtils::getCallInst(VT, getSSEName("pack", LongVT),
						    op0->getOperand(1), op0->getOperand(2),
						    "pack", &CI);
	  CI.replaceAllUsesWith(Pack);
	  instructionsToDelete.insert(&CI);
	  instructionsToDelete.insert(op0);
	  changed = true;
	}
      }
    }
  }
  
  void SSE::visitCombineInst(CombineInst &CI) {
    Instruction *combine1 = cast<CombineInst>(&CI);
    Value *v1 = CI.getOperand(0);
    Value *v2 = CI.getOperand(1);
    // If the destination is a combine instruction, do nothing; if
    // necessary, we'll handle the first combine instruction in the
    // series.
    //
    if (isa<CombineInst>(v1))
      return;
    // We must have two fixed-vector operands, and the first must have
    // twice as many elements as the second.  Also, the second must
    // have proper type (but the first need not).
    //
    const FixedVectorType *VT1 = dyn_cast<FixedVectorType>(v1->getType());
    if (!VT1) return;
    const FixedVectorType *VT2 = dyn_cast<FixedVectorType>(v2->getType());
    if (!VT2) return;
    if (VT1->getNumElements() != 2*VT2->getNumElements()) return;
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
      // Check for _mm_packs pattern.  Second combine must have
      // exactly one use, and it must be a cast to an appropriate
      // type.
      //
      Instruction *use = dyn_cast<Instruction>(*combine2->use_begin());
      if (!use) return;
      const FixedVectorType *VT = dyn_cast<FixedVectorType>(use->getType());
      if (!VT) return;
      if (VT->getNumElements() != VT1->getNumElements()) return;
      if (!isa<CastInst>(use))
	return;
      Value *op1, *op2;
      op1 = v2;
      op2 = combine2->getOperand(1);
      // Right now this only works for signed values
      //
      Value *eight = ConstantUInt::get(Type::UByteTy, 8);
      op1 = VectorUtils::getCallInst(VT2, getSSEName("slli", VT2),
				     op1, eight, "slli", &CI);
      op1 = VectorUtils::getCallInst(VT2, getSSEName("srai", VT2),
				     op1, eight, "srai", &CI);
      op2 = VectorUtils::getCallInst(VT2, getSSEName("slli", VT2),
				     op2, eight, "slli", &CI);
      op2 = VectorUtils::getCallInst(VT2, getSSEName("srai", VT2),
				     op2, eight, "srai", &CI);
      use->replaceAllUsesWith(VectorUtils::getCallInst(VT, getSSEName("packs", VT1),
						       op1, op2, "packs", &CI));
      instructionsToDelete.insert(use);
      instructionsToDelete.insert(combine2);
      instructionsToDelete.insert(&CI);
      Instruction *op0 = dyn_cast<Instruction>(CI.getOperand(0));
      if (op0)
	instructionsToDelete.insert(op0);
      changed = true;
    }  else if (combine2->hasNUses(2)) {
      Value::use_iterator I = combine2->use_begin();
      ExtractInst *extract0 = dyn_cast<ExtractInst>(*I++);
      ExtractInst *extract1 = dyn_cast<ExtractInst>(*I);
      assert(extract0 && extract1);
      CallInst *unpackhi = VectorUtils::getCallInst(VT2, getSSEName("unpackhi", VT2),
						  combine1->getOperand(1), combine2->getOperand(1),
						  "unpackhi", extract0);
      CallInst *unpacklo = VectorUtils::getCallInst(VT2, getSSEName("unpacklo", VT2),
						  combine1->getOperand(1), combine2->getOperand(1),
						  "unpacklo", extract0);
      if (cast<ConstantUInt>(extract0->getOperand(1))->getValue() == 1) {
	extract0->replaceAllUsesWith(unpackhi);
	extract1->replaceAllUsesWith(unpacklo);
      } else {
	extract0->replaceAllUsesWith(unpacklo);
	extract1->replaceAllUsesWith(unpackhi);
      }
      instructionsToDelete.insert(combine1);
      instructionsToDelete.insert(combine2);
      instructionsToDelete.insert(extract0);
      instructionsToDelete.insert(extract1);
      Instruction *op0 = dyn_cast<Instruction>(CI.getOperand(0));
      if (op0)
	instructionsToDelete.insert(op0);
      changed = true;
    }
  }

  void SSE::visitVSelectInst(VSelectInst &VI) {
    const FixedVectorType *VT = dyn_cast<FixedVectorType>(VI.getType());
    if (!VT || !isProperType(VT)) return;
    Value *mask = VI.getOperand(0);
    CallInst *And = VectorUtils::getCallInst(VT, "_mm_and_si128", VI.getOperand(1), 
					     mask, "and", &VI);
    CallInst *AndNot = VectorUtils::getCallInst(VT, "_mm_andnot_si128", mask, 
						VI.getOperand(2), "andnot", &VI);
    CallInst *Or = VectorUtils::getCallInst(VT, "_mm_or_si128", And, AndNot,
					    "or", &VI);
    VI.replaceAllUsesWith(Or);
    instructionsToDelete.insert(&VI);
    changed = true;
  }

  void SSE::visitShiftInst(ShiftInst &SI) {
    const FixedVectorType *VT = dyn_cast<FixedVectorType>(SI.getType());
    if (!VT) return;
    CallInst *CI = dyn_cast<CallInst>(SI.getOperand(0));
    if (CI && isFullCompose(CI)) {
      Value *op1 = CI->getOperand(1);
      Value *op2 = CI->getOperand(2);
      VT = cast<FixedVectorType>(op1->getType());
      std::string shortName;
      if (SI.getOpcode() == Instruction::Shl)
	shortName = "slli";
      else if (VT->getElementType()->isSigned())
	shortName = "srai";
      else
	shortName = "srli";
      CallInst *shiftLo = VectorUtils::getCallInst(VT, getSSEName(shortName, VT),
						   op1, SI.getOperand(1), "shiftLo", &SI);
      CallInst *shiftHi = VectorUtils::getCallInst(VT, getSSEName(shortName, VT),
						   op2, SI.getOperand(1), "shiftHi", &SI);
      CallInst *fullCompose = new CallInst(CI->getCalledFunction(), shiftLo,
					   shiftHi, "fullCompose", &SI);
      SI.replaceAllUsesWith(fullCompose);
      instructionsToDelete.insert(&SI);
      instructionsToDelete.insert(CI);
      changed = true;
    } else if (CI && isComposeIntrinsic(CI)) {
      const FixedVectorType* Ty = cast<FixedVectorType>(CI->getOperand(1)->getType());
      Value *shamtLo = SI.getOperand(1);
      Instruction *shamtHi = BinaryOperator::create(Instruction::Sub, ConstantUInt::get(Type::UByteTy, 16),
						    shamtLo, "sub", &SI);
      Instruction *lo = VectorUtils::getCallInst(Ty, "_mm_srli_" + getSSESuffix(Ty),
						 CI->getOperand(1), shamtLo, "lo", &SI);
      Instruction *hi = VectorUtils::getCallInst(Ty, "_mm_slli_" + getSSESuffix(Ty),
						 CI->getOperand(2), shamtHi, "hi", &SI);
      Instruction *compose = new CallInst(CI->getCalledFunction(), lo, hi, "compose", &SI);
      SI.replaceAllUsesWith(compose);
      instructionsToDelete.insert(&SI);
      instructionsToDelete.insert(CI);
      changed = true;
    } else {
      std::string shortName;
      if (SI.getOpcode() == Instruction::Shr) {
	if (VT->getElementType()->isSigned())
	  shortName = "srai";
	else
	  shortName = "srli";
      } else {
	shortName = "slli";
      }
      CallInst *shift = VectorUtils::getCallInst(VT, getSSEName(shortName, VT),
						 SI.getOperand(0), SI.getOperand(1),
						 "shift", &SI);
      SI.replaceAllUsesWith(shift);
      instructionsToDelete.insert(&SI);
      changed = true;
    }
  }

  static const Type *getSignedType(const Type *Ty) {
    if (Ty->isSigned())
      return Ty;
    switch(Ty->getTypeID()) {
    case Type::UIntTyID:
      return Type::IntTy;
    default:
      std::cerr << "Can't handle type " << Ty->getDescription() << "\n";
    }
    return 0;
  }

  void SSE::addComposeConstant(BinaryOperator &Add,
			      Value *arg1, Value *arg2) {
    CallInst *compose = dyn_cast<CallInst>(arg1);//Add.getOperand(0));
    if (!compose || !isComposeIntrinsic(compose) || !compose->hasOneUse()) return;
    Value *op1 = compose->getOperand(1);
    Value *op2 = compose->getOperand(2);
    CastInst *addCast0 = dyn_cast<CastInst>(arg2);//Add.getOperand(1));
    if (!addCast0) return;
    CallInst *splat = dyn_cast<CallInst>(addCast0->getOperand(0));
    if (!isMMCall(splat, "splat")) return;
    Constant *C = dyn_cast<Constant>(splat->getOperand(1));
    if (!C) return;
    const FixedVectorType *LongVT = dyn_cast<FixedVectorType>(Add.getType());
    const FixedVectorType *ShortVT = dyn_cast<FixedVectorType>(op1->getType());
    if (!LongVT || !ShortVT) return;
    const FixedVectorType *HalfVT = FixedVectorType::get(getSignedType(LongVT->getElementType()), 
							 LongVT->getNumElements() / 2);
    CallInst *splat2 = VectorUtils::getCallInst(HalfVT, getSSEName("splat", HalfVT),
						C, "splat", &Add);
    CallInst *unpackLo = VectorUtils::getCallInst(HalfVT, getSSEName("unpacklo", ShortVT),
						  op1, op2, "unpackLo", &Add);
    CallInst *unpackHi = VectorUtils::getCallInst(HalfVT, getSSEName("unpackhi", ShortVT),
						  op1, op2, "unpackHi", &Add);
    CallInst *addLo = VectorUtils::getCallInst(HalfVT, getSSEName("add", HalfVT),
					       unpackLo, splat2, "addLo", &Add);
    CallInst *addHi = VectorUtils::getCallInst(HalfVT, getSSEName("add", HalfVT),
					       unpackHi, splat2, "addHi", &Add);
    CallInst *fullCompose = VectorUtils::getCallInst(LongVT, "fullCompose_" + HalfVT->getElementType()->getDescription(),
						     addLo, addHi, "fullCompose", &Add);
    Add.replaceAllUsesWith(fullCompose);
    instructionsToDelete.insert(addCast0);
    instructionsToDelete.insert(splat);
    instructionsToDelete.insert(compose);
    instructionsToDelete.insert(&Add);
    changed = true;
  }

  /// FIXME: This is very specialized to the form add(compose,
  /// cast(cast(constant))).  Generalize this!!!
  ///
  void SSE::visitAdd(BinaryOperator &Add) {
    addComposeConstant(Add, Add.getOperand(0), Add.getOperand(1));
    addComposeConstant(Add, Add.getOperand(1), Add.getOperand(0));
  }

  void SSE::visitMul(BinaryOperator &BO) {
    CastInst *op0 = dyn_cast<CastInst>(BO.getOperand(0));
    CastInst *op1 = dyn_cast<CastInst>(BO.getOperand(1));
    if (!op0 || !op1) return;
    const FixedVectorType *Ty = dyn_cast<FixedVectorType>(op0->getType());
    const FixedVectorType* RetTy = dyn_cast<FixedVectorType>(op0->getOperand(0)->getType());
    if (Ty && RetTy) {
      Instruction *hi = VectorUtils::getCallInst(RetTy, "_mm_mulhi_" + getSSESuffix(RetTy),
						 op0->getOperand(0), op1->getOperand(0),
						 "mul", &BO);
      Instruction *lo = VectorUtils::getCallInst(RetTy, "_mm_mullo_" + getSSESuffix(RetTy),
						 op0->getOperand(0), op1->getOperand(0),
						 "mul", &BO);
      Instruction *result = VectorUtils::getCallInst(Ty, "compose_" + RetTy->getElementType()->getDescription(),
						     lo, hi, "compose", &BO);
      BO.replaceAllUsesWith(result);
      instructionsToDelete.insert(&BO);
      instructionsToDelete.insert(op0);
      instructionsToDelete.insert(op1);
      changed = true;
    }
  }

  void SSE::visitSetCondInst(SetCondInst &BO) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(BO.getOperand(0)->getType());
    if (!VT) return;
    std::string name;
    switch(BO.getOpcode()) {
    case Instruction::VSetGT:
      name = "cmpgt";
      break;
    default:
      std::cerr << "Can't handle instruction " << BO;
      exit(1);
    }
    std::string fullName = "_mm_" + name + "_" + getSSESuffix(VT);
    CallInst *call = 
      VectorUtils::getCallInst(BO.getType(), fullName,
			       BO.getOperand(0), BO.getOperand(1),
			       "cmp", &BO);
    BO.replaceAllUsesWith(call);
    instructionsToDelete.insert(&BO);
    changed = true;
  }

  void SSE::visitSub(BinaryOperator &BO) {
    const FixedVectorType *VT = 
      dyn_cast<FixedVectorType>(BO.getOperand(0)->getType());
    if (!VT) return;
    CallInst *sub = VectorUtils::getCallInst(VT, getSSEName("sub", VT),
					     BO.getOperand(0), BO.getOperand(1),
					     "sub", &BO);
    BO.replaceAllUsesWith(sub);
    instructionsToDelete.insert(&BO);
    changed = true;
  }

  void SSE::visitCallInst(CallInst &CI) {
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
      std::string fullName = "_mm_" + shortName + "_" + 
	getSSESuffix(VT);
      std::vector<Value*> args;
      for (unsigned i = 1; i < CI.getNumOperands(); ++i)
	args.push_back(CI.getOperand(i));
      CallInst *call = VectorUtils::getCallInst(VT, fullName, args, "mm_call", &CI); 
      CI.replaceAllUsesWith(call);
      instructionsToDelete.insert(&CI);
    }
    changed = true;
  }

}
