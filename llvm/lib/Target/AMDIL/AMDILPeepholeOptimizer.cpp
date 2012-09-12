//===-- AMDILPeepholeOptimizer.cpp ----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "peepholeopt"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME 0
#endif

#include "AMDILAlgorithms.tpp"
#include "AMDILDevices.h"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILModuleInfo.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Instructions.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Transforms/Utils/Local.h"
#include <sstream>
STATISTIC(PointerAssignments, "Number of dynamic pointer "
          "assigments discovered");
STATISTIC(PointerSubtract, "Number of pointer subtractions discovered");
STATISTIC(LocalFuncs, "Number of get_local_size(N) functions removed");

using namespace llvm;
// The Peephole optimization pass is used to do simple last minute optimizations
// that are required for correct code or to remove redundant functions
namespace {
class LLVM_LIBRARY_VISIBILITY AMDILPeepholeOpt : public FunctionPass {
public:
  TargetMachine &TM;
  static char ID;
  AMDILPeepholeOpt(TargetMachine &tm, CodeGenOpt::Level OL);
  ~AMDILPeepholeOpt();
  const char *getPassName() const;
  bool runOnFunction(Function &F);
  bool doInitialization(Module &M);
  bool doFinalization(Module &M);
  void getAnalysisUsage(AnalysisUsage &AU) const;
protected:
private:
  // Function to initiate all of the instruction level optimizations.
  // Returns true if a new instruction was generated and false otherwise.
  bool instLevelOptimizations(Instruction *inst);
  // Quick check to see if we need to dump all of the pointers into the
  // arena. If this is correct, then we set all pointers to exist in arena. This
  // is a workaround for aliasing of pointers in a struct/union.
  bool dumpAllIntoArena(Function &F);
  // Because I don't want to invalidate any pointers while in the
  // safeNestedForEachFunction. I push atomic conversions to a vector and handle
  // it later. This function does the conversions if required.
  void doAtomicConversionIfNeeded(Function &F);
  // Because __amdil_is_constant cannot be properly evaluated if
  // optimizations are disabled, the call's are placed in a vector
  // and evaluated after the __amdil_image* functions are evaluated
  // which should allow the __amdil_is_constant function to be
  // evaluated correctly.
  void doIsConstCallConversionIfNeeded();
  bool mChanged;
  bool mDebug;
  bool mRWGOpt;
  bool mConvertAtomics;
  CodeGenOpt::Level optLevel;
  // Run a series of tests to see if we can optimize a CALL instruction.
  bool optimizeCallInst(Instruction *inst);
  // A peephole optimization to optimize bit extract sequences.
  bool optimizeBitExtract(Instruction *inst);
  // A peephole optimization to optimize bit insert sequences.
  bool optimizeBitInsert(Instruction *inst);
  // A peephole optimization that does the following transform:
  // ((((B ^ -1) | C) & A) ^ -1)
  // ==>
  // BFI(A, (B & (C ^ -1)), -1)
  bool optimizeBFI(Instruction *inst);
  bool setupBitInsert(Instruction *base,
                      Instruction *&src,
                      Constant *&mask,
                      Constant *&shift,
                      bool &inv_mode);
  // A peephole optimization to optimize [d]class calls that or the results.
  bool optimizeClassInst(Instruction *inst);
  // Generate F2U4 intrinisic
  bool genIntrF2U4(Instruction *inst);

  // On 7XX and 8XX operations, we do not have 24 bit signed operations. So in
  // this case we need to expand them. These functions check for 24bit functions
  // and then expand.
  bool isSigned24BitOps(CallInst *CI);
  void expandSigned24BitOps(CallInst *CI);
  // One optimization that can occur is that if the required workgroup size is
  // specified then the result of get_local_size is known at compile time and
  // can be returned accordingly.
  bool isRWGLocalOpt(CallInst *CI);
  void expandRWGLocalOpt(CallInst *CI);
  // On northern island cards, the division is slightly less accurate than on
  // previous generations, so we need to utilize a more accurate division. So we
  // can translate the accurate divide to a normal divide on all other cards.
  bool convertAccurateDivide(CallInst *CI);
  void expandAccurateDivide(CallInst *CI);
  // If the alignment is set incorrectly, it can produce really inefficient
  // code. This checks for this scenario and fixes it if possible.
  bool correctMisalignedMemOp(Instruction *inst);

  // If we are in no opt mode, then we need to make sure that
  // local samplers are properly propagated as constant propagation
  // doesn't occur and we need to know the value of kernel defined
  // samplers at compile time.
  bool propagateSamplerInst(CallInst *CI);

  LLVMContext *mCTX;
  Function *mF;
  const AMDILSubtarget *mSTM;
  AMDILModuleInfo *mAMI;
  SmallVector< std::pair<CallInst *, Function *>, 16> atomicFuncs;
  SmallVector<CallInst *, 16> isConstVec;
}; // class AMDILPeepholeOpt
char AMDILPeepholeOpt::ID = 0;

/*
   getMaskBitfield() returns true if 'val' is a mask, which is defined to
   be a value whose 1 bits (in binary format) are all next to each other.
   And 'start_bit' set to the first 1 bit and 'bitwidth' sets to the width
   of all the 1-bits (the number of 1 bits). Bit number always starts from 0.

   For exaample:  given that val = 0xFF00;  start_bit = 8 and bitwidth = 8.
*/
bool
getMaskBitfield(unsigned int val,
                unsigned int &start_bit,
                unsigned int &bitwidth)
{
  if (val == 0) {
    // zero, no bitfield
    return false;
  }

  bitwidth = 0;
  start_bit = 0;
  while ((val & 1) == 0) {
    ++start_bit;
    val = (val >> 1);
  }
  if (val > 0) {
    while ( (val & 1)  == 1) {
      ++bitwidth;
      val = (val >> 1);
    }
  }

  if (val > 0) {
    // non-continguous 1 bits.
    return false;
  }
  return true;
}
bool getVectorComponent(Instruction *inst, int tid, unsigned int numElem,
                        Value*& vecval, unsigned& whichelem)
{
  ExtractElementInst *einst = dyn_cast<ExtractElementInst>(inst);
  if (!einst) {
    return false;
  }

  vecval = einst->getVectorOperand();
  VectorType *vt = dyn_cast<VectorType>(vecval->getType());
  assert (
    vt && "ExtractElementInst must have a vector type as its first argument");
  Type *et = vt->getElementType();
  if ( (vt->getNumElements() != numElem) ||
       (et->getTypeID() != tid) ) {
    return false;
  }
  ConstantInt *cv = dyn_cast<ConstantInt>(einst->getIndexOperand());
  if (!cv) {
    return false;
  }

  whichelem = (unsigned)cv->getZExtValue();
  return true;
}
bool getIntValue(Instruction *Inst, Value *& Src, unsigned int &src_start,
                 unsigned int &dst_start, unsigned int &dst_width)
{
  Value *intval, *opnd1;
  bool hasmask = false;

  if (!Inst->getType()->isIntegerTy(32)) {
    return false;
  }
  intval = Inst;

  unsigned int start_pos = 0;
  unsigned int nbits = 32;
  if (Inst->getOpcode() == Instruction::And) {
    intval = Inst->getOperand(0);
    opnd1 = Inst->getOperand(1);

    ConstantInt *CI0 = dyn_cast<ConstantInt>(intval);
    ConstantInt *CI1 = dyn_cast<ConstantInt>(opnd1);
    if ((!CI0 && !CI1) || (CI0 && CI1)) {
      return false;
    }

    if (CI0) {
      Value *tmp = intval;
      intval = opnd1;
      opnd1 = tmp;
      CI1 = CI0;
    }

    unsigned int mask = CI1->getZExtValue();
    hasmask = getMaskBitfield(mask, start_pos, nbits);
    if (!hasmask) {
      return false;
    }
  }

  Instruction *tinst = dyn_cast<Instruction>(intval);
  if (!tinst) {
    return false;
  }

  unsigned int src_pos = start_pos;
  if (tinst->getOpcode() == Instruction::Shl) {
    ConstantInt *CI = dyn_cast<ConstantInt>(tinst->getOperand(1));
    if (hasmask && CI) {
      unsigned int amt = CI->getZExtValue();
      if (amt > src_pos) {
        return false;
      }
      src_pos -= amt;
    } else if (!hasmask && CI) {
      unsigned int amt = CI->getZExtValue();
      Instruction *tinst1 = dyn_cast<Instruction>(tinst->getOperand(0));
      if (tinst1 && tinst1->getOpcode() == Instruction::LShr) {
        //  {shl; Lshr} pattern
        if (ConstantInt *CI1 = dyn_cast<ConstantInt>(tinst1->getOperand(1))) {
          unsigned int amt1 = CI1->getZExtValue();
          if (amt >= amt1) {
            start_pos = amt - amt1;
            src_pos   = 0;
            nbits = 32 - amt;

            intval = tinst1->getOperand(0);
          } else {
            return false;
          }
        }
      } else if (amt < 32) {
        // Only shl
        start_pos = amt;
        src_pos   = 0;
        nbits = 32 - amt;

        intval = tinst->getOperand(0);
      }
    } else {
      return false;
    }
    intval = tinst->getOperand(0);
  }

  Src = intval;
  src_start = src_pos;
  dst_start = start_pos;
  dst_width = nbits;
  return true;
}
} // anonymous namespace

namespace llvm {
FunctionPass *
createAMDILPeepholeOpt(TargetMachine &tm, CodeGenOpt::Level OL)
{
  return new AMDILPeepholeOpt(tm, OL);
}
} // llvm namespace

AMDILPeepholeOpt::AMDILPeepholeOpt(TargetMachine &tm, CodeGenOpt::Level OL)
  : FunctionPass(ID), TM(tm)
{
  mDebug = DEBUGME;
  optLevel = OL;
}
AMDILPeepholeOpt::~AMDILPeepholeOpt()
{
}
const char *
AMDILPeepholeOpt::getPassName() const
{
  return "AMDIL PeepHole Optimization Pass";
}
bool
containsPointerType(Type *Ty)
{
  if (!Ty) {
    return false;
  }
  switch(Ty->getTypeID()) {
  default:
    return false;
  case Type::StructTyID: {
    const StructType *ST = dyn_cast<StructType>(Ty);
    for (StructType::element_iterator stb = ST->element_begin(),
         ste = ST->element_end(); stb != ste; ++stb) {
      if (!containsPointerType(*stb)) {
        continue;
      }
      return true;
    }
    break;
  }
  case Type::VectorTyID:
  case Type::ArrayTyID:
    return containsPointerType(dyn_cast<SequentialType>(Ty)->getElementType());
  case Type::PointerTyID:
    return true;
  };
  return false;
}
bool
AMDILPeepholeOpt::dumpAllIntoArena(Function &F)
{
  bool dumpAll = false;
  for (Function::const_arg_iterator cab = F.arg_begin(),
       cae = F.arg_end(); cab != cae; ++cab) {
    const Argument *arg = cab;
    const PointerType *PT = dyn_cast<PointerType>(arg->getType());
    if (!PT) {
      continue;
    }
    Type *DereferencedType = PT->getElementType();
    if (!dyn_cast<StructType>(DereferencedType)
        ) {
      continue;
    }
    if (!containsPointerType(DereferencedType)) {
      continue;
    }
    // FIXME: Because a pointer inside of a struct/union may be aliased to
    // another pointer we need to take the conservative approach and place all
    // pointers into the arena until more advanced detection is implemented.
    dumpAll = true;
  }
  return dumpAll;
}
void
AMDILPeepholeOpt::doIsConstCallConversionIfNeeded()
{
  if (isConstVec.empty()) {
    return;
  }
  for (unsigned x = 0, y = isConstVec.size(); x < y; ++x) {
    CallInst *CI = isConstVec[x];
    Constant *CV = dyn_cast<Constant>(CI->getOperand(0));
    Type *aType = Type::getInt32Ty(*mCTX);
    Value *Val = (CV != NULL) ? ConstantInt::get(aType, 1)
                 : ConstantInt::get(aType, 0);
    CI->replaceAllUsesWith(Val);
    CI->eraseFromParent();
  }
  isConstVec.clear();
}
void
AMDILPeepholeOpt::doAtomicConversionIfNeeded(Function &F)
{
  // Don't do anything if we don't have any atomic operations.
  if (atomicFuncs.empty()) {
    return;
  }
  // Change the function name for the atomic if it is required
  uint32_t size = atomicFuncs.size();
  for (uint32_t x = 0; x < size; ++x) {
    atomicFuncs[x].first->setOperand(
      atomicFuncs[x].first->getNumOperands()-1,
      atomicFuncs[x].second);
  }
  mChanged = true;
  if (mConvertAtomics) {
    return;
  }
  // If we did not convert all of the atomics, then we need to make sure that
  // the atomics that were not converted have their base pointers set to use the
  // arena path.
  Function::arg_iterator argB = F.arg_begin();
  Function::arg_iterator argE = F.arg_end();
  AMDILKernelManager *KM = mSTM->getKernelManager();
  AMDILMachineFunctionInfo *mMFI = getAnalysis<MachineFunctionAnalysis>().getMF()
                                   .getInfo<AMDILMachineFunctionInfo>();
  for (; argB != argE; ++argB) {
    if (mSTM->device()->isSupported(AMDILDeviceInfo::ArenaUAV)) {
      KM->setUAVID(argB,mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID));
      mMFI->uav_insert(mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID));
    } else {
      KM->setUAVID(argB,mSTM->device()->getResourceID(AMDILDevice::GLOBAL_ID));
      mMFI->uav_insert(mSTM->device()->getResourceID(AMDILDevice::GLOBAL_ID));
    }
  }
}
bool
AMDILPeepholeOpt::runOnFunction(Function &MF)
{
  mChanged = false;
  mF = &MF;
  mSTM = &TM.getSubtarget<AMDILSubtarget>();
  if (mDebug) {
    MF.dump();
  }
  mCTX = &MF.getType()->getContext();
  mConvertAtomics = true;
  if (dumpAllIntoArena(MF)) {
    for (Function::const_arg_iterator cab = MF.arg_begin(),
         cae = MF.arg_end(); cab != cae; ++cab) {
      const Argument *arg = cab;
      AMDILKernelManager *KM = mSTM->getKernelManager();
      KM->setUAVID(getBasePointerValue(arg),
                   mSTM->device()->getResourceID(AMDILDevice::GLOBAL_ID));
    }
  }
  mAMI = &(getAnalysis<MachineFunctionAnalysis>().getMF()
           .getMMI().getObjFileInfo<AMDILModuleInfo>());
  const AMDILKernel *kernel = mAMI->getKernel(MF.getName());
  if (kernel && kernel->mKernel && kernel->sgv) {
    mRWGOpt = kernel->sgv->mHasRWG;
  }
  for (inst_iterator I = inst_end(MF), E = inst_begin(MF); I != E; --I) {
    inst_iterator nextI = I;
    Instruction *inst = &*(--nextI);
    // If we don't optimize to a new instruction, decrement the
    // iterator, otherwise test the new instruction for further
    // optimizations.
    if (instLevelOptimizations(inst)) {
      // We have to check against inst_begin at each iteration of the loop
      // as it can be invalidated and 'I' can point to the first instruction.
      E = inst_begin(MF);
      if (I == E) break;
    }
  }

  doAtomicConversionIfNeeded(MF);
  doIsConstCallConversionIfNeeded();

  if (mDebug) {
    MF.dump();
  }
  return mChanged;
}
bool
AMDILPeepholeOpt::optimizeCallInst(Instruction *inst)
{
  CallInst *CI = dyn_cast<CallInst>(inst);

  assert(CI && "optimizeCallInst() expects Call instruction");

  if (isSigned24BitOps(CI)) {
    expandSigned24BitOps(CI);
    CI->eraseFromParent();
    return true;
  }
  if (isRWGLocalOpt(CI)) {
    expandRWGLocalOpt(CI);
    return true;
  }
  if (propagateSamplerInst(CI)) {
    return true;
  }
  if (convertAccurateDivide(CI)) {
    expandAccurateDivide(CI);
    CI->eraseFromParent();
    return true;
  }

  StringRef calleeName = CI->getOperand(CI->getNumOperands()-1)->getName();
  if (calleeName.startswith("__amdil_is_constant")) {
    // If we do not have optimizations, then this
    // cannot be properly evaluated, so we add the
    // call instruction to a vector and process
    // them at the end of processing after the
    // samplers have been correctly handled.
    if (optLevel == CodeGenOpt::None) {
      isConstVec.push_back(CI);
      return false;
    } else {
      Constant *CV = dyn_cast<Constant>(CI->getOperand(0));
      Type *aType = Type::getInt32Ty(*mCTX);
      Value *Val = (CV != NULL) ? ConstantInt::get(aType, 1)
                   : ConstantInt::get(aType, 0);
      CI->replaceAllUsesWith(Val);
      CI->eraseFromParent();
      return true;
    }
  }

  if (calleeName.equals("__amdil_is_asic_id_i32")) {
    ConstantInt *CV = dyn_cast<ConstantInt>(CI->getOperand(0));
    Type *aType = Type::getInt32Ty(*mCTX);
    Value *Val = CV;
    if (Val) {
      Val = ConstantInt::get(aType,
                             mSTM->device()->getDeviceFlag() & CV->getZExtValue());
    } else {
      Val = ConstantInt::get(aType, 0);
    }
    CI->replaceAllUsesWith(Val);
    CI->eraseFromParent();
    return true;
  }
  Function *F = dyn_cast<Function>(CI->getOperand(CI->getNumOperands()-1));
  if (!F) {
    return false;
  }
  if (F->getName().startswith("__atom") && !CI->getNumUses()
      && !F->getName().startswith("__atomic_load")
      && !F->getName().startswith("__atomic_store")
      && F->getName().find("_xchg") == StringRef::npos
      && F->getName().find("_noret") == StringRef::npos) {
    std::string buffer(F->getName().str() + "_noret");
    std::vector<Type*> callTypes;
    FunctionType *ptr = F->getFunctionType();
    callTypes.insert(callTypes.begin(), ptr->param_begin(), ptr->param_end());
    FunctionType *newFunc = FunctionType::get(Type::getVoidTy(F->getContext()),
                                              callTypes, false);
    std::vector<Value*> args;
    for (unsigned x = 0, y = CI->getNumArgOperands(); x < y; ++x) {
      args.push_back(CI->getArgOperand(x));
    }

    Function *newF = dyn_cast<Function>(
      F->getParent()->getOrInsertFunction(buffer, newFunc));
    CallInst *newCI = CallInst::Create(newF, args);
    newCI->insertAfter(CI);
    CI->eraseFromParent();
    return true;
  }

  if (!mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment)
      && !mSTM->device()->isSupported(AMDILDeviceInfo::MultiUAV)) {
    return false;
  }
  if (!mConvertAtomics) {
    return false;
  }
  StringRef name = F->getName();
  if (name.startswith("__atom") && name.find("_g") != StringRef::npos) {
    Value *ptr = CI->getOperand(0);
    const Value *basePtr = getBasePointerValue(ptr);
    const Argument *Arg = dyn_cast<Argument>(basePtr);
    if (Arg) {
      int32_t id = mAMI->getArgID(Arg);
      if (id >= 0) {
        std::stringstream ss;
        ss << name.data() << "_" << id << '\n';
        std::string val;
        ss >> val;
        F = dyn_cast<Function>(
          F->getParent()->getOrInsertFunction(val, F->getFunctionType()));
        atomicFuncs.push_back(std::make_pair(CI, F));
      } else {
        mConvertAtomics = false;
      }
    } else {
      mConvertAtomics = false;
    }
  }
  return false;
}
bool
AMDILPeepholeOpt::setupBitInsert(Instruction *base,
                                 Instruction *&src,
                                 Constant *&mask,
                                 Constant *&shift,
                                 bool &inv_type)
{
  if (!base) {
    if (mDebug) {
      dbgs() << "Null pointer passed into function.\n";
    }
    return false;
  }
  if (base->getOpcode() == Instruction::Shl) {
    shift = dyn_cast<Constant>(base->getOperand(1));
    inv_type = true;
  } else if (base->getOpcode() == Instruction::And) {
    mask = dyn_cast<Constant>(base->getOperand(1));
    inv_type = false;
  } else {
    if (mDebug) {
      dbgs() << "Failed setup with no Shl or And instruction on base opcode!\n";
    }
    // If the base is neither a Shl or a And, we don't fit any of the patterns above.
    return false;
  }
  src = dyn_cast<Instruction>(base->getOperand(0));
  if (!src) {
    if (mDebug) {
      dbgs() << "Failed setup since the base operand is not an instruction!\n";
    }
    return false;
  }
  if (src->getOpcode() == Instruction::Shl && !shift) {
    shift = dyn_cast<Constant>(src->getOperand(1));
    src = dyn_cast<Instruction>(src->getOperand(0));
  } else if (src->getOpcode() == Instruction::And && !mask) {
    mask = dyn_cast<Constant>(src->getOperand(1));
  }

  if (!mask && !shift) {
    if (mDebug) {
      dbgs() << "Failed setup since both mask and shift are NULL!\n";
    }
    // Did not find a constant mask or a shift.
    return false;
  }
  return true;
}
bool
AMDILPeepholeOpt::optimizeClassInst(Instruction *inst)
{
  assert (inst && (inst->getOpcode() == Instruction::Or) &&
          "optimizeClassInst() expects OR instruction");

  if (optLevel == CodeGenOpt::None) {
    return false;
  }
  // We want to optimize multiple __amdil_class_f[32|64] that are
  // seperated by 'or' instructions into a single call with the
  // second argument or'd together.
  CallInst *LHS = dyn_cast<CallInst>(inst->getOperand(0));
  CallInst *RHS = dyn_cast<CallInst>(inst->getOperand(1));
  if (!LHS || !RHS) {
    return false;
  }
  Value *LHSFunc, *LHSConst, *LHSVar;
  Value *RHSFunc, *RHSConst, *RHSVar;
  LHSFunc = LHS->getOperand(LHS->getNumOperands() - 1);
  LHSConst = dyn_cast<Constant>(LHS->getOperand(1));
  LHSVar = LHS->getOperand(0);
  RHSFunc = RHS->getOperand(RHS->getNumOperands() - 1);
  RHSConst = dyn_cast<Constant>(RHS->getOperand(1));
  RHSVar = RHS->getOperand(0);
  // If the functions aren't the class intrinsic, then fail.
  // If the names are not the same, then fail.
  if ((!LHSFunc->getName().startswith("__amdil_class_f")
       || !RHSFunc->getName().startswith("__amdil_class_f"))
      || LHSFunc->getName() != RHSFunc->getName()) {
    return false;
  }
  // We don't want to merge two class calls from different variables.
  if (LHSVar != RHSVar) {
    return false;
  }
  // If we don't have two constants, then fail.
  if (!LHSConst || !RHSConst) {
    return false;
  }
  Value *Operands[2] = {
    LHSVar,
    LHSConst
  };
  CallInst *newCall = CallInst::Create(dyn_cast<Function>(LHSFunc),
                                       Operands, "new_class");
  // Or the constants together, and then call the function all over again.
  inst->setOperand(0, LHSConst);
  inst->setOperand(1, RHSConst);
  inst->replaceAllUsesWith(newCall);
  newCall->insertAfter(inst);
  newCall->setOperand(1, inst);

  // We need to remove the functions if they only have a single
  // use.
  if (LHS->use_empty()) {
    LHS->eraseFromParent();
  }
  if (RHS->use_empty()) {
    RHS->eraseFromParent();
  }
  return true;
}
bool
AMDILPeepholeOpt::optimizeBitInsert(Instruction *inst)
{
  assert (inst && (inst->getOpcode() == Instruction::Or) &&
          "optimizeBitInserti() expects OR instruction");

  if (optLevel == CodeGenOpt::None) {
    return false;
  }
  // We want to do an optimization on a sequence of ops that in the end equals a
  // single ISA instruction.
  // The base pattern for this optimization is - ((A & B) << C) | ((D & E) << F)
  // Some simplified versions of this pattern are as follows:
  // (A & B) | (D & E) when B & E == 0 && C == 0 && F == 0
  // ((A & B) << C) | (D & E) when B ^ E == 0 && (1 << C) >= E
  // (A & B) | ((D & E) << F) when B ^ E == 0 && (1 << F) >= B
  // (A & B) | (D << F) when (1 << F) >= B
  // (A << C) | (D & E) when (1 << C) >= E
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    // The HD4XXX hardware doesn't support the ubit_insert instruction.
    return false;
  }
  Type *aType = inst->getType();
  bool isVector = aType->isVectorTy();
  int numEle = 1;
  // This optimization only works on 32bit integers.
  if (aType->getScalarType()
      != Type::getInt32Ty(inst->getContext())) {
    return false;
  }
  if (isVector) {
    const VectorType *VT = dyn_cast<VectorType>(aType);
    numEle = VT->getNumElements();
    // We currently cannot support more than 4 elements in a intrinsic and we
    // cannot support Vec3 types.
    if (numEle > 4 || numEle == 3) {
      return false;
    }
  }
  // TODO: Handle vectors.
  if (isVector) {
    if (mDebug) {
      dbgs() << "!!! Vectors are not supported yet!\n";
    }
    return false;
  }
  Instruction *LHSSrc = NULL, *RHSSrc = NULL;
  Constant *LHSMask = NULL, *RHSMask = NULL;
  Constant *LHSShift = NULL, *RHSShift = NULL;
  Instruction *LHS = dyn_cast<Instruction>(inst->getOperand(0));
  Instruction *RHS = dyn_cast<Instruction>(inst->getOperand(1));
  bool lhs_inv_type = false, rhs_inv_type = false;
  if (!setupBitInsert(LHS, LHSSrc, LHSMask, LHSShift, lhs_inv_type)) {
    if (mDebug) {
      dbgs() << "Found an OR Operation that failed setup!\n";
      inst->dump();
      if (LHS) { LHS->dump(); }
      if (LHSSrc) { LHSSrc->dump(); }
      if (LHSMask) { LHSMask->dump(); }
      if (LHSShift) { LHSShift->dump(); }
    }
    // There was an issue with the setup for BitInsert.
    return false;
  }
  if (!setupBitInsert(RHS, RHSSrc, RHSMask, RHSShift, rhs_inv_type)) {
    if (mDebug) {
      dbgs() << "Found an OR Operation that failed setup!\n";
      inst->dump();
      if (RHS) { RHS->dump(); }
      if (RHSSrc) { RHSSrc->dump(); }
      if (RHSMask) { RHSMask->dump(); }
      if (RHSShift) { RHSShift->dump(); }
    }
    // There was an issue with the setup for BitInsert.
    return false;
  }
  if (mDebug) {
    dbgs() <<
    "Found an OR operation that can possible be optimized to ubit insert!\n";
    dbgs() << "Op:        "; inst->dump();
    dbgs() << "LHS:       ";
    if (LHS) { LHS->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "LHS Src:   ";
    if (LHSSrc) { LHSSrc->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "LHS Mask:  ";
    if (LHSMask) { LHSMask->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "LHS Shift: ";
    if (LHSShift) { LHSShift->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "RHS:       ";
    if (RHS) { RHS->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "RHS Src:   ";
    if (RHSSrc) { RHSSrc->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "RHS Mask:  ";
    if (RHSMask) { RHSMask->dump(); } else { dbgs() << "(None)\n"; }
    dbgs() << "RHS Shift: ";
    if (RHSShift) { RHSShift->dump(); } else { dbgs() << "(None)\n"; }
  }
  Constant *offset = NULL;
  Constant *width = NULL;
  int32_t lhsMaskVal = 0, rhsMaskVal = 0;
  int32_t lhsShiftVal = 0, rhsShiftVal = 0;
  int32_t lhsMaskWidth = 0, rhsMaskWidth = 0;
  int32_t lhsMaskOffset = 0, rhsMaskOffset = 0;
  lhsMaskVal = (int32_t)(LHSMask
                         ? dyn_cast<ConstantInt>(LHSMask)->getZExtValue() : 0);
  rhsMaskVal = (int32_t)(RHSMask
                         ? dyn_cast<ConstantInt>(RHSMask)->getZExtValue() : 0);
  lhsShiftVal = (int32_t)(LHSShift
                          ? dyn_cast<ConstantInt>(LHSShift)->getZExtValue() : 0);
  rhsShiftVal = (int32_t)(RHSShift
                          ? dyn_cast<ConstantInt>(RHSShift)->getZExtValue() : 0);
  lhsMaskWidth = CountPopulation_32(lhsMaskVal);
  rhsMaskWidth = CountPopulation_32(rhsMaskVal);
  lhsMaskOffset = CountTrailingZeros_32(lhsMaskVal);
  rhsMaskOffset = CountTrailingZeros_32(rhsMaskVal);
  // TODO: Handle the case of A & B | D & ~B(i.e. inverted masks).
  if (mDebug) {
    dbgs() << "Found pattern: \'";
    if (lhs_inv_type) {
      dbgs() << "((A" << (LHSShift ? " << C)" : ")");
      dbgs() << (LHSMask ? " & B)" : ")");
    } else {
      dbgs() << "((A" << (LHSMask ? " & B)" : ")");
      dbgs() << (LHSShift ? " << C)" : ")");
    }
    dbgs() << " | ";
    if (rhs_inv_type) {
      dbgs() << "((D" << (RHSMask ? " & E)" : ")");
      dbgs() << (RHSShift ? " << F)\'\n" : ")\'\n");
    } else {
      dbgs() << "((D" << (RHSShift ? " << F)" : ")");
      dbgs() << (RHSMask ? " & E)\'\n" : ")\'\n");
    }
    dbgs() << "A = LHSSrc\t\tD = RHSSrc \n";
    dbgs() << "B = " << lhsMaskVal << "\t\tE = " << rhsMaskVal << "\n";
    dbgs() << "C = " << lhsShiftVal << "\t\tF = " << rhsShiftVal << "\n";
    dbgs() << "width(B) = " << lhsMaskWidth;
    dbgs() << "\twidth(E) = " << rhsMaskWidth << "\n";
    dbgs() << "offset(B) = " << lhsMaskOffset;
    dbgs() << "\toffset(E) = " << rhsMaskOffset << "\n";
    dbgs() << "Constraints: \n";
    dbgs() << "\t(1) (B";
    if (lhs_inv_type) {
      dbgs() << " << C";
    }
    dbgs() << ") ^ (E";
    if (rhs_inv_type) {
      dbgs() << " << F";
    }
    dbgs() << ") == 0\n";
    if (lhsMaskVal) {
      dbgs() << "\t(2-LHS) B is a mask\n";
    } else {
      dbgs() << "\t(2-LHS) C > 0\n";
    }
    if (rhsMaskVal) {
      dbgs() << "\t(2-RHS) E is a mask\n";
    } else {
      dbgs() << "\t(2-RHS) F > 0\n";
    }
    dbgs() << "\t(3-LHS) (B << C) is a mask\n";
    dbgs() << "\t(3-RHS) (E << F) is a mask\n";
    if (lhsMaskWidth) {
      if (lhs_inv_type) {
        dbgs() << "\t(4-LHS) (offset(B) + C) < 32)\n";
      } else {
        dbgs() << "\t(4-LHS) (offset(B) + width(B)) <= 32\n";
      }
    } else {
      dbgs() << "\t(4-LHS) (offset(B) - C > 0\n";
    }
    if (rhsMaskWidth) {
      if (rhs_inv_type) {
        dbgs() << "\t(4-RHS) (offset(E) + F) < 32)\n";
      } else {
        dbgs() << "\t(4-RHS) (offset(E) + width(E)) <= 32\n";
      }
    } else {
      dbgs() << "\t(4-RHS) (offset(E) - F > 0\n";
    }
    if (rhsMaskWidth) {
      if (lhsMaskWidth) {
        if (lhs_inv_type) {
          dbgs() << "\t(5-LHS) (offset(B) + C) >= (width(E) + offset(E) + F)\n";
        } else {
          dbgs() << "\t(5-LHS) offset(B) >= (width(E) + offset(E) + F)\n";
        }
      } else {
        dbgs() << "\t(5-LHS) C >= (width(E) + offset(E) + F)\n";
      }
    } else {
      if (lhsMaskWidth) {
        dbgs() << "\t(5-LHS) (offset(B) + C) >= F\n";
      } else {
        dbgs() << "\t(5-LHS) C >= F\n";
      }
    }
    if (lhsMaskWidth) {
      if (rhsMaskWidth) {
        if (rhs_inv_type) {
          dbgs() << "\t(5-RHS) (offset(E) + F) >= (width(B) + offset(B) + C)\n";
        } else {
          dbgs() << "\t(5-RHS) offset(E) >= (width(B) + offset(B) + C)\n";
        }
      } else {
        dbgs() << "\t(5-RHS) F >= (width(B) + offset(B) + C)\n";
      }
    } else {
      if (rhsMaskWidth) {
        dbgs() << "\t(5-RHS) (offset(E) + F) >= C\n";
      } else {
        dbgs() << "\t(5-RHS) F >= C\n";
      }
    }
  }
  if ((lhsMaskVal ||
       rhsMaskVal) &&
      !((lhsMaskVal <<
         (lhs_inv_type ? lhsShiftVal : 0)) ^
        (rhsMaskVal << (rhs_inv_type ? rhsShiftVal : 0)))) {
    DEBUG(dbgs() << "Failed constraint 1!\n");
    return false;
  }
  if (
    (rhsMaskWidth && lhsMaskWidth && lhs_inv_type &&
     (lhsMaskOffset + lhsShiftVal) >=
     (rhsMaskWidth + rhsMaskOffset + rhsShiftVal)) ||
    (rhsMaskWidth && lhsMaskWidth && !lhs_inv_type && lhsMaskOffset >=
     (rhsMaskWidth + rhsMaskOffset + rhsShiftVal)) ||
    (rhsMaskWidth && !lhsMaskWidth && lhsShiftVal >=
     (rhsMaskWidth + rhsMaskOffset + rhsShiftVal)) ||
    (!rhsMaskWidth && lhsMaskWidth &&
     ((lhsMaskOffset + lhsShiftVal) >= rhsShiftVal)) ||
    (!rhsMaskWidth && !lhsMaskWidth && (lhsShiftVal >= rhsShiftVal))
    ) {
    if (lhsMaskVal) {
      offset =
        ConstantInt::get(aType,
                         lhsMaskOffset +
                         (lhs_inv_type ? lhsShiftVal : (lhsMaskOffset ? -
                                                        lhsShiftVal : 0)),
                         false);
      width = ConstantInt::get(aType, lhsMaskWidth, false);
    } else {
      offset = ConstantInt::get(aType, lhsShiftVal, false);
      width = ConstantInt::get(aType, lhsMaskOffset - lhsShiftVal, false);
    }
    RHSSrc = RHS;
    if ((lhsMaskVal && !isMask_32(lhsMaskVal) && !isShiftedMask_32(lhsMaskVal))
        || (!lhsMaskVal && !lhsShiftVal)) {
      DEBUG(dbgs() << "Failed constraint 2-LHS!\n");
      return false;
    }
    if (lhsShiftVal && !isShiftedMask_32(lhsMaskVal << lhsShiftVal)) {
      DEBUG(dbgs() << "Failed constraint 3-LHS!\n");
      return false;
    }
    if ((lhsMaskVal && (lhsMaskOffset + lhsShiftVal) >= 32)
        || (lhsMaskOffset - lhsShiftVal) > 32) {
      DEBUG(dbgs() << "Failed constraint 4-LHS!\n");
      return false;
    }
    // If we have a mask offset, but we don't have a shift,
    // we need to make sure that the mask offset is returned back to 0.
    if (!LHSShift && lhsMaskOffset) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, LHSSrc, offset,
                                      "MaskShr1", LHS);
    } else if (!lhs_inv_type && lhsShiftVal && lhsMaskOffset) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, LHSSrc,
                                      offset, "MaskShr2", LHS);
      offset = ConstantInt::get(aType, lhsMaskOffset, false);
    } else if (lhs_inv_type && lhsShiftVal && lhsMaskOffset && lhsMaskWidth) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, LHS,
                                      offset, "MaskShr3", inst);
    }
    DEBUG(dbgs() << "Optimizing LHS!\n");
  } else if ((lhsMaskWidth && rhsMaskWidth && rhs_inv_type &&
              (rhsMaskOffset + rhsShiftVal) >=
              (lhsMaskWidth + lhsMaskOffset + lhsShiftVal)) ||
             (lhsMaskWidth && rhsMaskWidth && !rhs_inv_type && rhsMaskOffset >=
              (lhsMaskWidth + lhsMaskOffset + lhsShiftVal)) ||
             (lhsMaskWidth && !rhsMaskWidth && rhsShiftVal >=
              (lhsMaskWidth + lhsMaskOffset + lhsShiftVal)) ||
             (!lhsMaskWidth && rhsMaskWidth &&
              ((rhsMaskOffset + rhsShiftVal) >= lhsShiftVal)) ||
             (!lhsMaskWidth && !rhsMaskWidth && (rhsShiftVal >= lhsShiftVal))
             ) {
    if (rhsMaskVal) {
      offset =
        ConstantInt::get(aType,
                         rhsMaskOffset +
                         (rhs_inv_type ? rhsShiftVal : (rhsMaskOffset ? -
                                                        rhsShiftVal : 0)),
                         false);
      width = ConstantInt::get(aType, rhsMaskWidth, false);
    } else {
      offset = ConstantInt::get(aType, rhsShiftVal, false);
      width = ConstantInt::get(aType, rhsMaskOffset - rhsShiftVal, false);
    }
    LHSSrc = RHSSrc;
    RHSSrc = LHS;
    if ((rhsMaskVal && !isMask_32(rhsMaskVal) && !isShiftedMask_32(rhsMaskVal))
        || (!rhsMaskVal && !rhsShiftVal)) {
      DEBUG(dbgs() << "Failed constraint 2-RHS!\n");
      return false;
    }
    if (rhsShiftVal && !isShiftedMask_32(rhsMaskVal << rhsShiftVal)) {
      DEBUG(dbgs() << "Failed constraint 3-RHS!\n");
      return false;
    }
    if ((rhsMaskVal && (rhsMaskOffset + rhsShiftVal) >= 32)
        || (rhsMaskOffset - rhsShiftVal > 32)) {
      DEBUG(dbgs() << "Failed constraint 4-RHS!\n");
      return false;
    }
    if (!RHSShift && rhsMaskOffset) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, LHSSrc, offset,
                                      "MaskShr1", RHS);
    } else if (!rhs_inv_type && rhsShiftVal && rhsMaskOffset) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, LHSSrc,
                                      offset, "MaskShr2", RHS);
      offset = ConstantInt::get(aType, rhsMaskOffset, false);
    } else if (rhs_inv_type && rhsShiftVal && rhsMaskWidth && rhsMaskOffset) {
      LHSSrc = BinaryOperator::Create(Instruction::LShr, RHS,
                                      offset, "MaskShr3", inst);
    }
    DEBUG(dbgs() << "Optimizing RHS!\n");
  } else {
    DEBUG(dbgs() << "Failed constraint 5!\n");
    return false;
  }
  DEBUG(
    dbgs() << "Width:  ";
    if (width) { width->dump(); } else { dbgs() << "(0)\n"; }
    dbgs() << "Offset: ";
    if (offset) { offset->dump(); } else { dbgs() << "(0)\n"; }
    dbgs() << "LHSSrc: ";
    if (LHSSrc) { LHSSrc->dump(); } else { dbgs() << "(0)\n"; }
    dbgs() << "RHSSrc: ";
    if (RHSSrc) { RHSSrc->dump(); } else { dbgs() << "(0)\n"; }
    );
  if (!offset || !width) {
    DEBUG(dbgs() << "Either width or offset are NULL, failed detection!\n");
    return false;
  }
  // Lets create the function signature.
  std::vector<Type *> callTypes;
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  FunctionType *funcType = FunctionType::get(aType, callTypes, false);
  std::string name = "__amdil_ubit_insert";
  if (isVector) { name += "_v" + itostr(numEle) + "u32"; } else { name +=
                                                                    "_u32"; }
  Function *Func =
    dyn_cast<Function>(inst->getParent()->getParent()->getParent()->
                       getOrInsertFunction(llvm::StringRef(name), funcType));
  Value *Operands[4] = {
    width,
    offset,
    LHSSrc,
    RHSSrc
  };
  CallInst *CI = CallInst::Create(Func, Operands, "BitInsertOpt");
  if (mDebug) {
    dbgs() << "Old Inst: ";
    inst->dump();
    dbgs() << "New Inst: ";
    CI->dump();
    dbgs() << "\n\n";
  }
  CI->insertBefore(inst);
  inst->replaceAllUsesWith(CI);
  inst->eraseFromParent();
  return true;
}
bool
AMDILPeepholeOpt::optimizeBFI(Instruction *inst)
{
  assert (inst && (inst->getOpcode() == Instruction::Xor) &&
          "optimizeBitExtract() expects Xor instruction");
  if (mDebug) {
    dbgs() << "\nInst: "; inst->dump();
  }
  if (optLevel == CodeGenOpt::None) {
    return false;
  }
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    // The HD4XXX hardware doesn't support the ubit_insert instruction.
    return false;
  }
  Type *aType = inst->getType();
  // This optimization only works on 32bit integers.
  if (aType->getScalarType()
      != Type::getInt32Ty(inst->getContext())) {
    return false;
  }
  int numEle = 1;
  if (aType->isVectorTy()) {
    numEle = dyn_cast<VectorType>(aType)->getNumElements();
    if (numEle > 4 || numEle == 3) {
      return false;
    }
  }
  // The optimization we are doing is:
  // B` = B ^ -1
  // C` = B` | C
  // A` = C` & A
  // inst = A` ^ -1
  // (((B` | C) & A) ^ -1)
  // ==>
  // BFI(A, (B & (C ^ -1)), -1)
  Constant *Apneg1 = dyn_cast<Constant>(inst->getOperand(1));
  Instruction *Ap = dyn_cast<Instruction>(inst->getOperand(0));
  // Not a -1 or an 'AND' instruction, so can't proceed.
  if (Apneg1 == NULL || (Ap != NULL && Ap->getOpcode() != Instruction::And)) {
    // Inverted operands, swap them.
    Apneg1 = dyn_cast<Constant>(inst->getOperand(0));
    Ap = dyn_cast<Instruction>(inst->getOperand(1));
  }
  if (Apneg1 == NULL || Ap == NULL ||
      Ap->getOpcode() != Instruction::And) {
    return false;
  }
  if (mDebug) {
    dbgs() << "Ap: "; Ap->dump();
    dbgs() << "Ap-1: "; Apneg1->dump();
  }
  Instruction *Cp = dyn_cast<Instruction>(Ap->getOperand(0));
  Instruction *A = dyn_cast<Instruction>(Ap->getOperand(1));
  if (Cp == NULL || A == NULL) {
    return false;
  }
  if (mDebug) {
    dbgs() << "A: "; A->dump();
    dbgs() << "Cp: "; Cp->dump();
  }
  if (Cp->getOpcode() != Instruction::Or
      && A->getOpcode() == Instruction::Or) {
    // Operands are inverted, lets swap them.
    Cp = dyn_cast<Instruction>(Ap->getOperand(1));
    A = dyn_cast<Instruction>(Ap->getOperand(0));
  }
  if (Cp->getOpcode() != Instruction::Or) {
    // We don't have the right opcode.
    return false;
  }
  Instruction *Bp = dyn_cast<Instruction>(Cp->getOperand(0));
  Instruction *C = dyn_cast<Instruction>(Cp->getOperand(1));
  if (Bp != NULL || Bp->getOpcode() != Instruction::Xor) {
    // Operands are inverted, lets swap them.
    Bp = dyn_cast<Instruction>(Cp->getOperand(1));
    C = dyn_cast<Instruction>(Cp->getOperand(0));
  }
  if (Bp == NULL || Bp->getOpcode() != Instruction::Xor) {
    return false;
  }
  if (mDebug) {
    dbgs() << "C: "; C->dump();
    dbgs() << "Bp: "; Bp->dump();
  }
  Constant *Bpneg1 = dyn_cast<Constant>(Bp->getOperand(1));
  Instruction *B = dyn_cast<Instruction>(Bp->getOperand(0));
  if (B == NULL || Bpneg1 == NULL) {
    B = dyn_cast<Instruction>(Bp->getOperand(1));
    Bpneg1 = dyn_cast<Constant>(Bp->getOperand(0));
  }
  if (B == NULL || Bpneg1 == NULL) {
    return false;
  }
  if (mDebug) {
    dbgs() << "B: "; B->dump();
    dbgs() << "Bp-1: "; Bpneg1->dump();
  }
  if (aType->isVectorTy()) {
    ConstantDataVector *Bpneg1v = dyn_cast<ConstantDataVector>(Bpneg1);
    ConstantDataVector *Apneg1v = dyn_cast<ConstantDataVector>(Apneg1);
    if (Bpneg1v == NULL ||
        Apneg1v == NULL) {
      return false;
    }

    for (size_t x = 0, y = Bpneg1v->getNumElements(); x < y; ++x) {
      ConstantInt *neg1 = dyn_cast<ConstantInt>(Bpneg1v->getElementAsConstant(x));
      if (neg1 == NULL) {
        return false;
      }
      uint32_t maskVal = (uint32_t)neg1->getZExtValue();
      if (!isMask_32(maskVal)
          || CountTrailingOnes_32(maskVal) != 32) {
        return false;
      }
    }
    for (size_t x = 0, y = Apneg1v->getNumElements(); x < y; ++x) {
      ConstantInt *neg1 = dyn_cast<ConstantInt>(Apneg1v->getElementAsConstant(x));
      if (neg1 == NULL) {
        return false;
      }
      uint32_t maskVal = (uint32_t)neg1->getZExtValue();
      if (!isMask_32(maskVal)
          || CountTrailingOnes_32(maskVal) != 32) {
        return false;
      }
    }
  } else {
    ConstantInt *Bpneg1i = dyn_cast<ConstantInt>(Bpneg1);
    ConstantInt *Apneg1i = dyn_cast<ConstantInt>(Apneg1);
    if (Bpneg1i == NULL
        || Apneg1i == NULL) {
      return false;
    }
    uint32_t maskVal = Bpneg1i->getZExtValue();
    if (!isMask_32(maskVal)
        || CountTrailingOnes_32(maskVal) != 32) {
      return false;
    }
    maskVal = Apneg1i->getZExtValue();
    if (!isMask_32(maskVal)
        || CountTrailingOnes_32(maskVal) != 32) {
      return false;
    }
  }
  if (mDebug) {
    dbgs() << "Creating pattern BFI(A, (B & (C ^ -1)), -1)\n";
  }
  // Now that we have verified everything, lets create our result.
  std::vector<Type *> callTypes;
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  FunctionType *funcType = FunctionType::get(aType, callTypes, false);
  std::string name = "__amdil_bfi";
  if (aType->isVectorTy()) {
    name += "_v" + itostr(numEle) + "u32";
  } else {
    name += "_u32";
  }
  Function *Func =
    dyn_cast<Function>(inst->getParent()->getParent()->getParent()->
                       getOrInsertFunction(llvm::StringRef(name), funcType));
  C = BinaryOperator::Create(Instruction::Xor, C, Bpneg1, "bfiXor", inst);
  B = BinaryOperator::Create(Instruction::And, B, C, "bfiAnd", inst);
  Value *Operands[3] = {
    A,
    B,
    Bpneg1
  };
  CallInst *CI = CallInst::Create(Func, Operands, "BFI");
  if (mDebug) {
    dbgs() << "Old Inst: ";
    inst->dump();
    dbgs() << "New Inst: ";
    CI->dump();
    dbgs() << "\n\n";
  }
  CI->insertBefore(inst);
  inst->replaceAllUsesWith(CI);
  inst->eraseFromParent();
  return true;
}
bool
AMDILPeepholeOpt::optimizeBitExtract(Instruction *inst)
{
  assert (inst && (inst->getOpcode() == Instruction::And) &&
          "optimizeBitExtract() expects And instruction");

  if (optLevel == CodeGenOpt::None) {
    return false;
  }
  // We want to do some simple optimizations on Shift right/And patterns. The
  // basic optimization is to turn (A >> B) & C where A is a 32bit type, B is a
  // value smaller than 32 and C is a mask. If C is a constant value, then the
  // following transformation can occur. For signed integers, it turns into the
  // function call dst = __amdil_ibit_extract(log2(C), B, A) For unsigned
  // integers, it turns into the function call dst =
  // __amdil_ubit_extract(log2(C), B, A) The function __amdil_[u|i]bit_extract
  // can be found in Section 7.9 of the ATI IL spec of the stream SDK for
  // Evergreen hardware.
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    // This does not work on HD4XXX hardware.
    return false;
  }
  Type *aType = inst->getType();
  bool isVector = aType->isVectorTy();
  int numEle = 1;
  // This only works on 32bit integers
  if (aType->getScalarType()
      != Type::getInt32Ty(inst->getContext())) {
    return false;
  }
  if (isVector) {
    const VectorType *VT = dyn_cast<VectorType>(aType);
    numEle = VT->getNumElements();
    // We currently cannot support more than 4 elements in a intrinsic and we
    // cannot support Vec3 types.
    if (numEle > 4 || numEle == 3) {
      return false;
    }
  }
  BinaryOperator *ShiftInst = dyn_cast<BinaryOperator>(inst->getOperand(0));
  // If the first operand is not a shift instruction, then we can return as it
  // doesn't match this pattern.
  if (!ShiftInst || !ShiftInst->isShift()) {
    return false;
  }
  // If we are a shift left, then we need don't match this pattern.
  if (ShiftInst->getOpcode() == Instruction::Shl) {
    return false;
  }
  bool isSigned = ShiftInst->isArithmeticShift();
  Constant *AndMask = dyn_cast<Constant>(inst->getOperand(1));
  Constant *ShrVal = dyn_cast<Constant>(ShiftInst->getOperand(1));
  // Lets make sure that the shift value and the and mask are constant integers.
  if (!AndMask || !ShrVal) {
    return false;
  }
  Constant *newMaskConst;
  Constant *shiftValConst;
  if (isVector) {
    // Handle the vector case
    std::vector<Constant *> maskVals;
    std::vector<Constant *> shiftVals;
    ConstantDataVector *AndMaskVec = dyn_cast<ConstantDataVector>(AndMask);
    ConstantDataVector *ShrValVec = dyn_cast<ConstantDataVector>(ShrVal);
    Type *scalarType = AndMaskVec->getType()->getScalarType();
    assert(AndMaskVec->getNumElements() ==
           ShrValVec->getNumElements() && "cannot have a "
           "combination where the number of elements to a "
           "shift and an and are different!");
    for (size_t x = 0, y = AndMaskVec->getNumElements(); x < y; ++x) {
      ConstantInt *AndCI = dyn_cast<ConstantInt>(
        AndMaskVec->getElementAsConstant(x));
      ConstantInt *ShiftIC = dyn_cast<ConstantInt>(
        ShrValVec->getElementAsConstant(x));
      if (!AndCI || !ShiftIC) {
        return false;
      }
      uint32_t maskVal = (uint32_t)AndCI->getZExtValue();
      if (!isMask_32(maskVal)) {
        return false;
      }
      maskVal = (uint32_t)CountTrailingOnes_32(maskVal);
      uint32_t shiftVal = (uint32_t)ShiftIC->getZExtValue();
      // If the mask or shiftval is greater than the bitcount, then break out.
      if (maskVal >= 32 || shiftVal >= 32) {
        return false;
      }
      // If the mask val is greater than the the number of original bits left
      // then this optimization is invalid.
      if (maskVal > (32 - shiftVal)) {
        return false;
      }
      maskVals.push_back(ConstantInt::get(scalarType, maskVal, isSigned));
      shiftVals.push_back(ConstantInt::get(scalarType, shiftVal, isSigned));
    }
    newMaskConst = ConstantVector::get(maskVals);
    shiftValConst = ConstantVector::get(shiftVals);
  } else {
    // Handle the scalar case
    uint32_t maskVal = (uint32_t)dyn_cast<ConstantInt>(AndMask)->getZExtValue();
    // This must be a mask value where all lower bits are set to 1 and then any
    // bit higher is set to 0.
    if (!isMask_32(maskVal)) {
      return false;
    }
    maskVal = (uint32_t)CountTrailingOnes_32(maskVal);
    // Count the number of bits set in the mask, this is the width of the
    // resulting bit set that is extracted from the source value.
    uint32_t shiftVal = (uint32_t)dyn_cast<ConstantInt>(ShrVal)->getZExtValue();
    // If the mask or shift val is greater than the bitcount, then break out.
    if (maskVal >= 32 || shiftVal >= 32) {
      return false;
    }
    // If the mask val is greater than the the number of original bits left then
    // this optimization is invalid.
    if (maskVal > (32 - shiftVal)) {
      return false;
    }
    newMaskConst = ConstantInt::get(aType, maskVal, isSigned);
    shiftValConst = ConstantInt::get(aType, shiftVal, isSigned);
  }
  // Lets create the function signature.
  std::vector<Type *> callTypes;
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  callTypes.push_back(aType);
  FunctionType *funcType = FunctionType::get(aType, callTypes, false);
  std::string name = "__amdil_ubit_extract";
  if (isVector) {
    name += "_v" + itostr(numEle) + "i32";
  } else {
    name += "_i32";
  }
  // Lets create the function.
  Function *Func =
    dyn_cast<Function>(inst->getParent()->getParent()->getParent()->
                       getOrInsertFunction(llvm::StringRef(name), funcType));
  Value *Operands[3] = {
    newMaskConst,
    shiftValConst,
    ShiftInst->getOperand(0)
  };
  // Lets create the Call with the operands
  CallInst *CI = CallInst::Create(Func, Operands, "ByteExtractOpt");
  CI->insertBefore(inst);
  inst->replaceAllUsesWith(CI);
  inst->eraseFromParent();
  return true;
}
bool
getVectorComponent(Instruction *inst, int tid, unsigned int numElem,
                   Value*& vecval, unsigned& whichelem)
{
  ExtractElementInst *einst = dyn_cast<ExtractElementInst>(inst);
  if (!einst) {
    return false;
  }

  vecval = einst->getVectorOperand();
  VectorType *vt = dyn_cast<VectorType>(vecval->getType());
  assert (
    !vt && "ExtractElementInst must have a vector type as its first argument");
  Type       *et = vt->getElementType();
  if ( (vt->getNumElements() != numElem) ||
       (et->getTypeID() != tid) ) {
    return false;
  }
  ConstantInt *cv = dyn_cast<ConstantInt>(einst->getIndexOperand());
  if (!cv) {
    return false;
  }

  whichelem = (unsigned)cv->getZExtValue();
  return true;
}
bool getIntValue(Instruction *Inst, Value *& Val,
                 unsigned int &start_pos, unsigned int &nbits)
{
  Value *intval = NULL, *opnd1;
  bool hasmask = false;

  if (!Inst->getType()->isIntegerTy(32)) {
    return false;
  }

  start_pos = 0;
  nbits = 0;
  if (Inst->getOpcode() == Instruction::And) {
    intval = Inst->getOperand(0);
    opnd1 = Inst->getOperand(1);

    ConstantInt *CI0 = dyn_cast<ConstantInt>(intval);
    ConstantInt *CI1 = dyn_cast<ConstantInt>(opnd1);
    if ((!CI0 && !CI1) || (CI0 && CI1)) {
      return false;
    }

    if (CI0) {
      Value *tmp = intval;
      intval = opnd1;
      opnd1 = tmp;
      CI1 = CI0;
    }

    unsigned int mask = CI1->getZExtValue();
    hasmask = getMaskBitfield(mask, start_pos, nbits);
    if (!hasmask) {
      return false;
    }
  }

  Instruction *tinst = dyn_cast<Instruction>(intval);
  if (!tinst) {
    return false;
  }

  if (tinst->getOpcode() == Instruction::Shl) {
    ConstantInt *CI = dyn_cast<ConstantInt>(tinst->getOperand(1));
    if (hasmask && CI) {
      unsigned int amt = CI->getZExtValue();
      if (amt > start_pos) {
        return false;
      }
      start_pos -= amt;
    } else {
      return false;
    }
    intval = tinst->getOperand(0);
  } else if (tinst->getOpcode() == Instruction::LShr) {
    if (ConstantInt *CI = dyn_cast<ConstantInt>(tinst->getOperand(1))) {
      unsigned int amt = CI->getZExtValue();
      if (!hasmask) {
        start_pos = amt;
        nbits = 32 - amt;
      } else if ((amt + start_pos + nbits) > 31) {
        return false;
      } else {
        start_pos += amt;
      }
    } else {
      return false;
    }
    intval = tinst->getOperand(0);
  }

  Val = intval;
  return true;
}
/*
   format:     f_2_u4 dst, src
   semantics:  dist.xyzw =
                 (((uint32)src.x) & 0xFF) |
                 ((((uint32)src.y) & 0xFF) << 8) |
                 ((((uint32)src.z) & 0xFF) << 16) |
                 ((((uint32)src.w) & 0xFF) << 24);

   If this pattern is found, change the sequence of operations into a intrinic call
   to u32 __amdil_f_2_u4 (v4f32) (int_AMDIL_media_convert_f2v4u8).

   TODO: if src are not from the same vector, create a new vector.
*/
bool
AMDILPeepholeOpt::genIntrF2U4(Instruction *inst)
{
  // Try to handle the pattern:
  //   inst = or0 | or1 | or2 | or3
  Instruction *or0, *or1, *or2, *or3;
  or0 = dyn_cast<Instruction>(inst->getOperand(0));
  or1 = dyn_cast<Instruction>(inst->getOperand(1));
  if (!or0 || !or1) {
    return false;
  }

  bool is_or0 = (or0->getOpcode() == Instruction::Or);
  bool is_or1 = (or1->getOpcode() == Instruction::Or);
  if (is_or0 && is_or1) {
    Instruction *t0 = or0, *t1 = or1;
    or0 = dyn_cast<Instruction>(t0->getOperand(0));
    or1 = dyn_cast<Instruction>(t0->getOperand(1));
    or2 = dyn_cast<Instruction>(t1->getOperand(0));
    or3 = dyn_cast<Instruction>(t1->getOperand(1));
  } else if (is_or0 || is_or1) {
    if (is_or0) {
      // swap or0 and or1
      or2 = or0;
      or0 = or1;
      or1 = or2;
    }
    or2 = dyn_cast<Instruction>(or1->getOperand(0));
    or1 = dyn_cast<Instruction>(or1->getOperand(1));
    if (!or1 || !or2) {
      return false;
    }
    else {
      bool b1 = (or1->getOpcode() == Instruction::Or);
      bool b2 = (or2->getOpcode() == Instruction::Or);
      if ((b1 && b2) || (!b1 && !b2)) {
        return false;
      } else {
        if (b1) {
          // swap or1 and or2
          or3 = or1;
          or1 = or2;
          or2 = or3;
        }
        or3 = dyn_cast<Instruction>(or2->getOperand(0));
        or2 = dyn_cast<Instruction>(or2->getOperand(1));
      }
    }
  } else {
    return false;
  }

  // Sanity check
  if (!or0 || !or1 || !or2 || !or3) {
    return false;
  }

  // Check to see if all or's are from the same vector (v4f32), and each
  // one is converted to 8 bit integer...
  unsigned int dst_start[4], dst_width[4], src_start[4];
  Value *src[4];
  Instruction *dst[4];

  dst[0] = or0;
  dst[1] = or1;
  dst[2] = or2;
  dst[3] = or3;

  Value *v4f32val = NULL;
  for (int i=0; i < 4; ++i) {
    if (!getIntValue(dst[i], src[i], src_start[i],
                     dst_start[i], dst_width[i]) ||
        (dst_width[i] != 8) || (src_start[i] != 0) ||
        (src_start[i] > 24) || ((src_start[i] % 8) != 0)) {
      return false;
    }

    Instruction *tinst = dyn_cast<Instruction>(src[i]);
    if (!tinst ||
        (tinst->getOpcode() != Instruction::FPToUI)) {
      return false;
    }
    src[i] = tinst->getOperand(0);
    tinst = dyn_cast<Instruction>(src[i]);
    if (!tinst) {
      return false;
    }

    Value *vecval;
    unsigned int which;
    if (!getVectorComponent(tinst, Type::FloatTyID, 4,  vecval, which)) {
      return false;
    }

    if (v4f32val == NULL) {
      v4f32val = vecval;
    } else if (v4f32val != vecval) {
      return false;
    }

    if (which != (dst_start[i]/8)) {
      return false;
    }
  }

  // Check and record the correct order in pos[].
  int pos[4];
  for (int i=0; i < 4; ++i) {
    pos[i] = -1;
  }
  for (int i=0; i < 4; ++i) {
    unsigned int ix = (dst_start[i] / 8);
    if (pos[ix] != -1) {
      return false;
    }
    pos[ix] = i;
  }

  // Generate the intrinsic
  Type *rtype = inst->getType();
  std::vector<Type *> argtypes;
  argtypes.push_back(v4f32val->getType());
  FunctionType *functype = FunctionType::get(rtype, argtypes, false);
  Function *proto_f2u4 = dyn_cast<Function>(
    mF->getParent()->getOrInsertFunction("__amdil_f_2_u4", functype));

  CallInst *call_f2u4 = CallInst::Create(proto_f2u4, v4f32val, "F_2_U4", inst);
  inst->replaceAllUsesWith(call_f2u4);
  inst->eraseFromParent();

  return true;
}
bool
AMDILPeepholeOpt::instLevelOptimizations(Instruction* inst)
{
  assert (inst && "inst should not be NULL");

  bool isDebug = (optLevel == CodeGenOpt::None);
  bool isEGOrLater =
    (mSTM->device()->getGeneration() >= AMDILDeviceInfo::HD5XXX);

  // Remove dead inst (probably should do it in caller)
  if (!isDebug && isInstructionTriviallyDead(inst)) {
    inst->eraseFromParent();
    return true;
  }

  const unsigned opc = inst->getOpcode();

  if ((opc == Instruction::Or) && !isDebug && isEGOrLater &&
      genIntrF2U4(inst)) {
    return true;
  }
  if ((opc == Instruction::Call) && optimizeCallInst(inst)) {
    return true;
  }
  if ((opc == Instruction::And) && optimizeBitExtract(inst)) {
    return true;
  }
  if ((opc == Instruction::Or) && optimizeBitInsert(inst)) {
    return true;
  }
  if ((opc == Instruction::Xor) && optimizeBFI(inst)) {
    return true;
  }
  if (((opc == Instruction::Load) || (opc == Instruction::Store)) &&
      correctMisalignedMemOp(inst)) {
    return true;
  }
  // If we are loading from a NULL pointer, replace the load with 0.
  if ((opc == Instruction::Load)) {
    const Value *ptr = dyn_cast<LoadInst>(inst)->getPointerOperand();
    if (ptr && dyn_cast<ConstantPointerNull>(ptr)) {
      inst->replaceAllUsesWith(Constant::getNullValue(inst->getType()));
      return true;
    }
  }
  // If we are storing to a NULL pointer, then drop the store.
  if (opc == Instruction::Store) {
    const Value *ptr = dyn_cast<StoreInst>(inst)->getPointerOperand();
    if (ptr && dyn_cast<ConstantPointerNull>(ptr)) {
      inst->eraseFromParent();
      return true;
    }
  }
  if ((opc == Instruction::Or) && optimizeClassInst(inst)) {
    return true;
  }
  return false;
}
bool
AMDILPeepholeOpt::correctMisalignedMemOp(Instruction *inst)
{
  LoadInst *linst = dyn_cast<LoadInst>(inst);
  StoreInst *sinst = dyn_cast<StoreInst>(inst);
  unsigned alignment;
  Type* Ty = inst->getType();
  if (linst) {
    alignment = linst->getAlignment();
    Ty = inst->getType();
  } else if (sinst) {
    alignment = sinst->getAlignment();
    Ty = sinst->getValueOperand()->getType();
  } else {
    return false;
  }
  unsigned size = TM.getTargetData()->getTypeAllocSize(Ty);
  if (size == alignment || size < alignment) {
    return false;
  }
  if (!Ty->isStructTy()) {
    return false;
  }
  if (alignment < 4) {
    if (linst) {
      linst->setAlignment(0);
      return true;
    } else if (sinst) {
      sinst->setAlignment(0);
      return true;
    }
  }
  return false;
}
bool
AMDILPeepholeOpt::isSigned24BitOps(CallInst *CI)
{
  if (!CI) {
    return false;
  }
  Value *LHS = CI->getOperand(CI->getNumOperands() - 1);
  std::string namePrefix = LHS->getName().substr(0, 14);
  if (namePrefix != "__amdil_imad24" && namePrefix != "__amdil_imul24"
      && namePrefix != "__amdil__imul24_high") {
    return false;
  }
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::Signed24BitOps)) {
    return false;
  }
  return true;
}
void
AMDILPeepholeOpt::expandSigned24BitOps(CallInst *CI)
{
  assert(isSigned24BitOps(CI) && "Must be a "
         "signed 24 bit operation to call this function!");
  Value *LHS = CI->getOperand(CI->getNumOperands()-1);
  // On 7XX and 8XX we do not have signed 24bit, so we need to
  // expand it to the following:
  // imul24 turns into 32bit imul
  // imad24 turns into 32bit imad
  // imul24_high turns into 32bit imulhigh
  if (LHS->getName().substr(0, 14) == "__amdil_imad24") {
    Type *aType = CI->getOperand(0)->getType();
    bool isVector = aType->isVectorTy();
    int numEle = isVector ? dyn_cast<VectorType>(aType)->getNumElements() : 1;
    std::vector<Type*> callTypes;
    callTypes.push_back(CI->getOperand(0)->getType());
    callTypes.push_back(CI->getOperand(1)->getType());
    callTypes.push_back(CI->getOperand(2)->getType());
    FunctionType *funcType =
      FunctionType::get(CI->getOperand(0)->getType(), callTypes, false);
    std::string name = "__amdil_imad";
    if (isVector) {
      name += "_v" + itostr(numEle) + "i32";
    } else {
      name += "_i32";
    }
    Function *Func = dyn_cast<Function>(
      CI->getParent()->getParent()->getParent()->
      getOrInsertFunction(llvm::StringRef(name), funcType));
    Value *Operands[3] = {
      CI->getOperand(0),
      CI->getOperand(1),
      CI->getOperand(2)
    };
    CallInst *nCI = CallInst::Create(Func, Operands, "imad24");
    nCI->insertBefore(CI);
    CI->replaceAllUsesWith(nCI);
  } else if (LHS->getName().substr(0, 14) == "__amdil_imul24") {
    BinaryOperator *mulOp =
      BinaryOperator::Create(Instruction::Mul, CI->getOperand(0),
                             CI->getOperand(1), "imul24", CI);
    CI->replaceAllUsesWith(mulOp);
  } else if (LHS->getName().substr(0, 19) == "__amdil_imul24_high") {
    Type *aType = CI->getOperand(0)->getType();

    bool isVector = aType->isVectorTy();
    int numEle = isVector ? dyn_cast<VectorType>(aType)->getNumElements() : 1;
    std::vector<Type*> callTypes;
    callTypes.push_back(CI->getOperand(0)->getType());
    callTypes.push_back(CI->getOperand(1)->getType());
    FunctionType *funcType =
      FunctionType::get(CI->getOperand(0)->getType(), callTypes, false);
    std::string name = "__amdil_imul_high";
    if (isVector) {
      name += "_v" + itostr(numEle) + "i32";
    } else {
      name += "_i32";
    }
    Function *Func = dyn_cast<Function>(
      CI->getParent()->getParent()->getParent()->
      getOrInsertFunction(llvm::StringRef(name), funcType));
    Value *Operands[2] = {
      CI->getOperand(0),
      CI->getOperand(1)
    };
    CallInst *nCI = CallInst::Create(Func, Operands, "imul24_high");
    nCI->insertBefore(CI);
    CI->replaceAllUsesWith(nCI);
  }
}
bool
AMDILPeepholeOpt::isRWGLocalOpt(CallInst *CI)
{
  return (CI != NULL && mRWGOpt
          && CI->getOperand(CI->getNumOperands() - 1)->getName()
          == "__amdil_get_local_size_int"
          // We have to check if we are a kernel currently
          // because we inline everything and only kernels
          // should be left. However, in some cases, other
          // functions exist and we don't want to
          // optimize them because we don't track that
          // information.
          && mAMI->getKernel(mF->getName()));
}
void
AMDILPeepholeOpt::expandRWGLocalOpt(CallInst *CI)
{
  assert(isRWGLocalOpt(CI) &&
         "This optmization only works when the call inst is get_local_size!");
  std::vector<Constant *> consts;
  const AMDILKernel *kernel = mAMI->getKernel(mF->getName());
  for (uint32_t x = 0; x < 3; ++x) {
    // We don't have to check if sgv is valid or not as we
    // checked this case before we set mRWGOpt to true.
    uint32_t val = kernel->sgv->reqGroupSize[x];
    consts.push_back(ConstantInt::get(Type::getInt32Ty(*mCTX), val));
  }
  consts.push_back(ConstantInt::get(Type::getInt32Ty(*mCTX), 0));
  Value *cVec = ConstantVector::get(consts);
  CI->replaceAllUsesWith(cVec);
  ++LocalFuncs;
  return;
}
bool
AMDILPeepholeOpt::convertAccurateDivide(CallInst *CI)
{
  if (!CI) {
    return false;
  }
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD6XXX
      && (mSTM->getDeviceName() == "cayman"
          || mSTM->getDeviceName() == "kauai"
          || mSTM->getDeviceName() == "trinity")) {
    return false;
  }
  return CI->getOperand(CI->getNumOperands() - 1)->getName().substr(0, 20)
         == "__amdil_improved_div";
}
void
AMDILPeepholeOpt::expandAccurateDivide(CallInst *CI)
{
  assert(convertAccurateDivide(CI)
         && "expanding accurate divide can only happen if it is expandable!");
  BinaryOperator *divOp =
    BinaryOperator::Create(Instruction::FDiv, CI->getOperand(0),
                           CI->getOperand(1), "fdiv32", CI);
  CI->replaceAllUsesWith(divOp);
}
bool
AMDILPeepholeOpt::propagateSamplerInst(CallInst *CI)
{
  if (optLevel != CodeGenOpt::None) {
    return false;
  }

  if (!CI) {
    return false;
  }

  unsigned funcNameIdx = 0;
  funcNameIdx = CI->getNumOperands() - 1;
  StringRef calleeName = CI->getOperand(funcNameIdx)->getName();
  if (
    calleeName != "__amdil_image1d_read_norm"
    && calleeName != "__amdil_image1d_read_unnorm"
    && calleeName != "__amdil_image1d_array_read_norm"
    && calleeName != "__amdil_image1d_array_read_unnorm"
    && calleeName != "__amdil_image1d_buffer_read_norm"
    && calleeName != "__amdil_image1d_buffer_read_unnorm"
    && calleeName != "__amdil_image2d_read_norm"
    && calleeName != "__amdil_image2d_read_unnorm"
    && calleeName != "__amdil_image2d_array_read_norm"
    && calleeName != "__amdil_image2d_array_read_unnorm"
    && calleeName != "__amdil_image3d_read_norm"
    && calleeName != "__amdil_image3d_read_unnorm") {
    return false;
  }

  unsigned samplerIdx = 2;
  samplerIdx = 1;
  Value *sampler = CI->getOperand(samplerIdx);
  LoadInst *lInst = dyn_cast<LoadInst>(sampler);
  if (!lInst) {
    return false;
  }

  if (lInst->getPointerAddressSpace() != AMDILAS::PRIVATE_ADDRESS) {
    return false;
  }

  GlobalVariable *gv = dyn_cast<GlobalVariable>(lInst->getPointerOperand());
  // If we are loading from what is not a global value, then we
  // fail and return.
  if (!gv) {
    return false;
  }

  // If we don't have an initializer or we have an initializer and
  // the initializer is not a 32bit integer, we fail.
  if (!gv->hasInitializer()
      || !gv->getInitializer()->getType()->isIntegerTy(32)) {
    return false;
  }

  // Now that we have the global variable initializer, lets replace
  // all uses of the load instruction with the samplerVal and
  // reparse the __amdil_is_constant() function.
  Constant *samplerVal = gv->getInitializer();
  lInst->replaceAllUsesWith(samplerVal);
  return true;
}
bool
AMDILPeepholeOpt::doInitialization(Module &M)
{
  return false;
}
bool
AMDILPeepholeOpt::doFinalization(Module &M)
{
  return false;
}
void
AMDILPeepholeOpt::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.addRequired<MachineFunctionAnalysis>();
  FunctionPass::getAnalysisUsage(AU);
  AU.setPreservesAll();
}
