//===-- AMDILUtilityFunctions.cpp -----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides the implementations of functions that are declared in
// the AMDILUtilityFUnctions.h file.
//
//===----------------------------------------------------------------------===//

#include "AMDILUtilityFunctions.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILISelLowering.h"
#include "AMDILCompilerErrors.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Instruction.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ADT/ValueMap.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Debug.h"
#include "llvm/CodeGen/MachineInstr.h"
#include <cstdio>
#include <queue>
#include <list>
using namespace llvm;
int64_t GET_SCALAR_SIZE(llvm::Type *A) {
  return A->getScalarSizeInBits();
}
const TargetRegisterClass * getRegClassFromID(unsigned int ID) {
  switch (ID) {
  default:
    assert(0 && "Passed in ID does not match any register classes.");
    return NULL;
  case AMDIL::GPRI8RegClassID:
    return &AMDIL::GPRI8RegClass;
  case AMDIL::GPRI16RegClassID:
    return &AMDIL::GPRI16RegClass;
  case AMDIL::GPRI32RegClassID:
    return &AMDIL::GPRI32RegClass;
  case AMDIL::GPRF32RegClassID:
    return &AMDIL::GPRF32RegClass;
  case AMDIL::GPRI64RegClassID:
    return &AMDIL::GPRI64RegClass;
  case AMDIL::GPRF64RegClassID:
    return &AMDIL::GPRF64RegClass;
  case AMDIL::GPRV4F32RegClassID:
    return &AMDIL::GPRV4F32RegClass;
  case AMDIL::GPRV4I8RegClassID:
    return &AMDIL::GPRV4I8RegClass;
  case AMDIL::GPRV4I16RegClassID:
    return &AMDIL::GPRV4I16RegClass;
  case AMDIL::GPRV4I32RegClassID:
    return &AMDIL::GPRV4I32RegClass;
  case AMDIL::GPRV2F32RegClassID:
    return &AMDIL::GPRV2F32RegClass;
  case AMDIL::GPRV2I8RegClassID:
    return &AMDIL::GPRV2I8RegClass;
  case AMDIL::GPRV2I16RegClassID:
    return &AMDIL::GPRV2I16RegClass;
  case AMDIL::GPRV2I32RegClassID:
    return &AMDIL::GPRV2I32RegClass;
  case AMDIL::GPRV2F64RegClassID:
    return &AMDIL::GPRV2F64RegClass;
  case AMDIL::GPRV2I64RegClassID:
    return &AMDIL::GPRV2I64RegClass;
  };
}
const TargetRegisterClass* getRegClassFromType(unsigned int type) {
  switch (type) {
  default:
    assert(0 && "Passed in type does not match any register classes.");
  case MVT::i8:
    return &AMDIL::GPRI8RegClass;
  case MVT::i16:
    return &AMDIL::GPRI16RegClass;
  case MVT::i32:
    return &AMDIL::GPRI32RegClass;
  case MVT::f32:
    return &AMDIL::GPRF32RegClass;
  case MVT::i64:
    return &AMDIL::GPRI64RegClass;
  case MVT::f64:
    return &AMDIL::GPRF64RegClass;
  case MVT::v4f32:
    return &AMDIL::GPRV4F32RegClass;
  case MVT::v4i8:
    return &AMDIL::GPRV4I8RegClass;
  case MVT::v4i16:
    return &AMDIL::GPRV4I16RegClass;
  case MVT::v4i32:
    return &AMDIL::GPRV4I32RegClass;
  case MVT::v2f32:
    return &AMDIL::GPRV2F32RegClass;
  case MVT::v2i8:
    return &AMDIL::GPRV2I8RegClass;
  case MVT::v2i16:
    return &AMDIL::GPRV2I16RegClass;
  case MVT::v2i32:
    return &AMDIL::GPRV2I32RegClass;
  case MVT::v2f64:
    return &AMDIL::GPRV2F64RegClass;
  case MVT::v2i64:
    return &AMDIL::GPRV2I64RegClass;
  }
}
unsigned getRegClassFromName(const StringRef &name)
{
  if (name.find("v4i32") != StringRef::npos) {
    return AMDIL::GPRV4I32RegClassID;
  } else if (name.find("v2i32") != StringRef::npos) {
    return AMDIL::GPRV2I32RegClassID;
  } else if (name.find("i32") != StringRef::npos) {
    return AMDIL::GPRI32RegClassID;
  } else if (name.find("v4f32") != StringRef::npos) {
    return AMDIL::GPRV4F32RegClassID;
  } else if (name.find("v2f32") != StringRef::npos) {
    return AMDIL::GPRV2I32RegClassID;
  } else if (name.find("f32") != StringRef::npos) {
    return AMDIL::GPRF32RegClassID;
  } else if (name.find("v4i16") != StringRef::npos) {
    return AMDIL::GPRV4I16RegClassID;
  } else if (name.find("v2i16") != StringRef::npos) {
    return AMDIL::GPRV2I16RegClassID;
  } else if (name.find("i16") != StringRef::npos) {
    return AMDIL::GPRI16RegClassID;
  } else if (name.find("v4i8") != StringRef::npos) {
    return AMDIL::GPRV4I8RegClassID;
  } else if (name.find("v2i8") != StringRef::npos) {
    return AMDIL::GPRV2I8RegClassID;
  } else if (name.find("i8") != StringRef::npos) {
    return AMDIL::GPRI8RegClassID;
  } else if (name.find("v2i64") != StringRef::npos) {
    return AMDIL::GPRV2I64RegClassID;
  } else if (name.find("i64") != StringRef::npos) {
    return AMDIL::GPRI64RegClassID;
  } else if (name.find("v2f64") != StringRef::npos) {
    return AMDIL::GPRV2F64RegClassID;
  } else if (name.find("f64") != StringRef::npos) {
    return AMDIL::GPRF64RegClassID;
  }
  assert("Found a name that I couldn't determine a class for!");
  return AMDIL::GPRI32RegClassID;
}
void printSDNode(const SDNode *N) {
  printf("Opcode: %d isTargetOpcode: %d isMachineOpcode: %d\n",
         N->getOpcode(), N->isTargetOpcode(), N->isMachineOpcode());
  printf("Empty: %d OneUse: %d Size: %d NodeID: %d\n",
         N->use_empty(), N->hasOneUse(), (int)N->use_size(), N->getNodeId());
  for (unsigned int i = 0; i < N->getNumOperands(); ++i) {
    printf("OperandNum: %d ValueCount: %d ValueType: %d\n",
           i, N->getNumValues(), N->getValueType(0).getSimpleVT().SimpleTy);
    printSDValue(N->getOperand(i), 0);
  }
}
void printSDValue(const SDValue &Op, int level) {
  printf("\nOp: %p OpCode: %d NumOperands: %d ", &Op, Op.getOpcode(),
         Op.getNumOperands());
  printf("IsTarget: %d IsMachine: %d ", Op.isTargetOpcode(),
         Op.isMachineOpcode());
  if (Op.isMachineOpcode()) {
    printf("MachineOpcode: %d\n", Op.getMachineOpcode());
  } else {
    printf("\n");
  }
  EVT vt = Op.getValueType();
  printf("ValueType: %d \n", vt.getSimpleVT().SimpleTy);
  printf("UseEmpty: %d OneUse: %d\n", Op.use_empty(), Op.hasOneUse());
  if (level) {
    printf("Children for %d:\n", level);
    for (unsigned int i = 0; i < Op.getNumOperands(); ++i) {
      printf("Child %d->%d:", level, i);
      printSDValue(Op.getOperand(i), level - 1);
    }
  }
}
#undef ExpandCaseToAllScalarTypes
#define ExpandCaseToAllScalarTypes(Instr) \
case Instr ## i8r:  \
case Instr ## i16r: \
case Instr ## i32r: \
case Instr ## i64r: \
case Instr ## f32r: \
case Instr ## f64r:

bool isMoveOrEquivalent(unsigned int opcode) {
  switch (opcode) {
  default:
    return false;
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASCHAR);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASSHORT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASINT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASLONG);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASDOUBLE);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASFLOAT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2CHAR);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2SHORT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2INT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2FLOAT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2LONG);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV2DOUBLE);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV4CHAR);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV4SHORT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV4INT);
    ExpandCaseToAllScalarTypes(AMDIL::IL_ASV4FLOAT);
  case AMDIL::DLOf64r:
  case AMDIL::LLOi64r:
  case AMDIL::LLOv2i64r:
    return true;
  };
  return false;
}
#undef ExpandCaseToAllScalarTypes

bool check_type(const Value *ptr, unsigned int addrspace) {
  if (!ptr) {
    return false;
  }
  Type *ptrType = ptr->getType();
  return dyn_cast<PointerType>(ptrType)->getAddressSpace() == addrspace;
}
size_t getNumElements(Type * const T) {
  size_t size = 0;
  if (!T) {
    return size;
  }
  switch (T->getTypeID()) {
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
  case Type::LabelTyID:
    assert(0 && "These types are not supported by this backend");
  default:
  case Type::FloatTyID:
  case Type::DoubleTyID:
    size = 1;
    break;
  case Type::PointerTyID:
    size = getNumElements(dyn_cast<PointerType>(T));
    break;
  case Type::IntegerTyID:
    size = getNumElements(dyn_cast<IntegerType>(T));
    break;
  case Type::StructTyID:
    size = getNumElements(dyn_cast<StructType>(T));
    break;
  case Type::ArrayTyID:
    size = getNumElements(dyn_cast<ArrayType>(T));
    break;
  case Type::FunctionTyID:
    size = getNumElements(dyn_cast<FunctionType>(T));
    break;
  case Type::VectorTyID:
    size = getNumElements(dyn_cast<VectorType>(T));
    break;
  };
  return size;
}
size_t getNumElements(StructType * const ST) {
  size_t size = 0;
  if (!ST) {
    return size;
  }
  Type *curType;
  StructType::element_iterator eib;
  StructType::element_iterator eie;
  for (eib = ST->element_begin(), eie = ST->element_end();
       eib != eie; ++eib) {
    curType = *eib;
    size += getNumElements(curType);
  }
  return size;
}
size_t getNumElements(IntegerType * const IT) {
  return (!IT) ? 0 : 1;
}
size_t getNumElements(FunctionType * const FT) {
  assert(0 && "Should not be able to calculate the number of "
         "elements of a function type");
  return 0;
}
size_t getNumElements(ArrayType * const AT) {
  return (!AT) ? 0
         :  (size_t)(getNumElements(AT->getElementType()) *
                     AT->getNumElements());
}
size_t getNumElements(VectorType * const VT) {
  return (!VT) ? 0
         : VT->getNumElements() * getNumElements(VT->getElementType());
}
size_t getNumElements(PointerType * const PT) {
  size_t size = 0;
  if (!PT) {
    return size;
  }
  for (size_t x = 0, y = PT->getNumContainedTypes(); x < y; ++x) {
    size += getNumElements(PT->getContainedType(x));
  }
  return size;
}
const llvm::Value *getBasePointerValue(const llvm::Value *V)
{
  if (!V) {
    return NULL;
  }
  const Value *ret = NULL;
  ValueMap<const Value *, bool> ValueBitMap;
  std::queue<const Value *, std::list<const Value *> > ValueQueue;
  ValueQueue.push(V);
  while (!ValueQueue.empty()) {
    V = ValueQueue.front();
    if (ValueBitMap.find(V) == ValueBitMap.end()) {
      ValueBitMap[V] = true;
      if (dyn_cast<Argument>(V) && dyn_cast<PointerType>(V->getType())) {
        ret = V;
        break;
      } else if (dyn_cast<GlobalVariable>(V)) {
        ret = V;
        break;
      } else if (dyn_cast<Constant>(V)) {
        const ConstantExpr *CE = dyn_cast<ConstantExpr>(V);
        if (CE) {
          ValueQueue.push(CE->getOperand(0));
        }
      } else if (const AllocaInst *AI = dyn_cast<AllocaInst>(V)) {
        ret = AI;
        break;
      } else if (const Instruction *I = dyn_cast<Instruction>(V)) {
        uint32_t numOps = I->getNumOperands();
        for (uint32_t x = 0; x < numOps; ++x) {
          ValueQueue.push(I->getOperand(x));
        }
      } else {
        // assert(0 && "Found a Value that we didn't know how to handle!");
      }
    }
    ValueQueue.pop();
  }
  return ret;
}
const llvm::Value *getBasePointerValue(const llvm::MachineInstr *MI) {
  const Value *moVal = NULL;
  if (!MI->memoperands_empty()) {
    const MachineMemOperand *memOp = (*MI->memoperands_begin());
    moVal = memOp ? memOp->getValue() : NULL;
    moVal = getBasePointerValue(moVal);
  }
  return moVal;
}
bool commaPrint(int i, OSTREAM_TYPE &O) {
  O << ":" << i;
  return false;
}
bool isLoadInst(const llvm::MachineInstr *MI) {
  return !(MI->getDesc().TSFlags & (1ULL << AMDID::LOADCONST))
         && (MI->getDesc().TSFlags & (1ULL << AMDID::LOAD));
}
bool isPtrLoadInst(const llvm::MachineInstr *MI) {
  return isLoadInst(MI)
         && !(MI->getDesc().TSFlags & (1ULL << AMDID::IMAGE));
}
bool isSWSExtLoadInst(const llvm::MachineInstr *MI)
{
  return isPtrLoadInst(MI) && (MI->getDesc().TSFlags & (1ULL << AMDID::SWSEXTLD));
}
bool isExtLoadInst(const llvm::MachineInstr *MI) {
  return isPtrLoadInst(MI) && (MI->getDesc().TSFlags & AMDID::EXTLOAD);
}
bool isSExtLoadInst(const llvm::MachineInstr *MI) {
  return isPtrLoadInst(MI) && (MI->getDesc().TSFlags & (1ULL << AMDID::SEXTLOAD))
         && !(MI->getDesc().TSFlags & (1ULL << AMDID::ZEXTLOAD));
}
bool isAExtLoadInst(const llvm::MachineInstr *MI) {
  return isPtrLoadInst(MI) && (MI->getDesc().TSFlags & AMDID::AEXTLOAD);
}
bool isZExtLoadInst(const llvm::MachineInstr *MI) {
  return isPtrLoadInst(MI) && (MI->getDesc().TSFlags & (1ULL << AMDID::ZEXTLOAD))
         && !(MI->getDesc().TSFlags & (1ULL << AMDID::SEXTLOAD));
}
bool isPtrStoreInst(const llvm::MachineInstr *MI) {
  return isStoreInst(MI) && !(MI->getDesc().TSFlags & (1ULL << AMDID::IMAGE));
}
bool isStoreInst(const llvm::MachineInstr *MI) {
  return MI->getDesc().TSFlags & (1ULL << AMDID::STORE);
}
bool isArenaInst(const llvm::MachineInstr *MI) {
  return MI->getDesc().TSFlags & (1ULL << AMDID::ARENAUAV);
}
bool isTruncStoreInst(const llvm::MachineInstr *MI) {
  return MI->getDesc().TSFlags & (1ULL << AMDID::TRUNCATE);
}
bool isAtomicInst(const llvm::MachineInstr *MI) {
  return MI->getDesc().TSFlags & (1ULL << AMDID::ATOMIC);
}
bool isVolatileInst(const llvm::MachineInstr *MI) {
  if (!MI->memoperands_empty()) {
    for (MachineInstr::mmo_iterator mob = MI->memoperands_begin(),
         moe = MI->memoperands_end(); mob != moe; ++mob) {
      // If there is a volatile mem operand, this is a volatile instruction.
      if ((*mob)->isVolatile()) {
        return true;
      }
    }
  }
  return false;
}
bool isGlobalInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::GLOBAL);
}
bool isPrivateInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::PRIVATE);
}
bool isConstantInst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & (1ULL << AMDID::CONSTANT))
         || isConstantPoolInst(MI);
}
bool is64bitLSOp(const llvm::MachineInstr *MI)
{
  return (isPtrLoadInst(MI) || isPtrStoreInst(MI))
         && is64BitInst(MI);
}
bool isConstantPoolInst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & (1ULL << AMDID::CPOOL));
}
bool isRegionInst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & (1ULL << AMDID::REGION));
}
bool isGWSInst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & (1ULL << AMDID::GWS));
}
bool isLocalInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::LOCAL);
}
bool isLDSInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::LDS);
}
bool isGDSInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::GDS);
}
bool isUAVArenaInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::ARENAUAV);
}
bool isUAVRawInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::RAWUAV);
}
bool isCBInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::CBMEM);
}
bool isScratchInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::SCRATCH);
}
bool isImageInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::IMAGE);
}
bool is64BitImageInst(const llvm::MachineInstr *MI)
{
  return isImageInst(MI) && is64BitInst(MI);
}
bool isReadImageInst(const llvm::MachineInstr *MI)
{
  return isImageInst(MI) && isLoadInst(MI) && !isImageTXLDInst(MI);
}
bool isWriteImageInst(const llvm::MachineInstr *MI)
{
  return isImageInst(MI) && isStoreInst(MI);
}
bool isImageInfoInst(const llvm::MachineInstr* MI) {
  return isImageInst(MI)
         && (MI->getDesc().TSFlags & AMDID::INFO);
}
bool isImageInfo0Inst(const llvm::MachineInstr* MI) {
  return isImageInst(MI)
         && (MI->getDesc().TSFlags & (1ULL << AMDID::INFO0));
}
bool isImageInfo1Inst(const llvm::MachineInstr* MI) {
  return isImageInst(MI)
         && (MI->getDesc().TSFlags & (1ULL << AMDID::INFO1));
}
bool isImageTXLDInst(const llvm::MachineInstr* MI) {
  return isImageInst(MI)
         && MI->getDesc().TSFlags & (1ULL << AMDID::TXLD);
}
bool isSemaphoreInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::SEMA);
}
bool isAppendInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::APPEND);
}
bool isRegionAtomic(const llvm::MachineInstr *MI)
{
  return isAtomicInst(MI) && isRegionInst(MI);
}
bool is64BitRegionAtomic(const llvm::MachineInstr *MI)
{
  return isRegionAtomic(MI) && is64BitInst(MI);
}
bool isLocalAtomic(const llvm::MachineInstr *MI)
{
  return isAtomicInst(MI) && isLocalInst(MI);
}
bool is64BitLocalAtomic(const llvm::MachineInstr *MI)
{
  return isLocalAtomic(MI) && is64BitInst(MI);
}
bool isGlobalAtomic(const llvm::MachineInstr *MI)
{
  return isAtomicInst(MI) && (isGlobalInst(MI)
                              || isArenaInst(MI));
}
bool is64BitGlobalAtomic(const llvm::MachineInstr *MI)
{
  return isGlobalAtomic(MI) && is64BitInst(MI);
}
bool isArenaAtomic(const llvm::MachineInstr *MI)
{
  return isAtomicInst(MI) && isArenaInst(MI);
}
bool is64BitInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::ADDR64);
}
bool isPackedInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::PACKED);
}
bool isSub32BitIOInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::SUB32BITS);
}
bool isPackV2I8Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV2I8
         && isPackedInst(MI) && isStoreInst(MI);
}
bool isPackV2I16Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV2I16
         && isPackedInst(MI) && isStoreInst(MI);
}
bool isPackV4I8Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV4I8
         && isPackedInst(MI) && isStoreInst(MI);
}
bool isPackV4I16Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV4I16
         && isPackedInst(MI) && isStoreInst(MI);
}
bool isUnpackV2I8Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV2I8
         && isPackedInst(MI) && isLoadInst(MI);
}
bool isUnpackV2I16Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV2I16
         && isPackedInst(MI) && isLoadInst(MI);
}
bool isUnpackV4I8Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV4I8
         && isPackedInst(MI) && isLoadInst(MI);
}
bool isUnpackV4I16Inst(const llvm::MachineInstr *MI)
{
  return (MI->getDesc().TSFlags & AMDID::TYPEMASK) == AMDID::TYPEV4I16
         && isPackedInst(MI) && isLoadInst(MI);
}
bool isVectorOpInst(const llvm::MachineInstr *MI)
{
  return MI->getDesc().TSFlags & (1ULL << AMDID::VECTOR);
}
bool
isSkippedLiteral(const llvm::MachineInstr *MI, uint32_t opNum)
{
  uint32_t opcode = MI->getOpcode();
  if ((opcode >= AMDIL::VEXTRACTv2f32i
       && opcode <= AMDIL::VEXTRACTv4i8r)
      && (opNum == 2)) {
    return true;
  } else if ((opcode >= AMDIL::VINSERTv2f32ii)
             && (opcode <= AMDIL::VINSERTv4i8rr)
             && ((opNum == 3)  || (opNum == 4))) {
    return true;
  }
  return false;
}
bool
isBypassedLiteral(const llvm::MachineInstr *MI, uint32_t opNum)
{
  uint32_t opcode = MI->getOpcode();
  if (((int)opNum == (int)(MI->getNumOperands() - 1))
      && (isAtomicInst(MI)
          || isScratchInst(MI)
          || isLDSInst(MI)
          || isGDSInst(MI)
          || isUAVArenaInst(MI)
          || isUAVRawInst(MI)
          || isCBInst(MI)
          || opcode == AMDIL::CASE)) {
    return true;
  } else if (opNum == 1 &&
             (isAppendInst(MI)
              || isReadImageInst(MI)
              || isImageTXLDInst(MI)
              || isCBInst(MI)))  {
    return true;
  } else if  (opNum == 0 &&
              (isSemaphoreInst(MI)
               || isReadImageInst(MI)
               || isWriteImageInst(MI))) {
    return true;
  } else if (opNum == 3 && isReadImageInst(MI)) return true;

  return false;
}
bool isXComponentReg(unsigned reg) {
  return (reg >= AMDIL::Rx1 && reg < AMDIL::Rxy1)
         || reg == AMDIL::MEMx;
}
bool isYComponentReg(unsigned reg) {
  return (reg >= AMDIL::Ry1 && reg < AMDIL::Rz1);
}
bool isZComponentReg(unsigned reg) {
  return (reg >= AMDIL::Rz1 && reg < AMDIL::Rzw1);
}
bool isWComponentReg(unsigned reg) {
  return (reg >= AMDIL::Rw1 && reg < AMDIL::Rx1);
}
bool isXYComponentReg(unsigned reg) {
  return (reg >= AMDIL::Rxy1 && reg < AMDIL::Ry1)
         || reg == AMDIL::MEMxy;
}
bool isZWComponentReg(unsigned reg) {
  return (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP);
}
const char* getSrcSwizzle(unsigned idx) {
  const char *srcSwizzles[AMDIL_SRC_LAST]  = {
    "",
    ".x000", ".0x00", ".00x0", ".000x", ".y000", ".0y00", ".00y0", ".000y",
    ".z000", ".0z00", ".00z0", ".000z", ".w000", ".0w00", ".00w0", ".000w",
    ".xy00", ".00xy", ".zw00", ".00zw", ".xyz0", ".0xyz",
    ".xzxz", ".ywyw", ".x0y0", ".0x0y", ".0yzw", ".x0zw", ".xy0w",
    ".x", ".y", ".z", ".w", ".xyxy", ".zwzw", ".yzw0",
    ".z0w0", ".0z0w",
  };
  assert(idx < sizeof(srcSwizzles)/sizeof(srcSwizzles[0])
         && "Idx passed in is invalid!");
  return srcSwizzles[idx];
}
const char* getDstSwizzle(unsigned idx) {
  const char *dstSwizzles[AMDIL_DST_LAST] = {
    "", ".x___", "._y__", ".__z_", ".___w", ".xy__", ".__zw",
    ".xyz_"
  };
  assert(idx < sizeof(dstSwizzles)/sizeof(dstSwizzles[0])
         && "Idx passed in is invalid!");
  return dstSwizzles[idx];
}
/// Helper function to get the currently set flags
void getAsmPrinterFlags(MachineInstr *MI, AMDILAS::InstrResEnc &curRes)
{
  // We need 16 bits of information, but LLVMr127097 cut the field in half.
  // So we have to use two different fields to store all of our information.
  uint16_t upper = MI->getAsmPrinterFlags() << 8;
  uint16_t lower = MI->getFlags();
  curRes.u16all = upper | lower;
}
/// Helper function to clear the currently set flags and add the new flags.
void setAsmPrinterFlags(MachineInstr *MI, AMDILAS::InstrResEnc &curRes)
{
  // We need 16 bits of information, but LLVMr127097 cut the field in half.
  // So we have to use two different fields to store all of our information.
  MI->clearAsmPrinterFlags();
  MI->setFlags(0);
  uint8_t lower = curRes.u16all & 0xFF;
  uint8_t upper = (curRes.u16all >> 8) & 0xFF;
  MI->setFlags(lower);
  MI->setAsmPrinterFlag((llvm::MachineInstr::CommentFlag)upper);
}
// symTab is a dummy arg to ease the transition...
const char *
getTypeName(Type *ptr, const char *symTab,
            AMDILMachineFunctionInfo *mfi, bool signedType)
{
  Type *name = ptr;
  switch (ptr->getTypeID()) {
  case Type::StructTyID:
  {
    const StructType *ST = cast<StructType>(ptr);
    if (!ST->isOpaque())
      return "struct";
    // ptr is a pre-LLVM 3.0 "opaque" type.
    StringRef name = ST->getName();
    if (name.startswith( "struct._event_t" )) return "event";
    if (name.startswith( "struct._image1d_t" )) return "image1d";
    if (name.startswith( "struct._image1d_array_t" )) return "image1d_array";
    if (name.startswith( "struct._image2d_t" )) return "image2d";
    if (name.startswith( "struct._image2d_array_t" )) return "image2d_array";
    if (name.startswith( "struct._image3d_t" )) return "image3d";
    if (name.startswith( "struct._sema_t" )) return "semaphore";
    if (name.startswith( "struct._counter32_t" )) return "counter32";
    if (name.startswith( "struct._counter64_t" )) return "counter64";
    return "opaque";
    break;
  }
  case Type::FloatTyID:
    return "float";
  case Type::DoubleTyID:
  {
    return "double";
  }
  case Type::IntegerTyID:
  {
    LLVMContext& ctx = ptr->getContext();
    if (name == Type::getInt8Ty(ctx)) {
      return (signedType) ? "i8" : "u8";
    } else if (name == Type::getInt16Ty(ctx)) {
      return (signedType) ? "i16" : "u16";
    } else if (name == Type::getInt32Ty(ctx)) {
      return (signedType) ? "i32" : "u32";
    } else if(name == Type::getInt64Ty(ctx)) {
      return (signedType) ? "i64" : "u64";
    }
    break;
  }
  default:
    break;
  case Type::ArrayTyID:
  {
    const ArrayType *AT = cast<ArrayType>(ptr);
    name = AT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::VectorTyID:
  {
    const VectorType *VT = cast<VectorType>(ptr);
    name = VT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::PointerTyID:
  {
    const PointerType *PT = cast<PointerType>(ptr);
    name = PT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::FunctionTyID:
  {
    const FunctionType *FT = cast<FunctionType>(ptr);
    name = FT->getReturnType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  }
  name->dump();
  if (mfi) {
    mfi->addErrorMsg(amd::CompilerErrorMessage[UNKNOWN_TYPE_NAME]);
  }
  return "unknown";
}
