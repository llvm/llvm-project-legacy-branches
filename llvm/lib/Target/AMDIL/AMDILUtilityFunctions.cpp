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
int64_t GET_SCALAR_SIZE(llvm::Type *A)
{
  return A->getScalarSizeInBits();
}

const TargetRegisterClass * getRegClassFromID(unsigned int ID)
{
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

unsigned int getMoveInstFromID(unsigned int ID)
{
  switch (ID) {
  default:
    assert(0 && "Passed in ID does not match any move instructions.");
  case AMDIL::GPRI8RegClassID:
    return AMDIL::MOVE_i8;
  case AMDIL::GPRI16RegClassID:
    return AMDIL::MOVE_i16;
  case AMDIL::GPRI32RegClassID:
    return AMDIL::MOVE_i32;
  case AMDIL::GPRF32RegClassID:
    return AMDIL::MOVE_f32;
  case AMDIL::GPRI64RegClassID:
    return AMDIL::MOVE_i64;
  case AMDIL::GPRF64RegClassID:
    return AMDIL::MOVE_f64;
  case AMDIL::GPRV4F32RegClassID:
    return AMDIL::MOVE_v4f32;
  case AMDIL::GPRV4I8RegClassID:
    return AMDIL::MOVE_v4i8;
  case AMDIL::GPRV4I16RegClassID:
    return AMDIL::MOVE_v4i16;
  case AMDIL::GPRV4I32RegClassID:
    return AMDIL::MOVE_v4i32;
  case AMDIL::GPRV2F32RegClassID:
    return AMDIL::MOVE_v2f32;
  case AMDIL::GPRV2I8RegClassID:
    return AMDIL::MOVE_v2i8;
  case AMDIL::GPRV2I16RegClassID:
    return AMDIL::MOVE_v2i16;
  case AMDIL::GPRV2I32RegClassID:
    return AMDIL::MOVE_v2i32;
  case AMDIL::GPRV2F64RegClassID:
    return AMDIL::MOVE_v2f64;
  case AMDIL::GPRV2I64RegClassID:
    return AMDIL::MOVE_v2i64;
  };
  return -1;
}

unsigned int getPHIMoveInstFromID(unsigned int ID)
{
  switch (ID) {
  default:
    assert(0 && "Passed in ID does not match any move instructions.");
  case AMDIL::GPRI8RegClassID:
    return AMDIL::PHIMOVE_i8;
  case AMDIL::GPRI16RegClassID:
    return AMDIL::PHIMOVE_i16;
  case AMDIL::GPRI32RegClassID:
    return AMDIL::PHIMOVE_i32;
  case AMDIL::GPRF32RegClassID:
    return AMDIL::PHIMOVE_f32;
  case AMDIL::GPRI64RegClassID:
    return AMDIL::PHIMOVE_i64;
  case AMDIL::GPRF64RegClassID:
    return AMDIL::PHIMOVE_f64;
  case AMDIL::GPRV4F32RegClassID:
    return AMDIL::PHIMOVE_v4f32;
  case AMDIL::GPRV4I8RegClassID:
    return AMDIL::PHIMOVE_v4i8;
  case AMDIL::GPRV4I16RegClassID:
    return AMDIL::PHIMOVE_v4i16;
  case AMDIL::GPRV4I32RegClassID:
    return AMDIL::PHIMOVE_v4i32;
  case AMDIL::GPRV2F32RegClassID:
    return AMDIL::PHIMOVE_v2f32;
  case AMDIL::GPRV2I8RegClassID:
    return AMDIL::PHIMOVE_v2i8;
  case AMDIL::GPRV2I16RegClassID:
    return AMDIL::PHIMOVE_v2i16;
  case AMDIL::GPRV2I32RegClassID:
    return AMDIL::PHIMOVE_v2i32;
  case AMDIL::GPRV2F64RegClassID:
    return AMDIL::PHIMOVE_v2f64;
  case AMDIL::GPRV2I64RegClassID:
    return AMDIL::PHIMOVE_v2i64;
  };
  return -1;
}

const TargetRegisterClass* getRegClassFromType(unsigned int type)
{
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

void printSDNode(const SDNode *N)
{
  printf("Opcode: %d isTargetOpcode: %d isMachineOpcode: %d\n",
         N->getOpcode(), N->isTargetOpcode(), N->isMachineOpcode());
  printf("Empty: %d OneUse: %d Size: %d NodeID: %d\n",
         N->use_empty(), N->hasOneUse(), (int)N->use_size(), N->getNodeId());
  for (unsigned int i = 0; i < N->getNumOperands(); ++i) {
    printf("OperandNum: %d ValueCount: %d ValueType: %d\n",
           i, N->getNumValues(), N->getValueType(0) .getSimpleVT().SimpleTy);
    printSDValue(N->getOperand(i), 0);
  }
}

void printSDValue(const SDValue &Op, int level)
{
  printf("\nOp: %p OpCode: %d NumOperands: %d ", (void*)&Op, Op.getOpcode(),
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

bool isPHIMove(unsigned int opcode)
{
  switch (opcode) {
  default:
    return false;
    ExpandCaseToAllTypes(AMDIL::PHIMOVE);
    return true;
  }
  return false;
}

bool isMove(unsigned int opcode)
{
  switch (opcode) {
  default:
    return false;
    ExpandCaseToAllTypes(AMDIL::MOVE);
    return true;
  }
  return false;
}

bool isMoveOrEquivalent(unsigned int opcode)
{
  switch (opcode) {
  default:
    return isMove(opcode) || isPHIMove(opcode);
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
  case AMDIL::INTTOANY_i8:
  case AMDIL::INTTOANY_i16:
  case AMDIL::INTTOANY_i32:
  case AMDIL::INTTOANY_f32:
  case AMDIL::DLO:
  case AMDIL::LLO:
  case AMDIL::LLO_v2i64:
    return true;
  };
  return false;
}

bool check_type(const Value *ptr, unsigned int addrspace)
{
  if (!ptr) {
    return false;
  }
  Type *ptrType = ptr->getType();
  return dyn_cast<PointerType>(ptrType)->getAddressSpace() == addrspace;
}

size_t getNumElements(Type * const T)
{
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

size_t getNumElements(StructType * const ST)
{
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

size_t getNumElements(IntegerType * const IT)
{
  return (!IT) ? 0 : 1;
}

size_t getNumElements(FunctionType * const FT)
{
  assert(0 && "Should not be able to calculate the number of "
         "elements of a function type");
  return 0;
}

size_t getNumElements(ArrayType * const AT)
{
  return (!AT) ? 0
         :  (size_t)(getNumElements(AT->getElementType()) *
                     AT->getNumElements());
}

size_t getNumElements(VectorType * const VT)
{
  return (!VT) ? 0
         : VT->getNumElements() * getNumElements(VT->getElementType());
}

size_t getNumElements(PointerType * const PT)
{
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

const llvm::Value *getBasePointerValue(const llvm::MachineInstr *MI)
{
  const Value *moVal = NULL;
  if (!MI->memoperands_empty()) {
    const MachineMemOperand *memOp = (*MI->memoperands_begin());
    moVal = memOp ? memOp->getValue() : NULL;
    moVal = getBasePointerValue(moVal);
  }
  return moVal;
}

bool commaPrint(int i, OSTREAM_TYPE &O)
{
  O << ":" << i;
  return false;
}

bool isLoadInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  if (strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "LOADCONST")) {
    return false;
  }
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "LOAD");
}

bool isSWSExtLoadInst(const llvm::MachineInstr *MI)
{
  switch (MI->getOpcode()) {
  default:
    break;
    ExpandCaseToByteShortTypes(AMDIL::LOCALLOAD);
    ExpandCaseToByteShortTypes(AMDIL::GLOBALLOAD);
    ExpandCaseToByteShortTypes(AMDIL::REGIONLOAD);
    ExpandCaseToByteShortTypes(AMDIL::PRIVATELOAD);
    ExpandCaseToByteShortTypes(AMDIL::CPOOLLOAD);
    ExpandCaseToByteShortTypes(AMDIL::CONSTANTLOAD);
    return true;
  };
  return false;
}

bool isExtLoadInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "EXTLOAD");
}

bool isSExtLoadInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "SEXTLOAD");
}

bool isAExtLoadInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "AEXTLOAD");
}

bool isZExtLoadInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ZEXTLOAD");
}

bool isStoreInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "STORE");
}

bool isArenaInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ARENA");
}

bool isTruncStoreInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "TRUNCSTORE");
}

bool isAtomicInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM");
}

bool isVolatileInst(const llvm::MachineInstr *MI)
{
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
bool isGlobalInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "GLOBAL");
}
bool isPrivateInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "PRIVATE");
}
bool isConstantInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "CONSTANT")
         || strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "CPOOL");
}
bool is64bitLSOp(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return (isLoadInst(TM, MI) || isStoreInst(TM, MI))
         && strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "64_")
         && !strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "3264_");
}
bool isConstantPoolInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "CPOOL");
}
bool isRegionInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "REGION");
}
bool isLocalInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "LOCAL");
}
bool isImageInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "IMAGE");
}
bool is64BitImageInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return isImageInst(TM, MI) &&
         strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "64_");
}
bool isReadImageInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "IMAGE")
         && strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "READ");
}
bool isWriteImageInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "IMAGE")
         && strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "WRITE");
}
bool isImageInfoInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return isImageInst(TM, MI) &&
         strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "INFO");
}
bool isImageInfo0Inst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return isImageInst(TM, MI) &&
         strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "INFO0");
}
bool isImageInfo1Inst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return isImageInst(TM, MI) &&
         strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "INFO1");
}
bool isImageTXLDInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return isImageInst(TM, MI) &&
         strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "TXLD");
}
bool isSemaphoreInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "SEMAPHORE");
}
bool isAppendInst(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "APPEND");
}
bool isRegionAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM_R");
}
bool is64BitRegionAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM64_R");
}
bool isLocalAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM_L");
}
bool is64BitLocalAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM64_L");
}
bool isGlobalAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM_G")
         || isArenaAtomic(TM, MI);
}
bool is64BitGlobalAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM64_G");
}
bool isArenaAtomic(TargetMachine &TM, const llvm::MachineInstr *MI)
{
  return strstr(TM.getInstrInfo()->getName(MI->getOpcode()), "ATOM_A");
}

bool isXComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Rx1 && reg < AMDIL::Rxy1)
         || reg == AMDIL::MEMx;
}
bool isYComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Ry1 && reg < AMDIL::Rz1);
}
bool isZComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Rz1 && reg < AMDIL::Rzw1);
}
bool isWComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Rw1 && reg < AMDIL::Rx1);
}
bool isXYComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Rxy1 && reg < AMDIL::Ry1)
         || reg == AMDIL::MEMxy;
}
bool isZWComponentReg(unsigned reg)
{
  return (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP);
}

const char* getSrcSwizzle(unsigned idx)
{
  const char *srcSwizzles[AMDIL_SRC_LAST]  = {
    "",
    ".x000", ".0x00", ".00x0", ".000x", ".y000", ".0y00", ".00y0", ".000y",
    ".z000", ".0z00", ".00z0", ".000z", ".w000", ".0w00", ".00w0", ".000w",
    ".xy00", ".00xy", ".zw00", ".00zw", ".xyz0", ".0xyz",
    ".xzxz", ".ywyw", ".x0y0", ".0x0y", ".0yzw", ".x0zw", ".xy0w",
    ".x"   , ".y"   , ".z"   , ".w"   , ".xyxy", ".zwzw", ".yzw0",
    ".z0w0", ".0z0w",
  };
  assert(idx < sizeof(srcSwizzles)/sizeof(srcSwizzles[0])
         && "Idx passed in is invalid!");
  return srcSwizzles[idx];
}
const char* getDstSwizzle(unsigned idx)
{
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
  case Type::StructTyID: {
    const StructType *ST = cast<StructType>(ptr);
    if (!ST->isOpaque())
      return "struct";
    // ptr is a pre-LLVM 3.0 "opaque" type.
    StringRef name = ST->getName();
    if (name.startswith( "struct._event_t" ))         return "event";
    if (name.startswith( "struct._image1d_t" ))       return "image1d";
    if (name.startswith( "struct._image1d_array_t" )) return "image1d_array";
    if (name.startswith( "struct._image2d_t" ))       return "image2d";
    if (name.startswith( "struct._image2d_array_t" )) return "image2d_array";
    if (name.startswith( "struct._image3d_t" ))       return "image3d";
    if (name.startswith( "struct._sema_t" ))          return "semaphore";
    if (name.startswith( "struct._counter32_t" ))     return "counter32";
    if (name.startswith( "struct._counter64_t" ))     return "counter64";
    return "opaque";
    break;
  }
  case Type::FloatTyID:
    return "float";
  case Type::DoubleTyID: {
    return "double";
  }
  case Type::IntegerTyID: {
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
  case Type::ArrayTyID: {
    const ArrayType *AT = cast<ArrayType>(ptr);
    name = AT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::VectorTyID: {
    const VectorType *VT = cast<VectorType>(ptr);
    name = VT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::PointerTyID: {
    const PointerType *PT = cast<PointerType>(ptr);
    name = PT->getElementType();
    return getTypeName(name, symTab, mfi, signedType);
    break;
  }
  case Type::FunctionTyID: {
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
