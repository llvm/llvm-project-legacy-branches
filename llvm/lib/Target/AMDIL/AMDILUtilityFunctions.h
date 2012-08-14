//===-- AMDILUtilityFunctions.h -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides declarations for functions that are used across
// different classes and provide various conversions or utility to shorten the code.
//
//===----------------------------------------------------------------------===//

#ifndef AMDILUTILITYFUNCTIONS_H_
#define AMDILUTILITYFUNCTIONS_H_

#include "AMDIL.h"
#include "AMDILLLVMVersion.h"
#include "AMDILTargetMachine.h"
#include "llvm/ADT/SmallVector.h"

// Utility functions from ID
//
namespace llvm
{
class TargetRegisterClass;
class TargetMachine;
class SDValue;
class SDNode;
class Value;
class Type;
class TypeSymbolTable;
class StructType;
class IntegerType;
class FunctionType;
class VectorType;
class ArrayType;
class PointerType;
class OpaqueType;
class MachineInstr;
class AMDILMachineFunctionInfo;

}
enum SrcSwizzles {
  AMDIL_SRC_DFLT = 0,
  AMDIL_SRC_X000,
  AMDIL_SRC_0X00,
  AMDIL_SRC_00X0,
  AMDIL_SRC_000X,
  AMDIL_SRC_Y000,
  AMDIL_SRC_0Y00,
  AMDIL_SRC_00Y0,
  AMDIL_SRC_000Y,
  AMDIL_SRC_Z000,
  AMDIL_SRC_0Z00,
  AMDIL_SRC_00Z0,
  AMDIL_SRC_000Z,
  AMDIL_SRC_W000,
  AMDIL_SRC_0W00,
  AMDIL_SRC_00W0,
  AMDIL_SRC_000W,
  AMDIL_SRC_XY00,
  AMDIL_SRC_00XY,
  AMDIL_SRC_ZW00,
  AMDIL_SRC_00ZW,
  AMDIL_SRC_XYZ0,
  AMDIL_SRC_0XYZ,
  AMDIL_SRC_XZXZ,
  AMDIL_SRC_YWYW,
  AMDIL_SRC_X0Y0,
  AMDIL_SRC_0X0Y,
  AMDIL_SRC_0YZW,
  AMDIL_SRC_X0ZW,
  AMDIL_SRC_XY0W,
  AMDIL_SRC_XXXX,
  AMDIL_SRC_YYYY,
  AMDIL_SRC_ZZZZ,
  AMDIL_SRC_WWWW,
  AMDIL_SRC_XYXY,
  AMDIL_SRC_ZWZW,
  AMDIL_SRC_YZW0,
  AMDIL_SRC_Z0W0,
  AMDIL_SRC_0Z0W,
  AMDIL_SRC_LAST
};
enum DstSwizzles {
  AMDIL_DST_DFLT = 0,
  AMDIL_DST_X___,
  AMDIL_DST__Y__,
  AMDIL_DST___Z_,
  AMDIL_DST____W,
  AMDIL_DST_XY__,
  AMDIL_DST___ZW,
  AMDIL_DST_XYZ_,
  AMDIL_DST_LAST
};
// Function to get the correct src swizzle string from ID
const char *getSrcSwizzle(unsigned);

// Function to get the correct dst swizzle string from ID
const char *getDstSwizzle(unsigned);

const llvm::TargetRegisterClass *getRegClassFromID(unsigned int ID);

unsigned int getMoveInstFromID(unsigned int ID);
unsigned int getPHIMoveInstFromID(unsigned int ID);

// Utility functions from Type.
const llvm::TargetRegisterClass *getRegClassFromType(unsigned int type);
unsigned int getTargetIndependentMoveFromType(unsigned int type);

// Debug functions for SDNode and SDValue.
void printSDValue(const llvm::SDValue &Op, int level);
void printSDNode(const llvm::SDNode *N);

// Functions to check if an opcode is a specific type.
bool isMove(unsigned int opcode);
bool isPHIMove(unsigned int opcode);
bool isMoveOrEquivalent(unsigned int opcode);

// Function to check address space
bool check_type(const llvm::Value *ptr, unsigned int addrspace);

// Group of functions that recursively calculate the number of elements of a
// structure based on it's sub-types.
size_t getNumElements(llvm::Type * const T);
size_t getNumElements(llvm::StructType * const ST);
size_t getNumElements(llvm::IntegerType * const IT);
size_t getNumElements(llvm::FunctionType * const FT);
size_t getNumElements(llvm::ArrayType * const AT);
size_t getNumElements(llvm::VectorType * const VT);
size_t getNumElements(llvm::PointerType * const PT);
size_t getNumElements(llvm::OpaqueType * const OT);
const llvm::Value *getBasePointerValue(const llvm::Value *V);
const llvm::Value *getBasePointerValue(const llvm::MachineInstr *MI);

const char *
getTypeName(llvm::Type *ptr,
            const char *symTab,
            llvm::AMDILMachineFunctionInfo *mMFI,
            bool signedType);

int64_t GET_SCALAR_SIZE(llvm::Type* A);

// Helper functions that check the opcode for status information
bool isLoadInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isExtLoadInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isSWSExtLoadInst(const llvm::MachineInstr *MI);
bool isSExtLoadInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isZExtLoadInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isAExtLoadInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isStoreInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isTruncStoreInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isAtomicInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isVolatileInst(const llvm::MachineInstr *MI);
bool isGlobalInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isPrivateInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isConstantInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isConstantPoolInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isRegionInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isLocalInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isImageInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool is64BitImageInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isWriteImageInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isReadImageInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isImageInfoInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isImageInfo0Inst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isImageInfo1Inst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isImageTXLDInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isAppendInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isSemaphoreInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isRegionAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool is64BitRegionAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isLocalAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool is64BitLocalAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isGlobalAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool is64BitGlobalAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isArenaAtomic(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool isArenaInst(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
bool is64bitLSOp(llvm::TargetMachine &TM, const llvm::MachineInstr *MI);
// Helper functions that check a registe for status information.
bool isXComponentReg(unsigned);
bool isYComponentReg(unsigned);
bool isZComponentReg(unsigned);
bool isWComponentReg(unsigned);
bool isXYComponentReg(unsigned);
bool isZWComponentReg(unsigned);

// Macros that are used to help with switch statements for various data types
// However, these macro's do not return anything unlike the second set below.
#define ExpandCaseTo32bitIntTypes(Instr) \
case Instr##_i8: \
case Instr##_i16: \
case Instr##_i32:

#define ExpandCaseTo32bitIntTruncTypes(Instr) \
case Instr##_i16i8: \
case Instr##_i32i8: \
case Instr##_i32i16:

#define ExpandCaseToIntTypes(Instr) \
    ExpandCaseTo32bitIntTypes(Instr) \
case Instr##_i64:

#define ExpandCaseToIntTruncTypes(Instr) \
    ExpandCaseTo32bitIntTruncTypes(Instr) \
case Instr##_i64i8: \
case Instr##_i64i16: \
case Instr##_i64i32:

#define ExpandCaseToFloatTypes(Instr) \
    case Instr##_f32: \
case Instr##_f64:

#define ExpandCaseToFloatTruncTypes(Instr) \
case Instr##_f64f32:

#define ExpandCaseTo32bitScalarTypes(Instr) \
    ExpandCaseTo32bitIntTypes(Instr) \
case Instr##_f32:

#define ExpandCaseToAllScalarTypes(Instr) \
    ExpandCaseToFloatTypes(Instr) \
ExpandCaseToIntTypes(Instr)

#define ExpandCaseToAllScalarTruncTypes(Instr) \
    ExpandCaseToFloatTruncTypes(Instr) \
ExpandCaseToIntTruncTypes(Instr)

// Vector versions of above macros
#define ExpandCaseToVectorIntTypes(Instr) \
    case Instr##_v2i8: \
case Instr##_v4i8: \
case Instr##_v2i16: \
case Instr##_v4i16: \
case Instr##_v2i32: \
case Instr##_v4i32: \
case Instr##_v2i64:

#define ExpandCaseToVectorIntTruncTypes(Instr) \
case Instr##_v2i16i8: \
case Instr##_v4i16i8: \
case Instr##_v2i32i8: \
case Instr##_v4i32i8: \
case Instr##_v2i32i16: \
case Instr##_v4i32i16: \
case Instr##_v2i64i8: \
case Instr##_v2i64i16: \
case Instr##_v2i64i32:

#define ExpandCaseToVectorFloatTypes(Instr) \
    case Instr##_v2f32: \
case Instr##_v4f32: \
case Instr##_v2f64:

#define ExpandCaseToVectorFloatTruncTypes(Instr) \
case Instr##_v2f64f32:

#define ExpandCaseToVectorByteTypes(Instr) \
  case Instr##_v4i8: \
case Instr##_v2i16: \
case Instr##_v4i16:

#define ExpandCaseToAllVectorTypes(Instr) \
    ExpandCaseToVectorFloatTypes(Instr) \
ExpandCaseToVectorIntTypes(Instr)

#define ExpandCaseToAllVectorTruncTypes(Instr) \
    ExpandCaseToVectorFloatTruncTypes(Instr) \
ExpandCaseToVectorIntTruncTypes(Instr)

#define ExpandCaseToAllTypes(Instr) \
    ExpandCaseToAllVectorTypes(Instr) \
ExpandCaseToAllScalarTypes(Instr)

#define ExpandCaseToAllTruncTypes(Instr) \
    ExpandCaseToAllVectorTruncTypes(Instr) \
ExpandCaseToAllScalarTruncTypes(Instr)

#define ExpandCaseToPackedTypes(Instr) \
    case Instr##_v2i8: \
    case Instr##_v4i8: \
    case Instr##_v2i16: \
    case Instr##_v4i16:

#define ExpandCaseToByteShortScalarTypes(Instr) \
    case Instr##_i8: \
    case Instr##_i16:

#define ExpandCaseToByteShortTypes(Instr) \
    ExpandCaseToByteShortScalarTypes(Instr) \
    ExpandCaseToPackedTypes(Instr)

#define ExpandCaseToI8Types(Instr) \
    case Instr##_i8: \
    case Instr##_v2i8: \
    case Instr##_v4i8:

#define ExpandCaseToI16Types(Instr) \
    case Instr##_i16: \
    case Instr##_v2i16: \
    case Instr##_v4i16:

// Macros that expand into case statements with return values
#define ExpandCaseTo32bitIntReturn(Instr, Return) \
case Instr##_i8: return Return##_i8; \
case Instr##_i16: return Return##_i16; \
case Instr##_i32: return Return##_i32;

#define ExpandCaseToIntReturn(Instr, Return) \
    ExpandCaseTo32bitIntReturn(Instr, Return) \
case Instr##_i64: return Return##_i64;

#define ExpandCaseToFloatReturn(Instr, Return) \
    case Instr##_f32: return Return##_f32; \
case Instr##_f64: return Return##_f64;

#define ExpandCaseToAllScalarReturn(Instr, Return) \
    ExpandCaseToFloatReturn(Instr, Return) \
ExpandCaseToIntReturn(Instr, Return)

// These macros expand to common groupings of RegClass ID's
#define ExpandCaseTo1CompRegID \
case AMDIL::GPRI8RegClassID: \
case AMDIL::GPRI16RegClassID: \
case AMDIL::GPRI32RegClassID: \
case AMDIL::GPRF32RegClassID:

#define ExpandCaseTo2CompRegID \
    case AMDIL::GPRI64RegClassID: \
case AMDIL::GPRF64RegClassID: \
case AMDIL::GPRV2I8RegClassID: \
case AMDIL::GPRV2I16RegClassID: \
case AMDIL::GPRV2I32RegClassID: \
case AMDIL::GPRV2F32RegClassID:

// Macros that expand to case statements for specific bitlengths
#define ExpandCaseTo8BitType(Instr) \
    case Instr##_i8:

#define ExpandCaseTo16BitType(Instr) \
    case Instr##_v2i8: \
case Instr##_i16:

#define ExpandCaseTo32BitType(Instr) \
    case Instr##_v4i8: \
case Instr##_v2i16: \
case Instr##_i32: \
case Instr##_f32:

#define ExpandCaseTo64BitType(Instr) \
    case Instr##_v4i16: \
case Instr##_v2i32: \
case Instr##_v2f32: \
case Instr##_i64: \
case Instr##_f64:

#define ExpandCaseTo128BitType(Instr) \
    case Instr##_v4i32: \
case Instr##_v4f32: \
case Instr##_v2i64: \
case Instr##_v2f64:

bool commaPrint(int i, OSTREAM_TYPE &O);
/// Helper function to get the currently get/set flags.
void getAsmPrinterFlags(llvm::MachineInstr *MI, llvm::AMDILAS::InstrResEnc &curRes);
void setAsmPrinterFlags(llvm::MachineInstr *MI, llvm::AMDILAS::InstrResEnc &curRes);

#endif // AMDILUTILITYFUNCTIONS_H_
