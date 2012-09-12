//===-- AMDILIntrinsicInfo.cpp --------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the AMDIL Implementation of the IntrinsicInfo class.
//
//===----------------------------------------------------------------------===//

#include "AMDIL.h"
#include "AMDILIntrinsicInfo.h"
#include "AMDILTargetMachine.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include <cstring>
using namespace llvm;

#define GET_LLVM_INTRINSIC_FOR_GCC_BUILTIN
#include "AMDILGenIntrinsics.inc"
#undef GET_LLVM_INTRINSIC_FOR_GCC_BUILTIN

AMDILIntrinsicInfo::AMDILIntrinsicInfo(AMDILTargetMachine *tm)
  : TargetIntrinsicInfo(), mTM(tm)
{
}
std::string
AMDILIntrinsicInfo::getName(unsigned int IntrID, Type **Tys,
                            unsigned int numTys) const
{
  static const char* const names[] = {
#define GET_INTRINSIC_NAME_TABLE
#include "AMDILGenIntrinsics.inc"
#undef GET_INTRINSIC_NAME_TABLE
  };

  //assert(!isOverloaded(IntrID)
  //&& "AMDIL Intrinsics are not overloaded");
  if (IntrID < Intrinsic::num_intrinsics) {
    return "";
  }
  assert(IntrID < AMDILIntrinsic::num_AMDIL_intrinsics
         && "Invalid intrinsic ID");

  std::string Result(names[IntrID - Intrinsic::num_intrinsics]);
  return Result;
}
static bool
checkTruncation(const char *Name, unsigned int& Len)
{
  const char *ptr = Name + (Len - 1);
  while(ptr != Name && *ptr != '_') {
    --ptr;
  }
  // We don't want to truncate on atomic instructions
  // but we do want to enter the check Truncation
  // section so that we can translate the atomic
  // instructions if we need to.
  if (!strncmp(Name, "__atom", 6)) {
    return true;
  }
  if (strstr(ptr, "i32")
      || strstr(ptr, "u32")
      || strstr(ptr, "i64")
      || strstr(ptr, "u64")
      || strstr(ptr, "f32")
      || strstr(ptr, "f64")
      || strstr(ptr, "i16")
      || strstr(ptr, "u16")
      || strstr(ptr, "i8")
      || strstr(ptr, "u8")) {
    Len = (unsigned int)(ptr - Name);
    return true;
  }
  return false;
}
// We don't want to support both the OpenCL 1.0 atomics
// and the 1.1 atomics with different names, so we translate
// the 1.0 atomics to the 1.1 naming here if needed.
static char*
atomTranslateIfNeeded(const char *Name, unsigned int Len)
{
  char *buffer = NULL;
  if (strncmp(Name, "__atom_", 7))  {
    // If we are not starting with __atom_, then
    // go ahead and continue on with the allocation.
    buffer = new char[Len + 1];
    memcpy(buffer, Name, Len);
  } else {
    buffer = new char[Len + 3];
    memcpy(buffer, "__atomic_", 9);
    memcpy(buffer + 9, Name + 7, Len - 7);
    Len += 2;
  }
  buffer[Len] = '\0';
  return buffer;
}
unsigned int
AMDILIntrinsicInfo::lookupName(const char *Name, unsigned int Len) const
{
#define GET_FUNCTION_RECOGNIZER
#include "AMDILGenIntrinsics.inc"
#undef GET_FUNCTION_RECOGNIZER
  AMDILIntrinsic::ID IntrinsicID
    = (AMDILIntrinsic::ID)Intrinsic::not_intrinsic;
  if (checkTruncation(Name, Len)) {
    char *buffer = atomTranslateIfNeeded(Name, Len);
    IntrinsicID = getIntrinsicForGCCBuiltin("AMDIL", buffer);
    delete [] buffer;
  } else {
    IntrinsicID = getIntrinsicForGCCBuiltin("AMDIL", Name);
  }
  if (IntrinsicID != (AMDILIntrinsic::ID)Intrinsic::not_intrinsic) {
    return IntrinsicID;
  }
  return 0;
}
bool
AMDILIntrinsicInfo::isOverloaded(unsigned IntrID) const
{
  if (IntrID == 0)
    return false;
  // Overload Table
  unsigned id = IntrID - Intrinsic::num_intrinsics + 1;
#define GET_INTRINSIC_OVERLOAD_TABLE
#include "AMDILGenIntrinsics.inc"
#undef GET_INTRINSIC_OVERLOAD_TABLE
}
/// This defines the "getAttributes(ID id)" method.
#define GET_INTRINSIC_ATTRIBUTES
#include "AMDILGenIntrinsics.inc"
#undef GET_INTRINSIC_ATTRIBUTES

Function*
AMDILIntrinsicInfo::getDeclaration(Module *M, unsigned IntrID,
                                   Type **Tys,
                                   unsigned numTys) const
{
  assert(!isOverloaded(IntrID) && "AMDIL intrinsics are not overloaded");
  AttrListPtr AList = getAttributes((AMDILIntrinsic::ID) IntrID);
  LLVMContext& Context = M->getContext();
  unsigned int id = IntrID;
  Type *ResultTy = NULL;
  std::vector<Type*> ArgTys;
  bool IsVarArg = false;

#define GET_INTRINSIC_GENERATOR
#include "AMDILGenIntrinsics.inc"
#undef GET_INTRINSIC_GENERATOR
  // We need to add the resource ID argument for atomics.
  if (id >= AMDILIntrinsic::AMDIL_atomic_add_gi32
      && id <= AMDILIntrinsic::AMDIL_atomic_xor_ru64_noret) {
    ArgTys.push_back(IntegerType::get(Context, 32));
  }

  return cast<Function>(M->getOrInsertFunction(getName(IntrID),
                                               FunctionType::get(ResultTy,
                                                                 ArgTys,
                                                                 IsVarArg),
                                               AList));
}
