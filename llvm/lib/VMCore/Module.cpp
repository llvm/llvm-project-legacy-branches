//===-- Module.cpp - Implement the Module class ------------------*- C++ -*--=//
//
// This file implements the Module class for the VMCore library.
//
//===----------------------------------------------------------------------===//

#include "llvm/ValueHolderImpl.h"
#include "llvm/InstrTypes.h"
#include "llvm/BasicBlock.h"
#include "llvm/Method.h"
#include "llvm/Module.h"

// Instantiate Templates - This ugliness is the price we have to pay
// for having a DefHolderImpl.h file seperate from DefHolder.h!  :(
//
template class ValueHolder<Method, Module>;

Module::Module()
  : SymTabValue(0/*TODO: REAL TYPE*/, Value::ModuleVal, ""),
    MethodList(this, this) {
}

Module::~Module() {
  dropAllReferences();
  MethodList.delete_all();
  MethodList.setParent(0);
}


// dropAllReferences() - This function causes all the subinstructions to "let
// go" of all references that they are maintaining.  This allows one to
// 'delete' a whole class at a time, even though there may be circular
// references... first all references are dropped, and all use counts go to
// zero.  Then everything is delete'd for real.  Note that no operations are
// valid on an object that has "dropped all references", except operator 
// delete.
//
void Module::dropAllReferences() {
  MethodListType::iterator MI = MethodList.begin();
  for (; MI != MethodList.end(); MI++)
    (*MI)->dropAllReferences();
}
