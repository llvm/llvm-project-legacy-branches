//===-- llvm/OperandTraits.h - OperandTraits class definition ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the traits classes that are handy for enforcing the correct
// layout of various User subclasses. It also provides the means for accessing
// the operands in the most efficient manner.
//

#ifndef LLVM_OPERAND_TRAITS_H
#define LLVM_OPERAND_TRAITS_H

#include "llvm/User.h"

namespace llvm {

//===----------------------------------------------------------------------===//
//                          FixedNumOperands Trait Class
//===----------------------------------------------------------------------===//

template <unsigned ARITY>
struct FixedNumOperandTraits {
  static Use *op_begin(User* U) {
    return reinterpret_cast<Use*>(U) - ARITY;
  }
  static Use *op_end(User* U) {
    return reinterpret_cast<Use*>(U);
  }
  static unsigned operands(const User*) {
    return ARITY;
  }
  struct prefix {
    Use Ops[ARITY];
    prefix(); // DO NOT IMPLEMENT
  };
  template <class U>
  struct Layout {
    struct overlay : prefix, U {
      overlay(); // DO NOT IMPLEMENT
    };
  };
  static inline void *allocate(unsigned); // FIXME
};

//===----------------------------------------------------------------------===//
//                          OptionalOperands Trait Class
//===----------------------------------------------------------------------===//

template <unsigned ARITY = 1>
struct OptionalOperandTraits : FixedNumOperandTraits<ARITY> {
  static unsigned operands(const User *U) {
    return U->getNumOperands();
  }
};

//===----------------------------------------------------------------------===//
//                          VariadicOperand Trait Class
//===----------------------------------------------------------------------===//

template <unsigned MINARITY = 0>
struct VariadicOperandTraits {
  static Use *op_begin(User* U) {
    return reinterpret_cast<Use*>(U) - U->getNumOperands();
  }
  static Use *op_end(User* U) {
    return reinterpret_cast<Use*>(U);
  }
  static unsigned operands(const User *U) {
    return U->getNumOperands();
  }
  static inline void *allocate(unsigned); // FIXME
};

//===----------------------------------------------------------------------===//
//                          HungoffOperand Trait Class
//===----------------------------------------------------------------------===//

template <unsigned MINARITY = 1>
struct HungoffOperandTraits {
  static Use *op_begin(User* U) {
    return U->OperandList;
  }
  static Use *op_end(User* U) {
    return U->OperandList + U->getNumOperands();
  }
  static unsigned operands(const User *U) {
    return U->getNumOperands();
  }
  static inline void *allocate(unsigned); // FIXME
};

/// Macro for generating in-class operand accessor declarations
#define DECLARE_TRANSPARENT_OPERAND_ACCESSORS(VALUECLASS) \
  inline VALUECLASS *getOperand(unsigned) const; \
  inline void setOperand(unsigned, VALUECLASS*); \
  inline unsigned getNumOperands() const; \
  template <unsigned Idx> inline Use &Op(); \
  template <unsigned Idx> inline const Use &Op() const

/// Macro for generating out-of-class operand accessor definitions
#define DEFINE_TRANSPARENT_OPERAND_ACCESSORS(CLASS, VALUECLASS) \
VALUECLASS *CLASS::getOperand(unsigned i) const { \
  assert(i < OperandTraits<CLASS>::operands(this) && "getOperand() out of range!"); \
  return OperandTraits<CLASS>::op_begin(const_cast<CLASS*>(this))[i]; \
} \
void CLASS::setOperand(unsigned i, VALUECLASS *Val) { \
  assert(i < OperandTraits<CLASS>::operands(this) && "setOperand() out of range!"); \
  OperandTraits<CLASS>::op_begin(this)[i] = Val; \
} \
unsigned CLASS::getNumOperands() const { return OperandTraits<CLASS>::operands(this); } \
template <unsigned Idx> Use &CLASS::Op() { \
  return OperandTraits<CLASS>::op_begin(this)[Idx]; \
} \
template <unsigned Idx> const Use &CLASS::Op() const { \
  return OperandTraits<CLASS>::op_begin(const_cast<CLASS*>(this))[Idx]; \
}


} // End llvm namespace

#endif
