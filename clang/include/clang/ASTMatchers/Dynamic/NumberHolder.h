//===--- NumberHolder.h - Generic number type -*- C++ -*-===/
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Implements a simple number type holder data structure.
//  It provides the storage for the value plus templated conversion operators
//  to any other numeric type.
//  Used as the underlying number value on GenericValue.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_NUMBER_HOLDER_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_NUMBER_HOLDER_H

#include <limits>

#include "llvm/Support/DataTypes.h"
#include "llvm/Support/type_traits.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

class NumberHolder {
 public:
  template <typename T>
  explicit NumberHolder(T Input) {
    if (convertToImpl(Input, &Value.Unsigned64)) {
      Type = NT_Unsigned64;
    } else if (convertToImpl(Input, &Value.Signed64)) {
      Type = NT_Signed64;
    } else if (convertToImpl(Input, &Value.Double)) {
      Type = NT_Double;
    } else {
      Type = NT_Nothing;
    }
  }

  template <typename T>
  bool is() const {
    T Converted;
    return convertTo(&Converted);
  }
  template <typename T>
  T get() const {
    T Converted;
    convertTo(&Converted);
    return Converted;
  }

 private:
  template <typename DestType>
  bool convertTo(DestType* Dest) const {
    switch (Type) {
      case NT_Unsigned64: return convertToImpl(Value.Unsigned64, Dest);
      case NT_Signed64:   return convertToImpl(Value.Signed64, Dest);
      case NT_Double:     return convertToImpl(Value.Double, Dest);
      case NT_Nothing:    return false;
    }
    return false;
  }

  template <typename SourceType, typename DestType>
  static bool convertToImpl(const SourceType Source, DestType* Dest) {
    *Dest = Source;
    // Try to convert back. This makes sure we didn't lose precision.
    // Eg. Like converting 1.1 to int.
    if (SourceType(*Dest) != Source) return false;
    // Make sure that we didn't change the sign.
    // Eg. Converting -1 to uint32_t.
    if (isNegative(*Dest) ^ isNegative(Source))
      return false;
    return true;
  }

  template <typename ValueType>
  struct CanBeNegative : public llvm::integral_constant<
      bool, std::numeric_limits<ValueType>::is_signed> {};

  template <typename ValueType>
  static bool isNegative(ValueType Value, llvm::true_type CanBeNegative) {
    return Value < 0;
  }
  template <typename ValueType>
  static bool isNegative(ValueType Value, llvm::false_type CanBeNegative) {
    return false;
  }
  template <typename ValueType>
  static bool isNegative(ValueType Value) {
    return isNegative(Value, CanBeNegative<ValueType>());
  }

  enum Type {
    NT_Nothing,
    NT_Unsigned64,
    NT_Signed64,
    NT_Double
  };
  Type Type;
  union {
    uint64_t Unsigned64;
    int64_t Signed64;
    double Double;
  } Value;
};

}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_NUMBER_HOLDER_H
