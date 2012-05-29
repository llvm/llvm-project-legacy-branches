//===--- GenericValue.h - Polymorphic value type -*- C++ -*-===/
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Polymorphic value type that supports all the types required for dynamic
//  Matcher construction.
//  Used by the registry to construct matchers in a generic way.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_VALUE_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_VALUE_H

#include "GenericMatcher.h"
#include "NumberHolder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "llvm/ADT/StringRef.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

struct GenericError {
  std::string Message;
  GenericError() {}
  explicit GenericError(const std::string& Message) : Message(Message) { }
};

class GenericValue {
 public:
  GenericValue() : Type(VT_Nothing) { }
  template <typename T>
  GenericValue(const T& Value) : Type(VT_Nothing) {
    setImpl(Value);
  }
  GenericValue(const GenericValue& Other) : Type(VT_Nothing) {
    *this = Other;
  }

  ~GenericValue() {
    Reset();
  }

  GenericValue& operator=(const GenericValue& Other) {
    Reset();
    switch (Other.Type) {
      case VT_String:  setImpl(Other.get<std::string>());    break;
      case VT_Error:   setImpl(Other.get<GenericError>());   break;
      case VT_Bool:    setImpl(Other.get<bool>());           break;
      case VT_Matcher: setImpl(Other.get<GenericMatcher>()); break;
      case VT_Number:  setImpl(Other.get<NumberHolder>());   break;
      case VT_Nothing: Type = VT_Nothing;                    break;
    }
    return *this;
  }

  template <typename T>
  bool is() const {
    return isImpl(static_cast<T*>(NULL));
  }

  template <typename T>
  T get() const {
    assert(is<T>());
    return getImpl(static_cast<T*>(NULL));
  }

 private:
  void Reset() {
    switch (Type) {
      case VT_String:  delete Value.String;  break;
      case VT_Error:   delete Value.Error;   break;
      case VT_Matcher: delete Value.Matcher; break;
      case VT_Number:  delete Value.Number;  break;
      // Cases that do nothing.
      case VT_Bool: break;
      case VT_Nothing: break;
    }
    Type = VT_Nothing;
  }

  // std::string
  bool isImpl(std::string* Dummy) const { return Type == VT_String; }
  const std::string& getImpl(std::string* Dummy) const {
    return *Value.String;
  }
  void setImpl(const std::string& NewValue) {
    Type = VT_String;
    Value.String = new std::string(NewValue);
  }

  // GenericError
  bool isImpl(GenericError* Dummy) const { return Type == VT_Error; }
  const GenericError& getImpl(GenericError* Dummy) const {
    return *Value.Error;
  }
  void setImpl(const GenericError& NewValue) {
    Type = VT_Error;
    Value.Error = new GenericError(NewValue);
  }

  // bool
  bool isImpl(bool* Dummy) const { return Type == VT_Bool; }
  bool getImpl(bool* Dummy) const { return Value.Bool; }
  void setImpl(bool NewValue) {
    Type = VT_Bool;
    Value.Bool = NewValue;
  }

  // GenericMatcher and Matcher<T>
  bool isImpl(GenericMatcher* Dummy) const { return Type == VT_Matcher; }
  const GenericMatcher& getImpl(GenericMatcher* Dummy) const {
    return *Value.Matcher;
  }
  void setImpl(const GenericMatcher& NewValue) {
    Type = VT_Matcher;
    Value.Matcher = new GenericMatcher(NewValue);
  }

  template <typename T>
  bool isImpl(Matcher<T>* Dummy) const { return Type == VT_Matcher; }
  template <typename T>
  Matcher<T> getImpl(Matcher<T>* Dummy) const {
    return Value.Matcher->getAs<T>();
  }
  template <typename T>
  void setImpl(const Matcher<T>& NewValue) {
    Type = VT_Matcher;
    Value.Matcher = new GenericMatcher(NewValue);
  }

  // All numbers.
  bool isImpl(NumberHolder* Dummy) const { return Type == VT_Number; }
  const NumberHolder& getImpl(NumberHolder* Dummy) const {
    return *Value.Number;
  }
  void setImpl(const NumberHolder& NewValue) {
    Type = VT_Number;
    Value.Number = new NumberHolder(NewValue);
  }

  // For now, we assume any other type is a number.
  // A compile time error should stop any other use since it will not be
  // compatible with NumberHolder.
  template <typename T>
  bool isImpl(T* Dummy) const {
    return Type == VT_Number && Value.Number->is<T>();
  }
  template <typename T>
  T getImpl(T* Dummy) const {
    return Value.Number->get<T>();
  }
  template <typename T>
  void setImpl(const T& NewValue) {
    Type = VT_Number;
    Value.Number = new NumberHolder(NewValue);
  }

  enum ValueType {
    VT_Nothing,
    VT_Bool,
    VT_String,
    VT_Error,
    VT_Matcher,
    VT_Number
  };

  union AllValues {
    std::string* String;
    GenericError* Error;
    bool Bool;
    GenericMatcher* Matcher;
    NumberHolder* Number;
  };

  ValueType Type;
  AllValues Value;
};

}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_VALUE_H
