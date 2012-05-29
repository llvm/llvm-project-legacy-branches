//===--- Registry.h - Matcher registry -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Registry of all known matchers.
// The registry provides a generic interface to construct any matcher by name.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_REGISTRY_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_REGISTRY_H

#include <string>
#include <vector>

#include "clang/ASTMatchers/Dynamic/GenericValue.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

class Registry {
 public:
  // Consult the registry of known matchers and construct the appropriate
  // matcher by name.
  // It will return GenericError if the matcher is not found, or if the
  // number of arguments or argument types do not match the signature.
  static GenericValue constructMatcher(const std::string& MatcherName,
                                       const std::vector<GenericValue>& Args);

  static const std::string* getTypeMatcherName(const std::string& TypeName);
  static bool isKindName(const std::string& MatcherName);

 private:
  Registry();  // Declared, but not defined.
};

}  // namespace dynamic
}  // namespace ast_matchers
}  // namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_REGISTRY_H
