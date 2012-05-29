//===--- Parser.h - Matcher expression parser -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple matcher expression parser.
// The parser understand matcher expressions of the form:
//   MatcherName(Arg0, Arg1, ..., ArgN)
// as well as simple types like string, character, number and boolean literals.
// The parser does not know how to process the matchers. It delegates this task
// to a TokenProcessor object received as an argument.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_PARSER_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_PARSER_H

#include <vector>

#include "clang/ASTMatchers/Dynamic/GenericValue.h"
#include "llvm/ADT/StringRef.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

class Parser {
 public:
  struct TokenInfo {
    llvm::StringRef Token;
    int StartLine;
    int EndLine;
    int StartColumn;
    int EndColumn;
  };

  class TokenProcessor {
   public:
    virtual ~TokenProcessor(){}
    virtual GenericValue processValueToken(const GenericValue& Value,
                                           const TokenInfo& Info) {
      return Value;
    }
    virtual GenericValue processMatcherToken(
        llvm::StringRef MatcherName,
        const std::vector<GenericValue>& Args,
        const TokenInfo& Info) = 0;
  };

  static GenericValue parseMatcher(llvm::StringRef MatcherCode,
                                   TokenProcessor* Processor);

 private:
  Parser();  // Declared, but not defined.
};

}  // namespace dynamic
}  // namespace ast_matchers
}  // namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_PARSER_H
