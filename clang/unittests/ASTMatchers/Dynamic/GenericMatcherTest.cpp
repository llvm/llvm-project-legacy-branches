//===- unittest/ASTMatchers/Dynamic/GenericMatcherTest.cpp - GenericMatcher unit tests -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------------===//

#include "../ASTMatchersTest.h"
#include "clang/ASTMatchers/Dynamic/GenericMatcher.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {
namespace {

testing::AssertionResult matchesGeneric(const std::string& Code,
                                        const GenericMatcher& Generic) {
  if (matches(Code, Generic.getAs<clang::Decl>())) {
    return testing::AssertionSuccess();
  }
  if (matches(Code, Generic.getAs<clang::Stmt>())) {
    return testing::AssertionSuccess();
  }
  if (matches(Code, Generic.getAs<clang::QualType>())) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure();
}

TEST(GeneicMatcherTest, SingleNode) {
  EXPECT_TRUE(matchesGeneric("class X {};",
                             GenericMatcher(record(hasName("X")))));
  EXPECT_TRUE(matchesGeneric("int x;", GenericMatcher(variable())));
  EXPECT_TRUE(matchesGeneric("int foo() { return 1 + 1; }",
                             GenericMatcher(function())));

  EXPECT_FALSE(matchesGeneric("int x;", GenericMatcher(function())));
  EXPECT_FALSE(matchesGeneric("int foo() { return 1 + 1; }",
                              GenericMatcher(declarationReference())));
}

TEST(GeneicMatcherTest, AnyOf) {
  GenericMatcher Generic(record(hasName("X")));

  EXPECT_TRUE(matchesGeneric("class X {};", Generic));
  EXPECT_FALSE(matchesGeneric("int x;", Generic));
  EXPECT_FALSE(matchesGeneric("int foo() { return 1 + 1; }", Generic));

  Generic = GenericMatcher::anyOf(Generic, variable());

  EXPECT_TRUE(matchesGeneric("class X {};", Generic));
  EXPECT_TRUE(matchesGeneric("int x;", Generic));
  EXPECT_FALSE(matchesGeneric("int foo() { return 1 + 1; }", Generic));

  // We can mix different types of matchers (statements and declarations)
  Generic = GenericMatcher::anyOf(Generic,
                                  binaryOperator(hasOperatorName("+")));

  EXPECT_TRUE(matchesGeneric("class X {};", Generic));
  EXPECT_TRUE(matchesGeneric("int x;", Generic));
  EXPECT_TRUE(matchesGeneric("int foo() { return 1 + 1; }", Generic));
}

TEST(GeneicMatcherTest, AllOf) {
  GenericMatcher Generic(binaryOperator(hasOperatorName("+")));

  EXPECT_TRUE(matchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_TRUE(matchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE(matchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(matchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(
      Generic,
      binaryOperator(hasLHS(integerLiteral(equals(1)))));

  EXPECT_TRUE( matchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(matchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE( matchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(matchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(
      Generic,
      binaryOperator(hasRHS(integerLiteral(equals(3)))));

  EXPECT_FALSE(matchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(matchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE( matchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(matchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(Generic, function());
  // Now nothing matches. Not even the function one because it is an AllOf().

  EXPECT_FALSE(matchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(matchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_FALSE(matchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(matchesGeneric("void foo() { }", Generic));

  // Still no match, because it has to match all of it on the same node.
  EXPECT_FALSE(matchesGeneric("int foo() { return 1 + 3; }", Generic));
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
