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

testing::AssertionResult MatchesGeneric(const std::string& Code,
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
  EXPECT_TRUE(MatchesGeneric("class X {};",
                             GenericMatcher(Class(HasName("X")))));
  EXPECT_TRUE(MatchesGeneric("int x;", GenericMatcher(Variable())));
  EXPECT_TRUE(MatchesGeneric("int foo() { return 1 + 1; }",
                             GenericMatcher(Function())));

  EXPECT_FALSE(MatchesGeneric("int x;", GenericMatcher(Function())));
  EXPECT_FALSE(MatchesGeneric("int foo() { return 1 + 1; }",
                              GenericMatcher(DeclarationReference())));
}

TEST(GeneicMatcherTest, AnyOf) {
  GenericMatcher Generic(Class(HasName("X")));

  EXPECT_TRUE(MatchesGeneric("class X {};", Generic));
  EXPECT_FALSE(MatchesGeneric("int x;", Generic));
  EXPECT_FALSE(MatchesGeneric("int foo() { return 1 + 1; }", Generic));

  Generic = GenericMatcher::anyOf(Generic, Variable());

  EXPECT_TRUE(MatchesGeneric("class X {};", Generic));
  EXPECT_TRUE(MatchesGeneric("int x;", Generic));
  EXPECT_FALSE(MatchesGeneric("int foo() { return 1 + 1; }", Generic));

  // We can mix different types of matchers (statements and declarations)
  Generic = GenericMatcher::anyOf(Generic,
                                  BinaryOperator(HasOperatorName("+")));

  EXPECT_TRUE(MatchesGeneric("class X {};", Generic));
  EXPECT_TRUE(MatchesGeneric("int x;", Generic));
  EXPECT_TRUE(MatchesGeneric("int foo() { return 1 + 1; }", Generic));
}

TEST(GeneicMatcherTest, AllOf) {
  GenericMatcher Generic(BinaryOperator(HasOperatorName("+")));

  EXPECT_TRUE(MatchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_TRUE(MatchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE(MatchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(MatchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(
      Generic,
      BinaryOperator(HasLHS(IntegerLiteral(Equals(1)))));

  EXPECT_TRUE( MatchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE( MatchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(MatchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(
      Generic,
      BinaryOperator(HasRHS(IntegerLiteral(Equals(3)))));

  EXPECT_FALSE(MatchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_TRUE( MatchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(MatchesGeneric("void foo() { }", Generic));

  Generic = GenericMatcher::allOf(Generic, Function());
  // Now nothing matches. Not even the function one because it is an AllOf().

  EXPECT_FALSE(MatchesGeneric("int i = 1 + 1;", Generic));
  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", Generic));
  EXPECT_FALSE(MatchesGeneric("int i = 1 + 3;", Generic));
  EXPECT_FALSE(MatchesGeneric("void foo() { }", Generic));

  // Still no match, because it has to match all of it on the same node.
  EXPECT_FALSE(MatchesGeneric("int foo() { return 1 + 3; }", Generic));
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
