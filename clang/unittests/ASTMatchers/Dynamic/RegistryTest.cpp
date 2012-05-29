//===- unittest/ASTMatchers/Dynamic/RegistryTest.cpp - Registry unit tests -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------------===//

#include <vector>

#include "../ASTMatchersTest.h"
#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {
namespace {

testing::AssertionResult MatchesGeneric(const std::string& Code,
                                        const GenericValue& Value) {
  if (Value.is<GenericError>()) {
    EXPECT_EQ("", Value.get<GenericError>().Message);
    return testing::AssertionFailure();
  }
  const GenericMatcher Matcher = Value.get<GenericMatcher>();
  if (matches(Code, Matcher.getAs<clang::Decl>())) {
    return testing::AssertionSuccess();
  }
  if (matches(Code, Matcher.getAs<clang::Stmt>())) {
    return testing::AssertionSuccess();
  }
  if (matches(Code, Matcher.getAs<clang::QualType>())) {
    return testing::AssertionSuccess();
  }
  return testing::AssertionFailure();
}

GenericValue ConstructMatcher(const std::string& MatcherName) {
  const std::vector<GenericValue> Args;
  return Registry::constructMatcher(MatcherName, Args);
}

GenericValue ConstructMatcher(const std::string& MatcherName,
                              const GenericValue Arg1) {
  std::vector<GenericValue> Args;
  Args.push_back(Arg1);
  return Registry::constructMatcher(MatcherName, Args);
}

GenericValue ConstructMatcher(const std::string& MatcherName,
                              const GenericValue Arg1,
                              const GenericValue Arg2) {
  std::vector<GenericValue> Args;
  Args.push_back(Arg1);
  Args.push_back(Arg2);
  return Registry::constructMatcher(MatcherName, Args);
}

TEST(RegistryTest, CanConstructNoArgs) {
  const GenericValue IsArrowValue = ConstructMatcher("IsArrow");
  const GenericValue BoolValue = ConstructMatcher("BoolLiteral");

  const std::string ClassSnippet =
      "struct Foo { int x; };\n"
      "Foo* foo = new Foo;\n"
      "int i = foo->x;\n";
  const std::string BoolSnippet = "bool Foo = true;\n";

  EXPECT_TRUE(MatchesGeneric(ClassSnippet, IsArrowValue));
  EXPECT_TRUE(MatchesGeneric(BoolSnippet, BoolValue));
  EXPECT_FALSE(MatchesGeneric(ClassSnippet, BoolValue));
  EXPECT_FALSE(MatchesGeneric(BoolSnippet, IsArrowValue));
}

TEST(RegistryTest, ConstructWithSimpleArgs) {
  const GenericValue Value = ConstructMatcher("HasName", std::string("X"));
  EXPECT_TRUE(MatchesGeneric("class X {};", Value));
  EXPECT_FALSE(MatchesGeneric("int x;", Value));
}

TEST(RegistryTest, ContructWithMatcherArgs) {
  const GenericValue OperatorPlus =
      ConstructMatcher("HasOperatorName", std::string("+"));
  const GenericValue OperatorMinus =
      ConstructMatcher("HasOperatorName", std::string("-"));
  const GenericValue One =
      ConstructMatcher("IntegerLiteral", ConstructMatcher("Equals", 1));
  const GenericValue HasLHSOne = ConstructMatcher("HasLHS", One);

  const GenericValue OnePlus =
      ConstructMatcher("BinaryOperator", OperatorPlus, HasLHSOne);
  const GenericValue JustPlus =
      ConstructMatcher("BinaryOperator", OperatorPlus);

  const GenericValue OneMinus =
      ConstructMatcher("BinaryOperator", OperatorMinus, HasLHSOne);
  const GenericValue JustMinus =
      ConstructMatcher("BinaryOperator", OperatorMinus);

  EXPECT_TRUE( MatchesGeneric("int i = 1 + 1;", OnePlus));
  EXPECT_TRUE( MatchesGeneric("int i = 1 + 1;", JustPlus));
  EXPECT_FALSE(MatchesGeneric("int i = 1 + 1;", OneMinus));
  EXPECT_FALSE(MatchesGeneric("int i = 1 + 1;", JustMinus));

  EXPECT_FALSE(MatchesGeneric("int i = 1 - 1;", OnePlus));
  EXPECT_FALSE(MatchesGeneric("int i = 1 - 1;", JustPlus));
  EXPECT_TRUE( MatchesGeneric("int i = 1 - 1;", OneMinus));
  EXPECT_TRUE( MatchesGeneric("int i = 1 - 1;", JustMinus));

  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", OnePlus));
  EXPECT_TRUE( MatchesGeneric("int i = 2 + 1;", JustPlus));
  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", OneMinus));
  EXPECT_FALSE(MatchesGeneric("int i = 2 + 1;", JustMinus));
}

TEST(RegistryTest, Errors) {
  // Incorrect argument count.
  GenericValue BadArgCount = ConstructMatcher("HasBody");
  EXPECT_EQ("Incorrect argument count on function HasBody. "
            "(Expected = 1) != (Actual = 0)",
            BadArgCount.get<GenericError>().Message);
  BadArgCount = ConstructMatcher("IsArrow", std::string());
  EXPECT_EQ("Incorrect argument count on function IsArrow. "
            "(Expected = 0) != (Actual = 1)",
            BadArgCount.get<GenericError>().Message);

  // Bad argument type
  GenericValue BadArgType = ConstructMatcher("OfClass", true);
  EXPECT_EQ("Incorrect type on function OfClass for arg 0",
            BadArgType.get<GenericError>().Message);
  BadArgType = ConstructMatcher("Class", Class(), 3);
  EXPECT_EQ("Incorrect type on function Class for arg 1",
            BadArgType.get<GenericError>().Message);
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
