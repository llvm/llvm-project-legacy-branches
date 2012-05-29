//===- unittest/ASTMatchers/Dynamic/GenericValueTest.cpp - GenericValue unit tests -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------------===//

#include "clang/ASTMatchers/Dynamic/GenericValue.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {
namespace {

TEST(GenericValueTest, String) {
  const ::std::string kString = "string";
  GenericValue Value = kString;

  EXPECT_TRUE(Value.is< ::std::string>());
  EXPECT_EQ(kString, Value.get< ::std::string>());

  EXPECT_FALSE(Value.is<GenericError>());
  EXPECT_FALSE(Value.is<bool>());
  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_FALSE(Value.is<Matcher<clang::Decl> >());
  EXPECT_FALSE(Value.is<Matcher<clang::UnaryOperator> >());
  EXPECT_FALSE(Value.is<NumberHolder>());
  EXPECT_FALSE(Value.is<int>());
  EXPECT_FALSE(Value.is<double>());
}

TEST(GenericValueTest, GenericError) {
  const ::std::string kString = "string";
  GenericValue Value = GenericError(kString);

  EXPECT_FALSE(Value.is< ::std::string>());

  EXPECT_TRUE(Value.is<GenericError>());
  EXPECT_EQ(kString, Value.get<GenericError>().Message);

  EXPECT_FALSE(Value.is<bool>());
  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_FALSE(Value.is<Matcher<clang::Decl> >());
  EXPECT_FALSE(Value.is<Matcher<clang::UnaryOperator> >());
  EXPECT_FALSE(Value.is<NumberHolder>());
  EXPECT_FALSE(Value.is<int>());
  EXPECT_FALSE(Value.is<double>());
}

TEST(GenericValueTest, Bool) {
  GenericValue Value = true;

  EXPECT_FALSE(Value.is< ::std::string>());
  EXPECT_FALSE(Value.is<GenericError>());

  EXPECT_TRUE(Value.is<bool>());
  EXPECT_EQ(true, Value.get<bool>());

  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_FALSE(Value.is<Matcher<clang::Decl> >());
  EXPECT_FALSE(Value.is<Matcher<clang::UnaryOperator> >());
  EXPECT_FALSE(Value.is<NumberHolder>());
  EXPECT_FALSE(Value.is<int>());
  EXPECT_FALSE(Value.is<double>());
}

TEST(GenericValueTest, GenericMatcher) {
  GenericValue Value = Statement();

  EXPECT_FALSE(Value.is< ::std::string>());
  EXPECT_FALSE(Value.is<GenericError>());
  EXPECT_FALSE(Value.is<bool>());

  EXPECT_TRUE(Value.is<GenericMatcher>());
  EXPECT_TRUE(Value.is<Matcher<clang::Decl> >());
  EXPECT_TRUE(Value.is<Matcher<clang::UnaryOperator> >());

  EXPECT_FALSE(Value.is<NumberHolder>());
  EXPECT_FALSE(Value.is<int>());
  EXPECT_FALSE(Value.is<double>());

  // Conversion to any type of matcher works.
  // If they are not compatible it would just return a matcher that matches
  // nothing. That is tested in GenericMatcher's unittest.
  Value = GenericMatcher(Class());
  EXPECT_TRUE(Value.is<GenericMatcher>());
  EXPECT_TRUE(Value.is<Matcher<clang::Decl> >());
  EXPECT_TRUE(Value.is<Matcher<clang::UnaryOperator> >());

  Value = GenericMatcher(UnaryOperator());
  EXPECT_TRUE(Value.is<GenericMatcher>());
  EXPECT_TRUE(Value.is<Matcher<clang::Decl> >());
  EXPECT_TRUE(Value.is<Matcher<clang::Stmt> >());
  EXPECT_TRUE(Value.is<Matcher<clang::UnaryOperator> >());
}

TEST(GenericValueTest, NumberHolder) {
  GenericValue Value = NumberHolder(0);

  EXPECT_FALSE(Value.is< ::std::string>());
  EXPECT_FALSE(Value.is<GenericError>());
  EXPECT_FALSE(Value.is<bool>());

  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_FALSE(Value.is<Matcher<clang::Decl> >());
  EXPECT_FALSE(Value.is<Matcher<clang::UnaryOperator> >());

  EXPECT_TRUE(Value.is<NumberHolder>());
  EXPECT_EQ(0, Value.get<NumberHolder>().get<int>());
  EXPECT_TRUE(Value.is<int>());
  EXPECT_EQ(0, Value.get<int>());
  EXPECT_TRUE(Value.is<double>());
  EXPECT_EQ(0.0, Value.get<double>());

  // Conversions
  Value = NumberHolder(1.1);
  EXPECT_TRUE(Value.is<NumberHolder>());
  EXPECT_EQ(1.1, Value.get<NumberHolder>().get<double>());
  EXPECT_FALSE(Value.is<int>());
  EXPECT_TRUE(Value.is<double>());
  EXPECT_EQ(1.1, Value.get<double>());
}

TEST(GenericValueTest, Assignment) {
  GenericValue Value = true;
  EXPECT_TRUE(Value.is<bool>());
  EXPECT_EQ(true, Value.get<bool>());
  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_FALSE(Value.is<GenericError>());

  Value = GenericError("foo");
  EXPECT_FALSE(Value.is<bool>());
  EXPECT_FALSE(Value.is<GenericMatcher>());
  EXPECT_TRUE(Value.is<GenericError>());
  EXPECT_EQ("foo", Value.get<GenericError>().Message);

  Value = GenericMatcher(Class());
  EXPECT_FALSE(Value.is<bool>());
  EXPECT_TRUE(Value.is<GenericMatcher>());
  EXPECT_TRUE(Value.is<Matcher<clang::Decl> >());
  EXPECT_TRUE(Value.is<Matcher<clang::UnaryOperator> >());
  EXPECT_FALSE(Value.is<GenericError>());
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
