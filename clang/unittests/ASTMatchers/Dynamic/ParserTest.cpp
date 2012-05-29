//===- unittest/ASTMatchers/Dynamic/ParserTest.cpp - Parser unit tests -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-------------------------------------------------------------------===//

#include <string>
#include <vector>

#include "../ASTMatchersTest.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {
namespace {

class ValueProcessor : public Parser::TokenProcessor {
 public:
  virtual ~ValueProcessor() { }

  void Parse(const std::string& Code) {
    const GenericValue Value = Parser::parseMatcher(Code, this);
    if (Value.is<GenericError>()) {
      ValueInfo ToStore = { Value, Parser::TokenInfo() };
      Values.push_back(ToStore);
    }
  }

  GenericValue processValueToken(const GenericValue& Value,
                                 const Parser::TokenInfo& Info) {
    ValueInfo ToStore = { Value, Info };
    Values.push_back(ToStore);
    return Value;
  }
  GenericValue processMatcherToken(
      llvm::StringRef MatcherName,
      const std::vector<GenericValue>& Args,
      const Parser::TokenInfo& Info) {
    MatcherInfo ToStore = { MatcherName, Info, Args };
    Matchers.push_back(ToStore);
    // Return the matcher name with a special format to be able to EXPECT_EQ
    // down there. No reason to actually create matchers here.
    return GenericValue("__" + MatcherName.str() + "__");
  }

  struct ValueInfo {
    GenericValue Value;
    Parser::TokenInfo Info;
  };
  struct MatcherInfo {
    std::string MatcherName;
    Parser::TokenInfo Info;
    std::vector<GenericValue> Args;
  };

  std::vector<ValueInfo> Values;
  std::vector<MatcherInfo> Matchers;
};

TEST(ParserTest, ParseString) {
  ValueProcessor Processor;
  Processor.Parse("\"Foo\"");
  Processor.Parse("\"F\\\"o o\\n\"");
  Processor.Parse("\"\"");
  Processor.Parse("\"Baz");
  EXPECT_EQ(4ULL, Processor.Values.size());
  EXPECT_EQ("Foo", Processor.Values[0].Value.get<std::string>());
  EXPECT_EQ("F\"o o\n", Processor.Values[1].Value.get<std::string>());
  EXPECT_EQ("", Processor.Values[2].Value.get<std::string>());
  EXPECT_EQ("Error parsing string token: <\"Baz>",
            Processor.Values[3].Value.get<GenericError>().Message);
}

TEST(ParserTest, ParseChar) {
  ValueProcessor Processor;
  Processor.Parse("'a'");
  Processor.Parse("'^'");
  Processor.Parse("'\\n'");
  Processor.Parse("'aa'");
  Processor.Parse("'6");
  Processor.Parse("' '");
  Processor.Parse("','");
  EXPECT_EQ(7ULL, Processor.Values.size());
  EXPECT_EQ('a', Processor.Values[0].Value.get<int>());
  EXPECT_EQ('^', Processor.Values[1].Value.get<int>());
  EXPECT_EQ('\n', Processor.Values[2].Value.get<int>());
  EXPECT_EQ("Error parsing char token: <'aa'>",
            Processor.Values[3].Value.get<GenericError>().Message);
  EXPECT_EQ("Error parsing char token: <'6>",
            Processor.Values[4].Value.get<GenericError>().Message);
  EXPECT_EQ(' ', Processor.Values[5].Value.get<int>());
  EXPECT_EQ(',', Processor.Values[6].Value.get<int>());
}

TEST(ParserTest, ParseNumber) {
  ValueProcessor Processor;
  Processor.Parse("123456789012345");
  Processor.Parse("-1234567890123456");
  Processor.Parse("1.23e5");
  Processor.Parse("1.234e-5");
  Processor.Parse("0x5");
  Processor.Parse("0d3jg#");
  EXPECT_EQ(6ULL, Processor.Values.size());
  EXPECT_EQ(123456789012345ULL, Processor.Values[0].Value.get<uint64_t>());
  EXPECT_EQ(-1234567890123456LL, Processor.Values[1].Value.get<int64_t>());
  EXPECT_EQ(1.23e5, Processor.Values[2].Value.get<int>());
  EXPECT_EQ(1.234e-5, Processor.Values[3].Value.get<double>());
  EXPECT_EQ(0x5, Processor.Values[4].Value.get<char>());
  EXPECT_EQ("Error parsing number token: <0d3jg#>",
            Processor.Values[5].Value.get<GenericError>().Message);
}

TEST(ParserTest, ParseBool) {
  ValueProcessor Processor;
  Processor.Parse("true");
  Processor.Parse("false");
  EXPECT_EQ(2UL, Processor.Values.size());
  EXPECT_EQ(true, Processor.Values[0].Value.get<bool>());
  EXPECT_EQ(false, Processor.Values[1].Value.get<bool>());
}

bool MatchesInfo(const Parser::TokenInfo& Info,
                 int StartLine, int EndLine, int StartColumn, int EndColumn) {
  EXPECT_EQ(StartLine, Info.StartLine);
  EXPECT_EQ(EndLine, Info.EndLine);
  EXPECT_EQ(StartColumn, Info.StartColumn);
  EXPECT_EQ(EndColumn, Info.EndColumn);
  return Info.StartLine == StartLine && Info.EndLine == EndLine &&
      Info.StartColumn == StartColumn && Info.EndColumn == EndColumn;
}

TEST(ParserTest, ParseMatcher) {
  ValueProcessor Processor;
  Processor.Parse(" Foo ( 1.2, Bar (), Baz( \n \"B \\nA,\\\"Z\"), \ntrue)  ");
  for (size_t i = 0; i < Processor.Values.size(); ++i) {
    EXPECT_FALSE(Processor.Values[i].Value.is<GenericError>())
        << Processor.Values[i].Value.get<GenericError>().Message;
  }

  EXPECT_EQ(3ULL, Processor.Matchers.size());
  const ValueProcessor::MatcherInfo Bar = Processor.Matchers[0];
  EXPECT_EQ("Bar", Bar.MatcherName);
  EXPECT_TRUE(MatchesInfo(Bar.Info, 1, 1, 13, 18));
  EXPECT_EQ(0ULL, Bar.Args.size());

  const ValueProcessor::MatcherInfo Baz = Processor.Matchers[1];
  EXPECT_EQ("Baz", Baz.MatcherName);
  EXPECT_TRUE(MatchesInfo(Baz.Info, 1, 2, 21, 13));
  EXPECT_EQ(1ULL, Baz.Args.size());
  EXPECT_EQ("B \nA,\"Z", Baz.Args[0].get<std::string>());

  const ValueProcessor::MatcherInfo Foo = Processor.Matchers[2];
  EXPECT_EQ("Foo", Foo.MatcherName);
  EXPECT_TRUE(MatchesInfo(Foo.Info, 1, 3, 2, 5));
  EXPECT_EQ(4ULL, Foo.Args.size());
  EXPECT_EQ(1.2, Foo.Args[0].get<double>());
  EXPECT_EQ("__Bar__", Foo.Args[1].get<std::string>());
  EXPECT_EQ("__Baz__", Foo.Args[2].get<std::string>());
  EXPECT_EQ(true, Foo.Args[3].get<bool>());
}

class RealProcessor : public Parser::TokenProcessor {
 public:
  virtual ~RealProcessor() {}
  GenericValue processMatcherToken(llvm::StringRef MatcherName,
                                   const std::vector<GenericValue>& Args,
                                   const Parser::TokenInfo& Info) {
    return Registry::constructMatcher(MatcherName, Args);
  }
};

testing::AssertionResult MatchesGeneric(const std::string& Code,
                                        const GenericValue& Value) {
  if (!Value.is<GenericMatcher>())
    return testing::AssertionFailure();
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

TEST(ParserTest, FullParserTest) {
  RealProcessor Processor;
  const GenericValue Value = Parser::parseMatcher(
      "BinaryOperator( HasOperatorName(\"+\"),\n"
      "               HasLHS(IntegerLiteral(Equals(1))))", &Processor);
  EXPECT_TRUE(MatchesGeneric("int x = 1 + 1;", Value));
  EXPECT_FALSE(MatchesGeneric("int x = 2 + 1;", Value));

  const GenericValue Error = Parser::parseMatcher(
      "BinaryOperator( HasOperatorName(6),\n"
      "               HasLHS(IntegerLiteral(Equals(1))))", &Processor);
  EXPECT_EQ("Error parsing argument 0 for matcher BinaryOperator: "
            "Error building matcher HasOperatorName: "
            "Incorrect type on function HasOperatorName for arg 0",
            Error.get<GenericError>().Message);
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
