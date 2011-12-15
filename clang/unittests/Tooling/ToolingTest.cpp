//===- unittest/Tooling/ToolingTest.cpp - Tooling unit tests --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Twine.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclGroup.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "gtest/gtest.h"

namespace clang {
namespace tooling {

namespace {
/// Takes an ast consumer and returns it from CreateASTConsumer. This only
/// works with single translation unit compilations.
class TestAction : public clang::ASTFrontendAction {
 public:
  /// Takes ownership of TestConsumer.
  explicit TestAction(clang::ASTConsumer *TestConsumer)
      : TestConsumer(TestConsumer) {}

 protected:
  virtual clang::ASTConsumer* CreateASTConsumer(
      clang::CompilerInstance& compiler, llvm::StringRef dummy) {
    /// TestConsumer will be deleted by the framework calling us.
    return TestConsumer;
  }

 private:
  clang::ASTConsumer * const TestConsumer;
};

class FindTopLevelDeclConsumer : public clang::ASTConsumer {
 public:
  explicit FindTopLevelDeclConsumer(bool *FoundTopLevelDecl)
      : FoundTopLevelDecl(FoundTopLevelDecl) {}
  virtual bool HandleTopLevelDecl(clang::DeclGroupRef DeclGroup) {
    *FoundTopLevelDecl = true;
    return true;
  }
 private:
  bool * const FoundTopLevelDecl;
};
} // end namespace

TEST(RunSyntaxOnlyToolOnCode, FindsTopLevelDeclOnEmptyCode) {
  bool FoundTopLevelDecl = false;
  EXPECT_TRUE(RunSyntaxOnlyToolOnCode(
      new TestAction(new FindTopLevelDeclConsumer(&FoundTopLevelDecl)), ""));
  EXPECT_TRUE(FoundTopLevelDecl);
}

namespace {
class FindClassDeclXConsumer : public clang::ASTConsumer {
 public:
  FindClassDeclXConsumer(bool *FoundClassDeclX)
      : FoundClassDeclX(FoundClassDeclX) {}
  virtual bool HandleTopLevelDecl(clang::DeclGroupRef GroupRef) {
    if (CXXRecordDecl* Record = llvm::dyn_cast<clang::CXXRecordDecl>(
            *GroupRef.begin())) {
      if (Record->getName() == "X") {
        *FoundClassDeclX = true;
      }
    }
    return true;
  }
 private:
  bool *FoundClassDeclX;
};
} // end namespace

TEST(RunSyntaxOnlyToolOnCode, FindsClassDecl) {
  bool FoundClassDeclX = false;
  EXPECT_TRUE(RunSyntaxOnlyToolOnCode(new TestAction(
      new FindClassDeclXConsumer(&FoundClassDeclX)), "class X;"));
  EXPECT_TRUE(FoundClassDeclX);

  FoundClassDeclX = false;
  EXPECT_TRUE(RunSyntaxOnlyToolOnCode(new TestAction(
      new FindClassDeclXConsumer(&FoundClassDeclX)), "class Y;"));
  EXPECT_FALSE(FoundClassDeclX);
}

TEST(FindCompileArgsInJsonDatabase, FindsNothingIfEmpty) {
  std::string ErrorMessage;
  CompileCommand NotFound = FindCompileArgsInJsonDatabase(
      "a-file.cpp", "", ErrorMessage);
  EXPECT_TRUE(NotFound.CommandLine.empty()) << ErrorMessage;
  EXPECT_TRUE(NotFound.Directory.empty()) << ErrorMessage;
}

TEST(FindCompileArgsInJsonDatabase, ReadsSingleEntry) {
  llvm::StringRef Directory("/some/directory");
  llvm::StringRef FileName("/path/to/a-file.cpp");
  llvm::StringRef Command("/path/to/compiler and some arguments");
  std::string ErrorMessage;
  CompileCommand FoundCommand = FindCompileArgsInJsonDatabase(
      FileName,
      (llvm::Twine("[{\"directory\":\"") + Directory + "\"," +
                     "\"command\":\"" + Command + "\","
                     "\"file\":\"" + FileName + "\"}]").str(), ErrorMessage);
  EXPECT_EQ(Directory, FoundCommand.Directory) << ErrorMessage;
  ASSERT_EQ(4u, FoundCommand.CommandLine.size()) << ErrorMessage;
  EXPECT_EQ("/path/to/compiler", FoundCommand.CommandLine[0]) << ErrorMessage;
  EXPECT_EQ("and", FoundCommand.CommandLine[1]) << ErrorMessage;
  EXPECT_EQ("some", FoundCommand.CommandLine[2]) << ErrorMessage;
  EXPECT_EQ("arguments", FoundCommand.CommandLine[3]) << ErrorMessage;

  CompileCommand NotFound = FindCompileArgsInJsonDatabase(
      "a-file.cpp",
      (llvm::Twine("[{\"directory\":\"") + Directory + "\"," +
                     "\"command\":\"" + Command + "\","
                     "\"file\":\"" + FileName + "\"}]").str(), ErrorMessage);
  EXPECT_TRUE(NotFound.Directory.empty()) << ErrorMessage;
  EXPECT_TRUE(NotFound.CommandLine.empty()) << ErrorMessage;
}

TEST(FindCompileArgsInJsonDatabase, ReadsCompileCommandLinesWithSpaces) {
  llvm::StringRef Directory("/some/directory");
  llvm::StringRef FileName("/path/to/a-file.cpp");
  llvm::StringRef Command("\\\"/path to compiler\\\" \\\"and an argument\\\"");
  std::string ErrorMessage;
  CompileCommand FoundCommand = FindCompileArgsInJsonDatabase(
      FileName,
      (llvm::Twine("[{\"directory\":\"") + Directory + "\"," +
                     "\"command\":\"" + Command + "\","
                     "\"file\":\"" + FileName + "\"}]").str(), ErrorMessage);
  ASSERT_EQ(2u, FoundCommand.CommandLine.size());
  EXPECT_EQ("/path to compiler", FoundCommand.CommandLine[0]) << ErrorMessage;
  EXPECT_EQ("and an argument", FoundCommand.CommandLine[1]) << ErrorMessage;
}

TEST(FindCompileArgsInJsonDatabase, ReadsDirectoryWithSpaces) {
  llvm::StringRef Directory("/some directory / with spaces");
  llvm::StringRef FileName("/path/to/a-file.cpp");
  llvm::StringRef Command("a command");
  std::string ErrorMessage;
  CompileCommand FoundCommand = FindCompileArgsInJsonDatabase(
      FileName,
      (llvm::Twine("[{\"directory\":\"") + Directory + "\"," +
                     "\"command\":\"" + Command + "\","
                     "\"file\":\"" + FileName + "\"}]").str(), ErrorMessage);
  EXPECT_EQ(Directory, FoundCommand.Directory) << ErrorMessage;
}

TEST(FindCompileArgsInJsonDatabase, FindsEntry) {
  llvm::StringRef Directory("directory");
  llvm::StringRef FileName("file");
  llvm::StringRef Command("command");
  std::string JsonDatabase = "[";
  for (int I = 0; I < 10; ++I) {
    if (I > 0) JsonDatabase += ",";
    JsonDatabase += (llvm::Twine(
        "{\"directory\":\"") + Directory + llvm::Twine(I) + "\"," +
         "\"command\":\"" + Command + llvm::Twine(I) + "\","
         "\"file\":\"" + FileName + llvm::Twine(I) + "\"}").str();
  }
  JsonDatabase += "]";
  std::string ErrorMessage;
  CompileCommand FoundCommand = FindCompileArgsInJsonDatabase(
      "file4", JsonDatabase, ErrorMessage);
  EXPECT_EQ("directory4", FoundCommand.Directory) << ErrorMessage;
  ASSERT_EQ(1u, FoundCommand.CommandLine.size()) << ErrorMessage;
  EXPECT_EQ("command4", FoundCommand.CommandLine[0]) << ErrorMessage;
}

TEST(UnescapeJsonCommandLine, ReturnsEmptyArrayOnEmptyString) {
  std::vector<std::string> Result = UnescapeJsonCommandLine("");
  EXPECT_TRUE(Result.empty());
}

TEST(UnescapeJsonCommandLine, SplitsOnSpaces) {
  std::vector<std::string> Result = UnescapeJsonCommandLine("a b c");
  ASSERT_EQ(3ul, Result.size());
  EXPECT_EQ("a", Result[0]);
  EXPECT_EQ("b", Result[1]);
  EXPECT_EQ("c", Result[2]);
}

TEST(UnescapeJsonCommandLine, MungesMultipleSpaces) {
  std::vector<std::string> Result = UnescapeJsonCommandLine("   a   b   ");
  ASSERT_EQ(2ul, Result.size());
  EXPECT_EQ("a", Result[0]);
  EXPECT_EQ("b", Result[1]);
}

TEST(UnescapeJsonCommandLine, UnescapesBackslashCharacters) {
  std::vector<std::string> Backslash = UnescapeJsonCommandLine("a\\\\\\\\");
  ASSERT_EQ(1ul, Backslash.size());
  EXPECT_EQ("a\\", Backslash[0]);
  std::vector<std::string> Quote = UnescapeJsonCommandLine("a\\\\\\\"");
  ASSERT_EQ(1ul, Quote.size());
  EXPECT_EQ("a\"", Quote[0]);
}

TEST(UnescapeJsonCommandLine, DoesNotMungeSpacesBetweenQuotes) {
  std::vector<std::string> Result = UnescapeJsonCommandLine("\\\"  a  b  \\\"");
  ASSERT_EQ(1ul, Result.size());
  EXPECT_EQ("  a  b  ", Result[0]);
}

TEST(UnescapeJsonCommandLine, AllowsMultipleQuotedArguments) {
  std::vector<std::string> Result = UnescapeJsonCommandLine(
      "  \\\" a \\\"  \\\" b \\\"  ");
  ASSERT_EQ(2ul, Result.size());
  EXPECT_EQ(" a ", Result[0]);
  EXPECT_EQ(" b ", Result[1]);
}

TEST(UnescapeJsonCommandLine, AllowsEmptyArgumentsInQuotes) {
  std::vector<std::string> Result = UnescapeJsonCommandLine(
      "\\\"\\\"\\\"\\\"");
  ASSERT_EQ(1ul, Result.size());
  EXPECT_TRUE(Result[0].empty()) << Result[0];
}

TEST(UnescapeJsonCommandLine, ParsesEscapedQuotesInQuotedStrings) {
  std::vector<std::string> Result = UnescapeJsonCommandLine(
      "\\\"\\\\\\\"\\\"");
  ASSERT_EQ(1ul, Result.size());
  EXPECT_EQ("\"", Result[0]);
}

TEST(UnescapeJsonCommandLine, ParsesMultipleArgumentsWithEscapedCharacters) {
  std::vector<std::string> Result = UnescapeJsonCommandLine(
      "  \\\\\\\"  \\\"a \\\\\\\" b \\\"     \\\"and\\\\\\\\c\\\"   \\\\\\\"");
  ASSERT_EQ(4ul, Result.size());
  EXPECT_EQ("\"", Result[0]);
  EXPECT_EQ("a \" b ", Result[1]);
  EXPECT_EQ("and\\c", Result[2]);
  EXPECT_EQ("\"", Result[3]);
}

TEST(UnescapeJsonCommandLine, ParsesStringsWithoutSpacesIntoSingleArgument) {
  std::vector<std::string> QuotedNoSpaces = UnescapeJsonCommandLine(
      "\\\"a\\\"\\\"b\\\"");
  ASSERT_EQ(1ul, QuotedNoSpaces.size());
  EXPECT_EQ("ab", QuotedNoSpaces[0]);

  std::vector<std::string> MixedNoSpaces = UnescapeJsonCommandLine(
      "\\\"a\\\"bcd\\\"ef\\\"\\\"\\\"\\\"g\\\"");
  ASSERT_EQ(1ul, MixedNoSpaces.size());
  EXPECT_EQ("abcdefg", MixedNoSpaces[0]);
}

TEST(UnescapeJsonCommandLine, ParsesQuotedStringWithoutClosingQuote) {
  std::vector<std::string> Unclosed = UnescapeJsonCommandLine("\"abc");
  ASSERT_EQ(1ul, Unclosed.size());
  EXPECT_EQ("abc", Unclosed[0]);

  std::vector<std::string> EndsInBackslash = UnescapeJsonCommandLine("\"a\\");
  ASSERT_EQ(1ul, EndsInBackslash.size());
  EXPECT_EQ("a", EndsInBackslash[0]);

  std::vector<std::string> Empty = UnescapeJsonCommandLine("\"");
  ASSERT_EQ(1ul, Empty.size());
  EXPECT_EQ("", Empty[0]);
}

} // end namespace tooling
} // end namespace clang
