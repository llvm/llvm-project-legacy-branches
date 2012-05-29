//===- tools/ast-query/ASTQuery.cpp - Command line tool to query the AST ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//  This file implements a tool to query the AST of a compilation unit by using
//  the matcher syntax.
//
//  Usage:
//  ast-query <matcher> <cmake-output-dir> <file1> <file2> ...
//
//  Where <matcher> is a string representing the AST Matcher that will be run
//  over the compilation unit.
//
//  <cmake-output-dir> is a CMake build directory in which a file named
//  compile_commands.json exists (enable -DCMAKE_EXPORT_COMPILE_COMMANDS in
//  CMake to get this output).
//
//  <file1> ... specify the paths of files in the CMake source tree. This path
//  is looked up in the compile command database. If the path of a file is
//  absolute, it needs to point into CMake's source tree. If the path is
//  relative, the current working directory needs to be in the CMake source
//  tree and the file must be in a subdirectory of the current working
//  directory. "./" prefixes in the relative files will be automatically
//  removed, but the rest of a relative path must be a suffix of a path in
//  the compile command line database.
//
//  For example, to use find all calls to a function <func> on all files in a
//  subtree of the source tree, use:
//
//    /path/in/subtree $ ast-query "Call(Callee(HasName(\"func\")))" \
//                       /path/to/build $(find . -name '*.cpp')
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include <string>
#include <vector>

#include "llvm/Support/CommandLine.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/GenericMatcher.h"
#include "clang/ASTMatchers/Dynamic/GenericValue.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

namespace {

template <typename T>
std::string GetFilePath(const SourceManager& SourceManager,
                        const T& Node) {
  SourceLocation Location = Node.getLocStart();
  static const char InvalidLocation[] = "<nofile>";
  if (Location.isInvalid()) {
    return InvalidLocation;
  }

  const FileEntry* Entry = SourceManager.getFileEntryForID(
      SourceManager.getFileID(SourceManager.getSpellingLoc(Location)));
  std::string FileName = Entry ? Entry->getName() : InvalidLocation;
  return FileName + ":";
}

template <typename T>
std::string GetPositionString(const SourceManager& SourceManager,
                              const T& Node) {
  SourceLocation StartLocation = Node.getLocStart();
  StartLocation = StartLocation.isMacroID() ?
      SourceManager.getExpansionLoc(StartLocation) :
      SourceManager.getSpellingLoc(StartLocation);

  if (!StartLocation.isValid()) return std::string();

  PresumedLoc StartPresumed = SourceManager.getPresumedLoc(StartLocation);

  return (llvm::Twine(StartPresumed.getLine()) + ":" +
      llvm::Twine(StartPresumed.getColumn()) + ": ").str();
}

// Returns the text that makes up 'node' in the source.
// Returns an empty string if the text cannot be found.
template <typename T>
std::string GetText(const SourceManager& SourceManager, const T& Node) {
  SourceLocation StartSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocStart());
  SourceLocation EndSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocEnd());
  if (!StartSpellingLocation.isValid() || !EndSpellingLocation.isValid()) {
    return std::string();
  }
  bool Invalid = true;
  const char* Text =
      SourceManager.getCharacterData(StartSpellingLocation, &Invalid);
  if (Invalid) {
    return std::string();
  }
  std::pair<FileID, unsigned> Start =
      SourceManager.getDecomposedLoc(StartSpellingLocation);
  std::pair<FileID, unsigned> End =
      SourceManager.getDecomposedLoc(Lexer::getLocForEndOfToken(
          EndSpellingLocation, 0, SourceManager, LangOptions()));
  if (Start.first != End.first) {
    // Start and end are in different files.
    return std::string();
  }
  if (End.second < Start.second) {
    // Shuffling text with macros may cause this.
    return std::string();
  }
  std::string Line = std::string(Text, End.second - Start.second);
  const size_t EndOfLine = Line.find("\n");
  if (EndOfLine != Line.npos) {
    Line = Line.substr(0, EndOfLine);
  }
  return Line;
}

class RegistryProcessor : public dynamic::Parser::TokenProcessor {
 public:
  virtual ~RegistryProcessor() {}
  dynamic::GenericValue processMatcherToken(
      llvm::StringRef MatcherName,
      const std::vector<dynamic::GenericValue>& Args,
      const dynamic::Parser::TokenInfo& Info) {
    return dynamic::Registry::constructMatcher(MatcherName, Args);
  }
};

dynamic::GenericValue AddIdNode(const dynamic::GenericValue& Value) {
  std::vector<dynamic::GenericValue> Args;
  Args.push_back(std::string("__toplevel__"));
  Args.push_back(Value);
  return dynamic::Registry::constructMatcher("Id", Args);
}

class ReportTopLevel : public ast_matchers::MatchFinder::MatchCallback {
 public:
  virtual ~ReportTopLevel() {}
  virtual void run(const MatchFinder::MatchResult& Result) {
    if (const Decl* Node = Result.Nodes.getDeclAs<Decl>("__toplevel__")) {
      ReportNode(*Result.SourceManager, *Node);
    }
    if (const Stmt* Node = Result.Nodes.getStmtAs<Stmt>("__toplevel__")) {
      ReportNode(*Result.SourceManager, *Node);
    }
  }

 private:
  template <typename NodeType>
  void ReportNode(const SourceManager& SourceManager, const NodeType& Node) {
    std::cout << GetFilePath(SourceManager, Node)
              << GetPositionString(SourceManager, Node)
              << GetText(SourceManager, Node)
              << std::endl;
  }
};

}  // end anonymous namespace

cl::opt<std::string> MatcherCode(
  cl::Positional,
  cl::desc("<matcher-code>"));

cl::opt<std::string> BuildPath(
  cl::Positional,
  cl::desc("<build-path>"));

cl::list<std::string> SourcePaths(
  cl::Positional,
  cl::desc("<source0> [... <sourceN>]"),
  cl::OneOrMore);

int main(int argc, const char **argv) {
  llvm::OwningPtr<CompilationDatabase> Compilations(
    FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  cl::ParseCommandLineOptions(argc, argv);
  if (!Compilations) {
    std::string ErrorMessage;
    Compilations.reset(CompilationDatabase::loadFromDirectory(BuildPath,
                                                              ErrorMessage));
    if (!Compilations)
      llvm::report_fatal_error(ErrorMessage);
  }

  RegistryProcessor Processor;
  const dynamic::GenericValue Value = AddIdNode(
      dynamic::Parser::parseMatcher(MatcherCode, &Processor));
  if (Value.is<dynamic::GenericError>()) {
    std::cerr << Value.get<dynamic::GenericError>().Message << std::endl;
    return 1;
  } else if (!Value.is<dynamic::GenericMatcher>()) {
    std::cerr << "Invalid matcher code.";
    return 1;
  }

  tooling::ClangTool Tool(*Compilations, SourcePaths);
  ast_matchers::MatchFinder Finder;
  Finder.addMatcher(Value.get<internal::Matcher<clang::Decl> >(),
                    new ReportTopLevel());
  Finder.addMatcher(Value.get<internal::Matcher<clang::Stmt> >(),
                    new ReportTopLevel());
  return Tool.run(newFrontendActionFactory(&Finder));
}
