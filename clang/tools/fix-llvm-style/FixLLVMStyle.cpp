//=- tools/fix-llvm-style/FixLLVMStyle.cpp - Automatic LLVM style correction =//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// FIXME: This is an early first draft that needs clean-up.
//
//===----------------------------------------------------------------------===//

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"

using namespace clang;
using namespace clang::ast_matchers;
using clang::tooling::NewFrontendActionFactory;
using clang::tooling::Replacement;

// FIXME: Pull out helper methods in here into more fitting places.

template <typename T>
std::string GetFile(const clang::SourceManager& source_manager, const T& node) {
  clang::SourceLocation start_spelling_location =
      source_manager.getSpellingLoc(node.getLocStart());
  if (!start_spelling_location.isValid()) return std::string();
  clang::FileID file_id = source_manager.getFileID(start_spelling_location);
  const clang::FileEntry* file_entry =
      source_manager.getFileEntryForID(file_id);
  if (file_entry == NULL) return std::string();
  return file_entry->getName();
}

// Returns the text that makes up 'node' in the source.
// Returns an empty string if the text cannot be found.
static std::string GetText(const SourceManager &SourceManager,
                           SourceLocation LocStart, SourceLocation LocEnd) {
  SourceLocation StartSpellingLocatino =
      SourceManager.getSpellingLoc(LocStart);
  SourceLocation EndSpellingLocation =
      SourceManager.getSpellingLoc(LocEnd);
  if (!StartSpellingLocatino.isValid() || !EndSpellingLocation.isValid()) {
    return std::string();
  }
  bool Invalid = true;
  const char *Text =
    SourceManager.getCharacterData(StartSpellingLocatino, &Invalid);
  if (Invalid) {
    return std::string();
  }
  std::pair<FileID, unsigned> Start =
      SourceManager.getDecomposedLoc(StartSpellingLocatino);
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
  return std::string(Text, End.second - Start.second);
}

template <typename T>
static std::string GetText(const SourceManager &SourceManager, const T &Node) {
  return GetText(SourceManager, Node.getLocStart(), Node.getLocEnd());
}

namespace {
class FixLLVMStyle: public ast_matchers::MatchFinder::MatchCallback {
 public:
  FixLLVMStyle(tooling::Replacements *Replace)
      : Replace(Replace), EditFilesExpression(".*Tooling/.*") {}

  virtual void Run(const ast_matchers::MatchFinder::MatchResult &Result) {
    if (const CallExpr *Call = Result.Nodes.GetStmtAs<CallExpr>("call")) {
      llvm::errs() << "Skipping: "
                   << GetText(*Result.SourceManager, *Call) << "\n";
      return;
    }
    Replacement ReplaceText;
    std::string Name;
    std::string OldName;
    std::string SedCommand;
    if (const FunctionDecl *Declaration =
          Result.Nodes.GetDeclAs<FunctionDecl>("declaration")) {
      Name = Declaration->getNameAsString();
      OldName = Name;
      if (const CXXMethodDecl *Method =
            llvm::dyn_cast<CXXMethodDecl>(Declaration)) {
        if (Method->size_overridden_methods() > 0) {
          llvm::errs() << "Skipping: " << OldName << "\n";
          return;
        }
      }
      if (isupper(Name[0])) {
        Name[0] = tolower(Name[0]);
        if (Name == "new") Name = "create";

        if (const Expr *Callee = Result.Nodes.GetStmtAs<Expr>("callee")) {
          if (!EditFilesExpression.match(GetFile(*Result.SourceManager,
                                                 *Declaration))) {
            llvm::errs() << "Skipping: " << OldName << "\n";
            return;
          }
          std::string CalleeText = GetText(*Result.SourceManager, *Callee);
          std::string ReplacementText =
            CalleeText.substr(0, CalleeText.size() - OldName.size()) + Name;
          ReplaceText = Replacement(*Result.SourceManager, Callee,
                                    ReplacementText);
        } else {
          DeclarationNameInfo NameInfo = Declaration->getNameInfo();
          ReplaceText = Replacement(*Result.SourceManager, &NameInfo, Name);
        }
      }
    }
    if (EditFilesExpression.match(ReplaceText.GetFilePath())) {
      llvm::outs() << "s/" << OldName << "/" << Name << "/g;\n";
      Replace->insert(ReplaceText);
    }
  }

 private:
  tooling::Replacements *Replace;
  llvm::Regex EditFilesExpression;
};
} // end namespace

int main(int argc, char **argv) {
  tooling::RefactoringTool Tool(argc, argv);
  ast_matchers::MatchFinder Finder;
  Finder.AddMatcher(
      StatementMatcher(AnyOf(
        // Match method or function calls.
        Call(Callee(Id("declaration", Function())),
             Callee(Id("callee", Expression()))),
        // Match any calls to catch cases we can't handle yet.
        Id("call", Call()))),
      new FixLLVMStyle(&Tool.GetReplacements()));

  Finder.AddMatcher(
      DeclarationMatcher(AllOf(
        Id("declaration", Function()),
        Not(Constructor()))),
      new FixLLVMStyle(&Tool.GetReplacements()));
  return Tool.Run(NewFrontendActionFactory(&Finder));
}

