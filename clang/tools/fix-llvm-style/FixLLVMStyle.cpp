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

bool AllParentsMatch(SourceManager *SM, const CXXRecordDecl *Decl, llvm::Regex &EditFilesExpression) {
  if (!EditFilesExpression.match(GetFile(*SM, *Decl))) {
    return false;
  }
  typedef clang::CXXRecordDecl::base_class_const_iterator BaseIterator;
  for (BaseIterator It = Decl->bases_begin(),
                    End = Decl->bases_end(); It != End; ++It) {
    const clang::Type *TypeNode = It->getType().getTypePtr();
    clang::CXXRecordDecl *
      ClassDecl = TypeNode->getAsCXXRecordDecl();
    if (!AllParentsMatch(SM, ClassDecl, EditFilesExpression)) {
      return false;
    }
  }
  return true;
}

class FixLLVMStyle: public ast_matchers::MatchFinder::MatchCallback {
 public:
  FixLLVMStyle(tooling::Replacements *Replace)
      : Replace(Replace), EditFilesExpression(".*ASTMatchers/.*") {}

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
    if (const NamedDecl *Declaration =
          Result.Nodes.GetDeclAs<NamedDecl>("declaration")) {
      Name = Declaration->getNameAsString();
      OldName = Name;
      if (const CXXMethodDecl *Method =
            llvm::dyn_cast<CXXMethodDecl>(Declaration)) {
        if (Method->size_overridden_methods() > 0 &&
            !AllParentsMatch(Result.SourceManager, Method->getParent(), EditFilesExpression)) {
          llvm::errs() << "NotAllParentsMatch: " << OldName << "\n";
          return;
        }
      }
      if (isupper(Name[0])) {
        Name[0] = tolower(Name[0]);
        if (Name == "new") Name = "create";

        if (const DeclRefExpr *Reference = Result.Nodes.GetStmtAs<DeclRefExpr>("ref")) {
          ReplaceText = Replacement(*Result.SourceManager, Reference, Name);
        } else if (const Expr *Callee = Result.Nodes.GetStmtAs<Expr>("callee")) {
          if (const MemberExpr *Member = dyn_cast<MemberExpr>(Callee)) {
            llvm::errs() << OldName << "\n";
            assert(Member != NULL);
//          std::string CalleeText = GetText(*Result.SourceManager, *Callee);
//          llvm::outs() << "Callee: " << CalleeText << "\n";
//          std::string ReplacementText =
//            Name + CalleeText.substr(OldName.size(), CalleeText.size() - OldName.size());
            ReplaceText = Replacement(*Result.SourceManager, CharSourceRange::getTokenRange(SourceRange(Member->getMemberLoc(), Member->getMemberLoc())),
                                      Name);
          } else if (const DeclRefExpr *Ref = dyn_cast<DeclRefExpr>(Callee)) {
            (void)Ref;
            llvm::errs() << "XXX " << GetFile(*Result.SourceManager, *Callee) << "\n";
          } else {
            llvm::errs() << "*** " << GetFile(*Result.SourceManager, *Callee) << "\n";
            Callee->dump();
          }
        } else {
          DeclarationNameInfo NameInfo;
          if (const FunctionDecl *Function = llvm::dyn_cast<FunctionDecl>(Declaration)) {
            NameInfo = Function->getNameInfo();
          } else if (const UsingDecl *Using = llvm::dyn_cast<UsingDecl>(Declaration)) {
            NameInfo = Using->getNameInfo();
          }
          ReplaceText = Replacement(*Result.SourceManager, &NameInfo, Name);
          if (!ReplaceText.IsApplicable()) {
            llvm::errs() << "Not applicable: " << Name << "\n";
          }
        }
      }
    }
    if (EditFilesExpression.match(ReplaceText.GetFilePath())) {
      llvm::errs() << ReplaceText.GetFilePath() << ":" << ReplaceText.GetOffset() << ", " << ReplaceText.GetLength() << ": s/" << OldName << "/" << Name << "/g;\n";
//      Replace->insert(ReplaceText);
    }
  }

 private:
  tooling::Replacements *Replace;
  llvm::Regex EditFilesExpression;
};
} // end namespace

const internal::VariadicDynCastAllOfMatcher<clang::Decl, clang::UsingDecl> UsingDeclaration;
namespace clang { namespace ast_matchers {
AST_MATCHER_P(clang::UsingDecl, HasAnyUsingShadowDeclaration,
              internal::Matcher<clang::UsingShadowDecl>, InnerMatcher) {
  for (clang::UsingDecl::shadow_iterator I = Node.shadow_begin();
       I != Node.shadow_end(); ++I) {
    if (InnerMatcher.Matches(**I, Finder, Builder)) {
      return true;
    }
  }
  return false;
}
AST_MATCHER_P(clang::UsingShadowDecl, HasTargetDeclaration,
              internal::Matcher<clang::NamedDecl>, InnerMatcher) {
  return InnerMatcher.Matches(*Node.getTargetDecl(), Finder, Builder);
}
} }


int main(int argc, char **argv) {
  tooling::RefactoringTool Tool(argc, argv);
  ast_matchers::MatchFinder Finder;
  
  Finder.AddMatcher(StatementMatcher(AnyOf(
      StatementMatcher(Id("ref", DeclarationReference(To(Id("declaration", Function()))))),
      Call(Callee(Id("declaration", Function())),
           Callee(Id("callee", Expression()))))),
      new FixLLVMStyle(&Tool.GetReplacements()));

  Finder.AddMatcher(
      DeclarationMatcher(AnyOf(
        Id("declaration", UsingDeclaration(HasAnyUsingShadowDeclaration(HasTargetDeclaration(Function())))),
        AllOf(
          Id("declaration", Function()),
          Not(Constructor())))
        ),
      new FixLLVMStyle(&Tool.GetReplacements()));
  return Tool.Run(NewFrontendActionFactory(&Finder));
}

