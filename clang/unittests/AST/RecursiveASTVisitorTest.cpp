//===- unittest/Tooling/ASTMatchersTest.cpp - AST matcher unit tests ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {

using clang::tooling::runToolOnCode;

namespace {
/// Takes an ast consumer and returns it from CreateASTConsumer. This only
/// works with single translation unit compilations.
class TestAction : public ASTFrontendAction {
 public:
  /// Takes ownership of TestConsumer.
  explicit TestAction(ASTConsumer *TestConsumer)
    : TestConsumer(TestConsumer) {}

 protected:
  virtual ASTConsumer *CreateASTConsumer(CompilerInstance &Compiler,
                                         StringRef Dummy) {
    /// TestConsumer will be deleted by the framework calling us.
    return TestConsumer;
  }

 private:
  ASTConsumer * const TestConsumer;
};

class ExpectTypeVisitor : public RecursiveASTVisitor<ExpectTypeVisitor> {
public:
  ExpectTypeVisitor(bool *Found, ASTContext *Context, SourceManager *SM,
                    StringRef ExpectedType, int Line, int Column)
    : Found(Found), Context(Context), SM(SM), ExpectedType(ExpectedType),
      Line(Line), Column(Column) {}

  bool VisitTypeLoc(TypeLoc TypeLocation) {
    FullSourceLoc Location = Context->getFullLoc(TypeLocation.getBeginLoc());
    std::string LocationText;
    llvm::raw_string_ostream Stream(LocationText);
    Location.print(Stream, *SM);
    if (Location.isValid()) {
      llvm::errs() << Stream.str() << "\n" << TypeLocation.getType().getAsString() << "\n";
      if (TypeLocation.getType().getAsString() == ExpectedType &&
          Line == Location.getSpellingLineNumber() &&
          Column == Location.getSpellingColumnNumber())
        *Found = true;
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *Reference) {
    FullSourceLoc Location = Context->getFullLoc(Reference->getLocation());
    std::string LocationText;
    llvm::raw_string_ostream Stream(LocationText);
    Location.print(Stream, *SM);
    if (Location.isValid()) {
      llvm::errs() << Stream.str() << "\n" << Reference->getNameInfo().getAsString() << "\n";
      if (Reference->getNameInfo().getAsString() == ExpectedType &&
          Line == Location.getSpellingLineNumber() &&
          Column == Location.getSpellingColumnNumber())
        *Found = true;
    }
    return true;
  }

  bool VisitDecl(Decl *Declaration) {
    FullSourceLoc Location = Context->getFullLoc(Declaration->getLocation());
    std::string LocationText;
    llvm::raw_string_ostream Stream(LocationText);
    Location.print(Stream, *SM);
    if (Location.isValid()) {
      llvm::errs() << Stream.str() << "\n"; // << Declaration->getNameAsString() << "\n";
/*      if (TypeLocation.getType().getAsString() == ExpectedType &&
          Line == Location.getSpellingLineNumber() &&
          Column == Location.getSpellingColumnNumber())
        *Found = true;*/
    }
    return true;
  }

  bool shouldVisitTemplateInstantiations() {
    return true;
  }

private:
  bool * const Found;
  ASTContext * const Context;
  SourceManager * const SM;
  const std::string ExpectedType;
  const int Line;
  const int Column;
};

class RunVisitorConsumer : public ASTConsumer {
 public:
  explicit RunVisitorConsumer(bool *Found,
                              StringRef ExpectedType, int Line, int Column)
    : Found(Found), ExpectedType(ExpectedType), Line(Line), Column(Column) {}

  virtual void HandleTranslationUnit(ASTContext &Context) {
    ExpectTypeVisitor Visitor(Found, &Context, &Context.getSourceManager(),
                              ExpectedType, Line, Column);
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }
 private:
  bool * const Found;
  const std::string ExpectedType;
  const int Line;
  const int Column;
};
} // end namespace

TEST(RecursiveASTVisitor, VisitsBaseClassDeclarations) {
  bool Found = false;
  EXPECT_TRUE(runToolOnCode(new TestAction(
                new RunVisitorConsumer(&Found, "class X", 1, 30)),
                  "class X {}; class Y : public X {};"));
  EXPECT_TRUE(Found);
}

TEST(RecursiveASTVisitor, VisitsBaseClassTemplateArguments) {
  bool Found = false;
  EXPECT_TRUE(runToolOnCode(new TestAction(
                new RunVisitorConsumer(&Found, "x", 2, 3)),
                  "void x(); template <void (*T)()> class X {};\nX<x> y;"));
  EXPECT_TRUE(Found);
}

/* FIXME: According to Richard Smith this is a bug in the AST.
TEST(RecursiveASTVisitor, VisitsBaseClassTemplateArgumentsInInstantiation) {
  bool Found = false;
  EXPECT_TRUE(runToolOnCode(new TestAction(
                new RunVisitorConsumer(&Found, "x", 3, 43)),
                  "template <typename T> void x();\n"
                  "template <void (*T)()> class X {};\n"
                  "template <typename T> class Y : public X< x<T> > {};\n"
                  "Y<int> y;"));
  EXPECT_TRUE(Found);
}
*/

} // end namespace tooling
} // end namespace clang

