//===--- ASTMatchFinder.h - Structural query framework ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Provides a way to construct a FrontendAction that runs given matchers
//  over the AST and invokes a given callback on every match.
//
//  The general idea is to construct a matcher expression that describes a
//  subtree match on the AST. Next, a callback that is executed every time the
//  expression matches is registered, and the matcher is run over the AST of
//  some code. Matched subexpressions can be bound to string IDs and easily
//  be accessed from the registered callback. The callback can than use the
//  AST nodes that the subexpressions matched on to output information about
//  the match or construct changes that can be applied to the code.
//
//  Example:
//  class HandleMatch : public MatchFinder::MatchCallback {
//  public:
//    virtual void Run(const MatchFinder::MatchResult &Result) {
//      const CXXRecordDecl *Class =
//          Result.Nodes.GetDeclAs<CXXRecordDecl>("id");
//      ...
//    }
//  };
//
//  int main(int argc, char **argv) {
//    ClangTool Tool(argc, argv);
//    MatchFinder finder;
//    finder.AddMatcher(Id("id", Class(HasName("::a_namespace::AClass"))),
//                      new HandleMatch);
//    return Tool.Run(finder.NewFrontendActionFactory());
//  }
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_AST_MATCH_FINDER_H
#define LLVM_CLANG_AST_MATCHERS_AST_MATCH_FINDER_H

#include "clang/ASTMatchers/ASTMatchers.h"

namespace clang {

class FrontendAction;

namespace ast_matchers {

/// \brief A class to allow finding matches over the Clang AST.
///
/// After creation, you can add multiple matchers to the MatchFinder via
/// calls to AddMatcher(...).
///
/// Once all matchers are added, NewFrontendAction() returns a FrontendAction
/// that will trigger the callbacks specified via AddMatcher(...) when a match
/// is found.
///
/// See ASTMatchers.h for more information about how to create matchers.
///
/// Not intended to be subclassed.
class MatchFinder {
public:
  /// \brief Contains all information for a given match.
  ///
  /// Every time a match is found, the MatchFinder will invoke the registered
  /// MatchCallback with a MatchResult containing information about the match.
  struct MatchResult {
    MatchResult(const BoundNodes &Nodes, clang::ASTContext *Context,
                clang::SourceManager *SourceManager);

    /// \brief Contains the nodes bound on the current match.
    ///
    /// This allows user code to easily extract matched AST nodes.
    const BoundNodes Nodes;

    /// \brief Utilities for interpreting the matched AST structures.
    /// @{
    clang::ASTContext * const Context;
    clang::SourceManager * const SourceManager;
    /// @}
  };

  /// \brief Called when the Match registered for it was successfully found
  /// in the AST.
  class MatchCallback {
  public:
    virtual ~MatchCallback();
    virtual void Run(const MatchResult &Result) = 0;
  };

  /// \brief Called when parsing is finished. Intended for testing only.
  class ParsingDoneTestCallback {
  public:
    virtual ~ParsingDoneTestCallback();
    virtual void Run() = 0;
  };

  MatchFinder();
  ~MatchFinder();

  /// \brief Adds a matcher to execute when running over the AST.
  ///
  /// Calls 'Action' with the BoundNodes on every match.
  /// Adding more than one 'NodeMatch' allows finding different matches in a
  /// single pass over the AST.
  /// @{
  void AddMatcher(const DeclarationMatcher &NodeMatch,
                  MatchCallback *Action);
  void AddMatcher(const TypeMatcher &NodeMatch,
                  MatchCallback *Action);
  void AddMatcher(const StatementMatcher &NodeMatch,
                  MatchCallback *Action);
  /// @}

  /// \brief Creates a clang FrontendAction that finds all matches.
  FrontendAction *NewFrontendAction();

  /// \brief Registers a callback to notify the end of parsing.
  ///
  /// The provided closure is called after parsing is done, before the AST is
  /// traversed. Useful for benchmarking.
  /// Each call to FindAll(...) will call the closure once.
  void RegisterTestCallbackAfterParsing(ParsingDoneTestCallback *ParsingDone);

private:
  /// \brief The MatchCallback*'s will be called every time the
  /// UntypedBaseMatcher matches on the AST.
  std::vector< std::pair<
    const internal::UntypedBaseMatcher*,
    MatchCallback*> > Triggers;

  /// \brief Called when parsing is done.
  ParsingDoneTestCallback *ParsingDone;

  friend class MatchFinderFrontendActionFactory;
};

} // end namespace ast_matchers
} // end namespace clang

#endif // LLVM_CLANG_AST_MATCHERS_AST_MATCH_FINDER_H
