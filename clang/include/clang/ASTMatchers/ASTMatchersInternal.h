//===--- ASTMatchersInternal.h - Structural query framework -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Implements the base layer of the matcher framework.
//
//  Matchers are methods that return a Matcher<T> which provides a method
//  Matches(...) which is a predicate on an AST node. The Matches method's
//  parameters define the context of the match, which allows matchers to recurse
//  or store the current node as bound to a specific string, so that it can be
//  retrieved later.
//
//  In general, matchers have two parts:
//  1. A function Matcher<T> MatcherName(<arguments>) which returns a Matcher<T>
//     based on the arguments and optionally on template type deduction based
//     on the arguments. Matcher<T>s form an implicit reverse hierarchy
//     to clang's AST class hierarchy, meaning that you can use a Matcher<Base>
//     everywhere a Matcher<Derived> is required.
//  2. An implementation of a class derived from MatcherInterface<T>.
//
//  The matcher functions are defined in ASTMatchers.h. To make it possible
//  to implement both the matcher function and the implementation of the matcher
//  interface in one place, ASTMatcherMacros.h defines macros that allow
//  implementing a matcher in a single place.
//
//  This file contains the base classes needed to construct the actual matchers.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_AST_MATCHERS_INTERNAL_H
#define LLVM_CLANG_AST_MATCHERS_AST_MATCHERS_INTERNAL_H

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/Stmt.h"
#include "llvm/ADT/VariadicFunction.h"
#include <map>
#include <string>
#include <vector>

/// FIXME: Move into the llvm support library.
template <bool> struct CompileAssert {};
#define TOOLING_COMPILE_ASSERT(Expr, Msg) \
  typedef CompileAssert<(bool(Expr))> Msg[bool(Expr) ? 1 : -1]

namespace clang {
namespace ast_matchers {

class BoundNodes;

namespace internal {

class BoundNodesTreeBuilder;

/// \brief A tree of bound nodes in match results.
///
/// If a match can contain multiple matches on the same node with different
/// matching subexpressions, BoundNodesTree contains a branch for each of
/// those matching subexpressions.
///
/// BoundNodesTree's are created during the matching process; when a match
/// is found, we iterate over the tree and create a BoundNodes object containing
/// the union of all bound nodes on the path from the root to a each leaf.
class BoundNodesTree {
public:
  /// A visitor interface to visit all BoundNodes results for a BoundNodesTree.
  class Visitor {
  public:
    virtual ~Visitor() {}

    /// Called multiple times during a single call to VisitMatches(...).
    /// 'BoundNodesView' contains the bound nodes for a single match.
    virtual void VisitMatch(const BoundNodes& BoundNodesView) = 0;
  };

  BoundNodesTree();

  /// Create a BoundNodesTree from pre-filled maps of bindings.
  BoundNodesTree(const std::map<std::string, const clang::Decl*>& DeclBindings,
                 const std::map<std::string, const clang::Stmt*>& StmtBindings,
                 const std::vector<BoundNodesTree> RecursiveBindings);

  /// Adds all bound nodes to bound_nodes_builder.
  void CopyTo(BoundNodesTreeBuilder* Builder) const;

  /// Visits all matches that this BoundNodesTree represents.
  /// The ownership of 'visitor' remains at the caller.
  void VisitMatches(Visitor* ResultVisitor);

private:
  void VisitMatchesRecursively(
      Visitor* ResultVistior,
      std::map<std::string, const clang::Decl*> DeclBindings,
      std::map<std::string, const clang::Stmt*> StmtBindings);

  template <typename T>
  void CopyBindingsTo(const T& bindings, BoundNodesTreeBuilder* Builder) const;

  // FIXME: Find out whether we want to use different data structures here -
  // first benchmarks indicate that it doesn't matter though.

  std::map<std::string, const clang::Decl*> DeclBindings;
  std::map<std::string, const clang::Stmt*> StmtBindings;

  std::vector<BoundNodesTree> RecursiveBindings;
};

/// \brief Creates BoundNodesTree objects.
///
/// The tree builder is used during the matching process to insert the bound
/// nodes from the Id matcher.
class BoundNodesTreeBuilder {
public:
  BoundNodesTreeBuilder();

  /// Add a binding from an id to a node.
  /// FIXME: Add overloads for all AST base types.
  /// @{
  void SetBinding(const std::pair<const std::string,
                                  const clang::Decl*>& binding);
  void SetBinding(const std::pair<const std::string,
                                  const clang::Stmt*>& binding);
  /// @}

  /// Adds a branch in the tree.
  void AddMatch(const BoundNodesTree& Bindings);

  /// Returns a BoundNodes object containing all current bindings.
  BoundNodesTree Build() const;

private:
  BoundNodesTreeBuilder(const BoundNodesTreeBuilder&);  // DO NOT IMPLEMENT
  void operator=(const BoundNodesTreeBuilder&);  // DO NOT IMPLEMENT

  std::map<std::string, const clang::Decl*> DeclBindings;
  std::map<std::string, const clang::Stmt*> StmtBindings;

  std::vector<BoundNodesTree> RecursiveBindings;
};

class ASTMatchFinder;

/// \brief Generic interface for matchers on an AST node of type T.
///
/// Implement this if your matcher may need to inspect the children or
/// descendants of the node or bind matched nodes to names. If you are
/// writing a simple matcher that only inspects properties of the
/// current node and doesn't care about its children or descendants,
/// implement SingleNodeMatcherInterface instead.
template <typename T>
class MatcherInterface : public llvm::RefCountedBaseVPTR {
public:
  virtual ~MatcherInterface() {}

  /// \brief Returns true if 'Node' can be matched.
  ///
  /// May bind 'Node' to an ID via 'Builder', or recurse into
  /// the AST via 'Finder'.
  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const = 0;
};

/// \brief Interface for matchers that only evaluate properties on a single node.
template <typename T>
class SingleNodeMatcherInterface : public MatcherInterface<T> {
public:
  /// \brief Returns true if the matcher matches the provided node.
  ///
  /// A subclass must implement this instead of Matches().
  virtual bool MatchesNode(const T &Node) const = 0;

private:
  /// Implements MatcherInterface::Matches.
  virtual bool Matches(const T &Node,
                       ASTMatchFinder * /* Finder */,
                       BoundNodesTreeBuilder * /*  Builder */) const {
    return MatchesNode(Node);
  }
};

/// \brief Wrapper of a MatcherInterface<T> *that allows copying.
///
/// A Matcher<Base> can be used anywhere a Matcher<Derived> is
/// required. This establishes an is-a relationship which is reverse
/// to the AST hierarchy. In other words, Matcher<T> is contravariant
/// with respect to T. The relationship is built via a type conversion
/// operator rather than a type hierarchy to be able to templatize the
/// type hierarchy instead of spelling it out.
template <typename T>
class Matcher {
public:
  /// Takes ownership of the provided implementation pointer.
  explicit Matcher(MatcherInterface<T> *Implementation)
      : Implementation(Implementation) {}

  /// Forwards the call to the underlying MatcherInterface<T> pointer.
  bool Matches(const T &Node,
               ASTMatchFinder *Finder,
               BoundNodesTreeBuilder *Builder) const {
    return Implementation->Matches(Node, Finder, Builder);
  }

  /// Implicitly converts this object to a Matcher<Derived>; requires
  /// Derived to be derived from T.
  template <typename Derived>
  operator Matcher<Derived>() const {
    return Matcher<Derived>(new ImplicitCastMatcher<Derived>(*this));
  }

  /// Returns an ID that uniquely identifies the matcher.
  uint64_t GetID() const {
    /// FIXME: Document the requirements this imposes on matcher
    /// implementations (no new() implementation_ during a Matches()).
    return reinterpret_cast<uint64_t>(Implementation.getPtr());
  }

private:
  /// Allows conversion from Matcher<T> to Matcher<Derived> if Derived
  /// is derived from T.
  template <typename Derived>
  class ImplicitCastMatcher : public MatcherInterface<Derived> {
  public:
    explicit ImplicitCastMatcher(const Matcher<T> &From)
        : From(From) {}

    virtual bool Matches(const Derived &Node,
                         ASTMatchFinder *Finder,
                         BoundNodesTreeBuilder *Builder) const {
      return From.Matches(Node, Finder, Builder);
    }

  private:
    const Matcher<T> From;
  };

  llvm::IntrusiveRefCntPtr< MatcherInterface<T> > Implementation;
};  // class Matcher

/// A convenient helper for creating a Matcher<T> without specifying
/// the template type argument.
template <typename T>
inline Matcher<T> MakeMatcher(MatcherInterface<T> *Implementation) {
  return Matcher<T>(Implementation);
}

/// Matches declarations for QualType and CallExpr. Type argument
/// DeclMatcherT is required by PolymorphicMatcherWithParam1 but not
/// actually used.
template <typename T, typename DeclMatcherT>
class HasDeclarationMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT((llvm::is_same< DeclMatcherT,
                                         Matcher<clang::Decl> >::value),
                          instantiated_with_wrong_types);
public:
  explicit HasDeclarationMatcher(const Matcher<clang::Decl> &InnerMatcher)
      : InnerMatcher(InnerMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return MatchesSpecialized(Node, Finder, Builder);
  }

private:
  /// Extracts the CXXRecordDecl of a QualType and returns whether the inner
  /// matcher matches on it.
  bool MatchesSpecialized(const clang::QualType &Node, ASTMatchFinder *Finder,
                          BoundNodesTreeBuilder *Builder) const {
    /// FIXME: Add other ways to convert...
    clang::CXXRecordDecl *NodeAsRecordDecl = Node->getAsCXXRecordDecl();
    return NodeAsRecordDecl != NULL &&
      InnerMatcher.Matches(*NodeAsRecordDecl, Finder, Builder);
  }

  /// Extracts the Decl of the callee of a CallExpr and returns whether the
  /// inner matcher matches on it.
  bool MatchesSpecialized(const clang::CallExpr &Node, ASTMatchFinder *Finder,
                          BoundNodesTreeBuilder *Builder) const {
    const clang::Decl *NodeAsDecl = Node.getCalleeDecl();
    return NodeAsDecl != NULL &&
      InnerMatcher.Matches(*NodeAsDecl, Finder, Builder);
  }

  /// Extracts the Decl of the constructor call and returns whether the inner
  /// matcher matches on it.
  bool MatchesSpecialized(const clang::CXXConstructExpr &Node,
                          ASTMatchFinder *Finder,
                          BoundNodesTreeBuilder *Builder) const {
    const clang::Decl *NodeAsDecl = Node.getConstructor();
    return NodeAsDecl != NULL &&
      InnerMatcher.Matches(*NodeAsDecl, Finder, Builder);
  }

  /// Extracts the Decl of the constructor ran by the new expression and
  /// returns whether the inner matcher matches on it.
  bool MatchesSpecialized(const clang::CXXNewExpr& Node,
                          ASTMatchFinder* Finder,
                          BoundNodesTreeBuilder* Builder) const {
    const clang::Decl* NodeAsDecl = Node.getConstructor();
    return NodeAsDecl != NULL &&
        InnerMatcher.Matches(*NodeAsDecl, Finder, Builder);
  }

  const Matcher<clang::Decl> InnerMatcher;
};

/// IsBaseType<T>::value is true if T is a "base" type in the AST
/// node class hierarchies (i.e. if T is Decl, Stmt, or QualType).
template <typename T>
struct IsBaseType {
  static const bool value = (llvm::is_same<T, clang::Decl>::value ||
                             llvm::is_same<T, clang::Stmt>::value ||
                             llvm::is_same<T, clang::QualType>::value);
};
template <typename T>
const bool IsBaseType<T>::value;

/// Interface that can match any AST base node type and contains default
/// implementations returning false.
class UntypedBaseMatcher {
public:
  virtual ~UntypedBaseMatcher() {}

  virtual bool Matches(const clang::Decl &DeclNode, ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return false;
  }
  virtual bool Matches(const clang::QualType &TypeNode, ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return false;
  }
  virtual bool Matches(const clang::Stmt &StmtNode, ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return false;
  }

  /// Returns a unique ID for the matcher.
  virtual uint64_t GetID() const = 0;
};

/// An UntypedBaseMatcher that overwrites the Matches(...) method for node
/// type T. T must be an AST base type.
template <typename T>
class TypedBaseMatcher : public UntypedBaseMatcher {
  TOOLING_COMPILE_ASSERT(IsBaseType<T>::value,
                         typed_base_matcher_can_only_be_used_with_base_type);
public:
  explicit TypedBaseMatcher(const Matcher<T> &InnerMatcher)
      : InnerMatcher(InnerMatcher) {}

  using UntypedBaseMatcher::Matches;
  /// Implements UntypedBaseMatcher::Matches. Since T is guaranteed to
  /// be a "base" AST node type, this method is guaranteed to override
  /// one of the Matches() methods from UntypedBaseMatcher.
  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return InnerMatcher.Matches(Node, Finder, Builder);
  }

  /// Implements UntypedBaseMatcher::GetID.
  virtual uint64_t GetID() const {
    return InnerMatcher.GetID();
  }

private:
  Matcher<T> InnerMatcher;
};

/// FIXME: Find a better name.
///
/// Interface that allows matchers to traverse the AST.
/// This provides two entry methods for each base node type in the AST:
/// - MatchesChildOf:
///   Matches a matcher on every child node of the given node. Returns true
///   if at least one child node could be matched.
/// - MatchesDescendantOf:
///   Matches a matcher on all descendant nodes of the given node. Returns true
///   if at least one descendant matched.
class ASTMatchFinder {
public:
  /// Defines how we descend a level in the AST when we pass
  /// through expressions.
  enum TraversalKind {
    /// Will traverse any child nodes.
    TK_AsIs,
    /// Will not traverse implicit casts and parentheses.
    TK_IgnoreImplicitCastsAndParentheses
  };

  /// Defines how bindings are processed on recursive matches.
  enum BindKind {
    /// Stop at the first match and only bind the first match.
    BK_First,
    /// Create results for all combinations of bindings that match.
    BK_All
  };

  virtual ~ASTMatchFinder() {}

  /// Returns true if the given class is directly or indirectly derived
  /// from a base type with the given name. A class is considered to
  /// be also derived from itself.
  virtual bool ClassIsDerivedFrom(const clang::CXXRecordDecl *Declaration,
                                  const std::string &BaseName) const = 0;

  // FIXME: Implement for other base nodes.
  virtual bool MatchesChildOf(const clang::Decl &DeclNode,
                              const UntypedBaseMatcher &BaseMatcher,
                              BoundNodesTreeBuilder *Builder,
                              TraversalKind Traverse,
                              BindKind Bind) = 0;
  virtual bool MatchesChildOf(const clang::Stmt &StmtNode,
                              const UntypedBaseMatcher &BaseMatcher,
                              BoundNodesTreeBuilder *Builder,
                              TraversalKind Traverse,
                              BindKind Bind) = 0;

  virtual bool MatchesDescendantOf(const clang::Decl &DeclNode,
                                   const UntypedBaseMatcher &BaseMatcher,
                                   BoundNodesTreeBuilder *Builder,
                                   BindKind Bind) = 0;
  virtual bool MatchesDescendantOf(const clang::Stmt &StmtNode,
                                   const UntypedBaseMatcher &BaseMatcher,
                                   BoundNodesTreeBuilder *Builder,
                                   BindKind Bind) = 0;
};

/// Converts a Matcher<T> to a matcher of desired type To by "adapting"
/// a To into a T. The ArgumentAdapterT argument specifies how the
/// adaptation is done. For example:
///
///   ArgumentAdaptingMatcher<DynCastMatcher, T>(InnerMatcher);
/// returns a matcher that can be used where a Matcher<To> is required, if
/// To and T are in the same type hierarchy, and thus dyn_cast can be
/// called to convert a To to a T.
///
/// FIXME: Make sure all our applications of this class actually require
/// knowledge about the inner type. DynCastMatcher obviously does, but the
/// Has *matchers require the inner type solely for COMPILE_ASSERT purposes.
template <template <typename ToArg, typename FromArg> class ArgumentAdapterT,
          typename T>
class ArgumentAdaptingMatcher {
public:
  explicit ArgumentAdaptingMatcher(const Matcher<T> &InnerMatcher)
      : InnerMatcher(InnerMatcher) {}

  template <typename To>
  operator Matcher<To>() const {
    return Matcher<To>(new ArgumentAdapterT<To, T>(InnerMatcher));
  }

private:
  const Matcher<T> InnerMatcher;
};

/// A PolymorphicMatcherWithParamN<MatcherT, P1, ..., PN> object can be
/// created from N parameters p1, ..., pN (of type P1, ..., PN) and
/// used as a Matcher<T> where a MatcherT<T, P1, ..., PN>(p1, ..., pN)
/// can be constructed.
///
/// For example:
/// - PolymorphicMatcherWithParam0<IsDefinitionMatcher>()
///   creates an object that can be used as a Matcher<T> for any type T
///   where an IsDefinitionMatcher<T>() can be constructed.
/// - PolymorphicMatcherWithParam1<ValueEqualsMatcher, int>(42)
///   creates an object that can be used as a Matcher<T> for any type T
///   where a ValueEqualsMatcher<T, int>(42) can be constructed.
template <template <typename T> class MatcherT>
class PolymorphicMatcherWithParam0 {
public:
  template <typename T>
  operator Matcher<T>() const {
    return Matcher<T>(new MatcherT<T>());
  }
};

template <template <typename T, typename P1> class MatcherT,
          typename P1>
class PolymorphicMatcherWithParam1 {
public:
  explicit PolymorphicMatcherWithParam1(const P1 &Param1)
      : Param1(Param1) {}

  template <typename T>
  operator Matcher<T>() const {
    return Matcher<T>(new MatcherT<T, P1>(Param1));
  }

private:
  const P1 Param1;
};

template <template <typename T, typename P1, typename P2> class MatcherT,
          typename P1, typename P2>
class PolymorphicMatcherWithParam2 {
public:
  PolymorphicMatcherWithParam2(const P1 &Param1, const P2 &Param2)
      : Param1(Param1), Param2(Param2) {}

  template <typename T>
  operator Matcher<T>() const {
    return Matcher<T>(new MatcherT<T, P1, P2>(Param1, Param2));
  }

private:
  const P1 Param1;
  const P2 Param2;
};

/// Matches any instance of the given NodeType.
///
/// This is useful when a matcher syntactically requires a child matcher,
/// but the context doesn't care. See for example: True().
///
/// FIXME: Alternatively we could also create a IsAMatcher or something
/// that checks that a dyn_cast is possible. This is purely needed for the
/// difference between calling for example:
///   Class()
/// and
///   Class(SomeMatcher)
/// In the second case we need the correct type we were dyn_cast'ed to in order
/// to get the right type for the inner matcher. In the first case we don't need
/// that, but we use the type conversion anyway and insert a TrueMatcher.
template <typename T>
class TrueMatcher : public SingleNodeMatcherInterface<T>  {
public:
  virtual bool MatchesNode(const T &Node) const {
    return true;
  }
};

/// Provides a MatcherInterface<T> for a Matcher<To> that matches if T is
/// dyn_cast'able into To and the given Matcher<To> matches on the dyn_cast'ed
/// node.
template <typename T, typename To>
class DynCastMatcher : public MatcherInterface<T> {
public:
  explicit DynCastMatcher(const Matcher<To> &InnerMatcher)
      : InnerMatcher(InnerMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    const To *InnerMatchValue = llvm::dyn_cast<To>(&Node);
    return InnerMatchValue != NULL &&
      InnerMatcher.Matches(*InnerMatchValue, Finder, Builder);
  }

private:
  const Matcher<To> InnerMatcher;
};

/// Enables the user to pass a Matcher<clang::CXXMemberCallExpr> to Call().
/// FIXME: Alternatives are using more specific methods than Call, like
/// MemberCall, or not using VariadicFunction for Call and overloading it.
template <>
template <>
inline Matcher<clang::CXXMemberCallExpr>::
operator Matcher<clang::CallExpr>() const {
  return MakeMatcher(
    new DynCastMatcher<clang::CallExpr, clang::CXXMemberCallExpr>(*this));
}

/// Matcher<T> that wraps an inner Matcher<T> and binds the matched node to
/// an ID if the inner matcher matches on the node.
template <typename T>
class IdMatcher : public MatcherInterface<T> {
public:
  /// Creates an IdMatcher that binds to 'ID' if 'InnerMatcher' matches the
  /// node.
  IdMatcher(const std::string &ID, const Matcher<T> &InnerMatcher)
      : ID(ID), InnerMatcher(InnerMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    bool Result = InnerMatcher.Matches(Node, Finder, Builder);
    if (Result) {
      Builder->SetBinding(std::pair<const std::string, const T*>(ID, &Node));
    }
    return Result;
  }

private:
  const std::string ID;
  const Matcher<T> InnerMatcher;
};

/// Matches nodes of type T that have child nodes of type ChildT for
/// which a specified child matcher matches. ChildT must be an AST base
/// type.
template <typename T, typename ChildT>
class HasMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT(IsBaseType<ChildT>::value,
                         has_only_accepts_base_type_matcher);
public:
  explicit HasMatcher(const Matcher<ChildT> &ChildMatcher)
      : ChildMatcher(ChildMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return Finder->MatchesChildOf(
        Node, ChildMatcher, Builder,
        ASTMatchFinder::TK_IgnoreImplicitCastsAndParentheses,
        ASTMatchFinder::BK_First);
  }

 private:
  const TypedBaseMatcher<ChildT> ChildMatcher;
};

/// \brief Matches nodes of type T that have child nodes of type ChildT for
/// which a specified child matcher matches. ChildT must be an AST base
/// type.
/// As opposed to the HasMatcher, the ForEachMatcher will produce a match
/// for each child that matches.
template <typename T, typename ChildT>
class ForEachMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT(IsBaseType<ChildT>::value,
                         for_each_only_accepts_base_type_matcher);
 public:
  explicit ForEachMatcher(const Matcher<ChildT> &ChildMatcher)
      : ChildMatcher(ChildMatcher) {}

  virtual bool Matches(const T& Node,
                       ASTMatchFinder* Finder,
                       BoundNodesTreeBuilder* Builder) const {
    return Finder->MatchesChildOf(
      Node, ChildMatcher, Builder,
      ASTMatchFinder::TK_IgnoreImplicitCastsAndParentheses,
      ASTMatchFinder::BK_All);
  }

private:
  const TypedBaseMatcher<ChildT> ChildMatcher;
};

/// Matches nodes of type T if the given Matcher<T> does not match.
/// Type argument MatcherT is required by PolymorphicMatcherWithParam1
/// but not actually used. It will always be instantiated with a type
/// convertible to Matcher<T>.
template <typename T, typename MatcherT>
class NotMatcher : public MatcherInterface<T> {
public:
  explicit NotMatcher(const Matcher<T> &InnerMatcher)
      : InnerMatcher(InnerMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return !InnerMatcher.Matches(Node, Finder, Builder);
  }

private:
  const Matcher<T> InnerMatcher;
};

/// Matches nodes of type T for which both provided matchers
/// match. Type arguments MatcherT1 and MatcherT2 are required by
/// PolymorphicMatcherWithParam2 but not actually used. They will
/// always be instantiated with types convertible to Matcher<T>.
template <typename T, typename MatcherT1, typename MatcherT2>
class AllOfMatcher : public MatcherInterface<T> {
public:
  AllOfMatcher(const Matcher<T> &InnerMatcher1, const Matcher<T> &InnerMatcher2)
      : InnerMatcher1(InnerMatcher1), InnerMatcher2(InnerMatcher2) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return InnerMatcher1.Matches(Node, Finder, Builder) &&
           InnerMatcher2.Matches(Node, Finder, Builder);
  }

private:
  const Matcher<T> InnerMatcher1;
  const Matcher<T> InnerMatcher2;
};

/// Matches nodes of type T for which at least one of the two provided
/// matchers matches. Type arguments MatcherT1 and MatcherT2 are
/// required by PolymorphicMatcherWithParam2 but not actually
/// used. They will always be instantiated with types convertible to
/// Matcher<T>.
template <typename T, typename MatcherT1, typename MatcherT2>
class AnyOfMatcher : public MatcherInterface<T> {
public:
  AnyOfMatcher(const Matcher<T> &InnerMatcher1, const Matcher<T> &InnerMatcher2)
      : InnerMatcher1(InnerMatcher1), InnertMatcher2(InnerMatcher2) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return InnerMatcher1.Matches(Node, Finder, Builder) ||
           InnertMatcher2.Matches(Node, Finder, Builder);
  }

private:
  const Matcher<T> InnerMatcher1;
  const Matcher<T> InnertMatcher2;
};

/// Creates a Matcher<T> that matches if
/// T is dyn_cast'able into InnerT and all inner matchers match.
template<typename T, typename InnerT>
Matcher<T> MakeDynCastAllOfComposite(
    const Matcher<InnerT> *const InnerMatchers[], int Count) {
  if (Count == 0) {
    return ArgumentAdaptingMatcher<DynCastMatcher, InnerT>(
        MakeMatcher(new TrueMatcher<InnerT>));
  }
  Matcher<InnerT> InnerMatcher = *InnerMatchers[Count-1];
  for (int I = Count-2; I >= 0; --I) {
    InnerMatcher = MakeMatcher(
        new AllOfMatcher<InnerT, Matcher<InnerT>, Matcher<InnerT> >(
            *InnerMatchers[I], InnerMatcher));
  }
  return ArgumentAdaptingMatcher<DynCastMatcher, InnerT>(InnerMatcher);
}

/// Matches nodes of type T that have at least one descendant node of
/// type DescendantT for which the given inner matcher matches.
/// DescendantT must be an AST base type.
template <typename T, typename DescendantT>
class HasDescendantMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT(IsBaseType<DescendantT>::value,
                         has_descendant_only_accepts_base_type_matcher);
public:
  explicit HasDescendantMatcher(const Matcher<DescendantT> &DescendantMatcher)
      : DescendantMatcher(DescendantMatcher) {}

  virtual bool Matches(const T &Node,
                       ASTMatchFinder *Finder,
                       BoundNodesTreeBuilder *Builder) const {
    return Finder->MatchesDescendantOf(
        Node, DescendantMatcher, Builder, ASTMatchFinder::BK_First);
  }

 private:
  const TypedBaseMatcher<DescendantT> DescendantMatcher;
};

/// \brief Matches nodes of type T that have at least one descendant node of
/// type DescendantT for which the given inner matcher matches.
/// DescendantT must be an AST base type.
/// As opposed to HasDescendantMatcher, ForEachDescendantMatcher will match
/// for each descendant node that matches instead of only for the first.
template <typename T, typename DescendantT>
class ForEachDescendantMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT(IsBaseType<DescendantT>::value,
                         for_each_descendant_only_accepts_base_type_matcher);
 public:
  explicit ForEachDescendantMatcher(
      const Matcher<DescendantT>& DescendantMatcher)
      : DescendantMatcher(DescendantMatcher) {}

  virtual bool Matches(const T& Node,
                       ASTMatchFinder* Finder,
                       BoundNodesTreeBuilder* Builder) const {
    return Finder->MatchesDescendantOf(Node, DescendantMatcher, Builder,
                                       ASTMatchFinder::BK_All);
  }

private:
  const TypedBaseMatcher<DescendantT> DescendantMatcher;
};

/// Matches on nodes that have a getValue() method if getValue() equals the
/// value the ValueEqualsMatcher was constructed with.
template <typename T, typename ValueT>
class ValueEqualsMatcher : public SingleNodeMatcherInterface<T> {
  TOOLING_COMPILE_ASSERT((llvm::is_base_of<clang::CharacterLiteral, T>::value ||
                         llvm::is_base_of<clang::CXXBoolLiteralExpr,
                                          T>::value ||
                         llvm::is_base_of<clang::FloatingLiteral, T>::value ||
                         llvm::is_base_of<clang::IntegerLiteral, T>::value),
                         the_node_must_have_a_getValue_method);
public:
  explicit ValueEqualsMatcher(const ValueT &ExpectedValue)
      : ExpectedValue(ExpectedValue) {}

  virtual bool MatchesNode(const T &Node) const {
    return Node.getValue() == ExpectedValue;
  }

private:
  const ValueT ExpectedValue;
};

template <typename T>
class IsDefinitionMatcher : public SingleNodeMatcherInterface<T> {
  TOOLING_COMPILE_ASSERT(
    (llvm::is_base_of<clang::TagDecl, T>::value) ||
    (llvm::is_base_of<clang::VarDecl, T>::value) ||
    (llvm::is_base_of<clang::FunctionDecl, T>::value),
    is_definition_requires_isThisDeclarationADefinition_method);
public:
  virtual bool MatchesNode(const T &Node) const {
    return Node.isThisDeclarationADefinition();
  }
};

/// Matches on template instantiations for FunctionDecl, VarDecl or
/// CXXRecordDecl nodes.
template <typename T>
class IsTemplateInstantiationMatcher : public MatcherInterface<T> {
  TOOLING_COMPILE_ASSERT((llvm::is_base_of<clang::FunctionDecl, T>::value) ||
                         (llvm::is_base_of<clang::VarDecl, T>::value) ||
                         (llvm::is_base_of<clang::CXXRecordDecl, T>::value),
                         requires_getTemplateSpecializationKind_method);
 public:
  virtual bool Matches(const T& Node,
                       ASTMatchFinder* Finder,
                       BoundNodesTreeBuilder* Builder) const {
    return (Node.getTemplateSpecializationKind() ==
                clang::TSK_ImplicitInstantiation ||
            Node.getTemplateSpecializationKind() ==
                clang::TSK_ExplicitInstantiationDefinition);
  }
};

class IsArrowMatcher : public SingleNodeMatcherInterface<clang::MemberExpr> {
public:
  virtual bool MatchesNode(const clang::MemberExpr &Node) const {
    return Node.isArrow();
  }
};

class IsConstQualifiedMatcher
    : public SingleNodeMatcherInterface<clang::QualType> {
 public:
  virtual bool MatchesNode(const clang::QualType& Node) const {
    return Node.isConstQualified();
  }
};

/// A VariadicDynCastAllOfMatcher<SourceT, TargetT> object is a
/// variadic functor that takes a number of Matcher<TargetT> and returns a
/// Matcher<SourceT> that matches TargetT nodes that are matched by all of the
/// given matchers, if SourceT can be dynamically casted into TargetT.
///
/// For example:
///   const VariadicDynCastAllOfMatcher<
///       clang::Decl, clang::CXXRecordDecl> Class;
/// Creates a functor Class(...) that creates a Matcher<clang::Decl> given
/// a variable number of arguments of type Matcher<clang::CXXRecordDecl>.
/// The returned matcher matches if the given clang::Decl can by dynamically
/// casted to clang::CXXRecordDecl and all given matchers match.
template <typename SourceT, typename TargetT>
class VariadicDynCastAllOfMatcher
    : public llvm::VariadicFunction<
        Matcher<SourceT>, Matcher<TargetT>,
        MakeDynCastAllOfComposite<SourceT, TargetT> > {
public:
  VariadicDynCastAllOfMatcher() {}
};

} // end namespace internal
} // end namespace ast_matchers
} // end namespace clang

#endif // LLVM_CLANG_AST_MATCHERS_AST_MATCHERS_INTERNAL_H
