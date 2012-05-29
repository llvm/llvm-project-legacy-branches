//===--- GenericMatcher.h - Polymorphic matcher type -*- C++ -*-===/
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  A simple wrapper class that can impersonate any matcher type.
//  It provides appropriate conversion of matchers. An invalid conversion will
//  succeed, but return a matcher that won't match anything.
//  Eg. Converting a Matcher<Stmt> to Matcher<Decl>.
//  Used as the underlying matcher value on GenericValue.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_MATCHER_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_MATCHER_H

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchersInternal.h"
#include "llvm/ADT/OwningPtr.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

using ast_matchers::internal::Matcher;
using ast_matchers::internal::UntypedBaseMatcher;
using ast_matchers::internal::TypedBaseMatcher;
using ast_matchers::internal::ASTMatchFinder;
using ast_matchers::internal::BoundNodesTreeBuilder;

namespace internal {


}  // namespace internal

class GenericMatcher {
  // Gives the base types for Decl/Expr and T for everything else.
  // Callers to the GenericMatcher will use the derived classes when
  // setting/getting the contents. This struct helps on the conversion.
  template <typename T>
  struct DerivedTypeHelper {
    typedef typename llvm::conditional<llvm::is_base_of<clang::Decl, T>::value,
            clang::Decl, T>::type Decl_type;
    typedef typename llvm::conditional<llvm::is_base_of<clang::Stmt, T>::value,
            clang::Stmt, Decl_type>::type Type;
  };

 public:
  template <typename T>
  GenericMatcher(Matcher<T> Inner)
      : Untyped(makeUntyped(Inner)) { }

  template <typename T>
  Matcher<T> getAs() const {
    return ast_matchers::internal::MakeMatcher(new UntypedWrapper<T>(Untyped));
  }

  static GenericMatcher anyOf(const GenericMatcher& First,
                              const GenericMatcher& Second) {
    return GenericMatcher(new AnyOfUntypedMatcher(
        First.Untyped, Second.Untyped));
  }

  static GenericMatcher allOf(const GenericMatcher& First,
                              const GenericMatcher& Second) {
    return GenericMatcher(new AllOfUntypedMatcher(
        First.Untyped, Second.Untyped));
  }

 private:
  // Takes ownership.
  explicit GenericMatcher(UntypedBaseMatcher* Inner) : Untyped(Inner) { }

  typedef llvm::IntrusiveRefCntPtr<const UntypedBaseMatcher> UntypedMatcherPtr;

  template <typename T>
  static UntypedMatcherPtr makeUntyped(Matcher<T> Inner) {
    typedef typename DerivedTypeHelper<T>::Type BaseType;
    return new TypedBaseMatcher<BaseType>(
        maybeMakeDynCastMatcher(Inner, static_cast<BaseType*>(NULL)));
  }

  // Use DynCastMatcher when From and To are different types.
  // This allows skipping DynCastMatcher for QualType and CXXCtorInitializer
  // that do not support it.
  template <typename From, typename To>
  static Matcher<From> maybeMakeDynCastMatcher(const Matcher<To>& BaseMatcher,
                                               const From* dummy) {
    return MakeMatcher(
        new ast_matchers::internal::DynCastMatcher<From, To>(BaseMatcher));
  }
  template <typename T>
  static Matcher<T> maybeMakeDynCastMatcher(const Matcher<T>& BaseMatcher,
                                            const T* dummy) {
    return BaseMatcher;
  }

  template <typename T>
  class UntypedWrapper : public ast_matchers::internal::MatcherInterface<T> {
   public:
    explicit UntypedWrapper(const UntypedMatcherPtr& Inner) : Untyped(Inner) { }
    virtual ~UntypedWrapper() { }
    bool matches(const T& Node, ASTMatchFinder* Finder,
                 BoundNodesTreeBuilder* Builder) const {
      return Untyped->matches(Node, Finder, Builder);
    }

   private:
    const UntypedMatcherPtr Untyped;
  };

  class AnyOfUntypedMatcher : public UntypedBaseMatcher {
   public:
    AnyOfUntypedMatcher(const UntypedMatcherPtr& FirstIn,
                        const UntypedMatcherPtr& SecondIn)
        : First(FirstIn), Second(SecondIn) { }
    virtual ~AnyOfUntypedMatcher() {}

    bool matches(const clang::Decl &DeclNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(DeclNode, Finder, Builder) ||
          Second->matches(DeclNode, Finder, Builder);
    }
    bool matches(const clang::QualType &TypeNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(TypeNode, Finder, Builder) ||
          Second->matches(TypeNode, Finder, Builder);
    }
    bool matches(const clang::Stmt &StmtNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(StmtNode, Finder, Builder) ||
          Second->matches(StmtNode, Finder, Builder);
    }
    bool matches(const clang::CXXCtorInitializer &CtorInitNode,
                 ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(CtorInitNode, Finder, Builder) ||
          Second->matches(CtorInitNode, Finder, Builder);
    }

    uint64_t getID() const {
      return reinterpret_cast<uint64_t>(this);
    }

   private:
    const UntypedMatcherPtr First;
    const UntypedMatcherPtr Second;
  };

  class AllOfUntypedMatcher : public UntypedBaseMatcher {
   public:
    AllOfUntypedMatcher(const UntypedMatcherPtr& FirstIn,
                        const UntypedMatcherPtr& SecondIn)
        : First(FirstIn), Second(SecondIn) { }
    virtual ~AllOfUntypedMatcher() {}

    bool matches(const clang::Decl &DeclNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(DeclNode, Finder, Builder) &&
          Second->matches(DeclNode, Finder, Builder);
    }
    bool matches(const clang::QualType &TypeNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(TypeNode, Finder, Builder) &&
          Second->matches(TypeNode, Finder, Builder);
    }
    bool matches(const clang::Stmt &StmtNode, ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(StmtNode, Finder, Builder) &&
          Second->matches(StmtNode, Finder, Builder);
    }
    bool matches(const clang::CXXCtorInitializer &CtorInitNode,
                 ASTMatchFinder *Finder,
                 BoundNodesTreeBuilder *Builder) const {
      return First->matches(CtorInitNode, Finder, Builder) &&
          Second->matches(CtorInitNode, Finder, Builder);
    }

    uint64_t getID() const {
      return reinterpret_cast<uint64_t>(this);
    }

   private:
    const UntypedMatcherPtr First;
    const UntypedMatcherPtr Second;
  };

  UntypedMatcherPtr Untyped;
};

}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_GENERIC_MATCHER_H
