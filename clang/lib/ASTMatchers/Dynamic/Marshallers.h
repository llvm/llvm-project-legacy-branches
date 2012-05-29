//===--- Marshallers.h - Generic matcher function marshallers -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// A collection of template function and classes that provide a generic
// marshalling layer on top of matcher construct functions.
// These are used by the registry to export all marshaller constructors with
// the same generic interface.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MATCHERS_DYNAMIC_MARSHALLERS_H
#define LLVM_CLANG_AST_MATCHERS_DYNAMIC_MARSHALLERS_H

#include <list>
#include <string>
#include <vector>

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/Dynamic/GenericMatcher.h"
#include "clang/ASTMatchers/Dynamic/GenericValue.h"
#include "llvm/Support/type_traits.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

namespace internal {

// Generic MatcherCreate interface.
// Provides a run() method that constructs the matcher from the provided
// arguments.
class MatcherCreateCallback {
 public:
  virtual ~MatcherCreateCallback() { }
  virtual GenericValue run(const std::vector<GenericValue>& Args) const = 0;
};

// Generic callback implementation.
// The Marshaller function will unpack the arguments, call Func and repack the
// return value.
// It should also check that the arguments are valid for Func.
template <typename MarshallerType, typename FuncType>
class MatcherCreateCallbackImpl : public MatcherCreateCallback {
 public:
  MatcherCreateCallbackImpl(MarshallerType Marshaller,
                            FuncType Func,
                            const std::string& MatcherName)
      : Marshaller(Marshaller), Func(Func), MatcherName(MatcherName) { }

  GenericValue run(const std::vector<GenericValue>& Args) const {
    return Marshaller(Func, MatcherName, Args);
  }
 private:
  const MarshallerType Marshaller;
  const FuncType Func;
  const std::string MatcherName;
};

template <typename MarshallerType, typename FuncType>
MatcherCreateCallback* CreateMarshallerCallback(
    MarshallerType Marshaller,
    FuncType Func,
    const std::string& MatcherName) {
  return new MatcherCreateCallbackImpl<MarshallerType, FuncType>(
      Marshaller, Func, MatcherName);
}

#define CHECK_ARG_COUNT(count)                                        \
    if (Args.size() != count) {                                       \
      return GenericError(llvm::Twine(                                \
          "Incorrect argument count on function " + MatcherName +     \
          ". (Expected = " + llvm::Twine(count) +                     \
          ") != (Actual = " + llvm::Twine(Args.size()) + ")").str()); \
    }
#define CHECK_ARG_TYPE(index, type)                                \
    if (Args[index].is<GenericError>()) return Args[index];        \
    if (!Args[index].is<type>()) {                                 \
      return GenericError(llvm::Twine(                             \
          "Incorrect type on function " + MatcherName +            \
         " for arg " + llvm::Twine(index)).str());                 \
    }

// 0-arg marshaller function.
template <typename FuncType, typename ReturnType>
GenericValue MatcherMarshall0(FuncType Func,
                              const std::string& MatcherName,
                              const std::vector<GenericValue>& Args) {
  CHECK_ARG_COUNT(0);
  return GenericValue(ReturnType(Func()));
}

// 1-arg marshaller function.
template <typename FuncType, typename ReturnType, typename ArgType1>
GenericValue MatcherMarshall1(FuncType Func,
                              const std::string& MatcherName,
                              const std::vector<GenericValue>& Args) {
  CHECK_ARG_COUNT(1);
  CHECK_ARG_TYPE(0, ArgType1);
  return GenericValue(ReturnType(Func(Args[0].get<ArgType1>())));
}

// 2-arg marshaller function.
template <typename FuncType, typename ReturnType,
          typename ArgType1, typename ArgType2>
GenericValue MatcherMarshall2(FuncType Func,
                              const std::string& MatcherName,
                              const std::vector<GenericValue>& Args) {
  CHECK_ARG_COUNT(2);
  CHECK_ARG_TYPE(0, ArgType1);
  CHECK_ARG_TYPE(1, ArgType2);
  return GenericValue(ReturnType(Func(Args[0].get<ArgType1>(),
                                      Args[1].get<ArgType2>())));
}

// Variadic marshaller function.
template <typename BaseType, typename DerivedType>
class MatcherMarshallVariadic : public MatcherCreateCallback {
 public:
  explicit MatcherMarshallVariadic(const std::string& MatcherName)
      : MatcherName(MatcherName) { }

  typedef Matcher<DerivedType> DerivedMatcherType;

  GenericValue run(const std::vector<GenericValue>& Args) const {
    std::list<DerivedMatcherType> References;
    std::vector<const DerivedMatcherType*> InnerArgs(Args.size());
    for (size_t i = 0; i < Args.size(); ++i) {
      CHECK_ARG_TYPE(i, DerivedMatcherType);
      References.push_back(Args[i].get<DerivedMatcherType>());
      InnerArgs[i] = &References.back();
    }
    return GenericValue(
        ast_matchers::internal::MakeDynCastAllOfComposite<BaseType>(
            ArrayRef<const DerivedMatcherType*>(InnerArgs)));
  }

 private:
  const std::string MatcherName;
};

#undef CHECK_ARG_COUNT
#undef CHECK_ARG_TYPE

// We need to remove the const& out of the function parameters to be able to
// find values on GenericValue.
template <typename T>
struct remove_const_ref
    : public llvm::remove_const<typename llvm::remove_reference<T>::type> {
};

// Helper functions to select the appropriate marshaller functions.
// We have two names:
//  - MakeMatcherAutoMarshall: Detects the number of arguments, arguments types
//  and return types.
//  - MakeMatcherAutoMarshallPoly: Same as MakeMatcherAutoMarshall, but allows
//  passing the desired return type for polymorphic matchers.

// 0-arg
template <typename ReturnType, typename PolyReturnType>
MatcherCreateCallback* MakeMatcherAutoMarshallPoly(
    PolyReturnType (*Func)(),
    const std::string& MatcherName) {
  return CreateMarshallerCallback(
      MatcherMarshall0<PolyReturnType(*)(), ReturnType>, Func, MatcherName);
}
template <typename ReturnType>
MatcherCreateCallback* MakeMatcherAutoMarshall(
    ReturnType (*Func)(),
    const std::string& MatcherName) {
  return MakeMatcherAutoMarshallPoly<ReturnType>(Func, MatcherName);
}

// 1-arg
template <typename ReturnType, typename PolyReturnType, typename InArgType1>
MatcherCreateCallback* MakeMatcherAutoMarshallPoly(
    PolyReturnType (*Func)(InArgType1),
    const std::string& MatcherName) {
  typedef typename remove_const_ref<InArgType1>::type ArgType1;
  return CreateMarshallerCallback(
      MatcherMarshall1<PolyReturnType (*)(InArgType1), ReturnType, ArgType1>,
      Func, MatcherName);
}
template <typename ReturnType, typename InArgType1>
MatcherCreateCallback* MakeMatcherAutoMarshall(
    ReturnType (*Func)(InArgType1),
    const std::string& MatcherName) {
  return MakeMatcherAutoMarshallPoly<ReturnType>(Func, MatcherName);
}

// 2-arg
template <typename ReturnType, typename PolyReturnType,
          typename InArgType1, typename InArgType2>
MatcherCreateCallback* MakeMatcherAutoMarshallPoly(
    PolyReturnType (*Func)(InArgType1, InArgType2),
    const std::string& MatcherName) {
  typedef typename remove_const_ref<InArgType1>::type ArgType1;
  typedef typename remove_const_ref<InArgType2>::type ArgType2;
  return CreateMarshallerCallback(
      MatcherMarshall2<PolyReturnType (*)(InArgType1, InArgType2),
                       ReturnType, ArgType1, ArgType2>,
      Func, MatcherName);
}
template <typename ReturnType, typename InArgType1, typename InArgType2>
MatcherCreateCallback* MakeMatcherAutoMarshall(
    ReturnType (*Func)(InArgType1, InArgType2),
    const std::string& MatcherName) {
  return MakeMatcherAutoMarshallPoly<ReturnType>(Func, MatcherName);
}

// Variadic
template <typename BaseType, typename MatcherType>
MatcherCreateCallback* MakeMatcherAutoMarshall(
    ast_matchers::internal::VariadicDynCastAllOfMatcher<
        BaseType, MatcherType> Func,
    const std::string& MatcherName) {
  return new MatcherMarshallVariadic<BaseType, MatcherType>(MatcherName);
}


// Some basic functions that are easier to reimplement than reuse.
class MatcherMarshallAnyOf : public MatcherCreateCallback {
 public:
  GenericValue run(const std::vector<GenericValue>& Args) const {
    if (Args.empty())
      return GenericError("anyOf() needs at least one argument!");

    // Validate them
    for (size_t i = 0; i < Args.size(); ++i) {
      if (!Args[i].is<GenericMatcher>()) {
        return GenericError(llvm::Twine("Argument " + llvm::Twine(i) +
                                        " is not a matcher.").str());
      }
    }

    GenericMatcher Matcher = Args[0].get<GenericMatcher>();
    for (size_t i = 1; i < Args.size(); ++i) {
      Matcher = GenericMatcher::anyOf(Matcher, Args[i].get<GenericMatcher>());
    }
    return Matcher;
  }
};

class MatcherMarshallAllOf : public MatcherCreateCallback {
 public:
  GenericValue run(const std::vector<GenericValue>& Args) const {
    if (Args.empty())
      return GenericError("allOf() needs at least one argument!");

    // Validate them
    for (size_t i = 0; i < Args.size(); ++i) {
      if (!Args[i].is<GenericMatcher>()) {
        return GenericError(llvm::Twine("Argument " + llvm::Twine(i) +
                                        " is not a matcher.").str());
      }
    }

    GenericMatcher Matcher = Args[0].get<GenericMatcher>();
    for (size_t i = 1; i < Args.size(); ++i) {
      Matcher = GenericMatcher::allOf(Matcher, Args[i].get<GenericMatcher>());
    }
    return Matcher;
  }
};

}  // namespace internal
}  // namespace dynamic
}  // namespace ast_matchers
}  // namespace clang

#endif  // LLVM_CLANG_AST_MATCHERS_DYNAMIC_MARSHALLERS_H
