//===--- Registry.cpp - Matcher registry ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Registry of all known matchers.
// The registry provides a generic interface to construct any matcher by name.
//
//===----------------------------------------------------------------------===//

#include "clang/ASTMatchers/Dynamic/Registry.h"

#include <map>
#include <set>
#include <utility>

#include "Marshallers.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/Dynamic/GenericMatcher.h"
#include "llvm/ADT/OwningPtr.h"

namespace clang {

// A utility function that returns that name of a type, for all the types
// defined in the AST.
#define DECL(DERIVED, BASE) \
class DERIVED##Decl; \
const char* GetRegistryTypeName(DERIVED##Decl* dummy) { return #DERIVED; }
#include "clang/AST/DeclNodes.inc"
#define STMT(CLASS, PARENT) \
class CLASS; \
const char* GetRegistryTypeName(CLASS* dummy) { return #CLASS; }
#include "clang/AST/StmtNodes.inc"
const char* GetRegistryTypeName(Stmt* dummy) { return "Stmt"; }

namespace ast_matchers {

// Create some functions to help disambiguate certain overloaded matchers.

#define MAKE_HELPER_POLY_FUNCTION1(func, name, R) \
template <typename P1> \
R name(const P1& t1) { return func(t1); } \

MAKE_HELPER_POLY_FUNCTION1(HasType, HasType_Expr,
                           internal::Matcher<clang::Expr>)
MAKE_HELPER_POLY_FUNCTION1(HasType, HasType_ValueDecl,
                           internal::Matcher<clang::ValueDecl>)

namespace dynamic {

namespace {

using internal::MatcherCreateCallback;

typedef std::map<std::string, const MatcherCreateCallback*> ConstructorMap;
typedef std::map<std::string, std::string> KindNameMap;
typedef std::set<std::string> KindSet;
struct RegistryMaps {
  ConstructorMap Constructors;
  KindNameMap KindNames;
  KindSet AllKinds;
};

// If both input callbacks return a matcher, the output will be anyOf() of them.
// If only one of them (any of them) returns a matcher, the output will be that
// returned matcher.
// If they both return an error, the output will be any of them.
class MatcherCallbackUnion : public MatcherCreateCallback {
 public:
  MatcherCallbackUnion(const MatcherCreateCallback* Callback1,
                       const MatcherCreateCallback* Callback2)
      : Callback1(Callback1), Callback2(Callback2) { }

  GenericValue run(const std::vector<GenericValue>& args) const {
    // If they are both matchers, return the union of them.
    // If only one is a matcher, return that one.
    // Otherwise, return any of them, which should be an error.
    const GenericValue Out1 = Callback1->run(args);
    const GenericValue Out2 = Callback2->run(args);
    if (!Out1.is<GenericMatcher>()) {
      return Out2;
    } else if (!Out2.is<GenericMatcher>()) {
      return Out1;
    } else {
      return GenericMatcher::anyOf(Out1.get<GenericMatcher>(),
                                   Out2.get<GenericMatcher>());
    }
  }

 private:
  const llvm::OwningPtr<const MatcherCreateCallback> Callback1;
  const llvm::OwningPtr<const MatcherCreateCallback> Callback2;
};

void RegisterMatcher(const std::string& MatcherName,
                     MatcherCreateCallback* Callback,
                     RegistryMaps* Data) {
  const MatcherCreateCallback** MapCallback = &Data->Constructors[MatcherName];
  if (*MapCallback) {
    Callback = new MatcherCallbackUnion(*MapCallback, Callback);
  }
  *MapCallback = Callback;
}

template <typename T>
void RegisterKindName(const std::string& MatcherName, const T& t,
                      RegistryMaps* Data) { }
template <typename Base, typename Type>
void RegisterKindName(
    const std::string& MatcherName,
    ast_matchers::internal::VariadicDynCastAllOfMatcher<Base, Type> Func,
    RegistryMaps* Data) {
  Data->KindNames[GetRegistryTypeName(static_cast<Type*>(NULL))] = MatcherName;
  Data->AllKinds.insert(MatcherName);
}

#define MATCH_NS ::clang::ast_matchers

#define REGISTER_NAMED_MATCHER_AUTO(name, func)                               \
    RegisterMatcher(#name,                                                    \
                    internal::MakeMatcherAutoMarshall(MATCH_NS::func, #name), \
                    Data)

#define REGISTER_MATCHER_AUTO(name)                                           \
    REGISTER_NAMED_MATCHER_AUTO(name, name);                                  \
    RegisterKindName(#name, MATCH_NS::name, Data)

#define REGISTER_POLY_NAMED_MATCHER_AUTO(name, func, R)                       \
    RegisterMatcher(#name,                                                    \
                    internal::MakeMatcherAutoMarshallPoly<Matcher<R> >(       \
                        MATCH_NS::func, #name), Data)

#define REGISTER_POLY_MATCHER_AUTO(name, R)                                   \
    REGISTER_POLY_NAMED_MATCHER_AUTO(name, name, R)

#define REGISTER_MATCHER1(name, R, P1)                                        \
    RegisterMatcher(#name, internal::MakeMatcherAutoMarshallPoly<R, R, P1>(   \
        MATCH_NS::name, #name), Data)

RegistryMaps* RegisterMatchers() {
  RegistryMaps* Data = new RegistryMaps();

  REGISTER_MATCHER_AUTO(BinaryOperator);
  REGISTER_MATCHER_AUTO(BindTemporaryExpression);
  REGISTER_MATCHER_AUTO(BoolLiteral);
  REGISTER_MATCHER_AUTO(Call);
  REGISTER_MATCHER_AUTO(CharacterLiteral);
  REGISTER_MATCHER_AUTO(Class);
  REGISTER_MATCHER_AUTO(CompoundStatement);
  REGISTER_MATCHER_AUTO(ConditionalOperator);
  REGISTER_MATCHER_AUTO(ConstCast);
  REGISTER_MATCHER_AUTO(Constructor);
  REGISTER_MATCHER_AUTO(ConstructorCall);
  REGISTER_MATCHER_AUTO(DeclarationReference);
  REGISTER_MATCHER_AUTO(DeclarationStatement);
  REGISTER_MATCHER_AUTO(DefaultArgument);
  REGISTER_MATCHER_AUTO(Do);
  REGISTER_MATCHER_AUTO(DynamicCast);
  REGISTER_MATCHER_AUTO(ExplicitCast);
  REGISTER_MATCHER_AUTO(Expression);
  REGISTER_MATCHER_AUTO(Field);
  REGISTER_MATCHER_AUTO(For);
  REGISTER_MATCHER_AUTO(ForField);
  REGISTER_MATCHER_AUTO(Function);
  REGISTER_MATCHER_AUTO(FunctionalCast);
  REGISTER_MATCHER_AUTO(HasAnyConstructorInitializer);
  REGISTER_MATCHER_AUTO(HasAnyConstructorInitializer);
  REGISTER_MATCHER_AUTO(HasAnyParameter);
  REGISTER_MATCHER_AUTO(HasAnySubstatement);
  REGISTER_MATCHER_AUTO(HasBody);
  REGISTER_MATCHER_AUTO(HasConditionVariableStatement);
  REGISTER_MATCHER_AUTO(HasDestinationType);
  REGISTER_MATCHER_AUTO(HasEitherOperand);
  REGISTER_MATCHER_AUTO(HasFalseExpression);
  REGISTER_MATCHER_AUTO(HasImplicitDestinationType);
  REGISTER_MATCHER_AUTO(HasInitializer);
  REGISTER_MATCHER_AUTO(HasLHS);
  REGISTER_MATCHER_AUTO(HasName);
  REGISTER_MATCHER_AUTO(HasObjectExpression);
  REGISTER_MATCHER_AUTO(HasOverloadedOperatorName);
  REGISTER_MATCHER_AUTO(HasParameter);
  REGISTER_MATCHER_AUTO(HasRHS);
  REGISTER_MATCHER_AUTO(HasSourceExpression);
  REGISTER_MATCHER_AUTO(HasTrueExpression);
  REGISTER_MATCHER_AUTO(HasUnaryOperand);
  REGISTER_MATCHER_AUTO(If);
  REGISTER_MATCHER_AUTO(ImplicitCast);
  REGISTER_MATCHER_AUTO(IntegerLiteral);
  REGISTER_MATCHER_AUTO(IsArrow);
  REGISTER_MATCHER_AUTO(IsConstQualified);
  REGISTER_MATCHER_AUTO(IsDerivedFrom);
  REGISTER_MATCHER_AUTO(IsImplicit);
  REGISTER_MATCHER_AUTO(IsWritten);
  REGISTER_MATCHER_AUTO(Member);
  REGISTER_MATCHER_AUTO(MemberExpression);
  REGISTER_MATCHER_AUTO(Method);
  REGISTER_MATCHER_AUTO(NameableDeclaration);
  REGISTER_MATCHER_AUTO(NewExpression);
  REGISTER_MATCHER_AUTO(OfClass);
  REGISTER_MATCHER_AUTO(On);
  REGISTER_MATCHER_AUTO(OnImplicitObjectArgument);
  REGISTER_MATCHER_AUTO(OverloadedOperatorCall);
  REGISTER_MATCHER_AUTO(ReinterpretCast);
  REGISTER_MATCHER_AUTO(Statement);
  REGISTER_MATCHER_AUTO(StatementCountIs);
  REGISTER_MATCHER_AUTO(StaticCast);
  REGISTER_MATCHER_AUTO(StringLiteral);
  REGISTER_MATCHER_AUTO(SwitchCase);
  REGISTER_MATCHER_AUTO(To);
  REGISTER_MATCHER_AUTO(UnaryOperator);
  REGISTER_MATCHER_AUTO(Variable);
  REGISTER_MATCHER_AUTO(While);
  REGISTER_MATCHER_AUTO(WithInitializer);

  // HasType is very special. It is overloaded on parameter and return value.
  REGISTER_NAMED_MATCHER_AUTO(HasType, HasType_Expr<Matcher<clang::QualType> >);
  REGISTER_NAMED_MATCHER_AUTO(HasType, HasType_Expr<Matcher<clang::Decl> >);
  REGISTER_NAMED_MATCHER_AUTO(HasType,
                              HasType_ValueDecl<Matcher<clang::QualType> >);
  REGISTER_NAMED_MATCHER_AUTO(HasType,
                              HasType_ValueDecl<Matcher<clang::Decl> >);

  // True
  REGISTER_POLY_MATCHER_AUTO(True, clang::Stmt);
  REGISTER_POLY_MATCHER_AUTO(True, clang::QualType);
  REGISTER_POLY_MATCHER_AUTO(True, clang::Decl);
  REGISTER_POLY_MATCHER_AUTO(True, clang::CXXCtorInitializer);

  // HasAnyArgument
  REGISTER_POLY_MATCHER_AUTO(HasAnyArgument, clang::CallExpr);
  REGISTER_POLY_MATCHER_AUTO(HasAnyArgument, clang::CXXConstructExpr);

  // HasDeclaration
  REGISTER_POLY_MATCHER_AUTO(HasDeclaration, clang::QualType);
  REGISTER_POLY_MATCHER_AUTO(HasDeclaration, clang::CallExpr);
  REGISTER_POLY_MATCHER_AUTO(HasDeclaration, clang::CXXConstructExpr);

  // HasArgument
  REGISTER_POLY_MATCHER_AUTO(HasArgument, clang::CallExpr);
  REGISTER_POLY_MATCHER_AUTO(HasArgument, clang::CXXConstructExpr);

  // HasOperatorName
  REGISTER_POLY_MATCHER_AUTO(HasOperatorName, clang::BinaryOperator);
  REGISTER_POLY_MATCHER_AUTO(HasOperatorName, clang::UnaryOperator);

  // IsDefinition
  REGISTER_POLY_MATCHER_AUTO(IsDefinition, clang::TagDecl);
  REGISTER_POLY_MATCHER_AUTO(IsDefinition, clang::VarDecl);
  REGISTER_POLY_MATCHER_AUTO(IsDefinition, clang::FunctionDecl);

  // IsTemplateInstantiation
  REGISTER_POLY_MATCHER_AUTO(IsTemplateInstantiation, clang::FunctionDecl);
  REGISTER_POLY_MATCHER_AUTO(IsTemplateInstantiation, clang::VarDecl);
  REGISTER_POLY_MATCHER_AUTO(IsTemplateInstantiation, clang::CXXRecordDecl);

  // ArgumentCountIs
  REGISTER_POLY_MATCHER_AUTO(ArgumentCountIs, clang::CallExpr);
  REGISTER_POLY_MATCHER_AUTO(ArgumentCountIs, clang::CXXConstructExpr);

  // For if() and (?:)
  REGISTER_POLY_MATCHER_AUTO(HasCondition, clang::IfStmt);
  REGISTER_POLY_MATCHER_AUTO(HasCondition, clang::ConditionalOperator);

  // Equals. TODO: Needs more.
  REGISTER_POLY_NAMED_MATCHER_AUTO(Equals, Equals<bool>,
                                   clang::CXXBoolLiteralExpr);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Equals, Equals<unsigned long long>,
                                   clang::IntegerLiteral);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Equals, Equals<long long>,
                                   clang::IntegerLiteral);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Equals, Equals<unsigned>,
                                   clang::CharacterLiteral);

  // Has/hasDescendant/forEach/forEachDescendant for Decl and Stmt
  REGISTER_POLY_NAMED_MATCHER_AUTO(Has, has<clang::Decl>, clang::Decl);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Has, has<clang::Stmt>, clang::Stmt);
  REGISTER_POLY_NAMED_MATCHER_AUTO(hasDescendant, hasDescendant<clang::Decl>,
                                   clang::Decl);
  REGISTER_POLY_NAMED_MATCHER_AUTO(hasDescendant, hasDescendant<clang::Stmt>,
                                   clang::Stmt);
  REGISTER_POLY_NAMED_MATCHER_AUTO(forEach, forEach<clang::Decl>, clang::Decl);
  REGISTER_POLY_NAMED_MATCHER_AUTO(forEach, forEach<clang::Stmt>, clang::Stmt);
  REGISTER_POLY_NAMED_MATCHER_AUTO(forEachDescendant,
                                   forEachDescendant<clang::Decl>, clang::Decl);
  REGISTER_POLY_NAMED_MATCHER_AUTO(forEachDescendant,
                                   forEachDescendant<clang::Stmt>, clang::Stmt);


  // Id
  REGISTER_NAMED_MATCHER_AUTO(Id, Id<clang::Decl>);
  REGISTER_NAMED_MATCHER_AUTO(Id, Id<clang::Stmt>);

  // Not. One per basic type.
  REGISTER_POLY_NAMED_MATCHER_AUTO(Not, Not<Matcher<clang::Decl> >,
                                   clang::Decl);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Not, Not<Matcher<clang::Stmt> >,
                                   clang::Stmt);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Not, Not<Matcher<clang::QualType> >,
                                   clang::QualType);
  REGISTER_POLY_NAMED_MATCHER_AUTO(Not,
                                   Not<Matcher<clang::CXXCtorInitializer> >,
                                   clang::CXXCtorInitializer);

  // ThisPointerType is overloaded.
  REGISTER_MATCHER1(ThisPointerType, Matcher<clang::CallExpr>,
                    const Matcher<clang::QualType>&);
  REGISTER_MATCHER1(ThisPointerType, Matcher<clang::CallExpr>,
                    const Matcher<clang::Decl>&);

  // Callee is overloaded.
  REGISTER_MATCHER1(Callee, Matcher<clang::CallExpr>,
                    const Matcher<clang::Stmt>&);
  REGISTER_MATCHER1(Callee, Matcher<clang::CallExpr>,
                    const Matcher<clang::Decl>&);

  // PointsTo is overloaded.
  REGISTER_MATCHER1(PointsTo, Matcher<clang::QualType>,
                    const Matcher<clang::QualType>&);
  REGISTER_MATCHER1(PointsTo, Matcher<clang::QualType>,
                    const Matcher<clang::Decl>&);

  // References is overloaded.
  REGISTER_MATCHER1(References, Matcher<clang::QualType>,
                    const Matcher<clang::QualType>&);
  REGISTER_MATCHER1(References, Matcher<clang::QualType>,
                    const Matcher<clang::Decl>&);

  // Some hardcoded marshallers
  RegisterMatcher("allOf", new internal::MatcherMarshallAllOf, Data);
  RegisterMatcher("anyOf", new internal::MatcherMarshallAnyOf, Data);

  return Data;
}

// The registry is const to make it thread-safe.
static const RegistryMaps* const RegistryData = RegisterMatchers();

}  // anonymous namespace

// static
GenericValue Registry::constructMatcher(
    const std::string& MatcherName,
    const std::vector<GenericValue>& Args) {
  ConstructorMap::const_iterator it =
      RegistryData->Constructors.find(MatcherName);
  if (it == RegistryData->Constructors.end()) {
    return GenericError("Not found: " + MatcherName);
  }

  return it->second->run(Args);
}

// static
const std::string* Registry::getTypeMatcherName(const std::string& TypeName) {
  KindNameMap::const_iterator it = RegistryData->KindNames.find(TypeName);
  if (it == RegistryData->KindNames.end()) return NULL;
  return &it->second;
}

// static
bool Registry::isKindName(const std::string& MatcherName) {
  return RegistryData->AllKinds.count(MatcherName);
}

}  // namespace dynamic
}  // namespace ast_matchers
}  // namespace clang
