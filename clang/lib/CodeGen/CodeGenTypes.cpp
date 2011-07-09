//===--- CodeGenTypes.cpp - Type translation for LLVM CodeGen -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is the code that handles AST -> LLVM type lowering.
//
//===----------------------------------------------------------------------===//

#include "CodeGenTypes.h"
#include "CGCall.h"
#include "CGCXXABI.h"
#include "CGRecordLayout.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecordLayout.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/Target/TargetData.h"
using namespace clang;
using namespace CodeGen;

CodeGenTypes::CodeGenTypes(ASTContext &Ctx, llvm::Module& M,
                           const llvm::TargetData &TD, const ABIInfo &Info,
                           CGCXXABI &CXXABI, const CodeGenOptions &CGO)
  : Context(Ctx), Target(Ctx.Target), TheModule(M), TheTargetData(TD),
    TheABIInfo(Info), TheCXXABI(CXXABI), CodeGenOpts(CGO) {
}

CodeGenTypes::~CodeGenTypes() {
  for (llvm::DenseMap<const Type *, CGRecordLayout *>::iterator
         I = CGRecordLayouts.begin(), E = CGRecordLayouts.end();
      I != E; ++I)
    delete I->second;

  for (llvm::FoldingSet<CGFunctionInfo>::iterator
       I = FunctionInfos.begin(), E = FunctionInfos.end(); I != E; )
    delete &*I++;
}

void CodeGenTypes::addRecordTypeName(const RecordDecl *RD, const llvm::Type *Ty,
                                     llvm::StringRef suffix) {
  llvm::SmallString<256> TypeName;
  llvm::raw_svector_ostream OS(TypeName);
  OS << RD->getKindName() << '.';
  
  // Name the codegen type after the typedef name
  // if there is no tag type name available
  if (RD->getIdentifier()) {
    // FIXME: We should not have to check for a null decl context here.
    // Right now we do it because the implicit Obj-C decls don't have one.
    if (RD->getDeclContext())
      OS << RD->getQualifiedNameAsString();
    else
      RD->printName(OS);
  } else if (const TypedefNameDecl *TDD = RD->getTypedefNameForAnonDecl()) {
    // FIXME: We should not have to check for a null decl context here.
    // Right now we do it because the implicit Obj-C decls don't have one.
    if (TDD->getDeclContext())
      OS << TDD->getQualifiedNameAsString();
    else
      TDD->printName(OS);
  } else
    OS << "anon";

  if (!suffix.empty())
    OS << suffix;

  // FIXME!
#if 1
  abort();
#else
  TheModule.addTypeName(OS.str(), Ty);
#endif
}

/// ConvertType - Convert the specified type to its LLVM form.
llvm::Type *CodeGenTypes::ConvertType(QualType T) {
  T = Context.getCanonicalType(T);

  // See if type is already cached.
  llvm::DenseMap<const Type *, llvm::Type *>::iterator
    I = TypeCache.find(T.getTypePtr());
  // If type is found in map then use it. Otherwise, convert type T.
  if (I != TypeCache.end())
    return I->second;

  llvm::Type *ResultType = ConvertNewType(T);
  TypeCache.insert(std::make_pair(T.getTypePtr(), ResultType));
  return ResultType;
}

/// ConvertTypeForMem - Convert type T into a llvm::Type.  This differs from
/// ConvertType in that it is used to convert to the memory representation for
/// a type.  For example, the scalar representation for _Bool is i1, but the
/// memory representation is usually i8 or i32, depending on the target.
llvm::Type *CodeGenTypes::ConvertTypeForMem(QualType T){
  llvm::Type *R = ConvertType(T);

  // If this is a non-bool type, don't map it.
  if (!R->isIntegerTy(1))
    return R;

  // Otherwise, return an integer of the target-specified size.
  return llvm::IntegerType::get(getLLVMContext(),
                                (unsigned)Context.getTypeSize(T));

}

// Code to verify a given function type is complete, i.e. the return type
// and all of the argument types are complete.
const TagType *CodeGenTypes::VerifyFuncTypeComplete(const Type* T) {
  const FunctionType *FT = cast<FunctionType>(T);
  if (const TagType* TT = FT->getResultType()->getAs<TagType>())
    if (!TT->getDecl()->isDefinition())
      return TT;
  if (const FunctionProtoType *FPT = dyn_cast<FunctionProtoType>(T))
    for (unsigned i = 0, e = FPT->getNumArgs(); i != e; i++)
      if (const TagType *TT = FPT->getArgType(i)->getAs<TagType>())
        if (!TT->getDecl()->isDefinition())
          return TT;
  return 0;
}

/// UpdateCompletedType - When we find the full definition for a TagDecl,
/// replace the 'opaque' type we previously made for it if applicable.
void CodeGenTypes::UpdateCompletedType(const TagDecl *TD) {
  // If this is an enum being completed, then we flush all non-struct types from
  // the cache.  This allows function types and other things that may be derived
  // from the enum to be recomputed.
  if (isa<EnumDecl>(TD)) {
    TypeCache.clear();
    return;
  }
  
  const RecordDecl *RD = cast<RecordDecl>(TD);
  if (!RD->isDependentType())
    ConvertRecordDeclType(RD);
}

static llvm::Type *getTypeForFormat(llvm::LLVMContext &VMContext,
                                    const llvm::fltSemantics &format) {
  if (&format == &llvm::APFloat::IEEEsingle)
    return llvm::Type::getFloatTy(VMContext);
  if (&format == &llvm::APFloat::IEEEdouble)
    return llvm::Type::getDoubleTy(VMContext);
  if (&format == &llvm::APFloat::IEEEquad)
    return llvm::Type::getFP128Ty(VMContext);
  if (&format == &llvm::APFloat::PPCDoubleDouble)
    return llvm::Type::getPPC_FP128Ty(VMContext);
  if (&format == &llvm::APFloat::x87DoubleExtended)
    return llvm::Type::getX86_FP80Ty(VMContext);
  assert(0 && "Unknown float format!");
  return 0;
}

llvm::Type *CodeGenTypes::ConvertNewType(QualType T) {
  const clang::Type &Ty = *Context.getCanonicalType(T).getTypePtr();

  switch (Ty.getTypeClass()) {
#define TYPE(Class, Base)
#define ABSTRACT_TYPE(Class, Base)
#define NON_CANONICAL_TYPE(Class, Base) case Type::Class:
#define DEPENDENT_TYPE(Class, Base) case Type::Class:
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(Class, Base) case Type::Class:
#include "clang/AST/TypeNodes.def"
    llvm_unreachable("Non-canonical or dependent types aren't possible.");
    break;

  case Type::Builtin: {
    switch (cast<BuiltinType>(Ty).getKind()) {
    case BuiltinType::Void:
    case BuiltinType::ObjCId:
    case BuiltinType::ObjCClass:
    case BuiltinType::ObjCSel:
      // LLVM void type can only be used as the result of a function call.  Just
      // map to the same as char.
      return llvm::Type::getInt8Ty(getLLVMContext());

    case BuiltinType::Bool:
      // Note that we always return bool as i1 for use as a scalar type.
      return llvm::Type::getInt1Ty(getLLVMContext());

    case BuiltinType::Char_S:
    case BuiltinType::Char_U:
    case BuiltinType::SChar:
    case BuiltinType::UChar:
    case BuiltinType::Short:
    case BuiltinType::UShort:
    case BuiltinType::Int:
    case BuiltinType::UInt:
    case BuiltinType::Long:
    case BuiltinType::ULong:
    case BuiltinType::LongLong:
    case BuiltinType::ULongLong:
    case BuiltinType::WChar_S:
    case BuiltinType::WChar_U:
    case BuiltinType::Char16:
    case BuiltinType::Char32:
      return llvm::IntegerType::get(getLLVMContext(),
        static_cast<unsigned>(Context.getTypeSize(T)));

    case BuiltinType::Float:
    case BuiltinType::Double:
    case BuiltinType::LongDouble:
      return getTypeForFormat(getLLVMContext(),
                              Context.getFloatTypeSemantics(T));

    case BuiltinType::NullPtr: {
      // Model std::nullptr_t as i8*
      const llvm::Type *Ty = llvm::Type::getInt8Ty(getLLVMContext());
      return llvm::PointerType::getUnqual(Ty);
    }
        
    case BuiltinType::UInt128:
    case BuiltinType::Int128:
      return llvm::IntegerType::get(getLLVMContext(), 128);
    
    case BuiltinType::Overload:
    case BuiltinType::Dependent:
    case BuiltinType::BoundMember:
    case BuiltinType::UnknownAny:
      llvm_unreachable("Unexpected placeholder builtin type!");
      break;
    }
    llvm_unreachable("Unknown builtin type!");
    break;
  }
  case Type::Complex: {
    const llvm::Type *EltTy =
      ConvertType(cast<ComplexType>(Ty).getElementType());
    return llvm::StructType::get(EltTy, EltTy, NULL);
  }
  case Type::LValueReference:
  case Type::RValueReference: {
    RecursionStatePointerRAII X(RecursionState);
    const ReferenceType &RTy = cast<ReferenceType>(Ty);
    QualType ETy = RTy.getPointeeType();
    llvm::Type *PointeeType = ConvertTypeForMem(ETy);
    unsigned AS = Context.getTargetAddressSpace(ETy);
    return llvm::PointerType::get(PointeeType, AS);
  }
  case Type::Pointer: {
    RecursionStatePointerRAII X(RecursionState);
    const PointerType &PTy = cast<PointerType>(Ty);
    QualType ETy = PTy.getPointeeType();
    llvm::Type *PointeeType = ConvertTypeForMem(ETy);
    if (PointeeType->isVoidTy())
      PointeeType = llvm::Type::getInt8Ty(getLLVMContext());
    unsigned AS = Context.getTargetAddressSpace(ETy);
    return llvm::PointerType::get(PointeeType, AS);
  }

  case Type::VariableArray: {
    const VariableArrayType &A = cast<VariableArrayType>(Ty);
    assert(A.getIndexTypeCVRQualifiers() == 0 &&
           "FIXME: We only handle trivial array types so far!");
    // VLAs resolve to the innermost element type; this matches
    // the return of alloca, and there isn't any obviously better choice.
    return ConvertTypeForMem(A.getElementType());
  }
  case Type::IncompleteArray: {
    const IncompleteArrayType &A = cast<IncompleteArrayType>(Ty);
    assert(A.getIndexTypeCVRQualifiers() == 0 &&
           "FIXME: We only handle trivial array types so far!");
    // int X[] -> [0 x int]
    return llvm::ArrayType::get(ConvertTypeForMem(A.getElementType()), 0);
  }
  case Type::ConstantArray: {
    const ConstantArrayType &A = cast<ConstantArrayType>(Ty);
    const llvm::Type *EltTy = ConvertTypeForMem(A.getElementType());
    return llvm::ArrayType::get(EltTy, A.getSize().getZExtValue());
  }
  case Type::ExtVector:
  case Type::Vector: {
    const VectorType &VT = cast<VectorType>(Ty);
    return llvm::VectorType::get(ConvertType(VT.getElementType()),
                                 VT.getNumElements());
  }
  case Type::FunctionNoProto:
  case Type::FunctionProto: {
    // First, check whether we can build the full function type.  If the
    // function type depends on an incomplete type (e.g. a struct or enum), we
    // cannot lower the function type.
    if (VerifyFuncTypeComplete(&Ty))
      // This function's type depends on an incomplete tag type.
      // Return a placeholder type.
      return llvm::StructType::get(getLLVMContext());

    // The function type can be built; call the appropriate routines to
    // build it.
    const CGFunctionInfo *FI;
    bool isVariadic;
    if (const FunctionProtoType *FPT = dyn_cast<FunctionProtoType>(&Ty)) {
      FI = &getFunctionInfo(
                   CanQual<FunctionProtoType>::CreateUnsafe(QualType(FPT, 0)));
      isVariadic = FPT->isVariadic();
    } else {
      const FunctionNoProtoType *FNPT = cast<FunctionNoProtoType>(&Ty);
      FI = &getFunctionInfo(
                CanQual<FunctionNoProtoType>::CreateUnsafe(QualType(FNPT, 0)));
      isVariadic = true;
    }

    return GetFunctionType(*FI, isVariadic);
  }

  case Type::ObjCObject:
    return ConvertType(cast<ObjCObjectType>(Ty).getBaseType());

  case Type::ObjCInterface: {
    // Objective-C interfaces are always opaque (outside of the
    // runtime, which can do whatever it likes); we never refine
    // these.
    llvm::Type *&T = InterfaceTypes[cast<ObjCInterfaceType>(&Ty)];
    if (!T)
      T = llvm::StructType::get(getLLVMContext());
    return T;
  }

  case Type::ObjCObjectPointer: {
    RecursionStatePointerRAII X(RecursionState);
    // Protocol qualifications do not influence the LLVM type, we just return a
    // pointer to the underlying interface type. We don't need to worry about
    // recursive conversion.
    const llvm::Type *T =
      ConvertType(cast<ObjCObjectPointerType>(Ty).getPointeeType());
    return llvm::PointerType::getUnqual(T);
  }

  case Type::Record:
    return ConvertRecordDeclType(cast<RecordType>(Ty).getDecl());

  case Type::Enum: {
    const EnumDecl *ED = cast<EnumType>(Ty).getDecl();
    if (ED->isDefinition() || ED->isFixed())
      return ConvertType(ED->getIntegerType());
    // Return a placeholder type.
    return llvm::Type::getVoidTy(getLLVMContext());
  }

  case Type::BlockPointer: {
    RecursionStatePointerRAII X(RecursionState);
    const QualType FTy = cast<BlockPointerType>(Ty).getPointeeType();
    llvm::Type *PointeeType = ConvertTypeForMem(FTy);
    unsigned AS = Context.getTargetAddressSpace(FTy);
    return llvm::PointerType::get(PointeeType, AS);
  }

  case Type::MemberPointer: {
    return getCXXABI().ConvertMemberPointerType(cast<MemberPointerType>(&Ty));
  }
  }

  // FIXME: implement.
  return llvm::StructType::get(getLLVMContext());
}

/// ConvertRecordDeclType - Lay out a tagged decl type like struct or union.
llvm::StructType *CodeGenTypes::ConvertRecordDeclType(const RecordDecl *RD) {
  // TagDecl's are not necessarily unique, instead use the (clang)
  // type connected to the decl.
  const Type *Key = Context.getTagDeclType(RD).getTypePtr();

  llvm::StructType *&Entry = RecordDeclTypes[Key];

  // If we don't have a StructType at all yet, create the forward declaration.
  if (Entry == 0)
    Entry = llvm::StructType::createNamed(getLLVMContext(), 
                                          std::string(RD->getKindName()) + "." +
                                          RD->getQualifiedNameAsString());
  llvm::StructType *Ty = Entry;

  // If this is still a forward declaration, or the LLVM type is already
  // complete, there's nothing more to do.
  if (!RD->isDefinition() || !Ty->isOpaque())
    return Ty;
  
  // If we're recursively nested inside the conversion of a pointer inside the
  // struct, defer conversion.
  if (RecursionState == RS_StructPointer) {
    DeferredRecords.push_back(RD);
    return Ty;
  }

  // Okay, this is a definition of a type.  Compile the implementation now.
  RecursionStateTy SavedRecursionState = RecursionState;
  RecursionState = RS_Struct;

  // Force conversion of non-virtual base classes recursively.
  if (const CXXRecordDecl *CRD = dyn_cast<CXXRecordDecl>(RD)) {
    for (CXXRecordDecl::base_class_const_iterator i = CRD->bases_begin(),
         e = CRD->bases_end(); i != e; ++i) {
      if (!i->isVirtual()) {
        const CXXRecordDecl *Base =
          cast<CXXRecordDecl>(i->getType()->getAs<RecordType>()->getDecl());
        ConvertRecordDeclType(Base);
      }
    }
  }

  // Layout fields.
  CGRecordLayout *Layout = ComputeRecordLayout(RD, Ty);
  CGRecordLayouts[Key] = Layout;

  
  // Restore our recursion state.  If we're done converting the outer-most
  // record, then convert any deferred structs as well.
  RecursionState = SavedRecursionState;
  if (RecursionState == RS_Normal)
    while (!DeferredRecords.empty())
      ConvertRecordDeclType(DeferredRecords.pop_back_val());

  return Ty;
}

/// getCGRecordLayout - Return record layout info for the given record decl.
const CGRecordLayout &
CodeGenTypes::getCGRecordLayout(const RecordDecl *RD) {
  const Type *Key = Context.getTagDeclType(RD).getTypePtr();

  const CGRecordLayout *Layout = CGRecordLayouts.lookup(Key);
  if (!Layout) {
    // Compute the type information.
    ConvertRecordDeclType(RD);

    // Now try again.
    Layout = CGRecordLayouts.lookup(Key);
  }

  assert(Layout && "Unable to find record layout information for type");
  return *Layout;
}

void CodeGenTypes::addBaseSubobjectTypeName(const CXXRecordDecl *RD,
                                            const CGRecordLayout &layout) {
  llvm::StringRef suffix;
  if (layout.getBaseSubobjectLLVMType() != layout.getLLVMType())
    suffix = ".base";

  addRecordTypeName(RD, layout.getBaseSubobjectLLVMType(), suffix);
}

bool CodeGenTypes::isZeroInitializable(QualType T) {
  // No need to check for member pointers when not compiling C++.
  if (!Context.getLangOptions().CPlusPlus)
    return true;
  
  T = Context.getBaseElementType(T);
  
  // Records are non-zero-initializable if they contain any
  // non-zero-initializable subobjects.
  if (const RecordType *RT = T->getAs<RecordType>()) {
    const CXXRecordDecl *RD = cast<CXXRecordDecl>(RT->getDecl());
    return isZeroInitializable(RD);
  }

  // We have to ask the ABI about member pointers.
  if (const MemberPointerType *MPT = T->getAs<MemberPointerType>())
    return getCXXABI().isZeroInitializable(MPT);
  
  // Everything else is okay.
  return true;
}

bool CodeGenTypes::isZeroInitializable(const CXXRecordDecl *RD) {
  return getCGRecordLayout(RD).isZeroInitializable();
}
