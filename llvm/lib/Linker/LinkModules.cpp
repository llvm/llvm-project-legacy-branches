//===- lib/Linker/LinkModules.cpp - Module Linker Implementation ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the LLVM module linker.
//
//===----------------------------------------------------------------------===//

#include "llvm/Linker.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Path.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
// TypeMap implementation.
//===----------------------------------------------------------------------===//

namespace {
class TypeMapTy : public ValueMapTypeRemapper {
  /// MappedTypes - This is a mapping from a source type to a destination type
  /// to use.
  DenseMap<Type*, Type*> MappedTypes;

  /// DefinitionsToResolve - This is a list of non-opaque structs in the source
  /// module that are mapped to an opaque struct in the destination module.
  SmallVector<StructType*, 16> DefinitionsToResolve;
public:
  
  /// addTypeMapping - Indicate that the specified type in the destination
  /// module is conceptually equivalent to the specified type in the source
  /// module.  This updates the type mapping for equivalent types, and returns
  /// false.  If there is a hard type conflict (maybe merging "int x" with
  /// "extern float x") this returns true.
  bool addTypeMapping(Type *DstTy, Type *SrcTy);

  /// linkDefinedTypeBodies - Produce a body for an opaque type in the dest
  /// module from a type definition in the source module.
  void linkDefinedTypeBodies();
  
  /// get - Return the mapped type to use for the specified input type from the
  /// source module.
  Type *get(Type *T);

  FunctionType *get(FunctionType *T) {return cast<FunctionType>(get((Type*)T));}

private:
  Type *getImpl(Type *T);
  /// remapType - Implement the ValueMapTypeRemapper interface.
  Type *remapType(Type *SrcTy) {
    return get(SrcTy);
  }
  
  bool addTypeMappingRec(Type *DstTy, Type *SrcTy);
};
}

bool TypeMapTy::addTypeMapping(Type *DstTy, Type *SrcTy) {
  Type *&T = MappedTypes[SrcTy];
  if (T)
    return T != DstTy;
  
  if (DstTy == SrcTy) {
    T = DstTy;
    return false;
  }
  
  return addTypeMappingRec(DstTy, SrcTy);
}


/// addTypeMappingRec - This is the implementation function for addTypeMapping,
/// which optimizes out the map lookup in the recursive walk.  
bool TypeMapTy::addTypeMappingRec(Type *DstTy, Type *SrcTy) {
  // Two types cannot be resolved together if they are of different primitive
  // type.  For example, we cannot resolve an int to a float.
  if (DstTy->getTypeID() != SrcTy->getTypeID()) return true;

  // Otherwise, resolve the used type used by this derived type.
  switch (DstTy->getTypeID()) {
  default:
    return true;
  case Type::StructTyID: {
    StructType *DstST = cast<StructType>(DstTy);
    StructType *SrcST = cast<StructType>(SrcTy);
    
    // If the destination type is opaque, then it should be resolved to the
    // input type.  If the source type is opaque, then it gets whatever the
    // destination type is.
    if (SrcST->isOpaque())
      break;
    // If the type is opaque in the dest module but not the src module, then we
    // should get the new type definition from the src module.
    if (DstST->isOpaque()) { 
      DefinitionsToResolve.push_back(SrcST);
      break;
    }
    
    if (DstST->getNumContainedTypes() != SrcST->getNumContainedTypes() ||
        DstST->isPacked() != SrcST->isPacked())
      return true;
    
    Type *&Entry = MappedTypes[SrcST];
    if (Entry)
      return Entry != DstTy;
    
    // Otherwise, we speculatively assume that the structs can be merged, add an
    // entry to the type map so we don't infinitely recurse.
    Entry = DstST;

    // Then call addTypeMapping on each entry (not "Rec") so that we get the
    // caching behavior of the map check for each element.
    for (unsigned i = 0, e = DstST->getNumContainedTypes(); i != e; ++i) {
      Type *SE = SrcST->getContainedType(i), *DE = DstST->getContainedType(i);
      if (addTypeMapping(DE, SE))
        return true;
    }
    return false;
  }
  case Type::FunctionTyID: {
    const FunctionType *DstFT = cast<FunctionType>(DstTy);
    const FunctionType *SrcFT = cast<FunctionType>(SrcTy);
    if (DstFT->isVarArg() != SrcFT->isVarArg() ||
        DstFT->getNumContainedTypes() != SrcFT->getNumContainedTypes())
      return true;

    for (unsigned i = 0, e = DstFT->getNumContainedTypes(); i != e; ++i) {
      Type *SE = SrcFT->getContainedType(i), *DE = DstFT->getContainedType(i);
      if (SE != DE && addTypeMappingRec(DE, SE))
        return true;
    }
    break;
  }
  case Type::ArrayTyID: {
    ArrayType *DAT = cast<ArrayType>(DstTy);
    ArrayType *SAT = cast<ArrayType>(SrcTy);
    if (DAT->getNumElements() != SAT->getNumElements() ||
        addTypeMappingRec(DAT->getElementType(), SAT->getElementType()))
      return true;
    break;
  }
  case Type::VectorTyID: {
    VectorType *DVT = cast<VectorType>(DstTy);
    VectorType *SVT = cast<VectorType>(SrcTy);
    if (DVT->getNumElements() != SVT->getNumElements() ||
        addTypeMappingRec(DVT->getElementType(), SVT->getElementType()))
      return true;
    break;
  }
  case Type::PointerTyID: {
    PointerType *DstPT = cast<PointerType>(DstTy);
    PointerType *SrcPT = cast<PointerType>(SrcTy);
    if (DstPT->getAddressSpace() != SrcPT->getAddressSpace() ||
        addTypeMappingRec(DstPT->getElementType(), SrcPT->getElementType()))
      return true;
    break;
  }
  }
  
  MappedTypes[SrcTy] = DstTy;
  return false;
}

/// linkDefinedTypeBodies - Produce a body for an opaque type in the dest
/// module from a type definition in the source module.
void TypeMapTy::linkDefinedTypeBodies() {
  SmallVector<Type*, 16> Elements;
  SmallString<16> TmpName;
  
  // Note that processing entries in this loop (calling 'get') can add new
  // entries to the DefinitionsToResolve vector.
  while (!DefinitionsToResolve.empty()) {
    StructType *SrcSTy = DefinitionsToResolve.pop_back_val();
    StructType *DstSTy = cast<StructType>(MappedTypes[SrcSTy]);
    
    // TypeMap is a many-to-one mapping, if there were multiple types that
    // provide a body for DstSTy then previous iterations of this loop may have
    // already handled it.  Just ignore this case.
    if (!DstSTy->isOpaque()) continue;
    assert(!SrcSTy->isOpaque() && "Not resolving a definition?");
    
    // Map the body of the source type over to a new body for the dest type.
    Elements.resize(SrcSTy->getNumElements());
    for (unsigned i = 0, e = Elements.size(); i != e; ++i)
      Elements[i] = getImpl(SrcSTy->getElementType(i));
    
    DstSTy->setBody(Elements, SrcSTy->isPacked());
    
    // If DstSTy has no name or has a longer name than STy, then viciously steal
    // STy's name.
    if (!SrcSTy->hasName()) continue;
    StringRef SrcName = SrcSTy->getName();
    
    if (!DstSTy->hasName() || DstSTy->getName().size() > SrcName.size()) {
      TmpName.insert(TmpName.end(), SrcName.begin(), SrcName.end());
      SrcSTy->setName("");
      DstSTy->setName(TmpName.str());
      TmpName.clear();
    }
  }
}


/// get - Return the mapped type to use for the specified input type from the
/// source module.
Type *TypeMapTy::get(Type *Ty) {
  Type *Result = getImpl(Ty);
  
  // If this caused a reference to any struct type, resolve it before returning.
  if (!DefinitionsToResolve.empty())
    linkDefinedTypeBodies();
  return Result;
}

/// getImpl - This is the recursive version of get().
Type *TypeMapTy::getImpl(Type *Ty) {
  // If we already have an entry for this type, return it.
  Type **Entry = &MappedTypes[Ty];
  if (*Entry) return *Entry;
  
  // If this is not a named struct type, then just map all of the elements and
  // then rebuild the type from inside out.
  if (!isa<StructType>(Ty) || cast<StructType>(Ty)->isAnonymous()) {
    // If there are no element types to map, then the type is itself.  This is
    // true for the anonymous {} struct, things like 'float', integers, etc.
    if (Ty->getNumContainedTypes() == 0)
      return *Entry = Ty;
    
    // Remap all of the elements, keeping track of whether any of them change.
    bool AnyChange = false;
    SmallVector<Type*, 4> ElementTypes;
    ElementTypes.resize(Ty->getNumContainedTypes());
    for (unsigned i = 0, e = Ty->getNumContainedTypes(); i != e; ++i) {
      ElementTypes[i] = getImpl(Ty->getContainedType(i));
      AnyChange |= ElementTypes[i] != Ty->getContainedType(i);
    }
    
    // If we found our type while recursively processing stuff, just use it.
    Entry = &MappedTypes[Ty];
    if (*Entry) return *Entry;
    
    // If all of the element types mapped directly over, then the type is usable
    // as-is.
    if (!AnyChange)
      return *Entry = Ty;
    
    // Otherwise, rebuild a modified type.
    switch (Ty->getTypeID()) {
    default: assert(0 && "unknown derived type to remap");
    case Type::ArrayTyID:
      return *Entry = ArrayType::get(ElementTypes[0],
                                     cast<ArrayType>(Ty)->getNumElements());
    case Type::VectorTyID: 
      return *Entry = VectorType::get(ElementTypes[0],
                                      cast<VectorType>(Ty)->getNumElements());
    case Type::PointerTyID:
      return *Entry = PointerType::get(ElementTypes[0],
                                      cast<PointerType>(Ty)->getAddressSpace());
    case Type::FunctionTyID:
      return *Entry = FunctionType::get(ElementTypes[0],
                                        ArrayRef<Type*>(ElementTypes).slice(1),
                                        cast<FunctionType>(Ty)->isVarArg());
    case Type::StructTyID:
      // Note that this is only reached for anonymous structs.
      return *Entry = StructType::get(Ty->getContext(), ElementTypes,
                                      cast<StructType>(Ty)->isPacked());
    }
  }

  // Otherwise, this is an unmapped named struct.  If the struct can be directly
  // mapped over, just use it as-is.  This happens in a case when the linked-in
  // module has something like:
  //   %T = type {%T*, i32}
  //   @GV = global %T* null
  // where T does not exist at all in the destination module.
  //
  // The other case we watch for is when the type is not in the destination
  // module, but that it has to be rebuilt because it refers to something that
  // is already mapped.  For example, if the destination module has:
  //  %A = type { i32 }
  // and the source module has something like
  //  %A' = type { i32 }
  //  %B = type { %A'* }
  //  @GV = global %B* null
  // then we want to create a new type: "%B = type { %A*}" and have it take the
  // pristine "%B" name from the source module.
  //
  // To determine which case this is, we have to recursively walk the type graph
  // speculating that we'll be able to reuse it unmodified.  Only if this is
  // safe would we map the entire thing over.  Because this is an optimization,
  // and is not required for the prettiness of the linked module, we just skip
  // it and always rebuild a type here.
  StructType *STy = cast<StructType>(Ty);
  
  // If the type is opaque, we can just use it directly.
  if (STy->isOpaque())
    return *Entry = STy;
  
  // Otherwise we create a new type and resolve its body later.  This will be
  // resolved by the top level of get().
  DefinitionsToResolve.push_back(STy);
  return *Entry = StructType::createNamed(STy->getContext(), "");
}



//===----------------------------------------------------------------------===//
// ModuleLinker implementation.
//===----------------------------------------------------------------------===//

namespace {
  /// ModuleLinker - This is an implementation class for the LinkModules
  /// function, which is the entrypoint for this file.
  class ModuleLinker {
    Module *DstM, *SrcM;
    
    TypeMapTy TypeMap; 

    /// ValueMap - Mapping of values from what they used to be in Src, to what
    /// they are now in DstM.  ValueToValueMapTy is a ValueMap, which involves
    /// some overhead due to the use of Value handles which the Linker doesn't
    /// actually need, but this allows us to reuse the ValueMapper code.
    ValueToValueMapTy ValueMap;
  public:
    std::string ErrorMsg;
    
    ModuleLinker(Module *dstM, Module *srcM) : DstM(dstM), SrcM(srcM) { }
    
    bool run();
    
  private:
    /// emitError - Helper method for setting a message and returning an error
    /// code.
    bool emitError(const Twine &Message) {
      ErrorMsg = Message.str();
      return true;
    }
    
    /// getLinkageResult - This analyzes the two global values and determines
    /// what the result will look like in the destination module.
    bool getLinkageResult(GlobalValue *Dest, const GlobalValue *Src,
                          GlobalValue::LinkageTypes &LT, bool &LinkFromSrc);

    /// getLinkedToGlobal - Given a global in the source module, return the
    /// global in the destination module that is being linked to, if any.
    GlobalValue *getLinkedToGlobal(GlobalValue *SrcGV) {
      // If the source has no name it can't link.  If it has local linkage,
      // there is no name match-up going on.
      if (!SrcGV->hasName() || SrcGV->hasLocalLinkage())
        return 0;
      
      // Otherwise see if we have a match in the destination module's symtab.
      GlobalValue *DGV = DstM->getNamedValue(SrcGV->getName());
      if (DGV == 0) return 0;
        
      // If we found a global with the same name in the dest module, but it has
      // internal linkage, we are really not doing any linkage here.
      if (DGV->hasLocalLinkage())
        return 0;

      // Otherwise, we do in fact link to the destination global.
      return DGV;
    }
    
    void computeTypeMapping();
    
    bool linkAppendingVars(GlobalVariable *DstGV, GlobalVariable *SrcGV);
    bool linkGlobalProto(GlobalVariable *SrcGV);
    bool linkFunctionProto(Function *SrcF);
    bool linkAliasProto(GlobalAlias *SrcA);
    
    void linkGlobalInits();
    void linkFunctionBody(Function *Dst, Function *Src);
    void linkAliasBodies();
    void linkNamedMDNodes();
  };
}



/// forceRenaming - The LLVM SymbolTable class autorenames globals that conflict
/// in the symbol table.  This is good for all clients except for us.  Go
/// through the trouble to force this back.
static void forceRenaming(GlobalValue *GV, StringRef Name) {
  // If the global doesn't force its name or if it already has the right name,
  // there is nothing for us to do.
  if (GV->hasLocalLinkage() || GV->getName() == Name)
    return;

  Module *M = GV->getParent();

  // If there is a conflict, rename the conflict.
  if (GlobalValue *ConflictGV = M->getNamedValue(Name)) {
    GV->takeName(ConflictGV);
    ConflictGV->setName(Name);    // This will cause ConflictGV to get renamed
    assert(ConflictGV->getName() != Name && "forceRenaming didn't work");
  } else {
    GV->setName(Name);              // Force the name back
  }
}

/// CopyGVAttributes - copy additional attributes (those not needed to construct
/// a GlobalValue) from the SrcGV to the DestGV.
static void CopyGVAttributes(GlobalValue *DestGV, const GlobalValue *SrcGV) {
  // Use the maximum alignment, rather than just copying the alignment of SrcGV.
  unsigned Alignment = std::max(DestGV->getAlignment(), SrcGV->getAlignment());
  DestGV->copyAttributesFrom(SrcGV);
  DestGV->setAlignment(Alignment);
  
  forceRenaming(DestGV, SrcGV->getName());
}

/// getLinkageResult - This analyzes the two global values and determines what
/// the result will look like in the destination module.  In particular, it
/// computes the resultant linkage type, computes whether the global in the
/// source should be copied over to the destination (replacing the existing
/// one), and computes whether this linkage is an error or not. It also performs
/// visibility checks: we cannot link together two symbols with different
/// visibilities.
bool ModuleLinker::getLinkageResult(GlobalValue *Dest, const GlobalValue *Src,
                                    GlobalValue::LinkageTypes &LT, 
                                    bool &LinkFromSrc) {
  assert(Dest && "Must have two globals being queried");
  assert(!Src->hasLocalLinkage() &&
         "If Src has internal linkage, Dest shouldn't be set!");
  
  // FIXME: GlobalAlias::isDeclaration is broken, should always be
  // false.
  bool SrcIsDeclaration = Src->isDeclaration() && !isa<GlobalAlias>(Src);
  bool DestIsDeclaration = Dest->isDeclaration() && !isa<GlobalAlias>(Dest);
  
  if (SrcIsDeclaration) {
    // If Src is external or if both Src & Dest are external..  Just link the
    // external globals, we aren't adding anything.
    if (Src->hasDLLImportLinkage()) {
      // If one of GVs has DLLImport linkage, result should be dllimport'ed.
      if (DestIsDeclaration) {
        LinkFromSrc = true;
        LT = Src->getLinkage();
      }
    } else if (Dest->hasExternalWeakLinkage()) {
      // If the Dest is weak, use the source linkage.
      LinkFromSrc = true;
      LT = Src->getLinkage();
    } else {
      LinkFromSrc = false;
      LT = Dest->getLinkage();
    }
  } else if (DestIsDeclaration && !Dest->hasDLLImportLinkage()) {
    // If Dest is external but Src is not:
    LinkFromSrc = true;
    LT = Src->getLinkage();
  } else if (Src->isWeakForLinker()) {
    // At this point we know that Dest has LinkOnce, External*, Weak, Common,
    // or DLL* linkage.
    if (Dest->hasExternalWeakLinkage() ||
        Dest->hasAvailableExternallyLinkage() ||
        (Dest->hasLinkOnceLinkage() &&
         (Src->hasWeakLinkage() || Src->hasCommonLinkage()))) {
      LinkFromSrc = true;
      LT = Src->getLinkage();
    } else {
      LinkFromSrc = false;
      LT = Dest->getLinkage();
    }
  } else if (Dest->isWeakForLinker()) {
    // At this point we know that Src has External* or DLL* linkage.
    if (Src->hasExternalWeakLinkage()) {
      LinkFromSrc = false;
      LT = Dest->getLinkage();
    } else {
      LinkFromSrc = true;
      LT = GlobalValue::ExternalLinkage;
    }
  } else {
    assert((Dest->hasExternalLinkage()  || Dest->hasDLLImportLinkage() ||
            Dest->hasDLLExportLinkage() || Dest->hasExternalWeakLinkage()) &&
           (Src->hasExternalLinkage()   || Src->hasDLLImportLinkage() ||
            Src->hasDLLExportLinkage()  || Src->hasExternalWeakLinkage()) &&
           "Unexpected linkage type!");
    return emitError("Linking globals named '" + Src->getName() +
                 "': symbol multiply defined!");
  }

  // Check visibility
  if (Src->getVisibility() != Dest->getVisibility() &&
      !SrcIsDeclaration && !DestIsDeclaration &&
      !Src->hasAvailableExternallyLinkage() &&
      !Dest->hasAvailableExternallyLinkage())
    return emitError("Linking globals named '" + Src->getName() +
                   "': symbols have different visibilities!");
  return false;
}

/// linkAppendingVars - If there were any appending global variables, link them
/// together now.  Return true on error.
bool ModuleLinker::linkAppendingVars(GlobalVariable *DstGV,
                                     GlobalVariable *SrcGV) {
 
  if (!SrcGV->hasAppendingLinkage() || !DstGV->hasAppendingLinkage())
    return emitError("Linking globals named '" + SrcGV->getName() +
           "': can only link appending global with another appending global!");
  
  ArrayType *DstTy = cast<ArrayType>(DstGV->getType()->getElementType());
  ArrayType *SrcTy = cast<ArrayType>(SrcGV->getType()->getElementType());
  // FIXME: Should map element type.
  Type *EltTy = DstTy->getElementType();
  
  // Check to see that they two arrays agree on type.
  if (EltTy != SrcTy->getElementType())
    return emitError("Appending variables with different element types!");
  if (DstGV->isConstant() != SrcGV->isConstant())
    return emitError("Appending variables linked with different const'ness!");
  
  if (DstGV->getAlignment() != SrcGV->getAlignment())
    return emitError(
             "Appending variables with different alignment need to be linked!");
  
  if (DstGV->getVisibility() != SrcGV->getVisibility())
    return emitError(
            "Appending variables with different visibility need to be linked!");
  
  if (DstGV->getSection() != SrcGV->getSection())
    return emitError(
          "Appending variables with different section name need to be linked!");
  
  uint64_t NewSize = DstTy->getNumElements() + SrcTy->getNumElements();
  ArrayType *NewType = ArrayType::get(EltTy, NewSize);
  
  // Create the new global variable.
  GlobalVariable *NG =
    new GlobalVariable(*DstGV->getParent(), NewType, SrcGV->isConstant(),
                       DstGV->getLinkage(), /*init*/0, SrcGV->getName(), DstGV,
                       DstGV->isThreadLocal(),
                       DstGV->getType()->getAddressSpace());
  
  // Propagate alignment, visibility and section info.
  CopyGVAttributes(NG, DstGV);
  
  // Merge the initializer.
  SmallVector<Constant*, 16> Elements;
  Elements.reserve(NewSize);
  if (ConstantArray *I = dyn_cast<ConstantArray>(DstGV->getInitializer())) {
    for (unsigned i = 0, e = DstTy->getNumElements(); i != e; ++i)
      Elements.push_back(I->getOperand(i));
  } else {
    assert(isa<ConstantAggregateZero>(DstGV->getInitializer()));
    Elements.append(DstTy->getNumElements(), Constant::getNullValue(EltTy));
  }
  if (const ConstantArray *I =dyn_cast<ConstantArray>(SrcGV->getInitializer())){
    // FIXME: Should map values from src to dest.
    for (unsigned i = 0, e = SrcTy->getNumElements(); i != e; ++i)
      Elements.push_back(I->getOperand(i));
  } else {
    assert(isa<ConstantAggregateZero>(SrcGV->getInitializer()));
    Elements.append(SrcTy->getNumElements(), Constant::getNullValue(EltTy));
  }
  NG->setInitializer(ConstantArray::get(NewType, Elements));
  
  // Replace any uses of the two global variables with uses of the new
  // global.
  ValueMap[SrcGV] = ConstantExpr::getBitCast(NG, SrcGV->getType());

  DstGV->replaceAllUsesWith(ConstantExpr::getBitCast(NG, DstGV->getType()));
  DstGV->eraseFromParent();
  
  // Zap the initializer in the source variable so we don't try to link it.
  SrcGV->setInitializer(0);
  SrcGV->setLinkage(GlobalValue::ExternalLinkage);
  return false;
}

/// computeTypeMapping - Loop over all of the linked values to compute type
/// mappings.  For example, if we link "extern Foo *x" and "Foo *x = NULL", then
/// we have two struct types 'Foo' but one got renamed when the module was
/// loaded into the same LLVMContext.
void ModuleLinker::computeTypeMapping() {
  // Incorporate globals.
  for (Module::global_iterator I = SrcM->global_begin(),
       E = SrcM->global_end(); I != E; ++I) {
    if (GlobalValue *DGV = getLinkedToGlobal(I))
      TypeMap.addTypeMapping(DGV->getType(), I->getType());
  }
  
  // Incorporate functions.
  for (Module::iterator I = SrcM->begin(), E = SrcM->end(); I != E; ++I) {
    if (GlobalValue *DGV = getLinkedToGlobal(I))
      TypeMap.addTypeMapping(DGV->getType(), I->getType());
  }
  
  // Don't bother incorporating aliases, they aren't generally typed well.
  
  // Now that we have discovered all of the type equivalences, get a body for
  // any 'opaque' types in the dest module that are now resolved. 
  TypeMap.linkDefinedTypeBodies();
}

/// linkGlobalProto - Loop through the global variables in the src module and
/// merge them into the dest module.
bool ModuleLinker::linkGlobalProto(GlobalVariable *SGV) {
  GlobalValue *DGV = getLinkedToGlobal(SGV);

  if (DGV) {
    // Concatenation of appending linkage variables is magic.
    if (DGV->hasAppendingLinkage() || SGV->hasAppendingLinkage())
      return linkAppendingVars(cast<GlobalVariable>(DGV), SGV);
    
    // Determine whether linkage of these two globals follows the source
    // module's definition or the destination module's definition.
    GlobalValue::LinkageTypes NewLinkage = GlobalValue::InternalLinkage;
    bool LinkFromSrc = false;
    if (getLinkageResult(DGV, SGV, NewLinkage, LinkFromSrc))
      return true;

    // If we're not linking from the source, then keep the definition that we
    // have.
    if (!LinkFromSrc) {
      // Special case for const propagation.
      if (GlobalVariable *DGVar = dyn_cast<GlobalVariable>(DGV))
        if (DGVar->isDeclaration() && SGV->isConstant() && !DGVar->isConstant())
          DGVar->setConstant(true);
      
      // Set calculated linkage.
      DGV->setLinkage(NewLinkage);
      
      // Make sure to remember this mapping.
      ValueMap[SGV] = ConstantExpr::getBitCast(DGV,TypeMap.get(SGV->getType()));
      
      // Destroy the source global's initializer (and convert it to a prototype)
      // so that we don't attempt to copy it over when processing global
      // initializers.
      SGV->setInitializer(0);
      SGV->setLinkage(GlobalValue::ExternalLinkage);
      return false;
    }
  }
  
  // No linking to be performed or linking from the source: simply create an
  // identical version of the symbol over in the dest module... the
  // initializer will be filled in later by LinkGlobalInits.
  GlobalVariable *NewDGV =
    new GlobalVariable(*DstM, TypeMap.get(SGV->getType()->getElementType()),
                       SGV->isConstant(), SGV->getLinkage(), /*init*/0,
                       SGV->getName(), /*insertbefore*/0,
                       SGV->isThreadLocal(),
                       SGV->getType()->getAddressSpace());
  // Propagate alignment, visibility and section info.
  CopyGVAttributes(NewDGV, SGV);

  if (DGV) {
    DGV->replaceAllUsesWith(ConstantExpr::getBitCast(NewDGV, DGV->getType()));
    DGV->eraseFromParent();
  }
  
  // Make sure to remember this mapping.
  ValueMap[SGV] = NewDGV;
  return false;
}

/// linkFunctionProto - Link the function in the source module into the
/// destination module if needed, setting up mapping information.
bool ModuleLinker::linkFunctionProto(Function *SF) {
  GlobalValue *DGV = getLinkedToGlobal(SF);

  if (DGV) {
    GlobalValue::LinkageTypes NewLinkage = GlobalValue::InternalLinkage;
    bool LinkFromSrc = false;
    if (getLinkageResult(DGV, SF, NewLinkage, LinkFromSrc))
      return true;
    
    if (!LinkFromSrc) {
      // Set calculated linkage
      DGV->setLinkage(NewLinkage);
      
      // Make sure to remember this mapping.
      ValueMap[SF] = ConstantExpr::getBitCast(DGV, TypeMap.get(SF->getType()));
      
      // Remove the body from the source module so we don't attempt to remap it.
      SF->deleteBody();
      return false;
    }
  }
  
  // If there is no linkage to be performed or we are linking from the source,
  // bring SF over.
  Function *NewDF = Function::Create(TypeMap.get(SF->getFunctionType()),
                                     SF->getLinkage(), SF->getName(), DstM);
  CopyGVAttributes(NewDF, SF);

  if (DGV) {
    // Any uses of DF need to change to NewDF, with cast.
    DGV->replaceAllUsesWith(ConstantExpr::getBitCast(NewDF, DGV->getType()));
    DGV->eraseFromParent();
  }
  
  ValueMap[SF] = NewDF;
  return false;
}

/// LinkAliasProto - Set up prototypes for any aliases that come over from the
/// source module.
bool ModuleLinker::linkAliasProto(GlobalAlias *SGA) {
  GlobalValue *DGV = getLinkedToGlobal(SGA);
  
  if (DGV) {
    GlobalValue::LinkageTypes NewLinkage = GlobalValue::InternalLinkage;
    bool LinkFromSrc = false;
    if (getLinkageResult(DGV, SGA, NewLinkage, LinkFromSrc))
      return true;
    
    if (!LinkFromSrc) {
      // Set calculated linkage.
      DGV->setLinkage(NewLinkage);
      
      // Make sure to remember this mapping.
      ValueMap[SGA] = ConstantExpr::getBitCast(DGV,TypeMap.get(SGA->getType()));
      
      // Remove the body from the source module so we don't attempt to remap it.
      SGA->setAliasee(0);
      return false;
    }
  }
  
  // If there is no linkage to be performed or we're linking from the source,
  // bring over SGA.
  GlobalAlias *NewDA = new GlobalAlias(TypeMap.get(SGA->getType()),
                                       SGA->getLinkage(), SGA->getName(),
                                       /*aliasee*/0, DstM);
  CopyGVAttributes(NewDA, SGA);

  if (DGV) {
    // Any uses of DGV need to change to NewDA, with cast.
    DGV->replaceAllUsesWith(ConstantExpr::getBitCast(NewDA, DGV->getType()));
    DGV->eraseFromParent();
  }
  
  ValueMap[SGA] = NewDA;
  return false;
}


// linkGlobalInits - Update the initializers in the Dest module now that all
// globals that may be referenced are in Dest.
void ModuleLinker::linkGlobalInits() {
  // Loop over all of the globals in the src module, mapping them over as we go
  for (Module::const_global_iterator I = SrcM->global_begin(),
       E = SrcM->global_end(); I != E; ++I) {
    if (!I->hasInitializer()) continue;      // Only process initialized GV's.
    
    // Grab destination global variable.
    GlobalVariable *DGV = cast<GlobalVariable>(ValueMap[I]);
    // Figure out what the initializer looks like in the dest module.
    DGV->setInitializer(MapValue(I->getInitializer(), ValueMap,
                                 RF_None, &TypeMap));
  }
}

// linkFunctionBody - Copy the source function over into the dest function and
// fix up references to values.  At this point we know that Dest is an external
// function, and that Src is not.
void ModuleLinker::linkFunctionBody(Function *Dst, Function *Src) {
  assert(Src && Dst && Dst->isDeclaration() && !Src->isDeclaration());

  // Go through and convert function arguments over, remembering the mapping.
  Function::arg_iterator DI = Dst->arg_begin();
  for (Function::arg_iterator I = Src->arg_begin(), E = Src->arg_end();
       I != E; ++I, ++DI) {
    DI->setName(I->getName());  // Copy the name over.

    // Add a mapping to our mapping.
    ValueMap[I] = DI;
  }

  // Splice the body of the source function into the dest function.
  Dst->getBasicBlockList().splice(Dst->end(), Src->getBasicBlockList());

  // At this point, all of the instructions and values of the function are now
  // copied over.  The only problem is that they are still referencing values in
  // the Source function as operands.  Loop through all of the operands of the
  // functions and patch them up to point to the local versions.
  for (Function::iterator BB = Dst->begin(), BE = Dst->end(); BB != BE; ++BB)
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I)
      RemapInstruction(I, ValueMap, RF_IgnoreMissingEntries, &TypeMap);

  // There is no need to map the arguments anymore.
  for (Function::arg_iterator I = Src->arg_begin(), E = Src->arg_end();
       I != E; ++I)
    ValueMap.erase(I);
}


void ModuleLinker::linkAliasBodies() {
  for (Module::alias_iterator I = SrcM->alias_begin(), E = SrcM->alias_end();
       I != E; ++I)
    if (Constant *Aliasee = I->getAliasee()) {
      GlobalAlias *DA = cast<GlobalAlias>(ValueMap[I]);
      DA->setAliasee(MapValue(Aliasee, ValueMap, RF_None, &TypeMap));
    }
}

/// linkNamedMDNodes - Insert all of the named mdnodes in Src into the Dest
/// module.
void ModuleLinker::linkNamedMDNodes() {
  for (Module::const_named_metadata_iterator I = SrcM->named_metadata_begin(),
       E = SrcM->named_metadata_end(); I != E; ++I) {
    NamedMDNode *DestNMD = DstM->getOrInsertNamedMetadata(I->getName());
    // Add Src elements into Dest node.
    for (unsigned i = 0, e = I->getNumOperands(); i != e; ++i)
      DestNMD->addOperand(MapValue(I->getOperand(i), ValueMap,
                                   RF_None, &TypeMap));
  }
}
  
bool ModuleLinker::run() {
  assert(DstM && "Null Destination module");
  assert(SrcM && "Null Source Module");

  // Inherit the target data from the source module if the destination module
  // doesn't have one already.
  if (DstM->getDataLayout().empty() && !SrcM->getDataLayout().empty())
    DstM->setDataLayout(SrcM->getDataLayout());

  // Copy the target triple from the source to dest if the dest's is empty.
  if (DstM->getTargetTriple().empty() && !SrcM->getTargetTriple().empty())
    DstM->setTargetTriple(SrcM->getTargetTriple());

  if (!SrcM->getDataLayout().empty() && !DstM->getDataLayout().empty() &&
      SrcM->getDataLayout() != DstM->getDataLayout())
    errs() << "WARNING: Linking two modules of different data layouts!\n";
  if (!SrcM->getTargetTriple().empty() &&
      DstM->getTargetTriple() != SrcM->getTargetTriple()) {
    errs() << "WARNING: Linking two modules of different target triples: ";
    if (!SrcM->getModuleIdentifier().empty())
      errs() << SrcM->getModuleIdentifier() << ": ";
    errs() << "'" << SrcM->getTargetTriple() << "' and '" 
           << DstM->getTargetTriple() << "'\n";
  }

  // Append the module inline asm string.
  if (!SrcM->getModuleInlineAsm().empty()) {
    if (DstM->getModuleInlineAsm().empty())
      DstM->setModuleInlineAsm(SrcM->getModuleInlineAsm());
    else
      DstM->setModuleInlineAsm(DstM->getModuleInlineAsm()+"\n"+
                               SrcM->getModuleInlineAsm());
  }

  // Update the destination module's dependent libraries list with the libraries
  // from the source module. There's no opportunity for duplicates here as the
  // Module ensures that duplicate insertions are discarded.
  for (Module::lib_iterator SI = SrcM->lib_begin(), SE = SrcM->lib_end();
       SI != SE; ++SI)
    DstM->addLibrary(*SI);
  
  // If the source library's module id is in the dependent library list of the
  // destination library, remove it since that module is now linked in.
  StringRef ModuleId = SrcM->getModuleIdentifier();
  if (!ModuleId.empty())
    DstM->removeLibrary(sys::path::stem(ModuleId));

  
  // Loop over all of the linked values to compute type mappings.
  computeTypeMapping();

  // Insert all of the globals in src into the DstM module... without linking
  // initializers (which could refer to functions not yet mapped over).
  for (Module::global_iterator I = SrcM->global_begin(),
       E = SrcM->global_end(); I != E; ++I)
    if (linkGlobalProto(I))
      return true;

  // Link the functions together between the two modules, without doing function
  // bodies... this just adds external function prototypes to the DstM
  // function...  We do this so that when we begin processing function bodies,
  // all of the global values that may be referenced are available in our
  // ValueMap.
  for (Module::iterator I = SrcM->begin(), E = SrcM->end(); I != E; ++I)
    if (linkFunctionProto(I))
      return true;

  // If there were any aliases, link them now.
  for (Module::alias_iterator I = SrcM->alias_begin(),
       E = SrcM->alias_end(); I != E; ++I)
    if (linkAliasProto(I))
      return true;

  // Update the initializers in the DstM module now that all globals that may
  // be referenced are in DstM.
  linkGlobalInits();

  // Link in the function bodies that are defined in the source module into
  // DstM.
  for (Module::iterator SF = SrcM->begin(), E = SrcM->end(); SF != E; ++SF) {
    if (SF->isDeclaration()) continue;      // No body if function is external.
    
    linkFunctionBody(cast<Function>(ValueMap[SF]), SF);
  }

  // Resolve all uses of aliases with aliasees.
  linkAliasBodies();

  // Remap all of the named mdnoes in Src into the DstM module. We do this
  // after linking GlobalValues so that MDNodes that reference GlobalValues
  // are properly remapped.
  linkNamedMDNodes();

  // Now that all of the types from the source are used, resolve any structs
  // copied over to the dest that didn't exist there.
  TypeMap.linkDefinedTypeBodies();
  
  return false;
}

//===----------------------------------------------------------------------===//
// LinkModules entrypoint.
//===----------------------------------------------------------------------===//

// LinkModules - This function links two modules together, with the resulting
// left module modified to be the composite of the two input modules.  If an
// error occurs, true is returned and ErrorMsg (if not null) is set to indicate
// the problem.  Upon failure, the Dest module could be in a modified state, and
// shouldn't be relied on to be consistent.
bool Linker::LinkModules(Module *Dest, Module *Src, std::string *ErrorMsg) {
  ModuleLinker TheLinker(Dest, Src);
  if (TheLinker.run()) {
    if (ErrorMsg) *ErrorMsg = TheLinker.ErrorMsg;
    return true;
  }
  
  return false;
}
