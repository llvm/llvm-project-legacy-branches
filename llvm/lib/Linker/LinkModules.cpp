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
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ValueSymbolTable.h"
#include "llvm/Instructions.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Path.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/ADT/DenseMap.h"
#include <map>
using namespace llvm;

// Error - Simple wrapper function to conditionally assign to E and return true.
// This just makes error return conditions a little bit simpler...
static inline bool Error(std::string *E, const Twine &Message) {
  if (E) *E = Message.str();
  return true;
}

namespace {
class TypeMap {
  /// MappedTypes - This is a mapping from a source type to a destination type
  /// to use.
  DenseMap<Type*, Type*> MappedTypes;
public:
  
  /// addTypeMapping - Indicate that the specified type in the destination
  /// module is conceptually equivalent to the specified type in the source
  /// module.  This updates the type mapping for equivalent types, and returns
  /// false.  If there is a hard type conflict (maybe merging "int x" with
  /// "extern float x") this returns true.
  bool addTypeMapping(Type *DstTy, Type *SrcTy);
  
  //void mapTypes(Value *Dst, Value *Src);

private:
  bool addTypeMappingRec(Type *DstTy, Type *SrcTy);
};
}

bool TypeMap::addTypeMapping(Type *DstTy, Type *SrcTy) {
  if (DstTy == SrcTy) return false;       // If already equal, noop.

  if (Type *T = MappedTypes[SrcTy])
    return T != DstTy;
  
  return addTypeMappingRec(DstTy, SrcTy);
}

/// addTypeMappingRec - This is the implementation function for addTypeMapping,
/// which optimizes out the map lookup in the recursive walk.  
bool TypeMap::addTypeMappingRec(Type *DstTy, Type *SrcTy) {
  // Two types cannot be resolved together if they are of different primitive
  // type.  For example, we cannot resolve an int to a float.
  if (DstTy->getTypeID() != SrcTy->getTypeID()) return true;

  // Otherwise, resolve the used type used by this derived type...
  switch (DstTy->getTypeID()) {
  default:
    return true;
  case Type::StructTyID: {
    StructType *DstST = cast<StructType>(DstTy);
    StructType *SrcST = cast<StructType>(SrcTy);
    
    // If the destination type is opaque, then it should be resolved to the
    // input type.  If the source type is opaque, then it gets whatever the
    // destination type is.
    if (DstST->isOpaque() || SrcST->isOpaque())
      break;
    
    if (DstST->getNumContainedTypes() != SrcST->getNumContainedTypes() ||
        DstST->isPacked() != SrcST->isPacked())
      return true;
    
    // Otherwise, we speculatively assume that the structs can be merged, add an
    // entry to the type map so we don't infinitely recurse.
    MappedTypes[SrcST] = DstST;

    // Then call addTypeMapping on each entry (not "Rec") so that we get the
    // caching behavior of the map check for each element.
    for (unsigned i = 0, e = DstST->getNumContainedTypes(); i != e; ++i) {
      Type *SE = SrcST->getContainedType(i), *DE = DstST->getContainedType(i);
      if (SE != DE && addTypeMapping(DE, SE))
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

/// ForceRenaming - The LLVM SymbolTable class autorenames globals that conflict
/// in the symbol table.  This is good for all clients except for us.  Go
/// through the trouble to force this back.
static void ForceRenaming(GlobalValue *GV, const std::string &Name) {
  assert(GV->getName() != Name && "Can't force rename to self");
  ValueSymbolTable &ST = GV->getParent()->getValueSymbolTable();

  // If there is a conflict, rename the conflict.
  if (GlobalValue *ConflictGV = cast_or_null<GlobalValue>(ST.lookup(Name))) {
    assert(ConflictGV->hasLocalLinkage() &&
           "Not conflicting with a static global, should link instead!");
    GV->takeName(ConflictGV);
    ConflictGV->setName(Name);    // This will cause ConflictGV to get renamed
    assert(ConflictGV->getName() != Name && "ForceRenaming didn't work");
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
}

/// GetLinkageResult - This analyzes the two global values and determines what
/// the result will look like in the destination module.  In particular, it
/// computes the resultant linkage type, computes whether the global in the
/// source should be copied over to the destination (replacing the existing
/// one), and computes whether this linkage is an error or not. It also performs
/// visibility checks: we cannot link together two symbols with different
/// visibilities.
static bool GetLinkageResult(GlobalValue *Dest, const GlobalValue *Src,
                             GlobalValue::LinkageTypes &LT, bool &LinkFromSrc,
                             std::string *Err) {
  assert((!Dest || !Src->hasLocalLinkage()) &&
         "If Src has internal linkage, Dest shouldn't be set!");
  if (!Dest) {
    // Linking something to nothing.
    LinkFromSrc = true;
    LT = Src->getLinkage();
  } else if (Src->isDeclaration()) {
    // If Src is external or if both Src & Dest are external..  Just link the
    // external globals, we aren't adding anything.
    if (Src->hasDLLImportLinkage()) {
      // If one of GVs has DLLImport linkage, result should be dllimport'ed.
      if (Dest->isDeclaration()) {
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
  } else if (Dest->isDeclaration() && !Dest->hasDLLImportLinkage()) {
    // If Dest is external but Src is not:
    LinkFromSrc = true;
    LT = Src->getLinkage();
  } else if (Src->hasAppendingLinkage() || Dest->hasAppendingLinkage()) {
    if (Src->getLinkage() != Dest->getLinkage())
      return Error(Err, "Linking globals named '" + Src->getName() +
            "': can only link appending global with another appending global!");
    LinkFromSrc = true; // Special cased.
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
    return Error(Err, "Linking globals named '" + Src->getName() +
                 "': symbol multiply defined!");
  }

  // Check visibility
  if (Dest && Src->getVisibility() != Dest->getVisibility() &&
      !Src->isDeclaration() && !Dest->isDeclaration() &&
      !Src->hasAvailableExternallyLinkage() &&
      !Dest->hasAvailableExternallyLinkage())
      return Error(Err, "Linking globals named '" + Src->getName() +
                   "': symbols have different visibilities!");
  return false;
}

// Insert all of the named mdnoes in Src into the Dest module.
static void LinkNamedMDNodes(Module *Dest, Module *Src,
                             ValueToValueMapTy &ValueMap) {
  for (Module::const_named_metadata_iterator I = Src->named_metadata_begin(),
         E = Src->named_metadata_end(); I != E; ++I) {
    const NamedMDNode *SrcNMD = I;
    NamedMDNode *DestNMD = Dest->getOrInsertNamedMetadata(SrcNMD->getName());
    // Add Src elements into Dest node.
    for (unsigned i = 0, e = SrcNMD->getNumOperands(); i != e; ++i)
      DestNMD->addOperand(cast<MDNode>(MapValue(SrcNMD->getOperand(i),
                                                ValueMap)));
  }
}

// LinkGlobals - Loop through the global variables in the src module and merge
// them into the dest module.
static bool LinkGlobals(Module *Dest, const Module *Src,
                        ValueToValueMapTy &ValueMap, TypeMap &MappedTypes,
                    std::multimap<std::string, GlobalVariable *> &AppendingVars,
                        std::string *Err) {
  ValueSymbolTable &DestSymTab = Dest->getValueSymbolTable();

  // Loop over all of the globals in the src module, mapping them over as we go
  for (Module::const_global_iterator I = Src->global_begin(),
       E = Src->global_end(); I != E; ++I) {
    const GlobalVariable *SGV = I;
    GlobalValue *DGV = 0;

    // Check to see if may have to link the global with the global, alias or
    // function.
    if (SGV->hasName() && !SGV->hasLocalLinkage())
      DGV = cast_or_null<GlobalValue>(DestSymTab.lookup(SGV->getName()));

    // If we found a global with the same name in the dest module, but it has
    // internal linkage, we are really not doing any linkage here.
    if (DGV && DGV->hasLocalLinkage())
      DGV = 0;

    // If types don't agree due to opaque types, try to resolve them.
    if (DGV && DGV->getType() != SGV->getType())
      MappedTypes.addTypeMapping(DGV->getType(), SGV->getType());

    assert((SGV->hasInitializer() || SGV->hasExternalWeakLinkage() ||
            SGV->hasExternalLinkage() || SGV->hasDLLImportLinkage()) &&
           "Global must either be external or have an initializer!");

    GlobalValue::LinkageTypes NewLinkage = GlobalValue::InternalLinkage;
    bool LinkFromSrc = false;
    if (GetLinkageResult(DGV, SGV, NewLinkage, LinkFromSrc, Err))
      return true;

    if (DGV == 0) {
      // No linking to be performed, simply create an identical version of the
      // symbol over in the dest module... the initializer will be filled in
      // later by LinkGlobalInits.
      GlobalVariable *NewDGV =
        new GlobalVariable(*Dest, SGV->getType()->getElementType(),
                           SGV->isConstant(), SGV->getLinkage(), /*init*/0,
                           SGV->getName(), 0, false,
                           SGV->getType()->getAddressSpace());
      // Propagate alignment, visibility and section info.
      CopyGVAttributes(NewDGV, SGV);
      NewDGV->setUnnamedAddr(SGV->hasUnnamedAddr());

      // If the LLVM runtime renamed the global, but it is an externally visible
      // symbol, DGV must be an existing global with internal linkage.  Rename
      // it.
      if (!NewDGV->hasLocalLinkage() && NewDGV->getName() != SGV->getName())
        ForceRenaming(NewDGV, SGV->getName());

      // Make sure to remember this mapping.
      ValueMap[SGV] = NewDGV;

      // Keep track that this is an appending variable.
      if (SGV->hasAppendingLinkage())
        AppendingVars.insert(std::make_pair(SGV->getName(), NewDGV));
      continue;
    }

    bool HasUnnamedAddr = SGV->hasUnnamedAddr() && DGV->hasUnnamedAddr();

    // If the visibilities of the symbols disagree and the destination is a
    // prototype, take the visibility of its input.
    if (DGV->isDeclaration())
      DGV->setVisibility(SGV->getVisibility());

    if (DGV->hasAppendingLinkage()) {
      // No linking is performed yet.  Just insert a new copy of the global, and
      // keep track of the fact that it is an appending variable in the
      // AppendingVars map.  The name is cleared out so that no linkage is
      // performed.
      GlobalVariable *NewDGV =
        new GlobalVariable(*Dest, SGV->getType()->getElementType(),
                           SGV->isConstant(), SGV->getLinkage(), /*init*/0,
                           "", 0, false,
                           SGV->getType()->getAddressSpace());

      // Set alignment allowing CopyGVAttributes merge it with alignment of SGV.
      NewDGV->setAlignment(DGV->getAlignment());
      // Propagate alignment, section and visibility info.
      CopyGVAttributes(NewDGV, SGV);

      // Make sure to remember this mapping...
      ValueMap[SGV] = NewDGV;

      // Keep track that this is an appending variable...
      AppendingVars.insert(std::make_pair(SGV->getName(), NewDGV));
      continue;
    }

    if (LinkFromSrc) {
      if (isa<GlobalAlias>(DGV))
        return Error(Err, "Global-Alias Collision on '" + SGV->getName() +
                     "': symbol multiple defined");

      // If the types don't match, and if we are to link from the source, nuke
      // DGV and create a new one of the appropriate type.  Note that the thing
      // we are replacing may be a function (if a prototype, weak, etc) or a
      // global variable.
      GlobalVariable *NewDGV =
        new GlobalVariable(*Dest, SGV->getType()->getElementType(),
                           SGV->isConstant(), NewLinkage, /*init*/0,
                           DGV->getName(), 0, false,
                           SGV->getType()->getAddressSpace());

      // Set the unnamed_addr.
      NewDGV->setUnnamedAddr(HasUnnamedAddr);

      // Propagate alignment, section, and visibility info.
      CopyGVAttributes(NewDGV, SGV);
      DGV->replaceAllUsesWith(ConstantExpr::getBitCast(NewDGV,
                                                              DGV->getType()));

      // DGV will conflict with NewDGV because they both had the same
      // name. We must erase this now so ForceRenaming doesn't assert
      // because DGV might not have internal linkage.
      if (GlobalVariable *Var = dyn_cast<GlobalVariable>(DGV))
        Var->eraseFromParent();
      else
        cast<Function>(DGV)->eraseFromParent();

      // If the symbol table renamed the global, but it is an externally visible
      // symbol, DGV must be an existing global with internal linkage.  Rename.
      if (NewDGV->getName() != SGV->getName() && !NewDGV->hasLocalLinkage())
        ForceRenaming(NewDGV, SGV->getName());

      // Inherit const as appropriate.
      NewDGV->setConstant(SGV->isConstant());

      // Make sure to remember this mapping.
      ValueMap[SGV] = NewDGV;
      continue;
    }

    // Not "link from source", keep the one in the DestModule and remap the
    // input onto it.

    // Special case for const propagation.
    if (GlobalVariable *DGVar = dyn_cast<GlobalVariable>(DGV))
      if (DGVar->isDeclaration() && SGV->isConstant() && !DGVar->isConstant())
        DGVar->setConstant(true);

    // SGV is global, but DGV is alias.
    if (isa<GlobalAlias>(DGV)) {
      // The only valid mappings are:
      // - SGV is external declaration, which is effectively a no-op.
      // - SGV is weak, when we just need to throw SGV out.
      if (!SGV->isDeclaration() && !SGV->isWeakForLinker())
        return Error(Err, "Global-Alias Collision on '" + SGV->getName() +
                     "': symbol multiple defined");
    }

    // Set calculated linkage and unnamed_addr
    DGV->setLinkage(NewLinkage);
    DGV->setUnnamedAddr(HasUnnamedAddr);

    // Make sure to remember this mapping...
    ValueMap[SGV] = ConstantExpr::getBitCast(DGV, SGV->getType());
  }
  return false;
}

static GlobalValue::LinkageTypes
CalculateAliasLinkage(const GlobalValue *SGV, const GlobalValue *DGV) {
  GlobalValue::LinkageTypes SL = SGV->getLinkage();
  GlobalValue::LinkageTypes DL = DGV->getLinkage();
  if (SL == GlobalValue::ExternalLinkage || DL == GlobalValue::ExternalLinkage)
    return GlobalValue::ExternalLinkage;
  else if (SL == GlobalValue::WeakAnyLinkage ||
           DL == GlobalValue::WeakAnyLinkage)
    return GlobalValue::WeakAnyLinkage;
  else if (SL == GlobalValue::WeakODRLinkage ||
           DL == GlobalValue::WeakODRLinkage)
    return GlobalValue::WeakODRLinkage;
  else if (SL == GlobalValue::InternalLinkage &&
           DL == GlobalValue::InternalLinkage)
    return GlobalValue::InternalLinkage;
  else if (SL == GlobalValue::LinkerPrivateLinkage &&
           DL == GlobalValue::LinkerPrivateLinkage)
    return GlobalValue::LinkerPrivateLinkage;
  else if (SL == GlobalValue::LinkerPrivateWeakLinkage &&
           DL == GlobalValue::LinkerPrivateWeakLinkage)
    return GlobalValue::LinkerPrivateWeakLinkage;
  else if (SL == GlobalValue::LinkerPrivateWeakDefAutoLinkage &&
           DL == GlobalValue::LinkerPrivateWeakDefAutoLinkage)
    return GlobalValue::LinkerPrivateWeakDefAutoLinkage;
  else {
    assert (SL == GlobalValue::PrivateLinkage &&
            DL == GlobalValue::PrivateLinkage && "Unexpected linkage type");
    return GlobalValue::PrivateLinkage;
  }
}

/// LinkAliases - Loop through the aliases in the src module and link them into
/// the dest module. We're assuming that all functions/global variables were
/// already linked in.
static bool LinkAliases(Module *Dest, const Module *Src,
                        ValueToValueMapTy &ValueMap, TypeMap &MappedTypes,
                        std::string *Err) {
  // Loop over all alias in the src module
  for (Module::const_alias_iterator I = Src->alias_begin(),
         E = Src->alias_end(); I != E; ++I) {
    const GlobalAlias *SGA = I;
    const GlobalValue *SAliasee = SGA->getAliasedGlobal();
    GlobalAlias *NewGA = NULL;

    // Globals were already linked, thus we can just query ValueMap for variant
    // of SAliasee in Dest.
    ValueToValueMapTy::const_iterator VMI = ValueMap.find(SAliasee);
    assert(VMI != ValueMap.end() && "Aliasee not linked");
    GlobalValue *DAliasee = cast<GlobalValue>(VMI->second);
    GlobalValue *DGV = NULL;

    // Fixup aliases to bitcasts.  Note that aliases to GEPs are still broken
    // by this, but aliases to GEPs are broken to a lot of other things, so
    // it's less important.
    Constant *DAliaseeConst = DAliasee;
    if (SGA->getType() != DAliasee->getType())
      DAliaseeConst = ConstantExpr::getBitCast(DAliasee, SGA->getType());

    // Try to find something 'similar' to SGA in destination module.
    if (!DGV && !SGA->hasLocalLinkage()) {
      DGV = Dest->getNamedAlias(SGA->getName());

      // If types don't agree due to opaque types, try to resolve them.
      if (DGV && DGV->getType() != SGA->getType())
        MappedTypes.addTypeMapping(DGV->getType(), SGA->getType());
    }

    if (!DGV && !SGA->hasLocalLinkage()) {
      DGV = Dest->getGlobalVariable(SGA->getName());

      // If types don't agree due to opaque types, try to resolve them.
      if (DGV && DGV->getType() != SGA->getType())
        MappedTypes.addTypeMapping(DGV->getType(), SGA->getType());
    }

    if (!DGV && !SGA->hasLocalLinkage()) {
      DGV = Dest->getFunction(SGA->getName());

      // If types don't agree due to opaque types, try to resolve them.
      if (DGV && DGV->getType() != SGA->getType())
        MappedTypes.addTypeMapping(DGV->getType(), SGA->getType());
    }

    // No linking to be performed on internal stuff.
    if (DGV && DGV->hasLocalLinkage())
      DGV = NULL;

    if (GlobalAlias *DGA = dyn_cast_or_null<GlobalAlias>(DGV)) {
      // Types are known to be the same, check whether aliasees equal. As
      // globals are already linked we just need query ValueMap to find the
      // mapping.
      if (DAliasee == DGA->getAliasedGlobal()) {
        // This is just two copies of the same alias. Propagate linkage, if
        // necessary.
        DGA->setLinkage(CalculateAliasLinkage(SGA, DGA));

        NewGA = DGA;
        // Proceed to 'common' steps
      } else
        return Error(Err, "Alias Collision on '"  + SGA->getName()+
                     "': aliases have different aliasees");
    } else if (GlobalVariable *DGVar = dyn_cast_or_null<GlobalVariable>(DGV)) {
      // The only allowed way is to link alias with external declaration or weak
      // symbol..
      if (DGVar->isDeclaration() || DGVar->isWeakForLinker()) {
        // But only if aliasee is global too...
        if (!isa<GlobalVariable>(DAliasee))
          return Error(Err, "Global-Alias Collision on '" + SGA->getName() +
                       "': aliasee is not global variable");

        NewGA = new GlobalAlias(SGA->getType(), SGA->getLinkage(),
                                SGA->getName(), DAliaseeConst, Dest);
        CopyGVAttributes(NewGA, SGA);

        // Any uses of DGV need to change to NewGA, with cast, if needed.
        if (SGA->getType() != DGVar->getType())
          DGVar->replaceAllUsesWith(ConstantExpr::getBitCast(NewGA,
                                                             DGVar->getType()));
        else
          DGVar->replaceAllUsesWith(NewGA);

        // DGVar will conflict with NewGA because they both had the same
        // name. We must erase this now so ForceRenaming doesn't assert
        // because DGV might not have internal linkage.
        DGVar->eraseFromParent();

        // Proceed to 'common' steps
      } else
        return Error(Err, "Global-Alias Collision on '" + SGA->getName() +
                     "': symbol multiple defined");
    } else if (Function *DF = dyn_cast_or_null<Function>(DGV)) {
      // The only allowed way is to link alias with external declaration or weak
      // symbol...
      if (DF->isDeclaration() || DF->isWeakForLinker()) {
        // But only if aliasee is function too.
        if (!isa<Function>(DAliasee))
          return Error(Err, "Function-Alias Collision on '" + SGA->getName() +
                       "': aliasee is not function");

        NewGA = new GlobalAlias(SGA->getType(), SGA->getLinkage(),
                                SGA->getName(), DAliaseeConst, Dest);
        CopyGVAttributes(NewGA, SGA);

        // Any uses of DF need to change to NewGA, with cast, if needed.
        if (SGA->getType() != DF->getType())
          DF->replaceAllUsesWith(ConstantExpr::getBitCast(NewGA,
                                                          DF->getType()));
        else
          DF->replaceAllUsesWith(NewGA);

        // DF will conflict with NewGA because they both had the same
        // name. We must erase this now so ForceRenaming doesn't assert
        // because DF might not have internal linkage.
        DF->eraseFromParent();

        // Proceed to 'common' steps
      } else
        return Error(Err, "Function-Alias Collision on '" + SGA->getName() +
                     "': symbol multiple defined");
    } else {
      // No linking to be performed, simply create an identical version of the
      // alias over in the dest module...
      NewGA = new GlobalAlias(SGA->getType(), SGA->getLinkage(),
                              SGA->getName(), DAliaseeConst, Dest);
      CopyGVAttributes(NewGA, SGA);

      // Proceed to 'common' steps
    }

    assert(NewGA && "No alias was created in destination module!");

    // If the symbol table renamed the alias, but it is an externally visible
    // symbol, DGA must be an global value with internal linkage. Rename it.
    if (NewGA->getName() != SGA->getName() &&
        !NewGA->hasLocalLinkage())
      ForceRenaming(NewGA, SGA->getName());

    // Remember this mapping so uses in the source module get remapped
    // later by MapValue.
    ValueMap[SGA] = NewGA;
  }

  return false;
}


// LinkGlobalInits - Update the initializers in the Dest module now that all
// globals that may be referenced are in Dest.
static void LinkGlobalInits(Module *Dest, const Module *Src,
                            ValueToValueMapTy &ValueMap) {
  // Loop over all of the globals in the src module, mapping them over as we go
  for (Module::const_global_iterator I = Src->global_begin(),
       E = Src->global_end(); I != E; ++I) {
    const GlobalVariable *SGV = I;

    if (SGV->hasInitializer()) {      // Only process initialized GV's
      // Figure out what the initializer looks like in the dest module.
      Constant *SInit =
        cast<Constant>(MapValue(SGV->getInitializer(), ValueMap));
      // Grab destination global variable or alias.
      GlobalValue *DGV = cast<GlobalValue>(ValueMap[SGV]->stripPointerCasts());

      // If dest if global variable, check that initializers match.
      if (GlobalVariable *DGVar = dyn_cast<GlobalVariable>(DGV)) {
        if (!DGVar->hasInitializer())
          // Copy the initializer over now.
          DGVar->setInitializer(SInit);
      } else {
        // Destination is alias, the only valid situation is when source is
        // weak. Also, note, that we already checked linkage in LinkGlobals(),
        // thus we assert here.
        // FIXME: Should we weaken this assumption, 'dereference' alias and
        // check for initializer of aliasee?
        assert(SGV->isWeakForLinker());
      }
    }
  }
}

// LinkFunctionProtos - Link the functions together between the two modules,
// without doing function bodies... this just adds external function prototypes
// to the Dest function.
//
static bool LinkFunctionProtos(Module *Dest, const Module *Src,
                               ValueToValueMapTy &ValueMap,TypeMap &MappedTypes,
                               std::string *Err) {
  ValueSymbolTable &DestSymTab = Dest->getValueSymbolTable();

  // Loop over all of the functions in the src module, mapping them over
  for (Module::const_iterator I = Src->begin(), E = Src->end(); I != E; ++I) {
    const Function *SF = I;   // SrcFunction
    GlobalValue *DGV = 0;

    // Check to see if may have to link the function with the global, alias or
    // function.
    if (SF->hasName() && !SF->hasLocalLinkage())
      DGV = cast_or_null<GlobalValue>(DestSymTab.lookup(SF->getName()));

    // If we found a global with the same name in the dest module, but it has
    // internal linkage, we are really not doing any linkage here.
    if (DGV && DGV->hasLocalLinkage())
      DGV = 0;

    // If types don't agree due to opaque types, try to resolve them.
    if (DGV && DGV->getType() != SF->getType())
      MappedTypes.addTypeMapping(DGV->getType(), SF->getType());

    GlobalValue::LinkageTypes NewLinkage = GlobalValue::InternalLinkage;
    bool LinkFromSrc = false;
    if (GetLinkageResult(DGV, SF, NewLinkage, LinkFromSrc, Err))
      return true;

    // If there is no linkage to be performed, just bring over SF without
    // modifying it.
    if (DGV == 0) {
      // Function does not already exist, simply insert an function signature
      // identical to SF into the dest module.
      Function *NewDF = Function::Create(SF->getFunctionType(),
                                         SF->getLinkage(),
                                         SF->getName(), Dest);
      CopyGVAttributes(NewDF, SF);

      // If the LLVM runtime renamed the function, but it is an externally
      // visible symbol, DF must be an existing function with internal linkage.
      // Rename it.
      if (!NewDF->hasLocalLinkage() && NewDF->getName() != SF->getName())
        ForceRenaming(NewDF, SF->getName());

      // ... and remember this mapping...
      ValueMap[SF] = NewDF;
      continue;
    }

    // If the visibilities of the symbols disagree and the destination is a
    // prototype, take the visibility of its input.
    if (DGV->isDeclaration())
      DGV->setVisibility(SF->getVisibility());

    if (LinkFromSrc) {
      if (isa<GlobalAlias>(DGV))
        return Error(Err, "Function-Alias Collision on '" + SF->getName() +
                     "': symbol multiple defined");

      // We have a definition of the same name but different type in the
      // source module. Copy the prototype to the destination and replace
      // uses of the destination's prototype with the new prototype.
      Function *NewDF = Function::Create(SF->getFunctionType(), NewLinkage,
                                         SF->getName(), Dest);
      CopyGVAttributes(NewDF, SF);

      // Any uses of DF need to change to NewDF, with cast
      DGV->replaceAllUsesWith(ConstantExpr::getBitCast(NewDF,
                                                              DGV->getType()));

      // DF will conflict with NewDF because they both had the same. We must
      // erase this now so ForceRenaming doesn't assert because DF might
      // not have internal linkage.
      if (GlobalVariable *Var = dyn_cast<GlobalVariable>(DGV))
        Var->eraseFromParent();
      else
        cast<Function>(DGV)->eraseFromParent();

      // If the symbol table renamed the function, but it is an externally
      // visible symbol, DF must be an existing function with internal
      // linkage.  Rename it.
      if (NewDF->getName() != SF->getName() && !NewDF->hasLocalLinkage())
        ForceRenaming(NewDF, SF->getName());

      // Remember this mapping so uses in the source module get remapped
      // later by MapValue.
      ValueMap[SF] = NewDF;
      continue;
    }

    // Not "link from source", keep the one in the DestModule and remap the
    // input onto it.

    if (isa<GlobalAlias>(DGV)) {
      // The only valid mappings are:
      // - SF is external declaration, which is effectively a no-op.
      // - SF is weak, when we just need to throw SF out.
      if (!SF->isDeclaration() && !SF->isWeakForLinker())
        return Error(Err, "Function-Alias Collision on '" + SF->getName() +
                     "': symbol multiple defined");
    }

    // Set calculated linkage
    DGV->setLinkage(NewLinkage);

    // Make sure to remember this mapping.
    ValueMap[SF] = ConstantExpr::getBitCast(DGV, SF->getType());
  }
  return false;
}

// LinkFunctionBody - Copy the source function over into the dest function and
// fix up references to values.  At this point we know that Dest is an external
// function, and that Src is not.
static void LinkFunctionBody(Function *Dest, Function *Src,
                             ValueToValueMapTy &ValueMap) {
  assert(Src && Dest && Dest->isDeclaration() && !Src->isDeclaration());

  // Go through and convert function arguments over, remembering the mapping.
  Function::arg_iterator DI = Dest->arg_begin();
  for (Function::arg_iterator I = Src->arg_begin(), E = Src->arg_end();
       I != E; ++I, ++DI) {
    DI->setName(I->getName());  // Copy the name information over...

    // Add a mapping to our local map
    ValueMap[I] = DI;
  }

  // Splice the body of the source function into the dest function.
  Dest->getBasicBlockList().splice(Dest->end(), Src->getBasicBlockList());

  // At this point, all of the instructions and values of the function are now
  // copied over.  The only problem is that they are still referencing values in
  // the Source function as operands.  Loop through all of the operands of the
  // functions and patch them up to point to the local versions.
  for (Function::iterator BB = Dest->begin(), BE = Dest->end(); BB != BE; ++BB)
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I)
      RemapInstruction(I, ValueMap, RF_IgnoreMissingEntries);

  // There is no need to map the arguments anymore.
  for (Function::arg_iterator I = Src->arg_begin(), E = Src->arg_end();
       I != E; ++I)
    ValueMap.erase(I);
}


// LinkFunctionBodies - Link in the function bodies that are defined in the
// source module into the DestModule.  This consists basically of copying the
// function over and fixing up references to values.
static void LinkFunctionBodies(Module *Dest, Module *Src,
                               ValueToValueMapTy &ValueMap) {

  // Loop over all of the functions in the src module, mapping them over as we
  // go.
  for (Module::iterator SF = Src->begin(), E = Src->end(); SF != E; ++SF) {
    if (!SF->isDeclaration()) {               // No body if function is external
      Function *DF = dyn_cast<Function>(ValueMap[SF]); // Destination function

      // Only provide the function body if there isn't one already.
      if (DF && DF->isDeclaration())
        LinkFunctionBody(DF, SF, ValueMap);
    }
  }
}

// LinkAppendingVars - If there were any appending global variables, link them
// together now.  Return true on error.
static bool LinkAppendingVars(Module *M,
                  std::multimap<std::string, GlobalVariable *> &AppendingVars,
                              std::string *ErrorMsg) {
  if (AppendingVars.empty()) return false; // Nothing to do.

  // Loop over the multimap of appending vars, processing any variables with the
  // same name, forming a new appending global variable with both of the
  // initializers merged together, then rewrite references to the old variables
  // and delete them.
  std::vector<Constant*> Inits;
  while (AppendingVars.size() > 1) {
    // Get the first two elements in the map.
    std::multimap<std::string,
      GlobalVariable*>::iterator Second = AppendingVars.begin(), First=Second++;

    // If the first two elements are for different names, there is no pair.
    // Otherwise there is a pair, so link them together...
    if (First->first == Second->first) {
      GlobalVariable *G1 = First->second, *G2 = Second->second;
      const ArrayType *T1 = cast<ArrayType>(G1->getType()->getElementType());
      const ArrayType *T2 = cast<ArrayType>(G2->getType()->getElementType());

      // Check to see that they two arrays agree on type.
      if (T1->getElementType() != T2->getElementType())
        return Error(ErrorMsg,
         "Appending variables with different element types need to be linked!");
      if (G1->isConstant() != G2->isConstant())
        return Error(ErrorMsg,
                     "Appending variables linked with different const'ness!");

      if (G1->getAlignment() != G2->getAlignment())
        return Error(ErrorMsg,
         "Appending variables with different alignment need to be linked!");

      if (G1->getVisibility() != G2->getVisibility())
        return Error(ErrorMsg,
         "Appending variables with different visibility need to be linked!");

      if (G1->getSection() != G2->getSection())
        return Error(ErrorMsg,
         "Appending variables with different section name need to be linked!");

      unsigned NewSize = T1->getNumElements() + T2->getNumElements();
      ArrayType *NewType = ArrayType::get(T1->getElementType(),
                                                         NewSize);

      G1->setName("");   // Clear G1's name in case of a conflict!

      // Create the new global variable.
      GlobalVariable *NG =
        new GlobalVariable(*M, NewType, G1->isConstant(), G1->getLinkage(),
                           /*init*/0, First->first, 0, G1->isThreadLocal(),
                           G1->getType()->getAddressSpace());

      // Propagate alignment, visibility and section info.
      CopyGVAttributes(NG, G1);

      // Merge the initializer.
      Inits.reserve(NewSize);
      if (ConstantArray *I = dyn_cast<ConstantArray>(G1->getInitializer())) {
        for (unsigned i = 0, e = T1->getNumElements(); i != e; ++i)
          Inits.push_back(I->getOperand(i));
      } else {
        assert(isa<ConstantAggregateZero>(G1->getInitializer()));
        Constant *CV = Constant::getNullValue(T1->getElementType());
        for (unsigned i = 0, e = T1->getNumElements(); i != e; ++i)
          Inits.push_back(CV);
      }
      if (ConstantArray *I = dyn_cast<ConstantArray>(G2->getInitializer())) {
        for (unsigned i = 0, e = T2->getNumElements(); i != e; ++i)
          Inits.push_back(I->getOperand(i));
      } else {
        assert(isa<ConstantAggregateZero>(G2->getInitializer()));
        Constant *CV = Constant::getNullValue(T2->getElementType());
        for (unsigned i = 0, e = T2->getNumElements(); i != e; ++i)
          Inits.push_back(CV);
      }
      NG->setInitializer(ConstantArray::get(NewType, Inits));
      Inits.clear();

      // Replace any uses of the two global variables with uses of the new
      // global.
      G1->replaceAllUsesWith(ConstantExpr::getBitCast(NG,
                             G1->getType()));
      G2->replaceAllUsesWith(ConstantExpr::getBitCast(NG,
                             G2->getType()));

      // Remove the two globals from the module now.
      M->getGlobalList().erase(G1);
      M->getGlobalList().erase(G2);

      // Put the new global into the AppendingVars map so that we can handle
      // linking of more than two vars.
      Second->second = NG;
    }
    AppendingVars.erase(First);
  }

  return false;
}

static void ResolveAliases(Module *Dest) {
  for (Module::alias_iterator I = Dest->alias_begin(), E = Dest->alias_end();
       I != E; ++I)
    // We can't sue resolveGlobalAlias here because we need to preserve
    // bitcasts and GEPs.
    if (const Constant *C = I->getAliasee()) {
      while (dyn_cast<GlobalAlias>(C))
        C = cast<GlobalAlias>(C)->getAliasee();
      const GlobalValue *GV = dyn_cast<GlobalValue>(C);
      if (C != I && !(GV && GV->isDeclaration()))
        I->replaceAllUsesWith(const_cast<Constant*>(C));
    }
}

// LinkModules - This function links two modules together, with the resulting
// left module modified to be the composite of the two input modules.  If an
// error occurs, true is returned and ErrorMsg (if not null) is set to indicate
// the problem.  Upon failure, the Dest module could be in a modified state, and
// shouldn't be relied on to be consistent.
bool
Linker::LinkModules(Module *Dest, Module *Src, std::string *ErrorMsg) {
  assert(Dest != 0 && "Invalid Destination module");
  assert(Src  != 0 && "Invalid Source Module");

  if (Dest->getDataLayout().empty()) {
    if (!Src->getDataLayout().empty()) {
      Dest->setDataLayout(Src->getDataLayout());
    } else {
      std::string DataLayout;

      if (Dest->getEndianness() == Module::AnyEndianness) {
        if (Src->getEndianness() == Module::BigEndian)
          DataLayout.append("E");
        else if (Src->getEndianness() == Module::LittleEndian)
          DataLayout.append("e");
      }

      if (Dest->getPointerSize() == Module::AnyPointerSize) {
        if (Src->getPointerSize() == Module::Pointer64)
          DataLayout.append(DataLayout.length() == 0 ? "p:64:64" : "-p:64:64");
        else if (Src->getPointerSize() == Module::Pointer32)
          DataLayout.append(DataLayout.length() == 0 ? "p:32:32" : "-p:32:32");
      }
      Dest->setDataLayout(DataLayout);
    }
  }

  // Copy the target triple from the source to dest if the dest's is empty.
  if (Dest->getTargetTriple().empty() && !Src->getTargetTriple().empty())
    Dest->setTargetTriple(Src->getTargetTriple());

  if (!Src->getDataLayout().empty() && !Dest->getDataLayout().empty() &&
      Src->getDataLayout() != Dest->getDataLayout())
    errs() << "WARNING: Linking two modules of different data layouts!\n";
  if (!Src->getTargetTriple().empty() &&
      Dest->getTargetTriple() != Src->getTargetTriple()) {
    errs() << "WARNING: Linking two modules of different target triples: ";
    if (!Src->getModuleIdentifier().empty())
      errs() << Src->getModuleIdentifier() << ": ";
    errs() << "'" << Src->getTargetTriple() << "' and '" 
           << Dest->getTargetTriple() << "'\n";
  }

  // Append the module inline asm string.
  if (!Src->getModuleInlineAsm().empty()) {
    if (Dest->getModuleInlineAsm().empty())
      Dest->setModuleInlineAsm(Src->getModuleInlineAsm());
    else
      Dest->setModuleInlineAsm(Dest->getModuleInlineAsm()+"\n"+
                               Src->getModuleInlineAsm());
  }

  // Update the destination module's dependent libraries list with the libraries
  // from the source module. There's no opportunity for duplicates here as the
  // Module ensures that duplicate insertions are discarded.
  for (Module::lib_iterator SI = Src->lib_begin(), SE = Src->lib_end();
       SI != SE; ++SI)
    Dest->addLibrary(*SI);

  // ValueMap - Mapping of values from what they used to be in Src, to what they
  // are now in Dest.  ValueToValueMapTy is a ValueMap, which involves some
  // overhead due to the use of Value handles which the Linker doesn't actually
  // need, but this allows us to reuse the ValueMapper code.
  ValueToValueMapTy ValueMap;

  // AppendingVars - Keep track of global variables in the destination module
  // with appending linkage.  After the module is linked together, they are
  // appended and the module is rewritten.
  std::multimap<std::string, GlobalVariable *> AppendingVars;
  for (Module::global_iterator I = Dest->global_begin(), E = Dest->global_end();
       I != E; ++I) {
    // Add all of the appending globals already in the Dest module to
    // AppendingVars.
    if (I->hasAppendingLinkage())
      AppendingVars.insert(std::make_pair(I->getName(), I));
  }

  TypeMap TheTypeMap;
  
  // Insert all of the globals in src into the Dest module... without linking
  // initializers (which could refer to functions not yet mapped over).
  if (LinkGlobals(Dest, Src, ValueMap, TheTypeMap, AppendingVars, ErrorMsg))
    return true;

  // Link the functions together between the two modules, without doing function
  // bodies... this just adds external function prototypes to the Dest
  // function...  We do this so that when we begin processing function bodies,
  // all of the global values that may be referenced are available in our
  // ValueMap.
  if (LinkFunctionProtos(Dest, Src, ValueMap, TheTypeMap, ErrorMsg))
    return true;

  // If there were any aliases, link them now. We really need to do this now,
  // because all of the aliases that may be referenced need to be available in
  // ValueMap
  if (LinkAliases(Dest, Src, ValueMap, TheTypeMap, ErrorMsg)) return true;

  // Update the initializers in the Dest module now that all globals that may
  // be referenced are in Dest.
  LinkGlobalInits(Dest, Src, ValueMap);

  // Link in the function bodies that are defined in the source module into the
  // DestModule.  This consists basically of copying the function over and
  // fixing up references to values.
  LinkFunctionBodies(Dest, Src, ValueMap);

  // If there were any appending global variables, link them together now.
  if (LinkAppendingVars(Dest, AppendingVars, ErrorMsg)) return true;

  // Resolve all uses of aliases with aliasees.
  ResolveAliases(Dest);

  // Remap all of the named mdnoes in Src into the Dest module. We do this
  // after linking GlobalValues so that MDNodes that reference GlobalValues
  // are properly remapped.
  LinkNamedMDNodes(Dest, Src, ValueMap);

  // If the source library's module id is in the dependent library list of the
  // destination library, remove it since that module is now linked in.
  const std::string &modId = Src->getModuleIdentifier();
  if (!modId.empty())
    Dest->removeLibrary(sys::path::stem(modId));

  return false;
}

// vim: sw=2
