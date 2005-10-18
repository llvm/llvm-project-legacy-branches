
/*  A Bison parser, made from /Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse llvmAsmparse
#define yylex llvmAsmlex
#define yyerror llvmAsmerror
#define yylval llvmAsmlval
#define yychar llvmAsmchar
#define yydebug llvmAsmdebug
#define yynerrs llvmAsmnerrs
#define	ESINT64VAL	257
#define	EUINT64VAL	258
#define	SINTVAL	259
#define	UINTVAL	260
#define	FPVAL	261
#define	VOID	262
#define	BOOL	263
#define	SBYTE	264
#define	UBYTE	265
#define	SHORT	266
#define	USHORT	267
#define	INT	268
#define	UINT	269
#define	LONG	270
#define	ULONG	271
#define	FLOAT	272
#define	DOUBLE	273
#define	TYPE	274
#define	LABEL	275
#define	VAR_ID	276
#define	LABELSTR	277
#define	STRINGCONSTANT	278
#define	IMPLEMENTATION	279
#define	ZEROINITIALIZER	280
#define	TRUETOK	281
#define	FALSETOK	282
#define	BEGINTOK	283
#define	ENDTOK	284
#define	DECLARE	285
#define	GLOBAL	286
#define	CONSTANT	287
#define	VOLATILE	288
#define	FIXED	289
#define	TO	290
#define	DOTDOTDOT	291
#define	NULL_TOK	292
#define	UNDEF	293
#define	CONST	294
#define	INTERNAL	295
#define	LINKONCE	296
#define	WEAK	297
#define	APPENDING	298
#define	OPAQUE	299
#define	NOT	300
#define	EXTERNAL	301
#define	TARGET	302
#define	TRIPLE	303
#define	ENDIAN	304
#define	POINTERSIZE	305
#define	LITTLE	306
#define	BIG	307
#define	DEPLIBS	308
#define	CALL	309
#define	TAIL	310
#define	CC_TOK	311
#define	CCC_TOK	312
#define	FASTCC_TOK	313
#define	COLDCC_TOK	314
#define	VECTOR	315
#define	OF	316
#define	RET	317
#define	BR	318
#define	SWITCH	319
#define	INVOKE	320
#define	UNWIND	321
#define	UNREACHABLE	322
#define	ADD	323
#define	SUB	324
#define	MUL	325
#define	DIV	326
#define	REM	327
#define	AND	328
#define	OR	329
#define	XOR	330
#define	SETLE	331
#define	SETGE	332
#define	SETLT	333
#define	SETGT	334
#define	SETEQ	335
#define	SETNE	336
#define	VSETLE	337
#define	VSETGE	338
#define	VSETLT	339
#define	VSETGT	340
#define	VSETEQ	341
#define	VSETNE	342
#define	MALLOC	343
#define	ALLOCA	344
#define	FREE	345
#define	LOAD	346
#define	STORE	347
#define	GETELEMENTPTR	348
#define	PHI_TOK	349
#define	CAST	350
#define	SELECT	351
#define	VSELECT	352
#define	SHL	353
#define	SHR	354
#define	VAARG	355
#define	VGATHER	356
#define	VIMM	357
#define	VSCATTER	358
#define	EXTRACT	359
#define	EXTRACTELEMENT	360
#define	COMBINE	361
#define	COMBINEELEMENT	362
#define	VAARG_old	363
#define	VANEXT_old	364

#line 14 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"

#include "ParserInternals.h"
#include "llvm/CallingConv.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/SymbolTable.h"
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/ADT/STLExtras.h"
#include <algorithm>
#include <iostream>
#include <list>
#include <utility>

int yyerror(const char *ErrorMsg); // Forward declarations to prevent "implicit
int yylex();                       // declaration" of xxx warnings.
int yyparse();

namespace llvm {
  std::string CurFilename;
}
using namespace llvm;

static Module *ParserResult;

// DEBUG_UPREFS - Define this symbol if you want to enable debugging output
// relating to upreferences in the input stream.
//
//#define DEBUG_UPREFS 1
#ifdef DEBUG_UPREFS
#define UR_OUT(X) std::cerr << X
#else
#define UR_OUT(X)
#endif

#define YYERROR_VERBOSE 1

static bool ObsoleteVarArgs;
static bool NewVarArgs;
static BasicBlock* CurBB;


// This contains info used when building the body of a function.  It is
// destroyed when the function is completed.
//
typedef std::vector<Value *> ValueList;           // Numbered defs
static void 
ResolveDefinitions(std::map<const Type *,ValueList> &LateResolvers,
                   std::map<const Type *,ValueList> *FutureLateResolvers = 0);

static struct PerModuleInfo {
  Module *CurrentModule;
  std::map<const Type *, ValueList> Values; // Module level numbered definitions
  std::map<const Type *,ValueList> LateResolveValues;
  std::vector<PATypeHolder>    Types;
  std::map<ValID, PATypeHolder> LateResolveTypes;

  /// PlaceHolderInfo - When temporary placeholder objects are created, remember
  /// how they were referenced and one which line of the input they came from so
  /// that we can resolve them later and print error messages as appropriate.
  std::map<Value*, std::pair<ValID, int> > PlaceHolderInfo;

  // GlobalRefs - This maintains a mapping between <Type, ValID>'s and forward
  // references to global values.  Global values may be referenced before they
  // are defined, and if so, the temporary object that they represent is held
  // here.  This is used for forward references of GlobalValues.
  //
  typedef std::map<std::pair<const PointerType *,
                             ValID>, GlobalValue*> GlobalRefsType;
  GlobalRefsType GlobalRefs;

  void ModuleDone() {
    // If we could not resolve some functions at function compilation time
    // (calls to functions before they are defined), resolve them now...  Types
    // are resolved when the constant pool has been completely parsed.
    //
    ResolveDefinitions(LateResolveValues);

    // Check to make sure that all global value forward references have been
    // resolved!
    //
    if (!GlobalRefs.empty()) {
      std::string UndefinedReferences = "Unresolved global references exist:\n";

      for (GlobalRefsType::iterator I = GlobalRefs.begin(), E =GlobalRefs.end();
           I != E; ++I) {
        UndefinedReferences += "  " + I->first.first->getDescription() + " " +
                               I->first.second.getName() + "\n";
      }
      ThrowException(UndefinedReferences);
    }

    Values.clear();         // Clear out function local definitions
    Types.clear();
    CurrentModule = 0;
  }


  // GetForwardRefForGlobal - Check to see if there is a forward reference
  // for this global.  If so, remove it from the GlobalRefs map and return it.
  // If not, just return null.
  GlobalValue *GetForwardRefForGlobal(const PointerType *PTy, ValID ID) {
    // Check to see if there is a forward reference to this global variable...
    // if there is, eliminate it and patch the reference to use the new def'n.
    GlobalRefsType::iterator I = GlobalRefs.find(std::make_pair(PTy, ID));
    GlobalValue *Ret = 0;
    if (I != GlobalRefs.end()) {
      Ret = I->second;
      GlobalRefs.erase(I);
    }
    return Ret;
  }
} CurModule;

static struct PerFunctionInfo {
  Function *CurrentFunction;     // Pointer to current function being created

  std::map<const Type*, ValueList> Values;   // Keep track of #'d definitions
  std::map<const Type*, ValueList> LateResolveValues;
  bool isDeclare;                // Is this function a forward declararation?

  /// BBForwardRefs - When we see forward references to basic blocks, keep
  /// track of them here.
  std::map<BasicBlock*, std::pair<ValID, int> > BBForwardRefs;
  std::vector<BasicBlock*> NumberedBlocks;
  unsigned NextBBNum;

  inline PerFunctionInfo() {
    CurrentFunction = 0;
    isDeclare = false;
  }

  inline void FunctionStart(Function *M) {
    CurrentFunction = M;
    NextBBNum = 0;
  }

  void FunctionDone() {
    NumberedBlocks.clear();

    // Any forward referenced blocks left?
    if (!BBForwardRefs.empty())
      ThrowException("Undefined reference to label " +
                     BBForwardRefs.begin()->first->getName());

    // Resolve all forward references now.
    ResolveDefinitions(LateResolveValues, &CurModule.LateResolveValues);

    Values.clear();         // Clear out function local definitions
    CurrentFunction = 0;
    isDeclare = false;
  }
} CurFun;  // Info for the current function...

static bool inFunctionScope() { return CurFun.CurrentFunction != 0; }


//===----------------------------------------------------------------------===//
//               Code to handle definitions of all the types
//===----------------------------------------------------------------------===//

static int InsertValue(Value *V,
                  std::map<const Type*,ValueList> &ValueTab = CurFun.Values) {
  if (V->hasName()) return -1;           // Is this a numbered definition?

  // Yes, insert the value into the value table...
  ValueList &List = ValueTab[V->getType()];
  List.push_back(V);
  return List.size()-1;
}

static const Type *getTypeVal(const ValID &D, bool DoNotImprovise = false) {
  switch (D.Type) {
  case ValID::NumberVal:               // Is it a numbered definition?
    // Module constants occupy the lowest numbered slots...
    if ((unsigned)D.Num < CurModule.Types.size())
      return CurModule.Types[(unsigned)D.Num];
    break;
  case ValID::NameVal:                 // Is it a named definition?
    if (const Type *N = CurModule.CurrentModule->getTypeByName(D.Name)) {
      D.destroy();  // Free old strdup'd memory...
      return N;
    }
    break;
  default:
    ThrowException("Internal parser error: Invalid symbol type reference!");
  }

  // If we reached here, we referenced either a symbol that we don't know about
  // or an id number that hasn't been read yet.  We may be referencing something
  // forward, so just create an entry to be resolved later and get to it...
  //
  if (DoNotImprovise) return 0;  // Do we just want a null to be returned?


  if (inFunctionScope()) {
    if (D.Type == ValID::NameVal)
      ThrowException("Reference to an undefined type: '" + D.getName() + "'");
    else
      ThrowException("Reference to an undefined type: #" + itostr(D.Num));
  }

  std::map<ValID, PATypeHolder>::iterator I =CurModule.LateResolveTypes.find(D);
  if (I != CurModule.LateResolveTypes.end())
    return I->second;

  Type *Typ = OpaqueType::get();
  CurModule.LateResolveTypes.insert(std::make_pair(D, Typ));
  return Typ;
 }

static Value *lookupInSymbolTable(const Type *Ty, const std::string &Name) {
  SymbolTable &SymTab =
    inFunctionScope() ? CurFun.CurrentFunction->getSymbolTable() :
                        CurModule.CurrentModule->getSymbolTable();
  return SymTab.lookup(Ty, Name);
}

// getValNonImprovising - Look up the value specified by the provided type and
// the provided ValID.  If the value exists and has already been defined, return
// it.  Otherwise return null.
//
static Value *getValNonImprovising(const Type *Ty, const ValID &D) {
  if (isa<FunctionType>(Ty))
    ThrowException("Functions are not values and "
                   "must be referenced as pointers");

  switch (D.Type) {
  case ValID::NumberVal: {                 // Is it a numbered definition?
    unsigned Num = (unsigned)D.Num;

    // Module constants occupy the lowest numbered slots...
    std::map<const Type*,ValueList>::iterator VI = CurModule.Values.find(Ty);
    if (VI != CurModule.Values.end()) {
      if (Num < VI->second.size())
        return VI->second[Num];
      Num -= VI->second.size();
    }

    // Make sure that our type is within bounds
    VI = CurFun.Values.find(Ty);
    if (VI == CurFun.Values.end()) return 0;

    // Check that the number is within bounds...
    if (VI->second.size() <= Num) return 0;

    return VI->second[Num];
  }

  case ValID::NameVal: {                // Is it a named definition?
    Value *N = lookupInSymbolTable(Ty, std::string(D.Name));
    if (N == 0) return 0;

    D.destroy();  // Free old strdup'd memory...
    return N;
  }

  // Check to make sure that "Ty" is an integral type, and that our
  // value will fit into the specified type...
  case ValID::ConstSIntVal:    // Is it a constant pool reference??
    if (!ConstantSInt::isValueValidForType(Ty, D.ConstPool64))
      ThrowException("Signed integral constant '" +
                     itostr(D.ConstPool64) + "' is invalid for type '" +
                     Ty->getDescription() + "'!");
    return ConstantSInt::get(Ty, D.ConstPool64);

  case ValID::ConstUIntVal:     // Is it an unsigned const pool reference?
    if (!ConstantUInt::isValueValidForType(Ty, D.UConstPool64)) {
      if (!ConstantSInt::isValueValidForType(Ty, D.ConstPool64)) {
        ThrowException("Integral constant '" + utostr(D.UConstPool64) +
                       "' is invalid or out of range!");
      } else {     // This is really a signed reference.  Transmogrify.
        return ConstantSInt::get(Ty, D.ConstPool64);
      }
    } else {
      return ConstantUInt::get(Ty, D.UConstPool64);
    }

  case ValID::ConstFPVal:        // Is it a floating point const pool reference?
    if (!ConstantFP::isValueValidForType(Ty, D.ConstPoolFP))
      ThrowException("FP constant invalid for type!!");
    return ConstantFP::get(Ty, D.ConstPoolFP);

  case ValID::ConstNullVal:      // Is it a null value?
    if (!isa<PointerType>(Ty))
      ThrowException("Cannot create a a non pointer null!");
    return ConstantPointerNull::get(cast<PointerType>(Ty));

  case ValID::ConstUndefVal:      // Is it an undef value?
    return UndefValue::get(Ty);

  case ValID::ConstantVal:       // Fully resolved constant?
    if (D.ConstantValue->getType() != Ty)
      ThrowException("Constant expression type different from required type!");
    return D.ConstantValue;

  default:
    assert(0 && "Unhandled case!");
    return 0;
  }   // End of switch

  assert(0 && "Unhandled case!");
  return 0;
}

// getVal - This function is identical to getValNonImprovising, except that if a
// value is not already defined, it "improvises" by creating a placeholder var
// that looks and acts just like the requested variable.  When the value is
// defined later, all uses of the placeholder variable are replaced with the
// real thing.
//
static Value *getVal(const Type *Ty, const ValID &ID) {
  if (Ty == Type::LabelTy)
    ThrowException("Cannot use a basic block here");

  // See if the value has already been defined.
  Value *V = getValNonImprovising(Ty, ID);
  if (V) return V;

  if (!Ty->isFirstClassType() && !isa<OpaqueType>(Ty))
    ThrowException("Invalid use of a composite type!");

  // If we reached here, we referenced either a symbol that we don't know about
  // or an id number that hasn't been read yet.  We may be referencing something
  // forward, so just create an entry to be resolved later and get to it...
  //
  V = new Argument(Ty);

  // Remember where this forward reference came from.  FIXME, shouldn't we try
  // to recycle these things??
  CurModule.PlaceHolderInfo.insert(std::make_pair(V, std::make_pair(ID,
                                                               llvmAsmlineno)));

  if (inFunctionScope())
    InsertValue(V, CurFun.LateResolveValues);
  else
    InsertValue(V, CurModule.LateResolveValues);
  return V;
}

/// getBBVal - This is used for two purposes:
///  * If isDefinition is true, a new basic block with the specified ID is being
///    defined.
///  * If isDefinition is true, this is a reference to a basic block, which may
///    or may not be a forward reference.
///
static BasicBlock *getBBVal(const ValID &ID, bool isDefinition = false) {
  assert(inFunctionScope() && "Can't get basic block at global scope!");

  std::string Name;
  BasicBlock *BB = 0;
  switch (ID.Type) {
  default: ThrowException("Illegal label reference " + ID.getName());
  case ValID::NumberVal:                // Is it a numbered definition?
    if (unsigned(ID.Num) >= CurFun.NumberedBlocks.size())
      CurFun.NumberedBlocks.resize(ID.Num+1);
    BB = CurFun.NumberedBlocks[ID.Num];
    break;
  case ValID::NameVal:                  // Is it a named definition?
    Name = ID.Name;
    if (Value *N = CurFun.CurrentFunction->
                   getSymbolTable().lookup(Type::LabelTy, Name))
      BB = cast<BasicBlock>(N);
    break;
  }

  // See if the block has already been defined.
  if (BB) {
    // If this is the definition of the block, make sure the existing value was
    // just a forward reference.  If it was a forward reference, there will be
    // an entry for it in the PlaceHolderInfo map.
    if (isDefinition && !CurFun.BBForwardRefs.erase(BB))
      // The existing value was a definition, not a forward reference.
      ThrowException("Redefinition of label " + ID.getName());

    ID.destroy();                       // Free strdup'd memory.
    return BB;
  }

  // Otherwise this block has not been seen before.
  BB = new BasicBlock("", CurFun.CurrentFunction);
  if (ID.Type == ValID::NameVal) {
    BB->setName(ID.Name);
  } else {
    CurFun.NumberedBlocks[ID.Num] = BB;
  }

  // If this is not a definition, keep track of it so we can use it as a forward
  // reference.
  if (!isDefinition) {
    // Remember where this forward reference came from.
    CurFun.BBForwardRefs[BB] = std::make_pair(ID, llvmAsmlineno);
  } else {
    // The forward declaration could have been inserted anywhere in the
    // function: insert it into the correct place now.
    CurFun.CurrentFunction->getBasicBlockList().remove(BB);
    CurFun.CurrentFunction->getBasicBlockList().push_back(BB);
  }
  ID.destroy();
  return BB;
}


//===----------------------------------------------------------------------===//
//              Code to handle forward references in instructions
//===----------------------------------------------------------------------===//
//
// This code handles the late binding needed with statements that reference
// values not defined yet... for example, a forward branch, or the PHI node for
// a loop body.
//
// This keeps a table (CurFun.LateResolveValues) of all such forward references
// and back patchs after we are done.
//

// ResolveDefinitions - If we could not resolve some defs at parsing
// time (forward branches, phi functions for loops, etc...) resolve the
// defs now...
//
static void 
ResolveDefinitions(std::map<const Type*,ValueList> &LateResolvers,
                   std::map<const Type*,ValueList> *FutureLateResolvers) {
  // Loop over LateResolveDefs fixing up stuff that couldn't be resolved
  for (std::map<const Type*,ValueList>::iterator LRI = LateResolvers.begin(),
         E = LateResolvers.end(); LRI != E; ++LRI) {
    ValueList &List = LRI->second;
    while (!List.empty()) {
      Value *V = List.back();
      List.pop_back();

      std::map<Value*, std::pair<ValID, int> >::iterator PHI =
        CurModule.PlaceHolderInfo.find(V);
      assert(PHI != CurModule.PlaceHolderInfo.end() && "Placeholder error!");

      ValID &DID = PHI->second.first;

      Value *TheRealValue = getValNonImprovising(LRI->first, DID);
      if (TheRealValue) {
        V->replaceAllUsesWith(TheRealValue);
        delete V;
        CurModule.PlaceHolderInfo.erase(PHI);
      } else if (FutureLateResolvers) {
        // Functions have their unresolved items forwarded to the module late
        // resolver table
        InsertValue(V, *FutureLateResolvers);
      } else {
        if (DID.Type == ValID::NameVal)
          ThrowException("Reference to an invalid definition: '" +DID.getName()+
                         "' of type '" + V->getType()->getDescription() + "'",
                         PHI->second.second);
        else
          ThrowException("Reference to an invalid definition: #" +
                         itostr(DID.Num) + " of type '" +
                         V->getType()->getDescription() + "'",
                         PHI->second.second);
      }
    }
  }

  LateResolvers.clear();
}

// ResolveTypeTo - A brand new type was just declared.  This means that (if
// name is not null) things referencing Name can be resolved.  Otherwise, things
// refering to the number can be resolved.  Do this now.
//
static void ResolveTypeTo(char *Name, const Type *ToTy) {
  ValID D;
  if (Name) D = ValID::create(Name);
  else      D = ValID::create((int)CurModule.Types.size());

  std::map<ValID, PATypeHolder>::iterator I =
    CurModule.LateResolveTypes.find(D);
  if (I != CurModule.LateResolveTypes.end()) {
    ((DerivedType*)I->second.get())->refineAbstractTypeTo(ToTy);
    CurModule.LateResolveTypes.erase(I);
  }
}

// setValueName - Set the specified value to the name given.  The name may be
// null potentially, in which case this is a noop.  The string passed in is
// assumed to be a malloc'd string buffer, and is free'd by this function.
//
static void setValueName(Value *V, char *NameStr) {
  if (NameStr) {
    std::string Name(NameStr);      // Copy string
    free(NameStr);                  // Free old string

    if (V->getType() == Type::VoidTy)
      ThrowException("Can't assign name '" + Name+"' to value with void type!");

    assert(inFunctionScope() && "Must be in function scope!");
    SymbolTable &ST = CurFun.CurrentFunction->getSymbolTable();
    if (ST.lookup(V->getType(), Name))
      ThrowException("Redefinition of value named '" + Name + "' in the '" +
                     V->getType()->getDescription() + "' type plane!");

    // Set the name.
    V->setName(Name);
  }
}

/// ParseGlobalVariable - Handle parsing of a global.  If Initializer is null,
/// this is a declaration, otherwise it is a definition.
static void ParseGlobalVariable(char *NameStr,GlobalValue::LinkageTypes Linkage,
                                bool isConstantGlobal, const Type *Ty,
                                Constant *Initializer) {
  if (isa<FunctionType>(Ty))
    ThrowException("Cannot declare global vars of function type!");

  const PointerType *PTy = PointerType::get(Ty);

  std::string Name;
  if (NameStr) {
    Name = NameStr;      // Copy string
    free(NameStr);       // Free old string
  }

  // See if this global value was forward referenced.  If so, recycle the
  // object.
  ValID ID;
  if (!Name.empty()) {
    ID = ValID::create((char*)Name.c_str());
  } else {
    ID = ValID::create((int)CurModule.Values[PTy].size());
  }

  if (GlobalValue *FWGV = CurModule.GetForwardRefForGlobal(PTy, ID)) {
    // Move the global to the end of the list, from whereever it was
    // previously inserted.
    GlobalVariable *GV = cast<GlobalVariable>(FWGV);
    CurModule.CurrentModule->getGlobalList().remove(GV);
    CurModule.CurrentModule->getGlobalList().push_back(GV);
    GV->setInitializer(Initializer);
    GV->setLinkage(Linkage);
    GV->setConstant(isConstantGlobal);
    InsertValue(GV, CurModule.Values);
    return;
  }

  // If this global has a name, check to see if there is already a definition
  // of this global in the module.  If so, merge as appropriate.  Note that
  // this is really just a hack around problems in the CFE.  :(
  if (!Name.empty()) {
    // We are a simple redefinition of a value, check to see if it is defined
    // the same as the old one.
    if (GlobalVariable *EGV =
                CurModule.CurrentModule->getGlobalVariable(Name, Ty)) {
      // We are allowed to redefine a global variable in two circumstances:
      // 1. If at least one of the globals is uninitialized or
      // 2. If both initializers have the same value.
      //
      if (!EGV->hasInitializer() || !Initializer ||
          EGV->getInitializer() == Initializer) {

        // Make sure the existing global version gets the initializer!  Make
        // sure that it also gets marked const if the new version is.
        if (Initializer && !EGV->hasInitializer())
          EGV->setInitializer(Initializer);
        if (isConstantGlobal)
          EGV->setConstant(true);
        EGV->setLinkage(Linkage);
        return;
      }

      ThrowException("Redefinition of global variable named '" + Name +
                     "' in the '" + Ty->getDescription() + "' type plane!");
    }
  }

  // Otherwise there is no existing GV to use, create one now.
  GlobalVariable *GV =
    new GlobalVariable(Ty, isConstantGlobal, Linkage, Initializer, Name,
                       CurModule.CurrentModule);
  InsertValue(GV, CurModule.Values);
}

// setTypeName - Set the specified type to the name given.  The name may be
// null potentially, in which case this is a noop.  The string passed in is
// assumed to be a malloc'd string buffer, and is freed by this function.
//
// This function returns true if the type has already been defined, but is
// allowed to be redefined in the specified context.  If the name is a new name
// for the type plane, it is inserted and false is returned.
static bool setTypeName(const Type *T, char *NameStr) {
  assert(!inFunctionScope() && "Can't give types function-local names!");
  if (NameStr == 0) return false;
 
  std::string Name(NameStr);      // Copy string
  free(NameStr);                  // Free old string

  // We don't allow assigning names to void type
  if (T == Type::VoidTy)
    ThrowException("Can't assign name '" + Name + "' to the void type!");

  // Set the type name, checking for conflicts as we do so.
  bool AlreadyExists = CurModule.CurrentModule->addTypeName(Name, T);

  if (AlreadyExists) {   // Inserting a name that is already defined???
    const Type *Existing = CurModule.CurrentModule->getTypeByName(Name);
    assert(Existing && "Conflict but no matching type?");

    // There is only one case where this is allowed: when we are refining an
    // opaque type.  In this case, Existing will be an opaque type.
    if (const OpaqueType *OpTy = dyn_cast<OpaqueType>(Existing)) {
      // We ARE replacing an opaque type!
      const_cast<OpaqueType*>(OpTy)->refineAbstractTypeTo(T);
      return true;
    }

    // Otherwise, this is an attempt to redefine a type. That's okay if
    // the redefinition is identical to the original. This will be so if
    // Existing and T point to the same Type object. In this one case we
    // allow the equivalent redefinition.
    if (Existing == T) return true;  // Yes, it's equal.

    // Any other kind of (non-equivalent) redefinition is an error.
    ThrowException("Redefinition of type named '" + Name + "' in the '" +
                   T->getDescription() + "' type plane!");
  }

  return false;
}

//===----------------------------------------------------------------------===//
// Code for handling upreferences in type names...
//

// TypeContains - Returns true if Ty directly contains E in it.
//
static bool TypeContains(const Type *Ty, const Type *E) {
  return std::find(Ty->subtype_begin(), Ty->subtype_end(),
                   E) != Ty->subtype_end();
}

namespace {
  struct UpRefRecord {
    // NestingLevel - The number of nesting levels that need to be popped before
    // this type is resolved.
    unsigned NestingLevel;

    // LastContainedTy - This is the type at the current binding level for the
    // type.  Every time we reduce the nesting level, this gets updated.
    const Type *LastContainedTy;

    // UpRefTy - This is the actual opaque type that the upreference is
    // represented with.
    OpaqueType *UpRefTy;

    UpRefRecord(unsigned NL, OpaqueType *URTy)
      : NestingLevel(NL), LastContainedTy(URTy), UpRefTy(URTy) {}
  };
}

// UpRefs - A list of the outstanding upreferences that need to be resolved.
static std::vector<UpRefRecord> UpRefs;

/// HandleUpRefs - Every time we finish a new layer of types, this function is
/// called.  It loops through the UpRefs vector, which is a list of the
/// currently active types.  For each type, if the up reference is contained in
/// the newly completed type, we decrement the level count.  When the level
/// count reaches zero, the upreferenced type is the type that is passed in:
/// thus we can complete the cycle.
///
static PATypeHolder HandleUpRefs(const Type *ty) {
  if (!ty->isAbstract()) return ty;
  PATypeHolder Ty(ty);
  UR_OUT("Type '" << Ty->getDescription() <<
         "' newly formed.  Resolving upreferences.\n" <<
         UpRefs.size() << " upreferences active!\n");

  // If we find any resolvable upreferences (i.e., those whose NestingLevel goes
  // to zero), we resolve them all together before we resolve them to Ty.  At
  // the end of the loop, if there is anything to resolve to Ty, it will be in
  // this variable.
  OpaqueType *TypeToResolve = 0;

  for (unsigned i = 0; i != UpRefs.size(); ++i) {
    UR_OUT("  UR#" << i << " - TypeContains(" << Ty->getDescription() << ", "
           << UpRefs[i].second->getDescription() << ") = "
           << (TypeContains(Ty, UpRefs[i].second) ? "true" : "false") << "\n");
    if (TypeContains(Ty, UpRefs[i].LastContainedTy)) {
      // Decrement level of upreference
      unsigned Level = --UpRefs[i].NestingLevel;
      UpRefs[i].LastContainedTy = Ty;
      UR_OUT("  Uplevel Ref Level = " << Level << "\n");
      if (Level == 0) {                     // Upreference should be resolved!
        if (!TypeToResolve) {
          TypeToResolve = UpRefs[i].UpRefTy;
        } else {
          UR_OUT("  * Resolving upreference for "
                 << UpRefs[i].second->getDescription() << "\n";
                 std::string OldName = UpRefs[i].UpRefTy->getDescription());
          UpRefs[i].UpRefTy->refineAbstractTypeTo(TypeToResolve);
          UR_OUT("  * Type '" << OldName << "' refined upreference to: "
                 << (const void*)Ty << ", " << Ty->getDescription() << "\n");
        }
        UpRefs.erase(UpRefs.begin()+i);     // Remove from upreference list...
        --i;                                // Do not skip the next element...
      }
    }
  }

  if (TypeToResolve) {
    UR_OUT("  * Resolving upreference for "
           << UpRefs[i].second->getDescription() << "\n";
           std::string OldName = TypeToResolve->getDescription());
    TypeToResolve->refineAbstractTypeTo(Ty);
  }

  return Ty;
}


// common code from the two 'RunVMAsmParser' functions
 static Module * RunParser(Module * M) {

  llvmAsmlineno = 1;      // Reset the current line number...
  ObsoleteVarArgs = false;
  NewVarArgs = false;

  CurModule.CurrentModule = M;
  yyparse();       // Parse the file, potentially throwing exception

  Module *Result = ParserResult;
  ParserResult = 0;

  //Not all functions use vaarg, so make a second check for ObsoleteVarArgs
  {
    Function* F;
    if ((F = Result->getNamedFunction("llvm.va_start"))
        && F->getFunctionType()->getNumParams() == 0)
      ObsoleteVarArgs = true;
    if((F = Result->getNamedFunction("llvm.va_copy"))
       && F->getFunctionType()->getNumParams() == 1)
      ObsoleteVarArgs = true;
  }

  if (ObsoleteVarArgs && NewVarArgs)
    ThrowException("This file is corrupt: it uses both new and old style varargs");

  if(ObsoleteVarArgs) {
    if(Function* F = Result->getNamedFunction("llvm.va_start")) {
      if (F->arg_size() != 0)
        ThrowException("Obsolete va_start takes 0 argument!");
      
      //foo = va_start()
      // ->
      //bar = alloca typeof(foo)
      //va_start(bar)
      //foo = load bar

      const Type* RetTy = Type::getPrimitiveType(Type::VoidTyID);
      const Type* ArgTy = F->getFunctionType()->getReturnType();
      const Type* ArgTyPtr = PointerType::get(ArgTy);
      Function* NF = Result->getOrInsertFunction("llvm.va_start", 
                                                 RetTy, ArgTyPtr, 0);

      while (!F->use_empty()) {
        CallInst* CI = cast<CallInst>(F->use_back());
        AllocaInst* bar = new AllocaInst(ArgTy, 0, "vastart.fix.1", CI);
        new CallInst(NF, bar, "", CI);
        Value* foo = new LoadInst(bar, "vastart.fix.2", CI);
        CI->replaceAllUsesWith(foo);
        CI->getParent()->getInstList().erase(CI);
      }
      Result->getFunctionList().erase(F);
    }
    
    if(Function* F = Result->getNamedFunction("llvm.va_end")) {
      if(F->arg_size() != 1)
        ThrowException("Obsolete va_end takes 1 argument!");

      //vaend foo
      // ->
      //bar = alloca 1 of typeof(foo)
      //vaend bar
      const Type* RetTy = Type::getPrimitiveType(Type::VoidTyID);
      const Type* ArgTy = F->getFunctionType()->getParamType(0);
      const Type* ArgTyPtr = PointerType::get(ArgTy);
      Function* NF = Result->getOrInsertFunction("llvm.va_end", 
                                                 RetTy, ArgTyPtr, 0);

      while (!F->use_empty()) {
        CallInst* CI = cast<CallInst>(F->use_back());
        AllocaInst* bar = new AllocaInst(ArgTy, 0, "vaend.fix.1", CI);
        new StoreInst(CI->getOperand(1), bar, CI);
        new CallInst(NF, bar, "", CI);
        CI->getParent()->getInstList().erase(CI);
      }
      Result->getFunctionList().erase(F);
    }

    if(Function* F = Result->getNamedFunction("llvm.va_copy")) {
      if(F->arg_size() != 1)
        ThrowException("Obsolete va_copy takes 1 argument!");
      //foo = vacopy(bar)
      // ->
      //a = alloca 1 of typeof(foo)
      //b = alloca 1 of typeof(foo)
      //store bar -> b
      //vacopy(a, b)
      //foo = load a
      
      const Type* RetTy = Type::getPrimitiveType(Type::VoidTyID);
      const Type* ArgTy = F->getFunctionType()->getReturnType();
      const Type* ArgTyPtr = PointerType::get(ArgTy);
      Function* NF = Result->getOrInsertFunction("llvm.va_copy", 
                                                 RetTy, ArgTyPtr, ArgTyPtr, 0);

      while (!F->use_empty()) {
        CallInst* CI = cast<CallInst>(F->use_back());
        AllocaInst* a = new AllocaInst(ArgTy, 0, "vacopy.fix.1", CI);
        AllocaInst* b = new AllocaInst(ArgTy, 0, "vacopy.fix.2", CI);
        new StoreInst(CI->getOperand(1), b, CI);
        new CallInst(NF, a, b, "", CI);
        Value* foo = new LoadInst(a, "vacopy.fix.3", CI);
        CI->replaceAllUsesWith(foo);
        CI->getParent()->getInstList().erase(CI);
      }
      Result->getFunctionList().erase(F);
    }
  }

  return Result;

 }

//===----------------------------------------------------------------------===//
//            RunVMAsmParser - Define an interface to this parser
//===----------------------------------------------------------------------===//
//
Module *llvm::RunVMAsmParser(const std::string &Filename, FILE *F) {
  set_scan_file(F);

  CurFilename = Filename;
  return RunParser(new Module(CurFilename));
}

Module *llvm::RunVMAsmParser(const char * AsmString, Module * M) {
  set_scan_string(AsmString);

  CurFilename = "from_memory";
  if (M == NULL) {
    return RunParser(new Module (CurFilename));
  } else {
    return RunParser(M);
  }
}


#line 865 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
typedef union {
  llvm::Module                           *ModuleVal;
  llvm::Function                         *FunctionVal;
  std::pair<llvm::PATypeHolder*, char*>  *ArgVal;
  llvm::BasicBlock                       *BasicBlockVal;
  llvm::TerminatorInst                   *TermInstVal;
  llvm::Instruction                      *InstVal;
  llvm::Constant                         *ConstVal;

  const llvm::Type                       *PrimType;
  llvm::PATypeHolder                     *TypeVal;
  llvm::Value                            *ValueVal;

  std::vector<std::pair<llvm::PATypeHolder*,char*> > *ArgList;
  std::vector<llvm::Value*>              *ValueList;
  std::list<llvm::PATypeHolder>          *TypeList;
  // Represent the RHS of PHI node
  std::list<std::pair<llvm::Value*,
                      llvm::BasicBlock*> > *PHIList;
  std::vector<std::pair<llvm::Constant*, llvm::BasicBlock*> > *JumpTable;
  std::vector<llvm::Constant*>           *ConstVector;

  llvm::GlobalValue::LinkageTypes         Linkage;
  int64_t                           SInt64Val;
  uint64_t                          UInt64Val;
  int                               SIntVal;
  unsigned                          UIntVal;
  double                            FPVal;
  bool                              BoolVal;

  char                             *StrVal;   // This memory is strdup'd!
  llvm::ValID                             ValIDVal; // strdup'd memory maybe!

  llvm::Instruction::BinaryOps            BinaryOpVal;
  llvm::Instruction::TermOps              TermOpVal;
  llvm::Instruction::MemoryOps            MemOpVal;
  llvm::Instruction::OtherOps             OtherOpVal;
  llvm::Module::Endianness                Endianness;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		480
#define	YYFLAG		-32768
#define	YYNTBASE	125

#define YYTRANSLATE(x) ((unsigned)(x) <= 364 ? yytranslate[x] : 188)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,   113,
   114,   120,     2,   121,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,   123,
   111,   124,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
   115,   112,   117,     2,     2,     2,     2,     2,   122,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,   116,
     2,     2,   118,     2,   119,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
    97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
   107,   108,   109,   110
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     6,     8,    10,    12,    14,    16,    18,
    20,    22,    24,    26,    28,    30,    32,    34,    36,    38,
    40,    42,    44,    46,    48,    50,    52,    54,    56,    58,
    60,    62,    64,    66,    68,    70,    72,    74,    76,    79,
    80,    82,    84,    86,    88,    89,    90,    92,    94,    96,
    99,   101,   103,   105,   107,   109,   111,   113,   115,   117,
   119,   121,   123,   125,   127,   129,   131,   133,   135,   137,
   139,   141,   144,   149,   155,   161,   168,   172,   175,   178,
   180,   184,   186,   190,   192,   193,   198,   202,   206,   211,
   216,   220,   223,   226,   229,   232,   235,   238,   241,   244,
   247,   250,   257,   263,   272,   279,   286,   293,   300,   304,
   306,   308,   310,   312,   315,   318,   321,   323,   328,   331,
   337,   343,   347,   352,   353,   355,   357,   361,   365,   369,
   373,   377,   379,   380,   382,   384,   386,   387,   390,   394,
   396,   398,   402,   404,   405,   412,   414,   416,   420,   422,
   424,   427,   428,   432,   434,   436,   438,   440,   442,   444,
   446,   450,   452,   454,   456,   458,   460,   463,   466,   469,
   473,   476,   477,   479,   482,   485,   489,   499,   509,   518,
   532,   534,   536,   543,   549,   552,   559,   567,   569,   573,
   575,   576,   579,   581,   587,   593,   599,   605,   608,   613,
   618,   625,   632,   637,   642,   647,   650,   658,   660,   663,
   664,   666,   667,   669,   670,   673,   679,   682,   688,   691,
   696,   703,   708,   713,   719,   728,   733,   742,   749
};

static const short yyrhs[] = {     5,
     0,     6,     0,     3,     0,     4,     0,    69,     0,    70,
     0,    71,     0,    72,     0,    73,     0,    74,     0,    75,
     0,    76,     0,    77,     0,    78,     0,    79,     0,    80,
     0,    81,     0,    82,     0,    83,     0,    84,     0,    85,
     0,    86,     0,    87,     0,    88,     0,    99,     0,   100,
     0,    16,     0,    14,     0,    12,     0,    10,     0,    17,
     0,    15,     0,    13,     0,    11,     0,   132,     0,   133,
     0,    18,     0,    19,     0,   157,   111,     0,     0,    41,
     0,    42,     0,    43,     0,    44,     0,     0,     0,    58,
     0,    59,     0,    60,     0,    57,     4,     0,   141,     0,
     8,     0,   143,     0,     8,     0,   143,     0,     9,     0,
    10,     0,    11,     0,    12,     0,    13,     0,    14,     0,
    15,     0,    16,     0,    17,     0,    18,     0,    19,     0,
    20,     0,    21,     0,    45,     0,   142,     0,   170,     0,
   112,     4,     0,   140,   113,   145,   114,     0,   115,     4,
   116,   143,   117,     0,   115,    61,    62,   143,   117,     0,
   115,    61,    62,     4,   143,   117,     0,   118,   144,   119,
     0,   118,   119,     0,   143,   120,     0,   143,     0,   144,
   121,   143,     0,   144,     0,   144,   121,    37,     0,    37,
     0,     0,   141,   115,   148,   117,     0,   141,   115,   117,
     0,   141,   122,    24,     0,   141,   123,   148,   124,     0,
   141,   118,   148,   119,     0,   141,   118,   119,     0,   141,
    38,     0,   141,    39,     0,   141,   170,     0,   141,   147,
     0,   141,    26,     0,   132,   126,     0,   133,     4,     0,
     9,    27,     0,     9,    28,     0,   135,     7,     0,    96,
   113,   146,    36,   141,   114,     0,    94,   113,   146,   184,
   114,     0,    97,   113,   146,   121,   146,   121,   146,   114,
     0,   127,   113,   146,   121,   146,   114,     0,   128,   113,
   146,   121,   146,   114,     0,   129,   113,   146,   121,   146,
   114,     0,   131,   113,   146,   121,   146,   114,     0,   148,
   121,   146,     0,   146,     0,    32,     0,    33,     0,   151,
     0,   151,   166,     0,   151,   167,     0,   151,    25,     0,
   152,     0,   152,   136,    20,   139,     0,   152,   167,     0,
   152,   136,   137,   149,   146,     0,   152,   136,    47,   149,
   141,     0,   152,    48,   154,     0,   152,    54,   111,   155,
     0,     0,    53,     0,    52,     0,    50,   111,   153,     0,
    51,   111,     4,     0,    49,   111,    24,     0,   115,   156,
   117,     0,   156,   121,    24,     0,    24,     0,     0,    22,
     0,    24,     0,   157,     0,     0,   141,   158,     0,   160,
   121,   159,     0,   159,     0,   160,     0,   160,   121,    37,
     0,    37,     0,     0,   138,   139,   157,   113,   161,   114,
     0,    29,     0,   118,     0,   137,   162,   163,     0,    30,
     0,   119,     0,   173,   165,     0,     0,    31,   168,   162,
     0,     3,     0,     4,     0,     7,     0,    27,     0,    28,
     0,    38,     0,    39,     0,   123,   148,   124,     0,   147,
     0,   125,     0,   157,     0,   170,     0,   169,     0,   141,
   171,     0,   173,   174,     0,   164,   174,     0,   175,   136,
   176,     0,   175,   178,     0,     0,    23,     0,    63,   172,
     0,    63,     8,     0,    64,    21,   171,     0,    64,     9,
   171,   121,    21,   171,   121,    21,   171,     0,    65,   134,
   171,   121,    21,   171,   115,   177,   117,     0,    65,   134,
   171,   121,    21,   171,   115,   117,     0,    66,   138,   139,
   171,   113,   181,   114,    36,    21,   171,    67,    21,   171,
     0,    67,     0,    68,     0,   177,   134,   169,   121,    21,
   171,     0,   134,   169,   121,    21,   171,     0,   136,   183,
     0,   141,   115,   171,   121,   171,   117,     0,   179,   121,
   115,   171,   121,   171,   117,     0,   172,     0,   180,   121,
   172,     0,   180,     0,     0,    56,    55,     0,    55,     0,
   127,   141,   171,   121,   171,     0,   128,   141,   171,   121,
   171,     0,   129,   141,   171,   121,   171,     0,   130,   141,
   171,   121,   171,     0,    46,   172,     0,   131,   172,   121,
   172,     0,    96,   172,    36,   141,     0,    97,   172,   121,
   172,   121,   172,     0,    98,   172,   121,   172,   121,   172,
     0,   101,   172,   121,   141,     0,   109,   172,   121,   141,
     0,   110,   172,   121,   141,     0,    95,   179,     0,   182,
   138,   139,   171,   113,   181,   114,     0,   187,     0,   121,
   180,     0,     0,    34,     0,     0,    35,     0,     0,    89,
   141,     0,    89,   141,   121,    15,   171,     0,    90,   141,
     0,    90,   141,   121,    15,   171,     0,    91,   172,     0,
   185,    92,   141,   171,     0,   185,    93,   172,   121,   141,
   171,     0,    94,   141,   171,   184,     0,   102,   141,   171,
   184,     0,   186,   103,   172,   121,   172,     0,   105,   172,
   121,   172,   121,   172,   121,   172,     0,   106,   172,   121,
   172,     0,   107,   172,   121,   172,   121,   172,   121,   172,
     0,   108,   172,   121,   172,   121,   172,     0,   104,   172,
   121,   141,   171,   184,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   986,   987,   994,   995,  1004,  1004,  1004,  1004,  1004,  1005,
  1005,  1005,  1006,  1006,  1006,  1006,  1006,  1006,  1007,  1007,
  1007,  1007,  1007,  1007,  1009,  1009,  1013,  1013,  1013,  1013,
  1014,  1014,  1014,  1014,  1015,  1015,  1016,  1016,  1019,  1022,
  1026,  1026,  1027,  1028,  1029,  1032,  1032,  1033,  1034,  1035,
  1049,  1049,  1050,  1050,  1052,  1061,  1061,  1061,  1061,  1061,
  1061,  1061,  1062,  1062,  1062,  1062,  1062,  1062,  1063,  1066,
  1069,  1075,  1082,  1094,  1099,  1104,  1116,  1125,  1128,  1136,
  1140,  1145,  1146,  1149,  1152,  1162,  1187,  1200,  1228,  1253,
  1273,  1285,  1294,  1298,  1357,  1363,  1371,  1376,  1381,  1384,
  1387,  1394,  1404,  1435,  1442,  1463,  1471,  1476,  1488,  1491,
  1498,  1498,  1508,  1515,  1519,  1522,  1525,  1538,  1558,  1560,
  1564,  1568,  1570,  1572,  1577,  1578,  1580,  1583,  1591,  1596,
  1598,  1602,  1606,  1614,  1614,  1615,  1615,  1617,  1623,  1628,
  1634,  1637,  1642,  1646,  1650,  1730,  1730,  1732,  1740,  1740,
  1742,  1746,  1746,  1755,  1758,  1761,  1764,  1767,  1770,  1773,
  1776,  1800,  1807,  1810,  1815,  1815,  1821,  1825,  1828,  1836,
  1845,  1849,  1859,  1870,  1873,  1876,  1879,  1882,  1896,  1900,
  1953,  1956,  1962,  1970,  1980,  1987,  1992,  1998,  2002,  2008,
  2008,  2010,  2013,  2019,  2032,  2041,  2047,  2053,  2065,  2073,
  2080,  2087,  2096,  2101,  2120,  2142,  2156,  2213,  2219,  2221,
  2225,  2228,  2232,  2235,  2240,  2244,  2248,  2252,  2256,  2263,
  2274,  2288,  2310,  2325,  2331,  2343,  2351,  2363,  2373
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","ESINT64VAL",
"EUINT64VAL","SINTVAL","UINTVAL","FPVAL","VOID","BOOL","SBYTE","UBYTE","SHORT",
"USHORT","INT","UINT","LONG","ULONG","FLOAT","DOUBLE","TYPE","LABEL","VAR_ID",
"LABELSTR","STRINGCONSTANT","IMPLEMENTATION","ZEROINITIALIZER","TRUETOK","FALSETOK",
"BEGINTOK","ENDTOK","DECLARE","GLOBAL","CONSTANT","VOLATILE","FIXED","TO","DOTDOTDOT",
"NULL_TOK","UNDEF","CONST","INTERNAL","LINKONCE","WEAK","APPENDING","OPAQUE",
"NOT","EXTERNAL","TARGET","TRIPLE","ENDIAN","POINTERSIZE","LITTLE","BIG","DEPLIBS",
"CALL","TAIL","CC_TOK","CCC_TOK","FASTCC_TOK","COLDCC_TOK","VECTOR","OF","RET",
"BR","SWITCH","INVOKE","UNWIND","UNREACHABLE","ADD","SUB","MUL","DIV","REM",
"AND","OR","XOR","SETLE","SETGE","SETLT","SETGT","SETEQ","SETNE","VSETLE","VSETGE",
"VSETLT","VSETGT","VSETEQ","VSETNE","MALLOC","ALLOCA","FREE","LOAD","STORE",
"GETELEMENTPTR","PHI_TOK","CAST","SELECT","VSELECT","SHL","SHR","VAARG","VGATHER",
"VIMM","VSCATTER","EXTRACT","EXTRACTELEMENT","COMBINE","COMBINEELEMENT","VAARG_old",
"VANEXT_old","'='","'\\\\'","'('","')'","'['","'x'","']'","'{'","'}'","'*'",
"','","'c'","'<'","'>'","INTVAL","EINT64VAL","ArithmeticOps","LogicalOps","SetCondOps",
"VSetCondOps","ShiftOps","SIntType","UIntType","IntType","FPType","OptAssign",
"OptLinkage","OptCallingConv","TypesV","UpRTypesV","Types","PrimType","UpRTypes",
"TypeListI","ArgTypeListI","ConstVal","ConstExpr","ConstVector","GlobalType",
"Module","FunctionList","ConstPool","BigOrLittle","TargetDefinition","LibrariesDefinition",
"LibList","Name","OptName","ArgVal","ArgListH","ArgList","FunctionHeaderH","BEGIN",
"FunctionHeader","END","Function","FunctionProto","@1","ConstValueRef","SymbolicValueRef",
"ValueRef","ResolvedVal","BasicBlockList","BasicBlock","InstructionList","BBTerminatorInst",
"JumpTable","Inst","PHIList","ValueRefList","ValueRefListE","OptTailCall","InstVal",
"IndexList","OptVolatile","OptFixed","MemoryInst", NULL
};
#endif

static const short yyr1[] = {     0,
   125,   125,   126,   126,   127,   127,   127,   127,   127,   128,
   128,   128,   129,   129,   129,   129,   129,   129,   130,   130,
   130,   130,   130,   130,   131,   131,   132,   132,   132,   132,
   133,   133,   133,   133,   134,   134,   135,   135,   136,   136,
   137,   137,   137,   137,   137,   138,   138,   138,   138,   138,
   139,   139,   140,   140,   141,   142,   142,   142,   142,   142,
   142,   142,   142,   142,   142,   142,   142,   142,   143,   143,
   143,   143,   143,   143,   143,   143,   143,   143,   143,   144,
   144,   145,   145,   145,   145,   146,   146,   146,   146,   146,
   146,   146,   146,   146,   146,   146,   146,   146,   146,   146,
   146,   147,   147,   147,   147,   147,   147,   147,   148,   148,
   149,   149,   150,   151,   151,   151,   151,   152,   152,   152,
   152,   152,   152,   152,   153,   153,   154,   154,   154,   155,
   156,   156,   156,   157,   157,   158,   158,   159,   160,   160,
   161,   161,   161,   161,   162,   163,   163,   164,   165,   165,
   166,   168,   167,   169,   169,   169,   169,   169,   169,   169,
   169,   169,   170,   170,   171,   171,   172,   173,   173,   174,
   175,   175,   175,   176,   176,   176,   176,   176,   176,   176,
   176,   176,   177,   177,   178,   179,   179,   180,   180,   181,
   181,   182,   182,   183,   183,   183,   183,   183,   183,   183,
   183,   183,   183,   183,   183,   183,   183,   183,   184,   184,
   185,   185,   186,   186,   187,   187,   187,   187,   187,   187,
   187,   187,   187,   187,   187,   187,   187,   187,   187
};

static const short yyr2[] = {     0,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     2,     0,
     1,     1,     1,     1,     0,     0,     1,     1,     1,     2,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     2,     4,     5,     5,     6,     3,     2,     2,     1,
     3,     1,     3,     1,     0,     4,     3,     3,     4,     4,
     3,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     6,     5,     8,     6,     6,     6,     6,     3,     1,
     1,     1,     1,     2,     2,     2,     1,     4,     2,     5,
     5,     3,     4,     0,     1,     1,     3,     3,     3,     3,
     3,     1,     0,     1,     1,     1,     0,     2,     3,     1,
     1,     3,     1,     0,     6,     1,     1,     3,     1,     1,
     2,     0,     3,     1,     1,     1,     1,     1,     1,     1,
     3,     1,     1,     1,     1,     1,     2,     2,     2,     3,
     2,     0,     1,     2,     2,     3,     9,     9,     8,    13,
     1,     1,     6,     5,     2,     6,     7,     1,     3,     1,
     0,     2,     1,     5,     5,     5,     5,     2,     4,     4,
     6,     6,     4,     4,     4,     2,     7,     1,     2,     0,
     1,     0,     1,     0,     2,     5,     2,     5,     2,     4,
     6,     4,     4,     5,     8,     4,     8,     6,     6
};

static const short yydefact[] = {   124,
    45,   117,   116,   152,    41,    42,    43,    44,    46,   172,
   114,   115,   172,   134,   135,     0,     0,    45,     0,   119,
    46,     0,    47,    48,    49,     0,     0,   173,   169,    40,
   149,   150,   151,   168,     0,     0,     0,   122,     0,     0,
     0,     0,    39,   153,    50,     1,     2,    52,    56,    57,
    58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
    68,    69,     0,     0,     0,   163,     0,     0,    51,    70,
    55,   164,    71,   146,   147,   148,   212,   171,     0,     0,
     0,   133,   123,   118,   111,   112,     0,     0,    72,     0,
     0,    54,    78,    80,     0,     0,    85,    79,   211,   213,
     0,   193,     0,     0,     0,     0,    46,   181,   182,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,     0,
     0,     0,     0,     0,     0,     0,     0,    25,    26,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   170,    46,   185,     0,     0,   208,   129,
   126,   125,   127,   128,   132,     0,   121,    56,    57,    58,
    59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
     0,     0,   120,     0,     0,    77,     0,   144,    84,    82,
     0,     0,   198,   192,   175,   174,     0,     0,    30,    34,
    29,    33,    28,    32,    27,    31,    35,    36,     0,     0,
   215,   217,   219,     0,     0,   206,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   130,     0,    99,
   100,     3,     4,    97,    98,   101,    96,    92,    93,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    95,    94,    53,     0,    53,    81,   143,   137,   140,   141,
     0,     0,    73,   154,   155,   156,   157,   158,   159,   160,
     0,   162,   166,   165,   167,     0,   176,     0,     0,     0,
     0,   210,     0,     0,     0,     0,     0,     0,   210,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   131,     0,     0,     0,    87,
   110,     0,    91,     0,    88,     0,     0,     0,     0,     0,
    74,    53,    75,   136,   138,     0,   145,    83,     0,     0,
     0,     0,     0,     0,     0,   222,     0,     0,   200,     0,
     0,   203,   223,     0,     0,   226,     0,     0,   204,   205,
     0,     0,     0,     0,   199,     0,   220,     0,     0,   210,
     0,     0,    86,     0,    90,    89,     0,     0,     0,     0,
    76,   142,   139,   161,     0,     0,   191,   216,   218,   188,
   209,     0,     0,     0,     0,   210,     0,     0,     0,   194,
   195,   196,   197,   191,     0,   224,     0,     0,     0,   109,
     0,     0,     0,     0,     0,     0,   190,     0,     0,     0,
     0,   201,   202,   229,     0,     0,   228,     0,   221,   103,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
   186,     0,     0,     0,   207,   102,     0,   105,   106,   107,
   108,     0,   179,     0,     0,     0,   187,   225,   227,     0,
   177,     0,   178,     0,     0,   104,     0,     0,     0,     0,
     0,     0,   184,     0,     0,   183,   180,     0,     0,     0
};

static const short yydefgoto[] = {    66,
   244,   257,   258,   259,   152,   260,   179,   180,   209,   181,
    18,     9,    26,    67,    68,   192,    70,    71,    95,   191,
   321,   282,   322,    87,   478,     1,     2,   163,    38,    83,
   166,    72,   335,   269,   270,   271,    27,    76,    10,    33,
    11,    12,    21,   283,    73,   285,   390,    13,    29,    30,
   154,   455,    78,   216,   417,   418,   155,   156,   346,   157,
   158,   159
};

static const short yypact[] = {-32768,
    11,   169,-32768,-32768,-32768,-32768,-32768,-32768,    40,   -11,
-32768,-32768,   -17,-32768,-32768,    68,   -68,    93,   -61,-32768,
    40,    66,-32768,-32768,-32768,  1089,   -22,-32768,-32768,    34,
-32768,-32768,-32768,-32768,    -5,    18,    30,-32768,     1,  1089,
    77,    77,-32768,-32768,-32768,-32768,-32768,     8,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   147,     6,    67,-32768,    34,    48,-32768,-32768,
   -74,-32768,-32768,-32768,-32768,-32768,  1261,-32768,   148,    80,
   174,   159,-32768,-32768,-32768,-32768,  1127,  1165,-32768,    71,
   126,-32768,-32768,   -74,   -55,    79,   899,-32768,-32768,-32768,
  1127,-32768,   135,  1203,    28,   154,    40,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1127,
  1127,  1127,  1127,  1127,  1127,  1127,  1127,-32768,-32768,  1127,
  1127,  1127,  1127,  1127,  1127,  1127,  1127,  1127,  1127,  1127,
  1127,  1127,  1127,-32768,    40,-32768,    46,    91,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   -76,-32768,   116,   145,   191,
   156,   192,   170,   193,   173,   194,   196,   197,   177,   201,
   199,   780,-32768,  1127,   456,-32768,  1127,   937,-32768,    97,
   100,   621,-32768,-32768,     8,-32768,   621,   621,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   621,  1089,
    98,    99,-32768,   621,   106,   101,   188,   104,   105,   108,
   621,   113,   114,   115,   117,   118,   119,   124,   621,   621,
   621,   621,   127,  1089,  1127,  1127,  1127,-32768,   213,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   136,
   137,   138,   975,   820,   228,  1165,   140,   141,   143,   144,
-32768,-32768,   -73,  1127,   -52,   -74,-32768,    34,-32768,   139,
   149,  1013,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  1165,-32768,-32768,-32768,-32768,   150,-32768,   152,   621,   243,
   244,   155,   621,   151,  1127,  1127,  1127,  1127,   155,  1127,
  1127,  1127,  1127,  1127,  1127,  1127,   157,   160,   161,   162,
  1127,   621,   621,   163,   164,-32768,  1165,  1165,  1165,-32768,
-32768,   -58,-32768,   -18,-32768,   -50,  1165,  1165,  1165,  1165,
-32768,   -27,-32768,-32768,-32768,  1051,-32768,-32768,   -29,   240,
   241,   176,   621,   621,  1127,-32768,   165,   621,-32768,   166,
   175,-32768,-32768,   621,   178,-32768,   179,   184,-32768,-32768,
   621,   621,   621,   621,-32768,   181,-32768,  1127,  1127,   155,
   229,   187,-32768,  1165,-32768,-32768,   195,   198,   203,   208,
-32768,-32768,-32768,-32768,   621,   621,  1127,-32768,-32768,-32768,
   209,   621,   210,  1127,  1127,   155,  1127,  1127,  1127,-32768,
-32768,-32768,-32768,  1127,   621,-32768,   153,  1127,  1165,-32768,
  1165,  1165,  1165,  1165,   211,   180,   209,   183,  1127,   158,
   621,-32768,-32768,-32768,   215,   216,-32768,   204,-32768,-32768,
   206,   217,   220,   225,   226,   227,   247,     5,   254,-32768,
-32768,   200,  1127,  1127,-32768,-32768,  1165,-32768,-32768,-32768,
-32768,   621,-32768,   701,    14,   256,-32768,-32768,-32768,   231,
-32768,   230,-32768,   701,   621,-32768,   277,   232,   242,   621,
   294,   301,-32768,   621,   621,-32768,-32768,   264,   346,-32768
};

static const short yypgoto[] = {-32768,
-32768,   270,   272,   279,-32768,   280,  -105,  -103,  -404,-32768,
   324,   340,   -98,   -35,-32768,   -26,-32768,   -57,   262,-32768,
   -86,   182,  -221,   320,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,     2,-32768,    27,-32768,-32768,   344,-32768,-32768,-32768,
-32768,   364,-32768,  -416,   202,   146,    10,-32768,   355,-32768,
-32768,-32768,-32768,-32768,    24,   -34,-32768,-32768,  -276,-32768,
-32768,-32768
};


#define	YYLAST		1371


static const short yytable[] = {    69,
   207,   183,   208,    19,    84,    28,    74,    94,   210,    90,
  -113,    28,    31,    69,   199,   200,   201,   202,   203,   204,
   205,   206,   353,   199,   200,   201,   202,   203,   204,   205,
   206,    19,   324,   454,   326,     3,   197,   462,   -53,    94,
   238,     4,    39,   331,   239,    98,    98,   468,   198,    43,
   464,     5,     6,     7,     8,    14,   234,    15,   373,   339,
   167,   182,   374,   186,   333,   187,    91,    98,    96,    45,
   374,    46,    47,   376,    92,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    14,   381,
    15,   374,    98,   407,   384,    75,    22,    23,    24,    25,
   375,    32,   374,   211,   212,    79,   214,   215,    85,    86,
   193,    62,    40,   196,   221,    82,    35,    36,    37,   424,
   -54,   453,   229,   230,   231,   232,   263,   265,    80,   266,
   463,   161,   162,     5,     6,     7,     8,   235,   236,    41,
    81,   213,   240,   241,   217,   218,   219,   -30,   -30,   220,
    89,   222,   223,   224,   225,   226,   227,   228,   -29,   -29,
    97,   268,   233,   199,   200,   201,   202,   203,   204,   205,
   206,   160,   -28,   -28,   289,   -27,   -27,   164,    63,   242,
   243,    64,   165,    69,    65,    93,   184,   185,   -40,   194,
    14,   188,    15,   237,   -34,   -33,   -32,   -31,   312,     4,
   -40,   -40,   -37,   -38,   245,   246,   332,    69,   313,   -40,
   -40,   -40,   -40,   273,   266,   -40,    16,   272,   290,   291,
   293,   294,    17,   295,   296,   297,   182,   182,   298,   182,
   370,   371,   372,   300,   301,   302,   316,   303,   304,   305,
   377,   378,   379,   380,   306,   314,   315,   311,   317,   318,
   319,   325,   327,   328,   182,   329,   330,   343,   344,   336,
   385,   386,   337,   479,   408,   348,   430,   452,   349,   334,
   340,   352,   341,   354,   441,   345,   465,   361,   359,   360,
   362,   363,   364,   368,   369,   392,   394,   410,   387,   456,
   182,   182,   182,   404,   438,   395,   439,   470,   397,   398,
   182,   182,   182,   182,   399,   350,   351,   409,   472,   268,
   355,   356,   357,   358,   474,   411,   457,   445,   412,   446,
   365,   475,   432,   413,   433,   434,   435,   436,   414,   419,
   421,   437,   207,   448,   208,   443,   444,   447,   449,   450,
   451,   405,   286,   287,   466,   480,   149,   182,   150,   207,
   467,   208,   471,    77,   288,   151,   153,    42,   190,   292,
   460,    88,   383,   261,    44,    20,   299,    34,   391,   428,
     0,     0,     0,     0,   307,   308,   309,   310,   406,     0,
     0,   431,   182,   262,   182,   182,   182,   182,     0,     0,
     0,     0,     0,   284,     0,     0,     0,     0,   284,   284,
     0,     0,     0,   422,   423,     0,   425,   426,   427,     0,
   284,     0,     0,     0,     0,   284,     0,     0,     0,     0,
   182,     0,   284,     0,     0,     0,     0,     0,   440,     0,
   284,   284,   284,   284,   342,     0,     0,     0,   347,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   458,   459,     0,     0,     0,   366,   367,   264,
    46,    47,     0,    92,    49,    50,    51,    52,    53,    54,
    55,    56,    57,    58,    59,    60,    61,    14,     0,    15,
     0,     0,     0,     0,     0,     0,     0,     0,   388,   389,
   284,     0,     0,   393,   284,     0,     0,     0,     0,   396,
    62,     0,     0,     0,     0,     0,   400,   401,   402,   403,
     0,     0,     0,   284,   284,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   415,   416,     0,     0,     0,     0,     0,   420,     0,     0,
     0,     0,     0,     0,   284,   284,     0,     0,     0,   284,
   429,     0,     0,     0,     0,   284,     0,     0,     0,     0,
     0,     0,   284,   284,   284,   284,   442,    63,     0,     0,
    64,     0,     0,    65,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   284,   284,     0,     0,
     0,     0,     0,   284,     0,     0,     0,   461,     0,     0,
     0,     0,     0,     0,     0,     0,   284,     0,     0,     0,
   469,     0,     0,     0,     0,   473,     0,     0,     0,   476,
   477,     0,   284,   274,   275,    46,    47,   276,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    14,     0,    15,     0,     0,   277,   278,     0,
     0,     0,     0,   284,     0,     0,     0,     0,   279,   280,
     0,     0,     0,     0,     0,     0,   284,     0,     0,     0,
     0,   284,     0,     0,     0,   284,   284,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
   111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
   121,   122,   123,   274,   275,     0,     0,   276,     0,     0,
     0,     0,     0,     0,   250,     0,   251,   252,     0,   138,
   139,     0,     0,     0,     0,     0,     0,   277,   278,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   279,   280,
     0,     0,     0,   281,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
   111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
   121,   122,   123,     0,    46,    47,     0,     0,     0,     0,
     0,     0,     0,     0,   250,     0,   251,   252,     0,   138,
   139,    14,     0,    15,     0,   247,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   248,   249,     0,
     0,     0,     0,   281,    46,    47,     0,    92,   168,   169,
   170,   171,   172,   173,   174,   175,   176,   177,   178,    60,
    61,    14,     0,    15,     0,     0,     0,     0,   110,   111,
   112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
   122,   123,     0,     0,    62,     0,     0,     0,     0,     0,
     0,     0,     0,   250,     0,   251,   252,     0,   138,   139,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   253,     0,     0,   254,     0,     0,
     0,   255,   256,    46,    47,     0,    92,    49,    50,    51,
    52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
    14,     0,    15,     0,     0,     0,     0,     0,     0,     0,
     0,    63,     0,     0,    64,   189,     0,    65,   323,     0,
     0,    46,    47,    62,    92,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    14,     0,
    15,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   267,     0,     0,     0,     0,     0,    46,
    47,    62,    92,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178,    60,    61,    14,     0,    15,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    63,     0,     0,    64,     0,     0,    65,    46,    47,    62,
    92,    49,    50,    51,    52,    53,    54,    55,    56,    57,
    58,    59,    60,    61,    14,     0,    15,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    63,   338,
     0,    64,     0,     0,    65,    46,    47,    62,    92,    49,
    50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
    60,    61,    14,     0,    15,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    63,   382,     0,    64,
     0,   320,    65,    46,    47,    62,    48,    49,    50,    51,
    52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
    14,     0,    15,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    63,     0,     0,    64,     0,     0,
    65,    46,    47,    62,    92,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    14,     0,
    15,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    63,     0,     0,    64,     0,     0,    65,    46,
    47,    62,    92,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178,    60,    61,    14,     0,    15,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    63,     0,     0,    64,     0,     0,    65,    46,    47,    62,
   195,    49,    50,    51,    52,    53,    54,    55,    56,    57,
    58,    59,    60,    61,    14,     0,    15,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    63,     0,
     0,    64,     0,     0,    65,     0,     0,    62,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    63,     0,     0,    64,
     0,     0,    65,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    99,   100,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
     0,     0,     0,     0,    63,   102,   103,    64,     0,     0,
    65,     0,     0,   104,   105,   106,   107,   108,   109,   110,
   111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
   121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
   131,   132,     0,     0,   133,   134,   135,   136,   137,   138,
   139,   140,   141,  -214,   142,   143,   144,   145,   146,   147,
   148
};

static const short yycheck[] = {    26,
   106,    88,   106,     2,    40,    23,    29,    65,   107,     4,
     0,    23,    30,    40,    10,    11,    12,    13,    14,    15,
    16,    17,   299,    10,    11,    12,    13,    14,    15,    16,
    17,    30,   254,   438,   256,    25,     9,   454,   113,    97,
   117,    31,   111,   117,   121,   120,   120,   464,    21,   111,
   455,    41,    42,    43,    44,    22,   155,    24,   117,   281,
    87,    88,   121,   119,   117,   121,    61,   120,    67,     4,
   121,     5,     6,   124,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,   117,
    24,   121,   120,   370,   124,   118,    57,    58,    59,    60,
   119,   119,   121,   130,   131,   111,   133,   134,    32,    33,
   101,    45,    20,   104,   141,   115,    49,    50,    51,   396,
   113,   117,   149,   150,   151,   152,   184,   185,   111,   187,
   117,    52,    53,    41,    42,    43,    44,    92,    93,    47,
   111,   132,    27,    28,   135,   136,   137,     3,     4,   140,
     4,   142,   143,   144,   145,   146,   147,   148,     3,     4,
   113,   188,   153,    10,    11,    12,    13,    14,    15,    16,
    17,    24,     3,     4,   210,     3,     4,     4,   112,     3,
     4,   115,    24,   210,   118,   119,   116,    62,    20,    55,
    22,   113,    24,   103,     4,     4,     4,     4,   234,    31,
    32,    33,     7,     7,     4,     7,   264,   234,   235,    41,
    42,    43,    44,   114,   272,    47,    48,   121,   121,   121,
   115,   121,    54,    36,   121,   121,   253,   254,   121,   256,
   317,   318,   319,   121,   121,   121,    24,   121,   121,   121,
   327,   328,   329,   330,   121,   236,   237,   121,   113,   113,
   113,    24,   113,   113,   281,   113,   113,    15,    15,   121,
    21,    21,   114,     0,    36,   115,   114,    21,   295,   268,
   121,   298,   121,   300,   117,   121,    21,   121,   305,   306,
   121,   121,   121,   121,   121,   121,   121,   374,   113,    36,
   317,   318,   319,   113,   115,   121,   114,    21,   121,   121,
   327,   328,   329,   330,   121,   296,   297,   121,    67,   336,
   301,   302,   303,   304,    21,   121,   117,   114,   121,   114,
   311,    21,   409,   121,   411,   412,   413,   414,   121,   121,
   121,   121,   438,   114,   438,   121,   121,   121,   114,   114,
   114,   368,   197,   198,   114,     0,    77,   374,    77,   455,
   121,   455,   121,    30,   209,    77,    77,    18,    97,   214,
   447,    42,   336,   182,    21,     2,   221,    13,   345,   404,
    -1,    -1,    -1,    -1,   229,   230,   231,   232,   369,    -1,
    -1,   408,   409,   182,   411,   412,   413,   414,    -1,    -1,
    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,   197,   198,
    -1,    -1,    -1,   394,   395,    -1,   397,   398,   399,    -1,
   209,    -1,    -1,    -1,    -1,   214,    -1,    -1,    -1,    -1,
   447,    -1,   221,    -1,    -1,    -1,    -1,    -1,   419,    -1,
   229,   230,   231,   232,   289,    -1,    -1,    -1,   293,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   443,   444,    -1,    -1,    -1,   312,   313,     4,
     5,     6,    -1,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    -1,    24,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,   344,
   289,    -1,    -1,   348,   293,    -1,    -1,    -1,    -1,   354,
    45,    -1,    -1,    -1,    -1,    -1,   361,   362,   363,   364,
    -1,    -1,    -1,   312,   313,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
   385,   386,    -1,    -1,    -1,    -1,    -1,   392,    -1,    -1,
    -1,    -1,    -1,    -1,   343,   344,    -1,    -1,    -1,   348,
   405,    -1,    -1,    -1,    -1,   354,    -1,    -1,    -1,    -1,
    -1,    -1,   361,   362,   363,   364,   421,   112,    -1,    -1,
   115,    -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   385,   386,    -1,    -1,
    -1,    -1,    -1,   392,    -1,    -1,    -1,   452,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   405,    -1,    -1,    -1,
   465,    -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,   474,
   475,    -1,   421,     3,     4,     5,     6,     7,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    22,    -1,    24,    -1,    -1,    27,    28,    -1,
    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    38,    39,
    -1,    -1,    -1,    -1,    -1,    -1,   465,    -1,    -1,    -1,
    -1,   470,    -1,    -1,    -1,   474,   475,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
    70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
    80,    81,    82,     3,     4,    -1,    -1,     7,    -1,    -1,
    -1,    -1,    -1,    -1,    94,    -1,    96,    97,    -1,    99,
   100,    -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
    -1,    -1,    -1,   123,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
    70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
    80,    81,    82,    -1,     5,     6,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    94,    -1,    96,    97,    -1,    99,
   100,    22,    -1,    24,    -1,    26,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    -1,
    -1,    -1,    -1,   123,     5,     6,    -1,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,    24,    -1,    -1,    -1,    -1,    69,    70,
    71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
    81,    82,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    94,    -1,    96,    97,    -1,    99,   100,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,    -1,    -1,
    -1,   122,   123,     5,     6,    -1,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,   112,    -1,    -1,   115,    37,    -1,   118,   119,    -1,
    -1,     5,     6,    45,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    37,    -1,    -1,    -1,    -1,    -1,     5,
     6,    45,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    -1,    24,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
   112,    -1,    -1,   115,    -1,    -1,   118,     5,     6,    45,
     8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    18,    19,    20,    21,    22,    -1,    24,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    37,
    -1,   115,    -1,    -1,   118,     5,     6,    45,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    -1,    24,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   112,    37,    -1,   115,
    -1,   117,   118,     5,     6,    45,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,    -1,    -1,
   118,     5,     6,    45,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   112,    -1,    -1,   115,    -1,    -1,   118,     5,
     6,    45,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    -1,    24,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
   112,    -1,    -1,   115,    -1,    -1,   118,     5,     6,    45,
     8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    18,    19,    20,    21,    22,    -1,    24,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
    -1,   115,    -1,    -1,   118,    -1,    -1,    45,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,
    -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    34,    35,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,   112,    55,    56,   115,    -1,    -1,
   118,    -1,    -1,    63,    64,    65,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
    80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
    90,    91,    -1,    -1,    94,    95,    96,    97,    98,    99,
   100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
   110
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 987 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  if (yyvsp[0].UIntVal > (uint32_t)INT32_MAX)     // Outside of my range!
    ThrowException("Value too large for type!");
  yyval.SIntVal = (int32_t)yyvsp[0].UIntVal;
;
    break;}
case 4:
#line 995 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  if (yyvsp[0].UInt64Val > (uint64_t)INT64_MAX)     // Outside of my range!
    ThrowException("Value too large for type!");
  yyval.SInt64Val = (int64_t)yyvsp[0].UInt64Val;
;
    break;}
case 39:
#line 1019 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.StrVal = yyvsp[-1].StrVal;
  ;
    break;}
case 40:
#line 1022 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.StrVal = 0;
  ;
    break;}
case 41:
#line 1026 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Linkage = GlobalValue::InternalLinkage; ;
    break;}
case 42:
#line 1027 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Linkage = GlobalValue::LinkOnceLinkage; ;
    break;}
case 43:
#line 1028 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Linkage = GlobalValue::WeakLinkage; ;
    break;}
case 44:
#line 1029 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Linkage = GlobalValue::AppendingLinkage; ;
    break;}
case 45:
#line 1030 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Linkage = GlobalValue::ExternalLinkage; ;
    break;}
case 46:
#line 1032 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.UIntVal = CallingConv::C; ;
    break;}
case 47:
#line 1033 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.UIntVal = CallingConv::C; ;
    break;}
case 48:
#line 1034 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.UIntVal = CallingConv::Fast; ;
    break;}
case 49:
#line 1035 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.UIntVal = CallingConv::Cold; ;
    break;}
case 50:
#line 1036 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
                   if ((unsigned)yyvsp[0].UInt64Val != yyvsp[0].UInt64Val)
                     ThrowException("Calling conv too large!");
                   yyval.UIntVal = yyvsp[0].UInt64Val;
                 ;
    break;}
case 52:
#line 1049 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType); ;
    break;}
case 54:
#line 1050 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType); ;
    break;}
case 55:
#line 1052 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!UpRefs.empty())
      ThrowException("Invalid upreference in type: " + (*yyvsp[0].TypeVal)->getDescription());
    yyval.TypeVal = yyvsp[0].TypeVal;
  ;
    break;}
case 69:
#line 1063 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TypeVal = new PATypeHolder(OpaqueType::get());
  ;
    break;}
case 70:
#line 1066 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType);
  ;
    break;}
case 71:
#line 1069 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{            // Named types are also simple types...
  yyval.TypeVal = new PATypeHolder(getTypeVal(yyvsp[0].ValIDVal));
;
    break;}
case 72:
#line 1075 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                   // Type UpReference
    if (yyvsp[0].UInt64Val > (uint64_t)~0U) ThrowException("Value out of range!");
    OpaqueType *OT = OpaqueType::get();        // Use temporary placeholder
    UpRefs.push_back(UpRefRecord((unsigned)yyvsp[0].UInt64Val, OT));  // Add to vector...
    yyval.TypeVal = new PATypeHolder(OT);
    UR_OUT("New Upreference!\n");
  ;
    break;}
case 73:
#line 1082 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{           // Function derived type?
    std::vector<const Type*> Params;
    for (std::list<llvm::PATypeHolder>::iterator I = yyvsp[-1].TypeList->begin(),
           E = yyvsp[-1].TypeList->end(); I != E; ++I)
      Params.push_back(*I);
    bool isVarArg = Params.size() && Params.back() == Type::VoidTy;
    if (isVarArg) Params.pop_back();

    yyval.TypeVal = new PATypeHolder(HandleUpRefs(FunctionType::get(*yyvsp[-3].TypeVal,Params,isVarArg)));
    delete yyvsp[-1].TypeList;      // Delete the argument list
    delete yyvsp[-3].TypeVal;      // Delete the return type handle
  ;
    break;}
case 74:
#line 1094 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{          // Sized array type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(ArrayType::get(*yyvsp[-1].TypeVal, (unsigned)yyvsp[-3].UInt64Val)));
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 75:
#line 1099 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{               // Vector type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(VectorType::get(*yyvsp[-1].TypeVal)));
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 76:
#line 1104 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{          // FixedVector type?
     const llvm::Type* ElemTy = yyvsp[-1].TypeVal->get();
     if ((unsigned)yyvsp[-2].UInt64Val != yyvsp[-2].UInt64Val) {
        ThrowException("Unsigned result not equal to signed result");
     }
     if(!ElemTy->isPrimitiveType()) {
        ThrowException("Element type of a FixedVectorType must be primitive");
     }
     yyval.TypeVal = new PATypeHolder(HandleUpRefs(FixedVectorType::get(*yyvsp[-1].TypeVal, (unsigned)yyvsp[-2].UInt64Val)));
     delete yyvsp[-1].TypeVal;
  ;
    break;}
case 77:
#line 1116 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                        // Structure type?
    std::vector<const Type*> Elements;
    for (std::list<llvm::PATypeHolder>::iterator I = yyvsp[-1].TypeList->begin(),
           E = yyvsp[-1].TypeList->end(); I != E; ++I)
      Elements.push_back(*I);

    yyval.TypeVal = new PATypeHolder(HandleUpRefs(StructType::get(Elements)));
    delete yyvsp[-1].TypeList;
  ;
    break;}
case 78:
#line 1125 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                                  // Empty structure type?
    yyval.TypeVal = new PATypeHolder(StructType::get(std::vector<const Type*>()));
  ;
    break;}
case 79:
#line 1128 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                             // Pointer type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(PointerType::get(*yyvsp[-1].TypeVal)));
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 80:
#line 1136 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TypeList = new std::list<PATypeHolder>();
    yyval.TypeList->push_back(*yyvsp[0].TypeVal); delete yyvsp[0].TypeVal;
  ;
    break;}
case 81:
#line 1140 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    (yyval.TypeList=yyvsp[-2].TypeList)->push_back(*yyvsp[0].TypeVal); delete yyvsp[0].TypeVal;
  ;
    break;}
case 83:
#line 1146 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    (yyval.TypeList=yyvsp[-2].TypeList)->push_back(Type::VoidTy);
  ;
    break;}
case 84:
#line 1149 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    (yyval.TypeList = new std::list<PATypeHolder>())->push_back(Type::VoidTy);
  ;
    break;}
case 85:
#line 1152 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TypeList = new std::list<PATypeHolder>();
  ;
    break;}
case 86:
#line 1162 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ // Nonempty unsized arr
    const ArrayType *ATy = dyn_cast<ArrayType>(yyvsp[-3].TypeVal->get());
    if (ATy == 0)
      ThrowException("Cannot make array constant with type: '" + 
                     (*yyvsp[-3].TypeVal)->getDescription() + "'!");
    const Type *ETy = ATy->getElementType();
    int NumElements = ATy->getNumElements();

    // Verify that we have the correct size...
    if (NumElements != -1 && NumElements != (int)yyvsp[-1].ConstVector->size())
      ThrowException("Type mismatch: constant sized array initialized with " +
                     utostr(yyvsp[-1].ConstVector->size()) +  " arguments, but has size of " + 
                     itostr(NumElements) + "!");

    // Verify all elements are correct type!
    for (unsigned i = 0; i < yyvsp[-1].ConstVector->size(); i++) {
      if (ETy != (*yyvsp[-1].ConstVector)[i]->getType())
        ThrowException("Element #" + utostr(i) + " is not of type '" + 
                       ETy->getDescription() +"' as required!\nIt is of type '"+
                       (*yyvsp[-1].ConstVector)[i]->getType()->getDescription() + "'.");
    }

    yyval.ConstVal = ConstantArray::get(ATy, *yyvsp[-1].ConstVector);
    delete yyvsp[-3].TypeVal; delete yyvsp[-1].ConstVector;
  ;
    break;}
case 87:
#line 1187 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const ArrayType *ATy = dyn_cast<ArrayType>(yyvsp[-2].TypeVal->get());
    if (ATy == 0)
      ThrowException("Cannot make array constant with type: '" + 
                     (*yyvsp[-2].TypeVal)->getDescription() + "'!");

    int NumElements = ATy->getNumElements();
    if (NumElements != -1 && NumElements != 0) 
      ThrowException("Type mismatch: constant sized array initialized with 0"
                     " arguments, but has size of " + itostr(NumElements) +"!");
    yyval.ConstVal = ConstantArray::get(ATy, std::vector<Constant*>());
    delete yyvsp[-2].TypeVal;
  ;
    break;}
case 88:
#line 1200 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const ArrayType *ATy = dyn_cast<ArrayType>(yyvsp[-2].TypeVal->get());
    if (ATy == 0)
      ThrowException("Cannot make array constant with type: '" + 
                     (*yyvsp[-2].TypeVal)->getDescription() + "'!");

    int NumElements = ATy->getNumElements();
    const Type *ETy = ATy->getElementType();
    char *EndStr = UnEscapeLexed(yyvsp[0].StrVal, true);
    if (NumElements != -1 && NumElements != (EndStr-yyvsp[0].StrVal))
      ThrowException("Can't build string constant of size " + 
                     itostr((int)(EndStr-yyvsp[0].StrVal)) +
                     " when array has size " + itostr(NumElements) + "!");
    std::vector<Constant*> Vals;
    if (ETy == Type::SByteTy) {
      for (char *C = yyvsp[0].StrVal; C != EndStr; ++C)
        Vals.push_back(ConstantSInt::get(ETy, *C));
    } else if (ETy == Type::UByteTy) {
      for (char *C = yyvsp[0].StrVal; C != EndStr; ++C)
        Vals.push_back(ConstantUInt::get(ETy, (unsigned char)*C));
    } else {
      free(yyvsp[0].StrVal);
      ThrowException("Cannot build string arrays of non byte sized elements!");
    }
    free(yyvsp[0].StrVal);
    yyval.ConstVal = ConstantArray::get(ATy, Vals);
    delete yyvsp[-2].TypeVal;
  ;
    break;}
case 89:
#line 1228 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ // Nonempty unsized arr
    const FixedVectorType *PTy = dyn_cast<FixedVectorType>(yyvsp[-3].TypeVal->get());
    if (PTy == 0)
      ThrowException("Cannot make vector constant with type: '" + 
                     (*yyvsp[-3].TypeVal)->getDescription() + "'!");
    const Type *ETy = PTy->getElementType();
    int NumElements = PTy->getNumElements();

    // Verify that we have the correct size...
    if (NumElements != -1 && NumElements != (int)yyvsp[-1].ConstVector->size())
      ThrowException("Type mismatch: constant sized vector initialized with " +
                     utostr(yyvsp[-1].ConstVector->size()) +  " arguments, but has size of " + 
                     itostr(NumElements) + "!");

    // Verify all elements are correct type!
    for (unsigned i = 0; i < yyvsp[-1].ConstVector->size(); i++) {
      if (ETy != (*yyvsp[-1].ConstVector)[i]->getType())
        ThrowException("Element #" + utostr(i) + " is not of type '" + 
           ETy->getDescription() +"' as required!\nIt is of type '"+
           (*yyvsp[-1].ConstVector)[i]->getType()->getDescription() + "'.");
    }

    yyval.ConstVal = ConstantVector::get(PTy, *yyvsp[-1].ConstVector);
    delete yyvsp[-3].TypeVal; delete yyvsp[-1].ConstVector;
  ;
    break;}
case 90:
#line 1253 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const StructType *STy = dyn_cast<StructType>(yyvsp[-3].TypeVal->get());
    if (STy == 0)
      ThrowException("Cannot make struct constant with type: '" + 
                     (*yyvsp[-3].TypeVal)->getDescription() + "'!");

    if (yyvsp[-1].ConstVector->size() != STy->getNumContainedTypes())
      ThrowException("Illegal number of initializers for structure type!");

    // Check to ensure that constants are compatible with the type initializer!
    for (unsigned i = 0, e = yyvsp[-1].ConstVector->size(); i != e; ++i)
      if ((*yyvsp[-1].ConstVector)[i]->getType() != STy->getElementType(i))
        ThrowException("Expected type '" +
                       STy->getElementType(i)->getDescription() +
                       "' for element #" + utostr(i) +
                       " of structure initializer!");

    yyval.ConstVal = ConstantStruct::get(STy, *yyvsp[-1].ConstVector);
    delete yyvsp[-3].TypeVal; delete yyvsp[-1].ConstVector;
  ;
    break;}
case 91:
#line 1273 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const StructType *STy = dyn_cast<StructType>(yyvsp[-2].TypeVal->get());
    if (STy == 0)
      ThrowException("Cannot make struct constant with type: '" + 
                     (*yyvsp[-2].TypeVal)->getDescription() + "'!");

    if (STy->getNumContainedTypes() != 0)
      ThrowException("Illegal number of initializers for structure type!");

    yyval.ConstVal = ConstantStruct::get(STy, std::vector<Constant*>());
    delete yyvsp[-2].TypeVal;
  ;
    break;}
case 92:
#line 1285 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const PointerType *PTy = dyn_cast<PointerType>(yyvsp[-1].TypeVal->get());
    if (PTy == 0)
      ThrowException("Cannot make null pointer constant with type: '" + 
                     (*yyvsp[-1].TypeVal)->getDescription() + "'!");

    yyval.ConstVal = ConstantPointerNull::get(PTy);
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 93:
#line 1294 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ConstVal = UndefValue::get(yyvsp[-1].TypeVal->get());
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 94:
#line 1298 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const PointerType *Ty = dyn_cast<PointerType>(yyvsp[-1].TypeVal->get());
    if (Ty == 0)
      ThrowException("Global const reference must be a pointer type!");

    // ConstExprs can exist in the body of a function, thus creating
    // GlobalValues whenever they refer to a variable.  Because we are in
    // the context of a function, getValNonImprovising will search the functions
    // symbol table instead of the module symbol table for the global symbol,
    // which throws things all off.  To get around this, we just tell
    // getValNonImprovising that we are at global scope here.
    //
    Function *SavedCurFn = CurFun.CurrentFunction;
    CurFun.CurrentFunction = 0;

    Value *V = getValNonImprovising(Ty, yyvsp[0].ValIDVal);

    CurFun.CurrentFunction = SavedCurFn;

    // If this is an initializer for a constant pointer, which is referencing a
    // (currently) undefined variable, create a stub now that shall be replaced
    // in the future with the right type of variable.
    //
    if (V == 0) {
      assert(isa<PointerType>(Ty) && "Globals may only be used as pointers!");
      const PointerType *PT = cast<PointerType>(Ty);

      // First check to see if the forward references value is already created!
      PerModuleInfo::GlobalRefsType::iterator I =
        CurModule.GlobalRefs.find(std::make_pair(PT, yyvsp[0].ValIDVal));
    
      if (I != CurModule.GlobalRefs.end()) {
        V = I->second;             // Placeholder already exists, use it...
        yyvsp[0].ValIDVal.destroy();
      } else {
        std::string Name;
        if (yyvsp[0].ValIDVal.Type == ValID::NameVal) Name = yyvsp[0].ValIDVal.Name;

        // Create the forward referenced global.
        GlobalValue *GV;
        if (const FunctionType *FTy = 
                 dyn_cast<FunctionType>(PT->getElementType())) {
          GV = new Function(FTy, GlobalValue::ExternalLinkage, Name,
                            CurModule.CurrentModule);
        } else {
          GV = new GlobalVariable(PT->getElementType(), false,
                                  GlobalValue::ExternalLinkage, 0,
                                  Name, CurModule.CurrentModule);
        }

        // Keep track of the fact that we have a forward ref to recycle it
        CurModule.GlobalRefs.insert(std::make_pair(std::make_pair(PT, yyvsp[0].ValIDVal), GV));
        V = GV;
      }
    }

    yyval.ConstVal = cast<GlobalValue>(V);
    delete yyvsp[-1].TypeVal;            // Free the type handle
  ;
    break;}
case 95:
#line 1357 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-1].TypeVal->get() != yyvsp[0].ConstVal->getType())
      ThrowException("Mismatched types for constant expression!");
    yyval.ConstVal = yyvsp[0].ConstVal;
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 96:
#line 1363 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const Type *Ty = yyvsp[-1].TypeVal->get();
    if (isa<FunctionType>(Ty) || Ty == Type::LabelTy || isa<OpaqueType>(Ty))
      ThrowException("Cannot create a null initialized value of this type!");
    yyval.ConstVal = Constant::getNullValue(Ty);
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 97:
#line 1371 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{      // integral constants
    if (!ConstantSInt::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].SInt64Val))
      ThrowException("Constant value doesn't fit in type!");
    yyval.ConstVal = ConstantSInt::get(yyvsp[-1].PrimType, yyvsp[0].SInt64Val);
  ;
    break;}
case 98:
#line 1376 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{            // integral constants
    if (!ConstantUInt::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].UInt64Val))
      ThrowException("Constant value doesn't fit in type!");
    yyval.ConstVal = ConstantUInt::get(yyvsp[-1].PrimType, yyvsp[0].UInt64Val);
  ;
    break;}
case 99:
#line 1381 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                      // Boolean constants
    yyval.ConstVal = ConstantBool::True;
  ;
    break;}
case 100:
#line 1384 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                     // Boolean constants
    yyval.ConstVal = ConstantBool::False;
  ;
    break;}
case 101:
#line 1387 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                   // Float & Double constants
    if (!ConstantFP::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].FPVal))
      ThrowException("Floating point constant invalid for type!!");
    yyval.ConstVal = ConstantFP::get(yyvsp[-1].PrimType, yyvsp[0].FPVal);
  ;
    break;}
case 102:
#line 1394 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!yyvsp[-3].ConstVal->getType()->isFirstClassType())
      ThrowException("cast constant expression from a non-primitive type: '" +
                     yyvsp[-3].ConstVal->getType()->getDescription() + "'!");
    if (!yyvsp[-1].TypeVal->get()->isFirstClassType())
      ThrowException("cast constant expression to a non-primitive type: '" +
                     yyvsp[-1].TypeVal->get()->getDescription() + "'!");
    yyval.ConstVal = ConstantExpr::getCast(yyvsp[-3].ConstVal, yyvsp[-1].TypeVal->get());
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 103:
#line 1404 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[-2].ConstVal->getType()))
      ThrowException("GetElementPtr requires a pointer operand!");

    // LLVM 1.2 and earlier used ubyte struct indices.  Convert any ubyte struct
    // indices to uint struct indices for compatibility.
    generic_gep_type_iterator<std::vector<Value*>::iterator>
      GTI = gep_type_begin(yyvsp[-2].ConstVal->getType(), yyvsp[-1].ValueList->begin(), yyvsp[-1].ValueList->end()),
      GTE = gep_type_end(yyvsp[-2].ConstVal->getType(), yyvsp[-1].ValueList->begin(), yyvsp[-1].ValueList->end());
    for (unsigned i = 0, e = yyvsp[-1].ValueList->size(); i != e && GTI != GTE; ++i, ++GTI)
      if (isa<StructType>(*GTI))        // Only change struct indices
        if (ConstantUInt *CUI = dyn_cast<ConstantUInt>((*yyvsp[-1].ValueList)[i]))
          if (CUI->getType() == Type::UByteTy)
            (*yyvsp[-1].ValueList)[i] = ConstantExpr::getCast(CUI, Type::UIntTy);

    const Type *IdxTy =
      GetElementPtrInst::getIndexedType(yyvsp[-2].ConstVal->getType(), *yyvsp[-1].ValueList, true);
    if (!IdxTy)
      ThrowException("Index list invalid for constant getelementptr!");

    std::vector<Constant*> IdxVec;
    for (unsigned i = 0, e = yyvsp[-1].ValueList->size(); i != e; ++i)
      if (Constant *C = dyn_cast<Constant>((*yyvsp[-1].ValueList)[i]))
        IdxVec.push_back(C);
      else
        ThrowException("Indices to constant getelementptr must be constants!");

    delete yyvsp[-1].ValueList;

    yyval.ConstVal = ConstantExpr::getGetElementPtr(yyvsp[-2].ConstVal, IdxVec);
  ;
    break;}
case 104:
#line 1435 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-5].ConstVal->getType() != Type::BoolTy)
      ThrowException("Select condition must be of boolean type!");
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("Select operand types must match!");
    yyval.ConstVal = ConstantExpr::getSelect(yyvsp[-5].ConstVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;
    break;}
case 105:
#line 1442 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("Binary operator types must match!");
    // HACK: llvm 1.3 and earlier used to emit invalid pointer constant exprs.
    // To retain backward compatibility with these early compilers, we emit a
    // cast to the appropriate integer type automatically if we are in the
    // broken case.  See PR424 for more information.
    if (!isa<PointerType>(yyvsp[-3].ConstVal->getType())) {
      yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
    } else {
      const Type *IntPtrTy = 0;
      switch (CurModule.CurrentModule->getPointerSize()) {
      case Module::Pointer32: IntPtrTy = Type::IntTy; break;
      case Module::Pointer64: IntPtrTy = Type::LongTy; break;
      default: ThrowException("invalid pointer binary constant expr!");
      }
      yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, ConstantExpr::getCast(yyvsp[-3].ConstVal, IntPtrTy),
                             ConstantExpr::getCast(yyvsp[-1].ConstVal, IntPtrTy));
      yyval.ConstVal = ConstantExpr::getCast(yyval.ConstVal, yyvsp[-3].ConstVal->getType());
    }
  ;
    break;}
case 106:
#line 1463 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("Logical operator types must match!");
    if (!yyvsp[-3].ConstVal->getType()->isIntegral() &&
	!yyvsp[-3].ConstVal->getType()->isIntegralVector())
      ThrowException("Logical operands must have integral types!");
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;
    break;}
case 107:
#line 1471 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("setcc operand types must match!");
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;
    break;}
case 108:
#line 1476 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-1].ConstVal->getType() != Type::UByteTy)
      ThrowException("Shift count for shift constant must be unsigned byte!");
    if (!yyvsp[-3].ConstVal->getType()->isInteger() &&
	!yyvsp[-3].ConstVal->getType()->isIntegerVector()) {
      ThrowException("Shift constant expression requires integer operand!");
    }
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].OtherOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;
    break;}
case 109:
#line 1488 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    (yyval.ConstVector = yyvsp[-2].ConstVector)->push_back(yyvsp[0].ConstVal);
  ;
    break;}
case 110:
#line 1491 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ConstVector = new std::vector<Constant*>();
    yyval.ConstVector->push_back(yyvsp[0].ConstVal);
  ;
    break;}
case 111:
#line 1498 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.BoolVal = false; ;
    break;}
case 112:
#line 1498 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.BoolVal = true; ;
    break;}
case 113:
#line 1508 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  yyval.ModuleVal = ParserResult = yyvsp[0].ModuleVal;
  CurModule.ModuleDone();
;
    break;}
case 114:
#line 1515 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
    CurFun.FunctionDone();
  ;
    break;}
case 115:
#line 1519 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
  ;
    break;}
case 116:
#line 1522 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
  ;
    break;}
case 117:
#line 1525 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ModuleVal = CurModule.CurrentModule;
    // Emit an error if there are any unresolved types left.
    if (!CurModule.LateResolveTypes.empty()) {
      const ValID &DID = CurModule.LateResolveTypes.begin()->first;
      if (DID.Type == ValID::NameVal)
        ThrowException("Reference to an undefined type: '"+DID.getName() + "'");
      else
        ThrowException("Reference to an undefined type: #" + itostr(DID.Num));
    }
  ;
    break;}
case 118:
#line 1538 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    // Eagerly resolve types.  This is not an optimization, this is a
    // requirement that is due to the fact that we could have this:
    //
    // %list = type { %list * }
    // %list = type { %list * }    ; repeated type decl
    //
    // If types are not resolved eagerly, then the two types will not be
    // determined to be the same type!
    //
    ResolveTypeTo(yyvsp[-2].StrVal, *yyvsp[0].TypeVal);

    if (!setTypeName(*yyvsp[0].TypeVal, yyvsp[-2].StrVal) && !yyvsp[-2].StrVal) {
      // If this is a named type that is not a redefinition, add it to the slot
      // table.
      CurModule.Types.push_back(*yyvsp[0].TypeVal);
    }

    delete yyvsp[0].TypeVal;
  ;
    break;}
case 119:
#line 1558 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{       // Function prototypes can be in const pool
  ;
    break;}
case 120:
#line 1560 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[0].ConstVal == 0) ThrowException("Global value initializer is not a constant!");
    ParseGlobalVariable(yyvsp[-3].StrVal, yyvsp[-2].Linkage, yyvsp[-1].BoolVal, yyvsp[0].ConstVal->getType(), yyvsp[0].ConstVal);
  ;
    break;}
case 121:
#line 1564 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    ParseGlobalVariable(yyvsp[-3].StrVal, GlobalValue::ExternalLinkage, yyvsp[-1].BoolVal, *yyvsp[0].TypeVal, 0);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 122:
#line 1568 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ 
  ;
    break;}
case 123:
#line 1570 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  ;
    break;}
case 124:
#line 1572 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ 
  ;
    break;}
case 125:
#line 1577 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Endianness = Module::BigEndian; ;
    break;}
case 126:
#line 1578 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.Endianness = Module::LittleEndian; ;
    break;}
case 127:
#line 1580 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    CurModule.CurrentModule->setEndianness(yyvsp[0].Endianness);
  ;
    break;}
case 128:
#line 1583 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[0].UInt64Val == 32)
      CurModule.CurrentModule->setPointerSize(Module::Pointer32);
    else if (yyvsp[0].UInt64Val == 64)
      CurModule.CurrentModule->setPointerSize(Module::Pointer64);
    else
      ThrowException("Invalid pointer size: '" + utostr(yyvsp[0].UInt64Val) + "'!");
  ;
    break;}
case 129:
#line 1591 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    CurModule.CurrentModule->setTargetTriple(yyvsp[0].StrVal);
    free(yyvsp[0].StrVal);
  ;
    break;}
case 131:
#line 1598 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
          CurModule.CurrentModule->addLibrary(yyvsp[0].StrVal);
          free(yyvsp[0].StrVal);
        ;
    break;}
case 132:
#line 1602 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
          CurModule.CurrentModule->addLibrary(yyvsp[0].StrVal);
          free(yyvsp[0].StrVal);
        ;
    break;}
case 133:
#line 1606 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
        ;
    break;}
case 137:
#line 1615 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.StrVal = 0; ;
    break;}
case 138:
#line 1617 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  if (*yyvsp[-1].TypeVal == Type::VoidTy)
    ThrowException("void typed arguments are invalid!");
  yyval.ArgVal = new std::pair<PATypeHolder*, char*>(yyvsp[-1].TypeVal, yyvsp[0].StrVal);
;
    break;}
case 139:
#line 1623 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = yyvsp[-2].ArgList;
    yyvsp[-2].ArgList->push_back(*yyvsp[0].ArgVal);
    delete yyvsp[0].ArgVal;
  ;
    break;}
case 140:
#line 1628 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = new std::vector<std::pair<PATypeHolder*,char*> >();
    yyval.ArgList->push_back(*yyvsp[0].ArgVal);
    delete yyvsp[0].ArgVal;
  ;
    break;}
case 141:
#line 1634 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = yyvsp[0].ArgList;
  ;
    break;}
case 142:
#line 1637 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = yyvsp[-2].ArgList;
    yyval.ArgList->push_back(std::pair<PATypeHolder*,
                            char*>(new PATypeHolder(Type::VoidTy), 0));
  ;
    break;}
case 143:
#line 1642 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = new std::vector<std::pair<PATypeHolder*,char*> >();
    yyval.ArgList->push_back(std::make_pair(new PATypeHolder(Type::VoidTy), (char*)0));
  ;
    break;}
case 144:
#line 1646 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ArgList = 0;
  ;
    break;}
case 145:
#line 1650 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  UnEscapeLexed(yyvsp[-3].StrVal);
  std::string FunctionName(yyvsp[-3].StrVal);
  free(yyvsp[-3].StrVal);  // Free strdup'd memory!
  
  if (!(*yyvsp[-4].TypeVal)->isFirstClassType() && *yyvsp[-4].TypeVal != Type::VoidTy)
    ThrowException("LLVM functions cannot return aggregate types!");

  std::vector<const Type*> ParamTypeList;
  if (yyvsp[-1].ArgList) {   // If there are arguments...
    for (std::vector<std::pair<PATypeHolder*,char*> >::iterator I = yyvsp[-1].ArgList->begin();
         I != yyvsp[-1].ArgList->end(); ++I)
      ParamTypeList.push_back(I->first->get());
  }

  bool isVarArg = ParamTypeList.size() && ParamTypeList.back() == Type::VoidTy;
  if (isVarArg) ParamTypeList.pop_back();

  const FunctionType *FT = FunctionType::get(*yyvsp[-4].TypeVal, ParamTypeList, isVarArg);
  const PointerType *PFT = PointerType::get(FT);
  delete yyvsp[-4].TypeVal;

  ValID ID;
  if (!FunctionName.empty()) {
    ID = ValID::create((char*)FunctionName.c_str());
  } else {
    ID = ValID::create((int)CurModule.Values[PFT].size());
  }

  Function *Fn = 0;
  // See if this function was forward referenced.  If so, recycle the object.
  if (GlobalValue *FWRef = CurModule.GetForwardRefForGlobal(PFT, ID)) {
    // Move the function to the end of the list, from whereever it was 
    // previously inserted.
    Fn = cast<Function>(FWRef);
    CurModule.CurrentModule->getFunctionList().remove(Fn);
    CurModule.CurrentModule->getFunctionList().push_back(Fn);
  } else if (!FunctionName.empty() &&     // Merge with an earlier prototype?
             (Fn = CurModule.CurrentModule->getFunction(FunctionName, FT))) {
    // If this is the case, either we need to be a forward decl, or it needs 
    // to be.
    if (!CurFun.isDeclare && !Fn->isExternal())
      ThrowException("Redefinition of function '" + FunctionName + "'!");
    
    // Make sure to strip off any argument names so we can't get conflicts.
    if (Fn->isExternal())
      for (Function::arg_iterator AI = Fn->arg_begin(), AE = Fn->arg_end();
           AI != AE; ++AI)
        AI->setName("");

  } else  {  // Not already defined?
    Fn = new Function(FT, GlobalValue::ExternalLinkage, FunctionName,
                      CurModule.CurrentModule);
    InsertValue(Fn, CurModule.Values);
  }

  CurFun.FunctionStart(Fn);
  Fn->setCallingConv(yyvsp[-5].UIntVal);

  // Add all of the arguments we parsed to the function...
  if (yyvsp[-1].ArgList) {                     // Is null if empty...
    if (isVarArg) {  // Nuke the last entry
      assert(yyvsp[-1].ArgList->back().first->get() == Type::VoidTy && yyvsp[-1].ArgList->back().second == 0&&
             "Not a varargs marker!");
      delete yyvsp[-1].ArgList->back().first;
      yyvsp[-1].ArgList->pop_back();  // Delete the last entry
    }
    Function::arg_iterator ArgIt = Fn->arg_begin();
    for (std::vector<std::pair<PATypeHolder*,char*> >::iterator I = yyvsp[-1].ArgList->begin();
         I != yyvsp[-1].ArgList->end(); ++I, ++ArgIt) {
      delete I->first;                          // Delete the typeholder...

      setValueName(ArgIt, I->second);           // Insert arg into symtab...
      InsertValue(ArgIt);
    }

    delete yyvsp[-1].ArgList;                     // We're now done with the argument list
  }
;
    break;}
case 148:
#line 1732 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  yyval.FunctionVal = CurFun.CurrentFunction;

  // Make sure that we keep track of the linkage type even if there was a
  // previous "declare".
  yyval.FunctionVal->setLinkage(yyvsp[-2].Linkage);
;
    break;}
case 151:
#line 1742 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  yyval.FunctionVal = yyvsp[-1].FunctionVal;
;
    break;}
case 152:
#line 1746 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ CurFun.isDeclare = true; ;
    break;}
case 153:
#line 1746 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  yyval.FunctionVal = CurFun.CurrentFunction;
  CurFun.FunctionDone();
;
    break;}
case 154:
#line 1755 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{    // A reference to a direct constant
    yyval.ValIDVal = ValID::create(yyvsp[0].SInt64Val);
  ;
    break;}
case 155:
#line 1758 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::create(yyvsp[0].UInt64Val);
  ;
    break;}
case 156:
#line 1761 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                     // Perhaps it's an FP constant?
    yyval.ValIDVal = ValID::create(yyvsp[0].FPVal);
  ;
    break;}
case 157:
#line 1764 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::create(ConstantBool::True);
  ;
    break;}
case 158:
#line 1767 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::create(ConstantBool::False);
  ;
    break;}
case 159:
#line 1770 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::createNull();
  ;
    break;}
case 160:
#line 1773 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::createUndef();
  ;
    break;}
case 161:
#line 1776 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ // Nonempty unsized vector
    const Type *ETy = (*yyvsp[-1].ConstVector)[0]->getType();
    int NumElements = yyvsp[-1].ConstVector->size(); 
    
    FixedVectorType* pt = FixedVectorType::get(ETy, NumElements);
    PATypeHolder* PTy = new PATypeHolder(
                                         HandleUpRefs(
                                            FixedVectorType::get(
                                                ETy, 
                                                NumElements)
                                            )
                                         );
    
    // Verify all elements are correct type!
    for (unsigned i = 0; i < yyvsp[-1].ConstVector->size(); i++) {
      if (ETy != (*yyvsp[-1].ConstVector)[i]->getType())
        ThrowException("Element #" + utostr(i) + " is not of type '" + 
                     ETy->getDescription() +"' as required!\nIt is of type '" +
                     (*yyvsp[-1].ConstVector)[i]->getType()->getDescription() + "'.");
    }

    yyval.ValIDVal = ValID::create(ConstantVector::get(pt, *yyvsp[-1].ConstVector));
    delete PTy; delete yyvsp[-1].ConstVector;
  ;
    break;}
case 162:
#line 1800 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValIDVal = ValID::create(yyvsp[0].ConstVal);
  ;
    break;}
case 163:
#line 1807 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{  // Is it an integer reference...?
    yyval.ValIDVal = ValID::create(yyvsp[0].SIntVal);
  ;
    break;}
case 164:
#line 1810 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                   // Is it a named reference...?
    yyval.ValIDVal = ValID::create(yyvsp[0].StrVal);
  ;
    break;}
case 167:
#line 1821 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValueVal = getVal(*yyvsp[-1].TypeVal, yyvsp[0].ValIDVal); delete yyvsp[-1].TypeVal;
  ;
    break;}
case 168:
#line 1825 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.FunctionVal = yyvsp[-1].FunctionVal;
  ;
    break;}
case 169:
#line 1828 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ // Do not allow functions with 0 basic blocks   
    yyval.FunctionVal = yyvsp[-1].FunctionVal;
  ;
    break;}
case 170:
#line 1836 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    setValueName(yyvsp[0].TermInstVal, yyvsp[-1].StrVal);
    InsertValue(yyvsp[0].TermInstVal);

    yyvsp[-2].BasicBlockVal->getInstList().push_back(yyvsp[0].TermInstVal);
    InsertValue(yyvsp[-2].BasicBlockVal);
    yyval.BasicBlockVal = yyvsp[-2].BasicBlockVal;
  ;
    break;}
case 171:
#line 1845 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyvsp[-1].BasicBlockVal->getInstList().push_back(yyvsp[0].InstVal);
    yyval.BasicBlockVal = yyvsp[-1].BasicBlockVal;
  ;
    break;}
case 172:
#line 1849 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BasicBlockVal = CurBB = getBBVal(ValID::create((int)CurFun.NextBBNum++), true);

    // Make sure to move the basic block to the correct location in the
    // function, instead of leaving it inserted wherever it was first
    // referenced.
    Function::BasicBlockListType &BBL = 
      CurFun.CurrentFunction->getBasicBlockList();
    BBL.splice(BBL.end(), BBL, yyval.BasicBlockVal);
  ;
    break;}
case 173:
#line 1859 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BasicBlockVal = CurBB = getBBVal(ValID::create(yyvsp[0].StrVal), true);

    // Make sure to move the basic block to the correct location in the
    // function, instead of leaving it inserted wherever it was first
    // referenced.
    Function::BasicBlockListType &BBL = 
      CurFun.CurrentFunction->getBasicBlockList();
    BBL.splice(BBL.end(), BBL, yyval.BasicBlockVal);
  ;
    break;}
case 174:
#line 1870 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{              // Return with a result...
    yyval.TermInstVal = new ReturnInst(yyvsp[0].ValueVal);
  ;
    break;}
case 175:
#line 1873 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                                       // Return with no result...
    yyval.TermInstVal = new ReturnInst();
  ;
    break;}
case 176:
#line 1876 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{                         // Unconditional Branch...
    yyval.TermInstVal = new BranchInst(getBBVal(yyvsp[0].ValIDVal));
  ;
    break;}
case 177:
#line 1879 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{  
    yyval.TermInstVal = new BranchInst(getBBVal(yyvsp[-3].ValIDVal), getBBVal(yyvsp[0].ValIDVal), getVal(Type::BoolTy, yyvsp[-6].ValIDVal));
  ;
    break;}
case 178:
#line 1882 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    SwitchInst *S = new SwitchInst(getVal(yyvsp[-7].PrimType, yyvsp[-6].ValIDVal), getBBVal(yyvsp[-3].ValIDVal), yyvsp[-1].JumpTable->size());
    yyval.TermInstVal = S;

    std::vector<std::pair<Constant*,BasicBlock*> >::iterator I = yyvsp[-1].JumpTable->begin(),
      E = yyvsp[-1].JumpTable->end();
    for (; I != E; ++I) {
      if (ConstantInt *CI = dyn_cast<ConstantInt>(I->first))
          S->addCase(CI, I->second);
      else
        ThrowException("Switch case is constant, but not a simple integer!");
    }
    delete yyvsp[-1].JumpTable;
  ;
    break;}
case 179:
#line 1896 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    SwitchInst *S = new SwitchInst(getVal(yyvsp[-6].PrimType, yyvsp[-5].ValIDVal), getBBVal(yyvsp[-2].ValIDVal), 0);
    yyval.TermInstVal = S;
  ;
    break;}
case 180:
#line 1901 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const PointerType *PFTy;
    const FunctionType *Ty;

    if (!(PFTy = dyn_cast<PointerType>(yyvsp[-10].TypeVal->get())) ||
        !(Ty = dyn_cast<FunctionType>(PFTy->getElementType()))) {
      // Pull out the types of all of the arguments...
      std::vector<const Type*> ParamTypes;
      if (yyvsp[-7].ValueList) {
        for (std::vector<Value*>::iterator I = yyvsp[-7].ValueList->begin(), E = yyvsp[-7].ValueList->end();
             I != E; ++I)
          ParamTypes.push_back((*I)->getType());
      }

      bool isVarArg = ParamTypes.size() && ParamTypes.back() == Type::VoidTy;
      if (isVarArg) ParamTypes.pop_back();

      Ty = FunctionType::get(yyvsp[-10].TypeVal->get(), ParamTypes, isVarArg);
      PFTy = PointerType::get(Ty);
    }

    Value *V = getVal(PFTy, yyvsp[-9].ValIDVal);   // Get the function we're calling...

    BasicBlock *Normal = getBBVal(yyvsp[-3].ValIDVal);
    BasicBlock *Except = getBBVal(yyvsp[0].ValIDVal);

    // Create the call node...
    if (!yyvsp[-7].ValueList) {                                   // Has no arguments?
      yyval.TermInstVal = new InvokeInst(V, Normal, Except, std::vector<Value*>());
    } else {                                     // Has arguments?
      // Loop through FunctionType's arguments and ensure they are specified
      // correctly!
      //
      FunctionType::param_iterator I = Ty->param_begin();
      FunctionType::param_iterator E = Ty->param_end();
      std::vector<Value*>::iterator ArgI = yyvsp[-7].ValueList->begin(), ArgE = yyvsp[-7].ValueList->end();

      for (; ArgI != ArgE && I != E; ++ArgI, ++I)
        if ((*ArgI)->getType() != *I)
          ThrowException("Parameter " +(*ArgI)->getName()+ " is not of type '" +
                         (*I)->getDescription() + "'!");

      if (I != E || (ArgI != ArgE && !Ty->isVarArg()))
        ThrowException("Invalid number of parameters detected!");

      yyval.TermInstVal = new InvokeInst(V, Normal, Except, *yyvsp[-7].ValueList);
    }
    cast<InvokeInst>(yyval.TermInstVal)->setCallingConv(yyvsp[-11].UIntVal);
  
    delete yyvsp[-10].TypeVal;
    delete yyvsp[-7].ValueList;
  ;
    break;}
case 181:
#line 1953 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TermInstVal = new UnwindInst();
  ;
    break;}
case 182:
#line 1956 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.TermInstVal = new UnreachableInst();
  ;
    break;}
case 183:
#line 1962 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.JumpTable = yyvsp[-5].JumpTable;
    Constant *V = cast<Constant>(getValNonImprovising(yyvsp[-4].PrimType, yyvsp[-3].ValIDVal));
    if (V == 0)
      ThrowException("May only switch on a constant pool value!");

    yyval.JumpTable->push_back(std::make_pair(V, getBBVal(yyvsp[0].ValIDVal)));
  ;
    break;}
case 184:
#line 1970 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.JumpTable = new std::vector<std::pair<Constant*, BasicBlock*> >();
    Constant *V = cast<Constant>(getValNonImprovising(yyvsp[-4].PrimType, yyvsp[-3].ValIDVal));

    if (V == 0)
      ThrowException("May only switch on a constant pool value!");

    yyval.JumpTable->push_back(std::make_pair(V, getBBVal(yyvsp[0].ValIDVal)));
  ;
    break;}
case 185:
#line 1980 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
  // Is this definition named?? if so, assign the name...
  setValueName(yyvsp[0].InstVal, yyvsp[-1].StrVal);
  InsertValue(yyvsp[0].InstVal);
  yyval.InstVal = yyvsp[0].InstVal;
;
    break;}
case 186:
#line 1987 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{    // Used for PHI nodes
    yyval.PHIList = new std::list<std::pair<Value*, BasicBlock*> >();
    yyval.PHIList->push_back(std::make_pair(getVal(*yyvsp[-5].TypeVal, yyvsp[-3].ValIDVal), getBBVal(yyvsp[-1].ValIDVal)));
    delete yyvsp[-5].TypeVal;
  ;
    break;}
case 187:
#line 1992 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.PHIList = yyvsp[-6].PHIList;
    yyvsp[-6].PHIList->push_back(std::make_pair(getVal(yyvsp[-6].PHIList->front().first->getType(), yyvsp[-3].ValIDVal),
                                 getBBVal(yyvsp[-1].ValIDVal)));
  ;
    break;}
case 188:
#line 1998 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{    // Used for call statements, and memory insts...
    yyval.ValueList = new std::vector<Value*>();
    yyval.ValueList->push_back(yyvsp[0].ValueVal);
  ;
    break;}
case 189:
#line 2002 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.ValueList = yyvsp[-2].ValueList;
    yyvsp[-2].ValueList->push_back(yyvsp[0].ValueVal);
  ;
    break;}
case 191:
#line 2008 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ yyval.ValueList = 0; ;
    break;}
case 192:
#line 2010 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = true;
  ;
    break;}
case 193:
#line 2013 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = false;
  ;
    break;}
case 194:
#line 2019 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!(*yyvsp[-3].TypeVal)->isInteger() && !(*yyvsp[-3].TypeVal)->isFloatingPoint() && 
        !isa<VectorType>((*yyvsp[-3].TypeVal).get()))
      ThrowException("Arithmetic operator requires integer, FP, or vector operands!");
    if(isa<FixedVectorType>((*yyvsp[-3].TypeVal).get()) && yyvsp[-4].BinaryOpVal == Instruction::Rem) {
      ThrowException(
        "Rem not supported on fixed vector types!");     // FIXME:  Why not?!
    }
    yyval.InstVal = BinaryOperator::create(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 195:
#line 2032 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!(*yyvsp[-3].TypeVal)->isIntegral() &&
	!(*yyvsp[-3].TypeVal)->isIntegralVector())
      ThrowException("Logical operator requires integral operands!");
    yyval.InstVal = BinaryOperator::create(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 196:
#line 2041 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new SetCondInst(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 197:
#line 2047 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new SetCondInst(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 198:
#line 2053 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    std::cerr << "WARNING: Use of eliminated 'not' instruction:"
              << " Replacing with 'xor'.\n";

    Value *Ones = ConstantIntegral::getAllOnesValue(yyvsp[0].ValueVal->getType());
    if (Ones == 0)
      ThrowException("Expected integral type for not instruction!");

    yyval.InstVal = BinaryOperator::create(Instruction::Xor, yyvsp[0].ValueVal, Ones);
    if (yyval.InstVal == 0)
      ThrowException("Could not create a xor instruction!");
  ;
    break;}
case 199:
#line 2065 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[0].ValueVal->getType() != Type::UByteTy)
      ThrowException("Shift amount must be ubyte!");
    if (!yyvsp[-2].ValueVal->getType()->isInteger() &&
	!yyvsp[-2].ValueVal->getType()->isIntegerVector())
      ThrowException("Shift requires integer operand!");
    yyval.InstVal = new ShiftInst(yyvsp[-3].OtherOpVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 200:
#line 2073 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!yyvsp[0].TypeVal->get()->isFirstClassType())
      ThrowException("cast instruction to a non-primitive type: '" +
                     yyvsp[0].TypeVal->get()->getDescription() + "'!");
    yyval.InstVal = new CastInst(yyvsp[-2].ValueVal, *yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 201:
#line 2080 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[-4].ValueVal->getType() != Type::BoolTy)
      ThrowException("select condition must be boolean!");
    if (yyvsp[-2].ValueVal->getType() != yyvsp[0].ValueVal->getType())
      ThrowException("select value types should match!");
    yyval.InstVal = new SelectInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 202:
#line 2087 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!yyvsp[-4].ValueVal->getType()->isBooleanVector())
      ThrowException("vselect condition must be boolean vector!");
    if (yyvsp[-2].ValueVal->getType() != yyvsp[0].ValueVal->getType())
      ThrowException("vselect value types should match!");
    if (!isa<VectorType>(yyvsp[-2].ValueVal->getType()))
      ThrowException("vselect value must be a vector!");
    yyval.InstVal = new VSelectInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 203:
#line 2096 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    NewVarArgs = true;
    yyval.InstVal = new VAArgInst(yyvsp[-2].ValueVal, *yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 204:
#line 2101 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    ObsoleteVarArgs = true;
    const Type* ArgTy = yyvsp[-2].ValueVal->getType();
    Function* NF = CurModule.CurrentModule->
      getOrInsertFunction("llvm.va_copy", ArgTy, ArgTy, 0);

    //b = vaarg a, t -> 
    //foo = alloca 1 of t
    //bar = vacopy a 
    //store bar -> foo
    //b = vaarg foo, t
    AllocaInst* foo = new AllocaInst(ArgTy, 0, "vaarg.fix");
    CurBB->getInstList().push_back(foo);
    CallInst* bar = new CallInst(NF, yyvsp[-2].ValueVal);
    CurBB->getInstList().push_back(bar);
    CurBB->getInstList().push_back(new StoreInst(bar, foo));
    yyval.InstVal = new VAArgInst(foo, *yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 205:
#line 2120 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    ObsoleteVarArgs = true;
    const Type* ArgTy = yyvsp[-2].ValueVal->getType();
    Function* NF = CurModule.CurrentModule->
      getOrInsertFunction("llvm.va_copy", ArgTy, ArgTy, 0);

    //b = vanext a, t ->
    //foo = alloca 1 of t
    //bar = vacopy a
    //store bar -> foo
    //tmp = vaarg foo, t
    //b = load foo
    AllocaInst* foo = new AllocaInst(ArgTy, 0, "vanext.fix");
    CurBB->getInstList().push_back(foo);
    CallInst* bar = new CallInst(NF, yyvsp[-2].ValueVal);
    CurBB->getInstList().push_back(bar);
    CurBB->getInstList().push_back(new StoreInst(bar, foo));
    Instruction* tmp = new VAArgInst(foo, *yyvsp[0].TypeVal);
    CurBB->getInstList().push_back(tmp);
    yyval.InstVal = new LoadInst(foo);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 206:
#line 2142 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const Type *Ty = yyvsp[0].PHIList->front().first->getType();
    if (!Ty->isFirstClassType())
      ThrowException("PHI node operands must be of first class type!");
    yyval.InstVal = new PHINode(Ty);
    ((PHINode*)yyval.InstVal)->reserveOperandSpace(yyvsp[0].PHIList->size());
    while (yyvsp[0].PHIList->begin() != yyvsp[0].PHIList->end()) {
      if (yyvsp[0].PHIList->front().first->getType() != Ty) 
        ThrowException("All elements of a PHI node must be of the same type!");
      cast<PHINode>(yyval.InstVal)->addIncoming(yyvsp[0].PHIList->front().first, yyvsp[0].PHIList->front().second);
      yyvsp[0].PHIList->pop_front();
    }
    delete yyvsp[0].PHIList;  // Free the list...
  ;
    break;}
case 207:
#line 2156 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const PointerType *PFTy;
    const FunctionType *Ty;

    if (!(PFTy = dyn_cast<PointerType>(yyvsp[-4].TypeVal->get())) ||
        !(Ty = dyn_cast<FunctionType>(PFTy->getElementType()))) {
      // Pull out the types of all of the arguments...
      std::vector<const Type*> ParamTypes;
      if (yyvsp[-1].ValueList) {
        for (std::vector<Value*>::iterator I = yyvsp[-1].ValueList->begin(), E = yyvsp[-1].ValueList->end();
             I != E; ++I)
          ParamTypes.push_back((*I)->getType());
      }

      bool isVarArg = ParamTypes.size() && ParamTypes.back() == Type::VoidTy;
      if (isVarArg) ParamTypes.pop_back();

      if (!(*yyvsp[-4].TypeVal)->isFirstClassType() && *yyvsp[-4].TypeVal != Type::VoidTy)
        ThrowException("LLVM functions cannot return aggregate types!");

      Ty = FunctionType::get(yyvsp[-4].TypeVal->get(), ParamTypes, isVarArg);
      PFTy = PointerType::get(Ty);
    }

    Value *V = getVal(PFTy, yyvsp[-3].ValIDVal);   // Get the function we're calling...

    // Create the call node...
    if (!yyvsp[-1].ValueList) {                                   // Has no arguments?
      // Make sure no arguments is a good thing!
      if (Ty->getNumParams() != 0)
        ThrowException("No arguments passed to a function that "
                       "expects arguments!");

      yyval.InstVal = new CallInst(V, std::vector<Value*>());
    } else {                                     // Has arguments?
      // Loop through FunctionType's arguments and ensure they are specified
      // correctly!
      //
      FunctionType::param_iterator I = Ty->param_begin();
      FunctionType::param_iterator E = Ty->param_end();
      std::vector<Value*>::iterator ArgI = yyvsp[-1].ValueList->begin(), ArgE = yyvsp[-1].ValueList->end();

      for (; ArgI != ArgE && I != E; ++ArgI, ++I)
        if ((*ArgI)->getType() != *I)
          ThrowException("Parameter " +(*ArgI)->getName()+ " is not of type '" +
                         (*I)->getDescription() + "'!");

      if (I != E || (ArgI != ArgE && !Ty->isVarArg()))
        ThrowException("Invalid number of parameters detected!");

      yyval.InstVal = new CallInst(V, *yyvsp[-1].ValueList);
    }
    cast<CallInst>(yyval.InstVal)->setTailCall(yyvsp[-6].BoolVal);
    cast<CallInst>(yyval.InstVal)->setCallingConv(yyvsp[-5].UIntVal);
    delete yyvsp[-4].TypeVal;
    delete yyvsp[-1].ValueList;
  ;
    break;}
case 208:
#line 2213 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = yyvsp[0].InstVal;
  ;
    break;}
case 209:
#line 2219 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ 
    yyval.ValueList = yyvsp[0].ValueList; 
  ;
    break;}
case 210:
#line 2221 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{ 
    yyval.ValueList = new std::vector<Value*>(); 
  ;
    break;}
case 211:
#line 2225 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = true;
  ;
    break;}
case 212:
#line 2228 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = false;
  ;
    break;}
case 213:
#line 2232 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = true;
  ;
    break;}
case 214:
#line 2235 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.BoolVal = false;
  ;
    break;}
case 215:
#line 2240 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new MallocInst(*yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 216:
#line 2244 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new MallocInst(*yyvsp[-3].TypeVal, getVal(yyvsp[-1].PrimType, yyvsp[0].ValIDVal));
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 217:
#line 2248 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new AllocaInst(*yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;
    break;}
case 218:
#line 2252 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    yyval.InstVal = new AllocaInst(*yyvsp[-3].TypeVal, getVal(yyvsp[-1].PrimType, yyvsp[0].ValIDVal));
    delete yyvsp[-3].TypeVal;
  ;
    break;}
case 219:
#line 2256 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[0].ValueVal->getType()))
      ThrowException("Trying to free nonpointer type " + 
                     yyvsp[0].ValueVal->getType()->getDescription() + "!");
    yyval.InstVal = new FreeInst(yyvsp[0].ValueVal);
  ;
    break;}
case 220:
#line 2263 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[-1].TypeVal->get()))
      ThrowException("Can't load from nonpointer type: " +
                     (*yyvsp[-1].TypeVal)->getDescription());
    if (!cast<PointerType>(yyvsp[-1].TypeVal->get())->getElementType()->isFirstClassType())
      ThrowException("Can't load from pointer of non-first-class type: " +
                     (*yyvsp[-1].TypeVal)->getDescription());
    yyval.InstVal = new LoadInst(getVal(*yyvsp[-1].TypeVal, yyvsp[0].ValIDVal), "", yyvsp[-3].BoolVal);
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 221:
#line 2274 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    const PointerType *PT = dyn_cast<PointerType>(yyvsp[-1].TypeVal->get());
    if (!PT)
      ThrowException("Can't store to a nonpointer type: " +
                     (*yyvsp[-1].TypeVal)->getDescription());
    const Type *ElTy = PT->getElementType();
    if (ElTy != yyvsp[-3].ValueVal->getType())
      ThrowException("Can't store '" + yyvsp[-3].ValueVal->getType()->getDescription() +
                     "' into space of type '" + ElTy->getDescription() + "'!");

    yyval.InstVal = new StoreInst(yyvsp[-3].ValueVal, getVal(*yyvsp[-1].TypeVal, yyvsp[0].ValIDVal), yyvsp[-5].BoolVal);
    delete yyvsp[-1].TypeVal;
  ;
    break;}
case 222:
#line 2288 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[-2].TypeVal->get()))
      ThrowException("getelementptr insn requires pointer operand!");

    // LLVM 1.2 and earlier used ubyte struct indices.  Convert any ubyte struct
    // indices to uint struct indices for compatibility.
    generic_gep_type_iterator<std::vector<Value*>::iterator>
      GTI = gep_type_begin(yyvsp[-2].TypeVal->get(), yyvsp[0].ValueList->begin(), yyvsp[0].ValueList->end()),
      GTE = gep_type_end(yyvsp[-2].TypeVal->get(), yyvsp[0].ValueList->begin(), yyvsp[0].ValueList->end());
    for (unsigned i = 0, e = yyvsp[0].ValueList->size(); i != e && GTI != GTE; ++i, ++GTI)
      if (isa<StructType>(*GTI))        // Only change struct indices
        if (ConstantUInt *CUI = dyn_cast<ConstantUInt>((*yyvsp[0].ValueList)[i]))
          if (CUI->getType() == Type::UByteTy)
            (*yyvsp[0].ValueList)[i] = ConstantExpr::getCast(CUI, Type::UIntTy);

    if (!GetElementPtrInst::getIndexedType(*yyvsp[-2].TypeVal, *yyvsp[0].ValueList, true))
      ThrowException("Invalid getelementptr indices for type '" +
                     (*yyvsp[-2].TypeVal)->getDescription()+ "'!");
    yyval.InstVal = new GetElementPtrInst(getVal(*yyvsp[-2].TypeVal, yyvsp[-1].ValIDVal), *yyvsp[0].ValueList);
    delete yyvsp[-2].TypeVal; delete yyvsp[0].ValueList;
  ;
    break;}
case 223:
#line 2310 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[-2].TypeVal->get()))
      ThrowException("Can't load vector from nonpointer type: " +
		     (*yyvsp[-2].TypeVal)->getDescription());
    if (!cast<PointerType>(yyvsp[-2].TypeVal->get())->getElementType()->isPrimitiveType())
      ThrowException("Can't create vector of non-primitive type: " +
                     (*yyvsp[-2].TypeVal)->getDescription());
    if (!VMemoryInst::checkNumIndices(*yyvsp[0].ValueList))
      ThrowException("vgather must have four indices for each array dimension!");
    if (!VMemoryInst::checkIndexType(*yyvsp[0].ValueList))
      ThrowException("vgather indices must be of type long!");
    yyval.InstVal = new VGatherInst(getVal(*yyvsp[-2].TypeVal, yyvsp[-1].ValIDVal), *yyvsp[0].ValueList);
    delete yyvsp[-2].TypeVal; delete yyvsp[0].ValueList;
  ;
    break;}
case 224:
#line 2325 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Length of vimm must be unsigned int!");
    yyval.InstVal = new VImmInst(yyvsp[-2].ValueVal, yyvsp[0].ValueVal, yyvsp[-4].BoolVal);
  ;
    break;}
case 225:
#line 2331 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<VectorType>(yyvsp[-6].ValueVal->getType()))
      ThrowException("First operand of extract must be a vector!");
    if (yyvsp[-4].ValueVal->getType() != Type::UIntTy)
      ThrowException("Second operand of extract must be a uint!");
    if (yyvsp[-2].ValueVal->getType() != Type::UIntTy)
      ThrowException("Third operand of extract must be a uint!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Fourth operand of extract must be a uint!");
    yyval.InstVal = new ExtractInst(yyvsp[-6].ValueVal, yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 226:
#line 2343 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<VectorType>(yyvsp[-2].ValueVal->getType()))
      ThrowException("First operand of extractelement must be a vector!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Second operand of extractelement must be a uint!");
    yyval.InstVal = new ExtractElementInst(yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 227:
#line 2351 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<VectorType>(yyvsp[-6].ValueVal->getType()))
      ThrowException("First operand of combine must be a vector!");
    if (!isa<VectorType>(yyvsp[-4].ValueVal->getType()))
      ThrowException("Second operand of combine must be a vector!");
    if (yyvsp[-2].ValueVal->getType() != Type::UIntTy)
      ThrowException("Third operand of combine must be a uint!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Fourth operand of combine must be a uint!");
    yyval.InstVal = new CombineInst(yyvsp[-6].ValueVal, yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 228:
#line 2363 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<VectorType>(yyvsp[-4].ValueVal->getType()))
      ThrowException("First operand of combineelement must be a vector!");
    if (yyvsp[-2].ValueVal->getType() != cast<VectorType>(yyvsp[-4].ValueVal->getType())->getElementType())
      ThrowException("Second operand of combineelement must be vector element type!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Third operand of combineelement must be a uint!");
    yyval.InstVal = new CombineElementInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;
    break;}
case 229:
#line 2373 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"
{
    if (!isa<PointerType>(yyvsp[-2].TypeVal->get()))
      ThrowException("Can't store to a nonpointer type: " +
                     (*yyvsp[-2].TypeVal)->getDescription());
    if (!isa<VectorType>(yyvsp[-4].ValueVal->getType()))
      ThrowException("Can't store nonvector type: " +
		     yyvsp[-4].ValueVal->getType()->getDescription());
    // FIXME: We may change this, but for now it makes things simpler
    // to require variable vectors in vscatters.  If you need to vscatter
    // a fixed vector, you can always cast it to a variable vector
    // first.
    //
    if (isa<FixedVectorType>(yyvsp[-4].ValueVal->getType()))
      ThrowException("Can't store fixed vector type: " +
		     yyvsp[-4].ValueVal->getType()->getDescription() +
		     " cast to variable vector first");
    const Type *ElTy = cast<PointerType>(yyvsp[-2].TypeVal->get())->getElementType();
    if (!ElTy->isPrimitiveType())
      ThrowException("Can't create vector of non-primitive type: " +
                     ElTy->getDescription());
    if (!VMemoryInst::checkNumIndices(*yyvsp[0].ValueList))
      ThrowException("vscatter must have four indices for each array dimension!");
    if (!VMemoryInst::checkIndexType(*yyvsp[0].ValueList))
      ThrowException("vscatter indices must be of type long!");
    const VectorType *VT = cast<VectorType>(yyvsp[-4].ValueVal->getType());
    if (VT->getElementType() != ElTy)
      ThrowException("Can't store '" + yyvsp[-4].ValueVal->getType()->getDescription() +
                     "' into space of type '" + VT->getDescription() + "'!");
    yyval.InstVal = new VScatterInst(yyvsp[-4].ValueVal, getVal(*yyvsp[-2].TypeVal, yyvsp[-1].ValIDVal), *yyvsp[0].ValueList);
    delete yyvsp[-2].TypeVal; delete yyvsp[0].ValueList; 
  ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 2406 "/Users/bocchino/llvm/obj/../src/lib/AsmParser/llvmAsmParser.y"

int yyerror(const char *ErrorMsg) {
  std::string where 
    = std::string((CurFilename == "-") ? std::string("<stdin>") : CurFilename)
                  + ":" + utostr((unsigned) llvmAsmlineno) + ": ";
  std::string errMsg = std::string(ErrorMsg) + "\n" + where + " while reading ";
  if (yychar == YYEMPTY || yychar == 0)
    errMsg += "end-of-file.";
  else
    errMsg += "token: '" + std::string(llvmAsmtext, llvmAsmleng) + "'";
  ThrowException(errMsg);
  return 0;
}
