/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* If NAME_PREFIX is specified substitute the variables and functions
   names.  */
#define yyparse llvmAsmparse
#define yylex   llvmAsmlex
#define yyerror llvmAsmerror
#define yylval  llvmAsmlval
#define yychar  llvmAsmchar
#define yydebug llvmAsmdebug
#define yynerrs llvmAsmnerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ESINT64VAL = 258,
     EUINT64VAL = 259,
     SINTVAL = 260,
     UINTVAL = 261,
     FPVAL = 262,
     VOID = 263,
     BOOL = 264,
     SBYTE = 265,
     UBYTE = 266,
     SHORT = 267,
     USHORT = 268,
     INT = 269,
     UINT = 270,
     LONG = 271,
     ULONG = 272,
     FLOAT = 273,
     DOUBLE = 274,
     TYPE = 275,
     LABEL = 276,
     VAR_ID = 277,
     LABELSTR = 278,
     STRINGCONSTANT = 279,
     IMPLEMENTATION = 280,
     ZEROINITIALIZER = 281,
     TRUETOK = 282,
     FALSETOK = 283,
     BEGINTOK = 284,
     ENDTOK = 285,
     DECLARE = 286,
     GLOBAL = 287,
     CONSTANT = 288,
     SECTION = 289,
     VOLATILE = 290,
     FIXED = 291,
     TO = 292,
     DOTDOTDOT = 293,
     NULL_TOK = 294,
     UNDEF = 295,
     CONST = 296,
     INTERNAL = 297,
     LINKONCE = 298,
     WEAK = 299,
     APPENDING = 300,
     OPAQUE = 301,
     NOT = 302,
     EXTERNAL = 303,
     TARGET = 304,
     TRIPLE = 305,
     ENDIAN = 306,
     POINTERSIZE = 307,
     LITTLE = 308,
     BIG = 309,
     ALIGN = 310,
     DEPLIBS = 311,
     CALL = 312,
     TAIL = 313,
     CC_TOK = 314,
     CCC_TOK = 315,
     FASTCC_TOK = 316,
     COLDCC_TOK = 317,
     VECTOR = 318,
     OF = 319,
     RET = 320,
     BR = 321,
     SWITCH = 322,
     INVOKE = 323,
     UNWIND = 324,
     UNREACHABLE = 325,
     ADD = 326,
     SUB = 327,
     MUL = 328,
     DIV = 329,
     REM = 330,
     AND = 331,
     OR = 332,
     XOR = 333,
     SETLE = 334,
     SETGE = 335,
     SETLT = 336,
     SETGT = 337,
     SETEQ = 338,
     SETNE = 339,
     VSETLE = 340,
     VSETGE = 341,
     VSETLT = 342,
     VSETGT = 343,
     VSETEQ = 344,
     VSETNE = 345,
     MALLOC = 346,
     ALLOCA = 347,
     FREE = 348,
     LOAD = 349,
     STORE = 350,
     GETELEMENTPTR = 351,
     PHI_TOK = 352,
     CAST = 353,
     SELECT = 354,
     VSELECT = 355,
     SHL = 356,
     SHR = 357,
     VAARG = 358,
     VGATHER = 359,
     VIMM = 360,
     VSCATTER = 361,
     EXTRACT = 362,
     EXTRACTELEMENT = 363,
     COMBINE = 364,
     COMBINEELEMENT = 365,
     VAARG_old = 366,
     VANEXT_old = 367
   };
#endif
#define ESINT64VAL 258
#define EUINT64VAL 259
#define SINTVAL 260
#define UINTVAL 261
#define FPVAL 262
#define VOID 263
#define BOOL 264
#define SBYTE 265
#define UBYTE 266
#define SHORT 267
#define USHORT 268
#define INT 269
#define UINT 270
#define LONG 271
#define ULONG 272
#define FLOAT 273
#define DOUBLE 274
#define TYPE 275
#define LABEL 276
#define VAR_ID 277
#define LABELSTR 278
#define STRINGCONSTANT 279
#define IMPLEMENTATION 280
#define ZEROINITIALIZER 281
#define TRUETOK 282
#define FALSETOK 283
#define BEGINTOK 284
#define ENDTOK 285
#define DECLARE 286
#define GLOBAL 287
#define CONSTANT 288
#define SECTION 289
#define VOLATILE 290
#define FIXED 291
#define TO 292
#define DOTDOTDOT 293
#define NULL_TOK 294
#define UNDEF 295
#define CONST 296
#define INTERNAL 297
#define LINKONCE 298
#define WEAK 299
#define APPENDING 300
#define OPAQUE 301
#define NOT 302
#define EXTERNAL 303
#define TARGET 304
#define TRIPLE 305
#define ENDIAN 306
#define POINTERSIZE 307
#define LITTLE 308
#define BIG 309
#define ALIGN 310
#define DEPLIBS 311
#define CALL 312
#define TAIL 313
#define CC_TOK 314
#define CCC_TOK 315
#define FASTCC_TOK 316
#define COLDCC_TOK 317
#define VECTOR 318
#define OF 319
#define RET 320
#define BR 321
#define SWITCH 322
#define INVOKE 323
#define UNWIND 324
#define UNREACHABLE 325
#define ADD 326
#define SUB 327
#define MUL 328
#define DIV 329
#define REM 330
#define AND 331
#define OR 332
#define XOR 333
#define SETLE 334
#define SETGE 335
#define SETLT 336
#define SETGT 337
#define SETEQ 338
#define SETNE 339
#define VSETLE 340
#define VSETGE 341
#define VSETLT 342
#define VSETGT 343
#define VSETEQ 344
#define VSETNE 345
#define MALLOC 346
#define ALLOCA 347
#define FREE 348
#define LOAD 349
#define STORE 350
#define GETELEMENTPTR 351
#define PHI_TOK 352
#define CAST 353
#define SELECT 354
#define VSELECT 355
#define SHL 356
#define SHR 357
#define VAARG 358
#define VGATHER 359
#define VIMM 360
#define VSCATTER 361
#define EXTRACT 362
#define EXTRACTELEMENT 363
#define COMBINE 364
#define COMBINEELEMENT 365
#define VAARG_old 366
#define VANEXT_old 367




/* Copy the first part of user declarations.  */
#line 14 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"

#include "ParserInternals.h"
#include "llvm/CallingConv.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/SymbolTable.h"
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/MathExtras.h"
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
static BasicBlock *CurBB;
static GlobalVariable *CurGV;


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
static GlobalVariable *
ParseGlobalVariable(char *NameStr,GlobalValue::LinkageTypes Linkage,
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
    return GV;
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
        return EGV;
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
  return GV;
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
                                                 RetTy, ArgTyPtr, (Type *)0);

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
                                                 RetTy, ArgTyPtr, (Type *)0);

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
                                                 RetTy, ArgTyPtr, ArgTyPtr,
                                                 (Type *)0);

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 870 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
typedef union YYSTYPE {
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
/* Line 191 of yacc.c.  */
#line 1204 "llvmAsmParser.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 1216 "llvmAsmParser.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1382

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  127
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  72
/* YYNRULES -- Number of rules. */
#define YYNRULES  243
/* YYNRULES -- Number of states. */
#define YYNSTATES  504

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   367

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     116,   117,   123,     2,   114,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     125,   113,   126,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   118,   115,   120,     2,     2,     2,     2,     2,   124,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     119,     2,     2,   121,     2,   122,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    11,    13,    15,    17,
      19,    21,    23,    25,    27,    29,    31,    33,    35,    37,
      39,    41,    43,    45,    47,    49,    51,    53,    55,    57,
      59,    61,    63,    65,    67,    69,    71,    73,    75,    77,
      79,    82,    83,    85,    87,    89,    91,    92,    93,    95,
      97,    99,   102,   103,   106,   107,   111,   114,   115,   117,
     118,   122,   124,   127,   129,   131,   133,   135,   137,   139,
     141,   143,   145,   147,   149,   151,   153,   155,   157,   159,
     161,   163,   165,   167,   169,   172,   177,   183,   189,   196,
     200,   203,   206,   208,   212,   214,   218,   220,   221,   226,
     230,   234,   239,   244,   248,   251,   254,   257,   260,   263,
     266,   269,   272,   275,   278,   285,   291,   300,   307,   314,
     321,   328,   332,   334,   336,   338,   340,   343,   346,   349,
     351,   356,   359,   360,   368,   369,   377,   381,   386,   387,
     389,   391,   395,   399,   403,   407,   411,   413,   414,   416,
     418,   420,   421,   424,   428,   430,   432,   436,   438,   439,
     448,   450,   452,   456,   458,   460,   463,   464,   468,   470,
     472,   474,   476,   478,   480,   482,   486,   488,   490,   492,
     494,   496,   499,   502,   505,   509,   512,   513,   515,   518,
     521,   525,   535,   545,   554,   568,   570,   572,   579,   585,
     588,   595,   603,   605,   609,   611,   612,   615,   617,   623,
     629,   635,   641,   644,   649,   654,   661,   668,   673,   678,
     683,   686,   694,   696,   699,   700,   702,   703,   705,   706,
     710,   717,   721,   728,   731,   736,   743,   748,   753,   759,
     768,   773,   782,   789
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short yyrhs[] =
{
     159,     0,    -1,     5,    -1,     6,    -1,     3,    -1,     4,
      -1,    71,    -1,    72,    -1,    73,    -1,    74,    -1,    75,
      -1,    76,    -1,    77,    -1,    78,    -1,    79,    -1,    80,
      -1,    81,    -1,    82,    -1,    83,    -1,    84,    -1,    85,
      -1,    86,    -1,    87,    -1,    88,    -1,    89,    -1,    90,
      -1,   101,    -1,   102,    -1,    16,    -1,    14,    -1,    12,
      -1,    10,    -1,    17,    -1,    15,    -1,    13,    -1,    11,
      -1,   135,    -1,   136,    -1,    18,    -1,    19,    -1,   168,
     113,    -1,    -1,    42,    -1,    43,    -1,    44,    -1,    45,
      -1,    -1,    -1,    60,    -1,    61,    -1,    62,    -1,    59,
       4,    -1,    -1,    55,     4,    -1,    -1,   114,    55,     4,
      -1,    34,    24,    -1,    -1,   144,    -1,    -1,   114,   147,
     146,    -1,   144,    -1,    55,     4,    -1,   150,    -1,     8,
      -1,   152,    -1,     8,    -1,   152,    -1,     9,    -1,    10,
      -1,    11,    -1,    12,    -1,    13,    -1,    14,    -1,    15,
      -1,    16,    -1,    17,    -1,    18,    -1,    19,    -1,    20,
      -1,    21,    -1,    46,    -1,   151,    -1,   181,    -1,   115,
       4,    -1,   149,   116,   154,   117,    -1,   118,     4,   119,
     152,   120,    -1,   118,    63,    64,   152,   120,    -1,   118,
      63,    64,     4,   152,   120,    -1,   121,   153,   122,    -1,
     121,   122,    -1,   152,   123,    -1,   152,    -1,   153,   114,
     152,    -1,   153,    -1,   153,   114,    38,    -1,    38,    -1,
      -1,   150,   118,   157,   120,    -1,   150,   118,   120,    -1,
     150,   124,    24,    -1,   150,   125,   157,   126,    -1,   150,
     121,   157,   122,    -1,   150,   121,   122,    -1,   150,    39,
      -1,   150,    40,    -1,   150,   181,    -1,   150,   156,    -1,
     150,    26,    -1,   135,   129,    -1,   136,     4,    -1,     9,
      27,    -1,     9,    28,    -1,   138,     7,    -1,    98,   116,
     155,    37,   150,   117,    -1,    96,   116,   155,   195,   117,
      -1,    99,   116,   155,   114,   155,   114,   155,   117,    -1,
     130,   116,   155,   114,   155,   117,    -1,   131,   116,   155,
     114,   155,   117,    -1,   132,   116,   155,   114,   155,   117,
      -1,   134,   116,   155,   114,   155,   117,    -1,   157,   114,
     155,    -1,   155,    -1,    32,    -1,    33,    -1,   160,    -1,
     160,   177,    -1,   160,   178,    -1,   160,    25,    -1,   161,
      -1,   161,   139,    20,   148,    -1,   161,   178,    -1,    -1,
     161,   139,   140,   158,   155,   162,   146,    -1,    -1,   161,
     139,    48,   158,   150,   163,   146,    -1,   161,    49,   165,
      -1,   161,    56,   113,   166,    -1,    -1,    54,    -1,    53,
      -1,    51,   113,   164,    -1,    52,   113,     4,    -1,    50,
     113,    24,    -1,   118,   167,   120,    -1,   167,   114,    24,
      -1,    24,    -1,    -1,    22,    -1,    24,    -1,   168,    -1,
      -1,   150,   169,    -1,   171,   114,   170,    -1,   170,    -1,
     171,    -1,   171,   114,    38,    -1,    38,    -1,    -1,   141,
     148,   168,   116,   172,   117,   145,   142,    -1,    29,    -1,
     121,    -1,   140,   173,   174,    -1,    30,    -1,   122,    -1,
     184,   176,    -1,    -1,    31,   179,   173,    -1,     3,    -1,
       4,    -1,     7,    -1,    27,    -1,    28,    -1,    39,    -1,
      40,    -1,   125,   157,   126,    -1,   156,    -1,   128,    -1,
     168,    -1,   181,    -1,   180,    -1,   150,   182,    -1,   184,
     185,    -1,   175,   185,    -1,   186,   139,   187,    -1,   186,
     189,    -1,    -1,    23,    -1,    65,   183,    -1,    65,     8,
      -1,    66,    21,   182,    -1,    66,     9,   182,   114,    21,
     182,   114,    21,   182,    -1,    67,   137,   182,   114,    21,
     182,   118,   188,   120,    -1,    67,   137,   182,   114,    21,
     182,   118,   120,    -1,    68,   141,   148,   182,   116,   192,
     117,    37,    21,   182,    69,    21,   182,    -1,    69,    -1,
      70,    -1,   188,   137,   180,   114,    21,   182,    -1,   137,
     180,   114,    21,   182,    -1,   139,   194,    -1,   150,   118,
     182,   114,   182,   120,    -1,   190,   114,   118,   182,   114,
     182,   120,    -1,   183,    -1,   191,   114,   183,    -1,   191,
      -1,    -1,    58,    57,    -1,    57,    -1,   130,   150,   182,
     114,   182,    -1,   131,   150,   182,   114,   182,    -1,   132,
     150,   182,   114,   182,    -1,   133,   150,   182,   114,   182,
      -1,    47,   183,    -1,   134,   183,   114,   183,    -1,    98,
     183,    37,   150,    -1,    99,   183,   114,   183,   114,   183,
      -1,   100,   183,   114,   183,   114,   183,    -1,   103,   183,
     114,   150,    -1,   111,   183,   114,   150,    -1,   112,   183,
     114,   150,    -1,    97,   190,    -1,   193,   141,   148,   182,
     116,   192,   117,    -1,   198,    -1,   114,   191,    -1,    -1,
      35,    -1,    -1,    36,    -1,    -1,    91,   150,   143,    -1,
      91,   150,   114,    15,   182,   143,    -1,    92,   150,   143,
      -1,    92,   150,   114,    15,   182,   143,    -1,    93,   183,
      -1,   196,    94,   150,   182,    -1,   196,    95,   183,   114,
     150,   182,    -1,    96,   150,   182,   195,    -1,   104,   150,
     182,   195,    -1,   197,   105,   183,   114,   183,    -1,   107,
     183,   114,   183,   114,   183,   114,   183,    -1,   108,   183,
     114,   183,    -1,   109,   183,   114,   183,   114,   183,   114,
     183,    -1,   110,   183,   114,   183,   114,   183,    -1,   106,
     183,   114,   150,   182,   195,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   992,   992,   993,  1000,  1001,  1010,  1010,  1010,  1010,
    1010,  1011,  1011,  1011,  1012,  1012,  1012,  1012,  1012,  1012,
    1013,  1013,  1013,  1013,  1013,  1013,  1015,  1015,  1019,  1019,
    1019,  1019,  1020,  1020,  1020,  1020,  1021,  1021,  1022,  1022,
    1025,  1028,  1032,  1033,  1034,  1035,  1036,  1038,  1039,  1040,
    1041,  1042,  1050,  1051,  1056,  1057,  1064,  1071,  1072,  1077,
    1078,  1079,  1083,  1096,  1096,  1097,  1097,  1099,  1108,  1108,
    1108,  1108,  1108,  1108,  1108,  1109,  1109,  1109,  1109,  1109,
    1109,  1110,  1113,  1116,  1122,  1129,  1141,  1145,  1150,  1164,
    1173,  1176,  1184,  1188,  1193,  1194,  1197,  1200,  1210,  1235,
    1248,  1276,  1301,  1321,  1333,  1342,  1346,  1405,  1411,  1419,
    1424,  1429,  1432,  1435,  1442,  1452,  1483,  1490,  1511,  1519,
    1524,  1536,  1539,  1546,  1546,  1556,  1563,  1567,  1570,  1573,
    1586,  1606,  1608,  1608,  1614,  1614,  1621,  1623,  1625,  1630,
    1631,  1633,  1636,  1644,  1649,  1651,  1655,  1659,  1667,  1667,
    1668,  1668,  1670,  1676,  1681,  1687,  1690,  1695,  1699,  1703,
    1789,  1789,  1791,  1799,  1799,  1801,  1805,  1805,  1814,  1817,
    1820,  1823,  1826,  1829,  1832,  1835,  1859,  1866,  1869,  1874,
    1874,  1880,  1884,  1887,  1895,  1904,  1908,  1918,  1929,  1932,
    1935,  1938,  1941,  1955,  1959,  2012,  2015,  2021,  2029,  2039,
    2046,  2051,  2057,  2061,  2067,  2067,  2069,  2072,  2078,  2091,
    2100,  2106,  2112,  2124,  2132,  2139,  2146,  2155,  2160,  2179,
    2201,  2215,  2272,  2278,  2280,  2284,  2287,  2291,  2294,  2299,
    2303,  2307,  2311,  2315,  2322,  2333,  2347,  2369,  2384,  2390,
    2402,  2410,  2422,  2432
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ESINT64VAL", "EUINT64VAL", "SINTVAL", 
  "UINTVAL", "FPVAL", "VOID", "BOOL", "SBYTE", "UBYTE", "SHORT", "USHORT", 
  "INT", "UINT", "LONG", "ULONG", "FLOAT", "DOUBLE", "TYPE", "LABEL", 
  "VAR_ID", "LABELSTR", "STRINGCONSTANT", "IMPLEMENTATION", 
  "ZEROINITIALIZER", "TRUETOK", "FALSETOK", "BEGINTOK", "ENDTOK", 
  "DECLARE", "GLOBAL", "CONSTANT", "SECTION", "VOLATILE", "FIXED", "TO", 
  "DOTDOTDOT", "NULL_TOK", "UNDEF", "CONST", "INTERNAL", "LINKONCE", 
  "WEAK", "APPENDING", "OPAQUE", "NOT", "EXTERNAL", "TARGET", "TRIPLE", 
  "ENDIAN", "POINTERSIZE", "LITTLE", "BIG", "ALIGN", "DEPLIBS", "CALL", 
  "TAIL", "CC_TOK", "CCC_TOK", "FASTCC_TOK", "COLDCC_TOK", "VECTOR", "OF", 
  "RET", "BR", "SWITCH", "INVOKE", "UNWIND", "UNREACHABLE", "ADD", "SUB", 
  "MUL", "DIV", "REM", "AND", "OR", "XOR", "SETLE", "SETGE", "SETLT", 
  "SETGT", "SETEQ", "SETNE", "VSETLE", "VSETGE", "VSETLT", "VSETGT", 
  "VSETEQ", "VSETNE", "MALLOC", "ALLOCA", "FREE", "LOAD", "STORE", 
  "GETELEMENTPTR", "PHI_TOK", "CAST", "SELECT", "VSELECT", "SHL", "SHR", 
  "VAARG", "VGATHER", "VIMM", "VSCATTER", "EXTRACT", "EXTRACTELEMENT", 
  "COMBINE", "COMBINEELEMENT", "VAARG_old", "VANEXT_old", "'='", "','", 
  "'\\\\'", "'('", "')'", "'['", "'x'", "']'", "'{'", "'}'", "'*'", "'c'", 
  "'<'", "'>'", "$accept", "INTVAL", "EINT64VAL", "ArithmeticOps", 
  "LogicalOps", "SetCondOps", "VSetCondOps", "ShiftOps", "SIntType", 
  "UIntType", "IntType", "FPType", "OptAssign", "OptLinkage", 
  "OptCallingConv", "OptAlign", "OptCAlign", "SectionString", 
  "OptSection", "GlobalVarAttributes", "GlobalVarAttribute", "TypesV", 
  "UpRTypesV", "Types", "PrimType", "UpRTypes", "TypeListI", 
  "ArgTypeListI", "ConstVal", "ConstExpr", "ConstVector", "GlobalType", 
  "Module", "FunctionList", "ConstPool", "@1", "@2", "BigOrLittle", 
  "TargetDefinition", "LibrariesDefinition", "LibList", "Name", "OptName", 
  "ArgVal", "ArgListH", "ArgList", "FunctionHeaderH", "BEGIN", 
  "FunctionHeader", "END", "Function", "FunctionProto", "@3", 
  "ConstValueRef", "SymbolicValueRef", "ValueRef", "ResolvedVal", 
  "BasicBlockList", "BasicBlock", "InstructionList", "BBTerminatorInst", 
  "JumpTable", "Inst", "PHIList", "ValueRefList", "ValueRefListE", 
  "OptTailCall", "InstVal", "IndexList", "OptVolatile", "OptFixed", 
  "MemoryInst", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,    61,    44,    92,    40,    41,    91,   120,
      93,   123,   125,    42,    99,    60,    62
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,   127,   128,   128,   129,   129,   130,   130,   130,   130,
     130,   131,   131,   131,   132,   132,   132,   132,   132,   132,
     133,   133,   133,   133,   133,   133,   134,   134,   135,   135,
     135,   135,   136,   136,   136,   136,   137,   137,   138,   138,
     139,   139,   140,   140,   140,   140,   140,   141,   141,   141,
     141,   141,   142,   142,   143,   143,   144,   145,   145,   146,
     146,   147,   147,   148,   148,   149,   149,   150,   151,   151,
     151,   151,   151,   151,   151,   151,   151,   151,   151,   151,
     151,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   153,   153,   154,   154,   154,   154,   155,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   155,   155,   155,   156,   156,   156,   156,   156,   156,
     156,   157,   157,   158,   158,   159,   160,   160,   160,   160,
     161,   161,   162,   161,   163,   161,   161,   161,   161,   164,
     164,   165,   165,   165,   166,   167,   167,   167,   168,   168,
     169,   169,   170,   171,   171,   172,   172,   172,   172,   173,
     174,   174,   175,   176,   176,   177,   179,   178,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   181,   181,   182,
     182,   183,   184,   184,   185,   186,   186,   186,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   188,   188,   189,
     190,   190,   191,   191,   192,   192,   193,   193,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   195,   195,   196,   196,   197,   197,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     0,     1,     1,     1,     1,     0,     0,     1,     1,
       1,     2,     0,     2,     0,     3,     2,     0,     1,     0,
       3,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     4,     5,     5,     6,     3,
       2,     2,     1,     3,     1,     3,     1,     0,     4,     3,
       3,     4,     4,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     6,     5,     8,     6,     6,     6,
       6,     3,     1,     1,     1,     1,     2,     2,     2,     1,
       4,     2,     0,     7,     0,     7,     3,     4,     0,     1,
       1,     3,     3,     3,     3,     3,     1,     0,     1,     1,
       1,     0,     2,     3,     1,     1,     3,     1,     0,     8,
       1,     1,     3,     1,     1,     2,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     2,     2,     2,     3,     2,     0,     1,     2,     2,
       3,     9,     9,     8,    13,     1,     1,     6,     5,     2,
       6,     7,     1,     3,     1,     0,     2,     1,     5,     5,
       5,     5,     2,     4,     4,     6,     6,     4,     4,     4,
       2,     7,     1,     2,     0,     1,     0,     1,     0,     3,
       6,     3,     6,     2,     4,     6,     4,     4,     5,     8,
       4,     8,     6,     6
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
     138,     0,    46,   129,     1,   128,   166,    42,    43,    44,
      45,    47,   186,   126,   127,   186,   148,   149,     0,     0,
      46,     0,   131,    47,     0,    48,    49,    50,     0,     0,
     187,   183,    41,   163,   164,   165,   182,     0,     0,     0,
     136,     0,     0,     0,     0,    40,   167,    51,     2,     3,
      64,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,     0,     0,     0,   177,     0,
       0,    63,    82,    67,   178,    83,   160,   161,   162,   226,
     185,     0,     0,     0,   147,   137,   130,   123,   124,     0,
       0,    84,     0,     0,    66,    90,    92,     0,     0,    97,
      91,   225,   227,     0,   207,     0,     0,     0,     0,    47,
     195,   196,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
      26,    27,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,    47,   199,     0,
       0,   222,   143,   140,   139,   141,   142,   146,     0,   134,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,   132,     0,     0,     0,    89,
     158,    96,    94,     0,     0,   212,   206,   189,   188,     0,
       0,    31,    35,    30,    34,    29,    33,    28,    32,    36,
      37,     0,     0,    54,    54,   233,     0,     0,   220,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   144,    59,   111,   112,     4,     5,   109,   110,   113,
     108,   104,   105,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,   106,    59,    65,     0,    65,
      93,   157,   151,   154,   155,     0,     0,    85,   168,   169,
     170,   171,   172,   173,   174,     0,   176,   180,   179,   181,
       0,   190,     0,     0,     0,   229,     0,   231,   224,     0,
       0,     0,     0,     0,     0,   224,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   145,     0,   135,     0,     0,     0,    99,   122,
       0,   103,     0,   100,     0,     0,     0,     0,     0,   133,
      86,    65,    87,   150,   152,     0,    57,    95,     0,     0,
       0,     0,     0,     0,     0,     0,   236,     0,     0,   214,
       0,     0,   217,   237,     0,     0,   240,     0,     0,   218,
     219,     0,     0,     0,     0,   213,     0,   234,     0,     0,
       0,     0,    61,    59,   224,     0,     0,     0,    98,   102,
     101,     0,     0,     0,     0,    88,   156,   153,    58,    52,
     175,     0,     0,   205,    54,    55,    54,   202,   223,     0,
       0,     0,     0,   224,     0,     0,     0,   208,   209,   210,
     211,   205,     0,   238,    56,    62,    60,     0,     0,     0,
     121,     0,     0,     0,     0,     0,   159,     0,     0,   204,
       0,     0,   230,   232,     0,     0,     0,   215,   216,   243,
       0,     0,   242,     0,   235,   115,     0,     0,     0,     0,
       0,     0,    53,     0,     0,     0,   203,   200,     0,     0,
       0,   221,   114,     0,   117,   118,   119,   120,     0,   193,
       0,     0,     0,   201,   239,   241,     0,   191,     0,   192,
       0,     0,   116,     0,     0,     0,     0,     0,     0,   198,
       0,     0,   197,   194
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,    68,   247,   260,   261,   262,   154,   263,   181,   182,
     211,   183,    20,    11,    28,   436,   295,   382,   399,   324,
     383,    69,    70,   194,    72,    73,    97,   193,   329,   286,
     330,    89,     1,     2,     3,   266,   242,   165,    40,    85,
     168,    74,   344,   273,   274,   275,    29,    78,    12,    35,
      13,    14,    23,   287,    75,   289,   407,    15,    31,    32,
     156,   481,    80,   218,   439,   440,   157,   158,   356,   159,
     160,   161
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -376
static const short yypact[] =
{
    -376,    11,   112,   145,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,    90,    13,  -376,  -376,    19,  -376,  -376,   120,   -47,
      -4,   -13,  -376,    90,    92,  -376,  -376,  -376,  1094,   -23,
    -376,  -376,   142,  -376,  -376,  -376,  -376,     9,    33,    40,
    -376,     0,  1094,    98,    98,  -376,  -376,  -376,  -376,  -376,
      47,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,  -376,  -376,  -376,  -376,   131,     5,    64,  -376,   142,
      52,  -376,  -376,   -56,  -376,  -376,  -376,  -376,  -376,  1270,
    -376,   149,    86,   171,   159,  -376,  -376,  -376,  -376,  1133,
    1172,  -376,    81,   146,  -376,  -376,   -56,   -20,    97,   860,
    -376,  -376,  -376,  1133,  -376,   158,  1211,    82,   316,    90,
    -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,  -376,  1133,  1133,  1133,  1133,  1133,  1133,  1133,  1133,
    -376,  -376,  1133,  1133,  1133,  1133,  1133,  1133,  1133,  1133,
    1133,  1133,  1133,  1133,  1133,  1133,  -376,    90,  -376,    65,
     115,  -376,  -376,  -376,  -376,  -376,  -376,  -376,   -19,  -376,
     153,   188,   217,   192,   219,   199,   220,   201,   221,   223,
     225,   203,   229,   227,   719,  -376,  1133,   485,  1133,  -376,
     899,  -376,   113,   118,   571,  -376,  -376,    47,  -376,   571,
     571,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,   571,  1094,   122,   123,  -376,   571,   124,   127,   206,
     130,   132,   133,   571,   138,   139,   141,   144,   147,   148,
     151,   571,   571,   571,   571,   152,  1094,  1133,  1133,  1133,
     232,  -376,   154,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,  -376,  -376,   129,   143,   155,   977,   840,   236,  1172,
     156,   161,   163,   164,  -376,  -376,   154,    -3,  1133,    22,
     -56,  -376,   142,  -376,   168,   150,  1016,  -376,  -376,  -376,
    -376,  -376,  -376,  -376,  -376,  1172,  -376,  -376,  -376,  -376,
     173,  -376,   175,   571,    35,  -376,    37,  -376,   176,   571,
     157,  1133,  1133,  1133,  1133,   176,  1133,  1133,  1133,  1133,
    1133,  1133,  1133,   177,   178,   180,   181,  1133,   571,   571,
     182,   191,  -376,    59,  -376,  1172,  1172,  1172,  -376,  -376,
      24,  -376,    -1,  -376,   -17,  1172,  1172,  1172,  1172,  -376,
    -376,    38,  -376,  -376,  -376,  1055,   251,  -376,   -15,   242,
     285,   195,   571,   308,   571,  1133,  -376,   200,   571,  -376,
     202,   204,  -376,  -376,   571,   205,  -376,   208,   222,  -376,
    -376,   571,   571,   571,   571,  -376,   197,  -376,  1133,  1133,
     291,   330,  -376,   154,   176,   298,   224,  1172,  -376,  -376,
    -376,   226,   234,   235,   237,  -376,  -376,  -376,  -376,   282,
    -376,   571,   571,  1133,   238,  -376,   238,  -376,   240,   571,
     241,  1133,  1133,   176,  1133,  1133,  1133,  -376,  -376,  -376,
    -376,  1133,   571,  -376,  -376,  -376,  -376,   239,  1133,  1172,
    -376,  1172,  1172,  1172,  1172,   335,  -376,   246,   243,   240,
     245,   286,  -376,  -376,  1133,   244,   571,  -376,  -376,  -376,
     252,   253,  -376,   248,  -376,  -376,   254,   256,   255,   259,
     260,   263,  -376,   322,     8,   331,  -376,  -376,   261,  1133,
    1133,  -376,  -376,  1172,  -376,  -376,  -376,  -376,   571,  -376,
     655,    16,   352,  -376,  -376,  -376,   265,  -376,   269,  -376,
     655,   571,  -376,   363,   271,   318,   571,   369,   370,  -376,
     571,   571,  -376,  -376
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -376,  -376,  -376,   313,   314,   315,  -376,   317,  -107,  -106,
    -375,  -376,   365,   375,   -92,  -376,  -207,    53,  -376,  -256,
    -376,   -38,  -376,   -28,  -376,   -54,   299,  -376,   -87,   218,
    -222,   364,  -376,  -376,  -376,  -376,  -376,  -376,  -376,  -376,
    -376,     2,  -376,    62,  -376,  -376,   386,  -376,  -376,  -376,
    -376,   407,  -376,  -361,    70,   215,   -91,  -376,   396,  -376,
    -376,  -376,  -376,  -376,    57,    -8,  -376,  -376,  -297,  -376,
    -376,  -376
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -229
static const short yytable[] =
{
      71,   209,   210,   185,    86,    21,    76,   297,   363,    92,
     339,     4,   195,    96,    71,   198,    42,   212,   201,   202,
     203,   204,   205,   206,   207,   208,   201,   202,   203,   204,
     205,   206,   207,   208,    21,   332,    30,   334,     7,     8,
       9,    10,    30,   215,    43,    96,   219,   220,   221,    33,
     352,   222,   354,   224,   225,   226,   227,   228,   229,   230,
     -65,   169,   184,   348,   235,   236,    41,   100,    93,    48,
      49,    98,    94,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    16,   427,    17,   480,
     353,   199,   353,   380,   188,   240,    47,   387,    77,   387,
      45,   241,   189,   200,   213,   214,   490,   216,   217,   390,
      64,   400,  -125,   387,   381,   223,   449,   340,    84,   488,
     100,   389,    81,   231,   232,   233,   234,   426,   479,   494,
      87,    88,   267,   269,   270,    91,   489,     5,   387,   163,
     164,    34,   342,     6,   388,   100,    82,   320,   321,    24,
      25,    26,    27,    83,     7,     8,     9,    10,   395,   237,
     238,   100,   272,   -66,    16,   -41,    17,    16,    99,    17,
      37,    38,    39,   162,   293,   166,     6,   -41,   -41,    65,
     243,   244,    66,   167,    71,    67,    95,   -41,   -41,   -41,
     -41,   -31,   -31,   -41,    18,   -30,   -30,   442,   318,   443,
     186,    19,   -29,   -29,   -28,   -28,   245,   246,    71,   319,
     187,   360,   361,   190,   341,   196,   365,   366,   367,   368,
     239,   -35,   270,   -34,   -33,   -32,   375,   276,   184,   184,
     -38,   184,   -39,   248,   249,   277,   294,   296,   384,   385,
     386,   300,   299,   301,   302,   325,   303,   304,   391,   392,
     393,   394,   306,   307,   265,   308,   322,   184,   309,   326,
     333,   310,   311,   401,   288,   312,   317,   346,   323,   288,
     288,   327,   335,   359,   343,   358,   362,   336,   364,   337,
     338,   288,   345,   369,   370,   380,   288,   349,   423,   350,
     355,   371,   372,   288,   373,   374,   378,   184,   184,   184,
     430,   288,   288,   288,   288,   379,   402,   184,   184,   184,
     184,   403,   405,   421,   409,   424,   411,   272,   412,   414,
     447,   448,   415,   450,   451,   452,   201,   202,   203,   204,
     205,   206,   207,   208,   425,   428,   416,   435,   429,   462,
     431,   353,   457,   478,   458,   459,   460,   461,   432,   433,
     422,   434,   441,   466,   444,   446,   455,   209,   210,   184,
     463,   464,   465,   288,   467,   471,   469,   470,   482,   288,
     473,   472,   474,   491,   209,   210,   475,   476,   484,   485,
     477,   483,   492,   493,   496,   497,   486,   498,   288,   288,
     500,   501,   151,   152,   153,    44,   155,    79,   192,   398,
     456,   184,   264,   184,   184,   184,   184,   397,    90,    46,
      22,    36,   408,   453,   290,   291,     0,     0,     0,     0,
       0,     0,   288,     0,   288,     0,   292,     0,   288,     0,
       0,   298,     0,     0,   288,     0,     0,     0,   305,     0,
       0,   288,   288,   288,   288,   184,   313,   314,   315,   316,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   288,   288,     0,     0,     0,     0,     0,     0,   288,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   268,
      48,    49,   288,    94,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    16,   351,    17,
       0,     0,     0,     0,   357,     0,   288,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,   376,   377,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   288,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   288,     0,     0,     0,     0,   288,   404,     0,   406,
     288,   288,     0,   410,   278,   279,    48,    49,   280,   413,
       0,     0,     0,     0,     0,     0,   417,   418,   419,   420,
       0,     0,     0,    16,     0,    17,     0,     0,   281,   282,
      65,     0,     0,    66,     0,     0,    67,     0,     0,     0,
     283,   284,     0,     0,     0,     0,   437,   438,     0,     0,
       0,     0,     0,     0,   445,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   454,     0,     0,
       0,     0,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,     0,     0,   278,   279,
       0,   468,   280,     0,     0,     0,     0,   253,     0,   254,
     255,     0,   140,   141,     0,     0,     0,     0,     0,     0,
       0,     0,   281,   282,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   487,   283,   284,   285,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   495,     0,     0,     0,
       0,   499,     0,     0,     0,   502,   503,     0,     0,     0,
       0,     0,     0,     0,    48,    49,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
       0,    16,     0,    17,     0,   250,     0,     0,     0,     0,
       0,   253,     0,   254,   255,     0,   140,   141,   251,   252,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     285,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   253,     0,   254,   255,     0,
     140,   141,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   256,     0,     0,
     257,     0,     0,   258,   259,    48,    49,     0,    94,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
      62,    63,    16,     0,    17,    48,    49,     0,    94,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    16,     0,    17,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,     0,
       0,     0,     0,     0,    48,    49,    64,    94,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    16,     0,    17,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   271,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,     0,     0,    66,     0,
       0,    67,   331,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,     0,     0,    66,     0,
       0,    67,    48,    49,     0,    94,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,   180,    62,    63,    16,
       0,    17,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,     0,     0,    66,     0,     0,
      67,    48,    49,    64,    94,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    16,     0,
      17,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   347,     0,     0,     0,     0,     0,
      48,    49,    64,    94,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    16,     0,    17,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,   396,     0,    66,     0,   328,    67,    48,
      49,    64,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    16,     0,    17,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    65,     0,     0,    66,     0,     0,    67,    48,    49,
      64,    94,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    16,     0,    17,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,     0,     0,    66,     0,     0,    67,    48,    49,    64,
      94,   170,   171,   172,   173,   174,   175,   176,   177,   178,
     179,   180,    62,    63,    16,     0,    17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
       0,     0,    66,     0,     0,    67,    48,    49,    64,   197,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    16,     0,    17,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,     0,
       0,    66,     0,     0,    67,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,     0,     0,
      66,     0,     0,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,   102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,    65,   104,   105,    66,
       0,     0,    67,     0,     0,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,     0,     0,   135,   136,   137,   138,
     139,   140,   141,   142,   143,  -228,   144,   145,   146,   147,
     148,   149,   150
};

static const short yycheck[] =
{
      28,   108,   108,    90,    42,     3,    29,   214,   305,     4,
     266,     0,   103,    67,    42,   106,    20,   109,    10,    11,
      12,    13,    14,    15,    16,    17,    10,    11,    12,    13,
      14,    15,    16,    17,    32,   257,    23,   259,    42,    43,
      44,    45,    23,   134,    48,    99,   137,   138,   139,    30,
      15,   142,    15,   144,   145,   146,   147,   148,   149,   150,
     116,    89,    90,   285,   155,   157,   113,   123,    63,     5,
       6,    69,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,   384,    24,   464,
      55,     9,    55,    34,   114,   114,     4,   114,   121,   114,
     113,   120,   122,    21,   132,   133,   481,   135,   136,   126,
      46,   126,     0,   114,    55,   143,   413,   120,   118,   480,
     123,   122,   113,   151,   152,   153,   154,   383,   120,   490,
      32,    33,   186,   187,   188,     4,   120,    25,   114,    53,
      54,   122,   120,    31,   120,   123,   113,   238,   239,    59,
      60,    61,    62,   113,    42,    43,    44,    45,   120,    94,
      95,   123,   190,   116,    22,    20,    24,    22,   116,    24,
      50,    51,    52,    24,   212,     4,    31,    32,    33,   115,
      27,    28,   118,    24,   212,   121,   122,    42,    43,    44,
      45,     3,     4,    48,    49,     3,     4,   404,   236,   406,
     119,    56,     3,     4,     3,     4,     3,     4,   236,   237,
      64,   302,   303,   116,   268,    57,   307,   308,   309,   310,
     105,     4,   276,     4,     4,     4,   317,   114,   256,   257,
       7,   259,     7,     4,     7,   117,   114,   114,   325,   326,
     327,   114,   118,    37,   114,   116,   114,   114,   335,   336,
     337,   338,   114,   114,   184,   114,    24,   285,   114,   116,
      24,   114,   114,    21,   194,   114,   114,   117,   114,   199,
     200,   116,   116,   301,   272,   118,   304,   116,   306,   116,
     116,   211,   114,   311,   312,    34,   216,   114,   379,   114,
     114,   114,   114,   223,   114,   114,   114,   325,   326,   327,
     387,   231,   232,   233,   234,   114,    21,   335,   336,   337,
     338,   116,     4,   116,   114,    24,   114,   345,   114,   114,
     411,   412,   114,   414,   415,   416,    10,    11,    12,    13,
      14,    15,    16,    17,     4,    37,   114,    55,   114,     4,
     114,    55,   429,    21,   431,   432,   433,   434,   114,   114,
     378,   114,   114,   444,   114,   114,   117,   464,   464,   387,
     114,   118,   117,   293,   120,   117,   114,   114,    37,   299,
     114,   117,   117,    21,   481,   481,   117,   117,   469,   470,
     117,   120,   117,   114,    21,   114,   473,    69,   318,   319,
      21,    21,    79,    79,    79,    20,    79,    32,    99,   346,
     428,   429,   184,   431,   432,   433,   434,   345,    44,    23,
       3,    15,   355,   421,   199,   200,    -1,    -1,    -1,    -1,
      -1,    -1,   352,    -1,   354,    -1,   211,    -1,   358,    -1,
      -1,   216,    -1,    -1,   364,    -1,    -1,    -1,   223,    -1,
      -1,   371,   372,   373,   374,   473,   231,   232,   233,   234,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   401,   402,    -1,    -1,    -1,    -1,    -1,    -1,   409,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
       5,     6,   422,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,   293,    24,
      -1,    -1,    -1,    -1,   299,    -1,   446,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,   318,   319,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   478,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   491,    -1,    -1,    -1,    -1,   496,   352,    -1,   354,
     500,   501,    -1,   358,     3,     4,     5,     6,     7,   364,
      -1,    -1,    -1,    -1,    -1,    -1,   371,   372,   373,   374,
      -1,    -1,    -1,    22,    -1,    24,    -1,    -1,    27,    28,
     115,    -1,    -1,   118,    -1,    -1,   121,    -1,    -1,    -1,
      39,    40,    -1,    -1,    -1,    -1,   401,   402,    -1,    -1,
      -1,    -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   422,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,     3,     4,
      -1,   446,     7,    -1,    -1,    -1,    -1,    96,    -1,    98,
      99,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   478,    39,    40,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   491,    -1,    -1,    -1,
      -1,   496,    -1,    -1,    -1,   500,   501,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,     6,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    22,    -1,    24,    -1,    26,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    98,    99,    -1,   101,   102,    39,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    98,    99,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,    -1,
     121,    -1,    -1,   124,   125,     5,     6,    -1,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,     5,     6,    -1,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,
      -1,    -1,    -1,    -1,     5,     6,    46,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,    -1,
      -1,   121,     5,     6,    -1,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,   118,    -1,    -1,
     121,     5,     6,    46,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    38,    -1,    -1,    -1,    -1,    -1,
       5,     6,    46,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    38,    -1,   118,    -1,   120,   121,     5,
       6,    46,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,    -1,    -1,   121,     5,     6,
      46,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,    -1,    -1,   121,     5,     6,    46,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,    -1,    -1,   121,     5,     6,    46,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,   118,    -1,    -1,   121,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    57,    58,   118,
      -1,    -1,   121,    -1,    -1,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    -1,    -1,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,   159,   160,   161,     0,    25,    31,    42,    43,    44,
      45,   140,   175,   177,   178,   184,    22,    24,    49,    56,
     139,   168,   178,   179,    59,    60,    61,    62,   141,   173,
      23,   185,   186,    30,   122,   176,   185,    50,    51,    52,
     165,   113,    20,    48,   140,   113,   173,     4,     5,     6,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    46,   115,   118,   121,   128,   148,
     149,   150,   151,   152,   168,   181,    29,   121,   174,   139,
     189,   113,   113,   113,   118,   166,   148,    32,    33,   158,
     158,     4,     4,    63,     8,   122,   152,   153,   168,   116,
     123,    35,    36,    47,    57,    58,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   106,   107,   108,   109,   110,   111,
     112,   130,   131,   132,   133,   134,   187,   193,   194,   196,
     197,   198,    24,    53,    54,   164,     4,    24,   167,   150,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,   135,   136,   138,   150,   155,   119,    64,   114,   122,
     116,    38,   153,   154,   150,   183,    57,     8,   183,     9,
      21,    10,    11,    12,    13,    14,    15,    16,    17,   135,
     136,   137,   141,   150,   150,   183,   150,   150,   190,   183,
     183,   183,   183,   150,   183,   183,   183,   183,   183,   183,
     183,   150,   150,   150,   150,   183,   141,    94,    95,   105,
     114,   120,   163,    27,    28,     3,     4,   129,     4,     7,
      26,    39,    40,    96,    98,    99,   118,   121,   124,   125,
     130,   131,   132,   134,   156,   181,   162,   152,     4,   152,
     152,    38,   150,   170,   171,   172,   114,   117,     3,     4,
       7,    27,    28,    39,    40,   125,   156,   180,   181,   182,
     182,   182,   182,   148,   114,   143,   114,   143,   182,   118,
     114,    37,   114,   114,   114,   182,   114,   114,   114,   114,
     114,   114,   114,   182,   182,   182,   182,   114,   148,   150,
     183,   183,    24,   114,   146,   116,   116,   116,   120,   155,
     157,   122,   157,    24,   157,   116,   116,   116,   116,   146,
     120,   152,   120,   168,   169,   114,   117,    38,   157,   114,
     114,   182,    15,    55,    15,   114,   195,   182,   118,   150,
     183,   183,   150,   195,   150,   183,   183,   183,   183,   150,
     150,   114,   114,   114,   114,   183,   182,   182,   114,   114,
      34,    55,   144,   147,   155,   155,   155,   114,   120,   122,
     126,   155,   155,   155,   155,   120,    38,   170,   144,   145,
     126,    21,    21,   116,   182,     4,   182,   183,   191,   114,
     182,   114,   114,   182,   114,   114,   114,   182,   182,   182,
     182,   116,   150,   183,    24,     4,   146,   195,    37,   114,
     155,   114,   114,   114,   114,    55,   142,   182,   182,   191,
     192,   114,   143,   143,   114,   182,   114,   183,   183,   195,
     183,   183,   183,   192,   182,   117,   150,   155,   155,   155,
     155,   155,     4,   114,   118,   117,   183,   120,   182,   114,
     114,   117,   117,   114,   117,   117,   117,   117,    21,   120,
     137,   188,    37,   120,   183,   183,   155,   182,   180,   120,
     137,    21,   117,   114,   180,   182,    21,   114,    69,   182,
      21,    21,   182,   182
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 993 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  if (yyvsp[0].UIntVal > (uint32_t)INT32_MAX)     // Outside of my range!
    ThrowException("Value too large for type!");
  yyval.SIntVal = (int32_t)yyvsp[0].UIntVal;
;}
    break;

  case 5:
#line 1001 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  if (yyvsp[0].UInt64Val > (uint64_t)INT64_MAX)     // Outside of my range!
    ThrowException("Value too large for type!");
  yyval.SInt64Val = (int64_t)yyvsp[0].UInt64Val;
;}
    break;

  case 40:
#line 1025 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.StrVal = yyvsp[-1].StrVal;
  ;}
    break;

  case 41:
#line 1028 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.StrVal = 0;
  ;}
    break;

  case 42:
#line 1032 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Linkage = GlobalValue::InternalLinkage; ;}
    break;

  case 43:
#line 1033 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Linkage = GlobalValue::LinkOnceLinkage; ;}
    break;

  case 44:
#line 1034 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Linkage = GlobalValue::WeakLinkage; ;}
    break;

  case 45:
#line 1035 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Linkage = GlobalValue::AppendingLinkage; ;}
    break;

  case 46:
#line 1036 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Linkage = GlobalValue::ExternalLinkage; ;}
    break;

  case 47:
#line 1038 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = CallingConv::C; ;}
    break;

  case 48:
#line 1039 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = CallingConv::C; ;}
    break;

  case 49:
#line 1040 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = CallingConv::Fast; ;}
    break;

  case 50:
#line 1041 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = CallingConv::Cold; ;}
    break;

  case 51:
#line 1042 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
                   if ((unsigned)yyvsp[0].UInt64Val != yyvsp[0].UInt64Val)
                     ThrowException("Calling conv too large!");
                   yyval.UIntVal = yyvsp[0].UInt64Val;
                 ;}
    break;

  case 52:
#line 1050 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = 0; ;}
    break;

  case 53:
#line 1051 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.UIntVal = yyvsp[0].UInt64Val;
  if (yyval.UIntVal != 0 && !isPowerOf2_32(yyval.UIntVal))
    ThrowException("Alignment must be a power of two!");
;}
    break;

  case 54:
#line 1056 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.UIntVal = 0; ;}
    break;

  case 55:
#line 1057 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.UIntVal = yyvsp[0].UInt64Val;
  if (yyval.UIntVal != 0 && !isPowerOf2_32(yyval.UIntVal))
    ThrowException("Alignment must be a power of two!");
;}
    break;

  case 56:
#line 1064 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  for (unsigned i = 0, e = strlen(yyvsp[0].StrVal); i != e; ++i)
    if (yyvsp[0].StrVal[i] == '"' || yyvsp[0].StrVal[i] == '\\')
      ThrowException("Invalid character in section name!");
  yyval.StrVal = yyvsp[0].StrVal;
;}
    break;

  case 57:
#line 1071 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.StrVal = 0; ;}
    break;

  case 58:
#line 1072 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.StrVal = yyvsp[0].StrVal; ;}
    break;

  case 59:
#line 1077 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {;}
    break;

  case 60:
#line 1078 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {;}
    break;

  case 61:
#line 1079 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurGV->setSection(yyvsp[0].StrVal);
    free(yyvsp[0].StrVal);
  ;}
    break;

  case 62:
#line 1083 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[0].UInt64Val != 0 && !isPowerOf2_32(yyvsp[0].UInt64Val))
      ThrowException("Alignment must be a power of two!");
    CurGV->setAlignment(yyvsp[0].UInt64Val);
  ;}
    break;

  case 64:
#line 1096 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType); ;}
    break;

  case 66:
#line 1097 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType); ;}
    break;

  case 67:
#line 1099 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!UpRefs.empty())
      ThrowException("Invalid upreference in type: " + (*yyvsp[0].TypeVal)->getDescription());
    yyval.TypeVal = yyvsp[0].TypeVal;
  ;}
    break;

  case 81:
#line 1110 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TypeVal = new PATypeHolder(OpaqueType::get());
  ;}
    break;

  case 82:
#line 1113 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TypeVal = new PATypeHolder(yyvsp[0].PrimType);
  ;}
    break;

  case 83:
#line 1116 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {            // Named types are also simple types...
  yyval.TypeVal = new PATypeHolder(getTypeVal(yyvsp[0].ValIDVal));
;}
    break;

  case 84:
#line 1122 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                   // Type UpReference
    if (yyvsp[0].UInt64Val > (uint64_t)~0U) ThrowException("Value out of range!");
    OpaqueType *OT = OpaqueType::get();        // Use temporary placeholder
    UpRefs.push_back(UpRefRecord((unsigned)yyvsp[0].UInt64Val, OT));  // Add to vector...
    yyval.TypeVal = new PATypeHolder(OT);
    UR_OUT("New Upreference!\n");
  ;}
    break;

  case 85:
#line 1129 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 86:
#line 1141 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {          // Sized array type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(ArrayType::get(*yyvsp[-1].TypeVal, (unsigned)yyvsp[-3].UInt64Val)));
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 87:
#line 1145 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {               // Vector type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(VectorType::get(*yyvsp[-1].TypeVal)));
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 88:
#line 1150 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {          // FixedVector type?
     const llvm::Type* ElemTy = yyvsp[-1].TypeVal->get();
     if ((unsigned)yyvsp[-2].UInt64Val != yyvsp[-2].UInt64Val) {
        ThrowException("Unsigned result not equal to signed result");
     }
     if(!ElemTy->isPrimitiveType()) {
        ThrowException("Element type of a FixedVectorType must be primitive");
     }
     //if (!isPowerOf2_32($2))
     //  ThrowException("Vector length should be a power of 2!");
     yyval.TypeVal = new PATypeHolder(HandleUpRefs(FixedVectorType::get(*yyvsp[-1].TypeVal, (unsigned)yyvsp[-2].UInt64Val)));
     delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 89:
#line 1164 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                        // Structure type?
    std::vector<const Type*> Elements;
    for (std::list<llvm::PATypeHolder>::iterator I = yyvsp[-1].TypeList->begin(),
           E = yyvsp[-1].TypeList->end(); I != E; ++I)
      Elements.push_back(*I);

    yyval.TypeVal = new PATypeHolder(HandleUpRefs(StructType::get(Elements)));
    delete yyvsp[-1].TypeList;
  ;}
    break;

  case 90:
#line 1173 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                                  // Empty structure type?
    yyval.TypeVal = new PATypeHolder(StructType::get(std::vector<const Type*>()));
  ;}
    break;

  case 91:
#line 1176 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                             // Pointer type?
    yyval.TypeVal = new PATypeHolder(HandleUpRefs(PointerType::get(*yyvsp[-1].TypeVal)));
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 92:
#line 1184 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TypeList = new std::list<PATypeHolder>();
    yyval.TypeList->push_back(*yyvsp[0].TypeVal); delete yyvsp[0].TypeVal;
  ;}
    break;

  case 93:
#line 1188 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    (yyval.TypeList=yyvsp[-2].TypeList)->push_back(*yyvsp[0].TypeVal); delete yyvsp[0].TypeVal;
  ;}
    break;

  case 95:
#line 1194 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    (yyval.TypeList=yyvsp[-2].TypeList)->push_back(Type::VoidTy);
  ;}
    break;

  case 96:
#line 1197 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    (yyval.TypeList = new std::list<PATypeHolder>())->push_back(Type::VoidTy);
  ;}
    break;

  case 97:
#line 1200 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TypeList = new std::list<PATypeHolder>();
  ;}
    break;

  case 98:
#line 1210 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 99:
#line 1235 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 100:
#line 1248 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 101:
#line 1276 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 102:
#line 1301 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 103:
#line 1321 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    const StructType *STy = dyn_cast<StructType>(yyvsp[-2].TypeVal->get());
    if (STy == 0)
      ThrowException("Cannot make struct constant with type: '" + 
                     (*yyvsp[-2].TypeVal)->getDescription() + "'!");

    if (STy->getNumContainedTypes() != 0)
      ThrowException("Illegal number of initializers for structure type!");

    yyval.ConstVal = ConstantStruct::get(STy, std::vector<Constant*>());
    delete yyvsp[-2].TypeVal;
  ;}
    break;

  case 104:
#line 1333 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    const PointerType *PTy = dyn_cast<PointerType>(yyvsp[-1].TypeVal->get());
    if (PTy == 0)
      ThrowException("Cannot make null pointer constant with type: '" + 
                     (*yyvsp[-1].TypeVal)->getDescription() + "'!");

    yyval.ConstVal = ConstantPointerNull::get(PTy);
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 105:
#line 1342 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ConstVal = UndefValue::get(yyvsp[-1].TypeVal->get());
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 106:
#line 1346 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 107:
#line 1405 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-1].TypeVal->get() != yyvsp[0].ConstVal->getType())
      ThrowException("Mismatched types for constant expression!");
    yyval.ConstVal = yyvsp[0].ConstVal;
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 108:
#line 1411 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    const Type *Ty = yyvsp[-1].TypeVal->get();
    if (isa<FunctionType>(Ty) || Ty == Type::LabelTy || isa<OpaqueType>(Ty))
      ThrowException("Cannot create a null initialized value of this type!");
    yyval.ConstVal = Constant::getNullValue(Ty);
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 109:
#line 1419 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {      // integral constants
    if (!ConstantSInt::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].SInt64Val))
      ThrowException("Constant value doesn't fit in type!");
    yyval.ConstVal = ConstantSInt::get(yyvsp[-1].PrimType, yyvsp[0].SInt64Val);
  ;}
    break;

  case 110:
#line 1424 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {            // integral constants
    if (!ConstantUInt::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].UInt64Val))
      ThrowException("Constant value doesn't fit in type!");
    yyval.ConstVal = ConstantUInt::get(yyvsp[-1].PrimType, yyvsp[0].UInt64Val);
  ;}
    break;

  case 111:
#line 1429 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                      // Boolean constants
    yyval.ConstVal = ConstantBool::True;
  ;}
    break;

  case 112:
#line 1432 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                     // Boolean constants
    yyval.ConstVal = ConstantBool::False;
  ;}
    break;

  case 113:
#line 1435 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                   // Float & Double constants
    if (!ConstantFP::isValueValidForType(yyvsp[-1].PrimType, yyvsp[0].FPVal))
      ThrowException("Floating point constant invalid for type!!");
    yyval.ConstVal = ConstantFP::get(yyvsp[-1].PrimType, yyvsp[0].FPVal);
  ;}
    break;

  case 114:
#line 1442 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!yyvsp[-3].ConstVal->getType()->isFirstClassType())
      ThrowException("cast constant expression from a non-primitive type: '" +
                     yyvsp[-3].ConstVal->getType()->getDescription() + "'!");
    if (!yyvsp[-1].TypeVal->get()->isFirstClassType())
      ThrowException("cast constant expression to a non-primitive type: '" +
                     yyvsp[-1].TypeVal->get()->getDescription() + "'!");
    yyval.ConstVal = ConstantExpr::getCast(yyvsp[-3].ConstVal, yyvsp[-1].TypeVal->get());
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 115:
#line 1452 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 116:
#line 1483 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-5].ConstVal->getType() != Type::BoolTy)
      ThrowException("Select condition must be of boolean type!");
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("Select operand types must match!");
    yyval.ConstVal = ConstantExpr::getSelect(yyvsp[-5].ConstVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;}
    break;

  case 117:
#line 1490 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 118:
#line 1511 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("Logical operator types must match!");
    if (!yyvsp[-3].ConstVal->getType()->isIntegral() &&
	!yyvsp[-3].ConstVal->getType()->isIntegralVector())
      ThrowException("Logical operands must have integral types!");
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;}
    break;

  case 119:
#line 1519 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-3].ConstVal->getType() != yyvsp[-1].ConstVal->getType())
      ThrowException("setcc operand types must match!");
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].BinaryOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;}
    break;

  case 120:
#line 1524 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-1].ConstVal->getType() != Type::UByteTy)
      ThrowException("Shift count for shift constant must be unsigned byte!");
    if (!yyvsp[-3].ConstVal->getType()->isInteger() &&
	!yyvsp[-3].ConstVal->getType()->isIntegerVector()) {
      ThrowException("Shift constant expression requires integer operand!");
    }
    yyval.ConstVal = ConstantExpr::get(yyvsp[-5].OtherOpVal, yyvsp[-3].ConstVal, yyvsp[-1].ConstVal);
  ;}
    break;

  case 121:
#line 1536 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    (yyval.ConstVector = yyvsp[-2].ConstVector)->push_back(yyvsp[0].ConstVal);
  ;}
    break;

  case 122:
#line 1539 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ConstVector = new std::vector<Constant*>();
    yyval.ConstVector->push_back(yyvsp[0].ConstVal);
  ;}
    break;

  case 123:
#line 1546 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.BoolVal = false; ;}
    break;

  case 124:
#line 1546 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.BoolVal = true; ;}
    break;

  case 125:
#line 1556 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.ModuleVal = ParserResult = yyvsp[0].ModuleVal;
  CurModule.ModuleDone();
;}
    break;

  case 126:
#line 1563 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
    CurFun.FunctionDone();
  ;}
    break;

  case 127:
#line 1567 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
  ;}
    break;

  case 128:
#line 1570 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ModuleVal = yyvsp[-1].ModuleVal;
  ;}
    break;

  case 129:
#line 1573 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 130:
#line 1586 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 131:
#line 1606 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {       // Function prototypes can be in const pool
  ;}
    break;

  case 132:
#line 1608 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[0].ConstVal == 0) ThrowException("Global value initializer is not a constant!");
    CurGV = ParseGlobalVariable(yyvsp[-3].StrVal, yyvsp[-2].Linkage, yyvsp[-1].BoolVal, yyvsp[0].ConstVal->getType(), yyvsp[0].ConstVal);
                                                       ;}
    break;

  case 133:
#line 1611 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurGV = 0;
  ;}
    break;

  case 134:
#line 1614 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurGV = ParseGlobalVariable(yyvsp[-3].StrVal, GlobalValue::ExternalLinkage,
                                             yyvsp[-1].BoolVal, *yyvsp[0].TypeVal, 0);
    delete yyvsp[0].TypeVal;
                                                   ;}
    break;

  case 135:
#line 1618 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurGV = 0;
  ;}
    break;

  case 136:
#line 1621 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { 
  ;}
    break;

  case 137:
#line 1623 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  ;}
    break;

  case 138:
#line 1625 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { 
  ;}
    break;

  case 139:
#line 1630 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Endianness = Module::BigEndian; ;}
    break;

  case 140:
#line 1631 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.Endianness = Module::LittleEndian; ;}
    break;

  case 141:
#line 1633 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurModule.CurrentModule->setEndianness(yyvsp[0].Endianness);
  ;}
    break;

  case 142:
#line 1636 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[0].UInt64Val == 32)
      CurModule.CurrentModule->setPointerSize(Module::Pointer32);
    else if (yyvsp[0].UInt64Val == 64)
      CurModule.CurrentModule->setPointerSize(Module::Pointer64);
    else
      ThrowException("Invalid pointer size: '" + utostr(yyvsp[0].UInt64Val) + "'!");
  ;}
    break;

  case 143:
#line 1644 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    CurModule.CurrentModule->setTargetTriple(yyvsp[0].StrVal);
    free(yyvsp[0].StrVal);
  ;}
    break;

  case 145:
#line 1651 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
          CurModule.CurrentModule->addLibrary(yyvsp[0].StrVal);
          free(yyvsp[0].StrVal);
        ;}
    break;

  case 146:
#line 1655 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
          CurModule.CurrentModule->addLibrary(yyvsp[0].StrVal);
          free(yyvsp[0].StrVal);
        ;}
    break;

  case 147:
#line 1659 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
        ;}
    break;

  case 151:
#line 1668 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.StrVal = 0; ;}
    break;

  case 152:
#line 1670 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  if (*yyvsp[-1].TypeVal == Type::VoidTy)
    ThrowException("void typed arguments are invalid!");
  yyval.ArgVal = new std::pair<PATypeHolder*, char*>(yyvsp[-1].TypeVal, yyvsp[0].StrVal);
;}
    break;

  case 153:
#line 1676 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = yyvsp[-2].ArgList;
    yyvsp[-2].ArgList->push_back(*yyvsp[0].ArgVal);
    delete yyvsp[0].ArgVal;
  ;}
    break;

  case 154:
#line 1681 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = new std::vector<std::pair<PATypeHolder*,char*> >();
    yyval.ArgList->push_back(*yyvsp[0].ArgVal);
    delete yyvsp[0].ArgVal;
  ;}
    break;

  case 155:
#line 1687 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = yyvsp[0].ArgList;
  ;}
    break;

  case 156:
#line 1690 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = yyvsp[-2].ArgList;
    yyval.ArgList->push_back(std::pair<PATypeHolder*,
                            char*>(new PATypeHolder(Type::VoidTy), 0));
  ;}
    break;

  case 157:
#line 1695 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = new std::vector<std::pair<PATypeHolder*,char*> >();
    yyval.ArgList->push_back(std::make_pair(new PATypeHolder(Type::VoidTy), (char*)0));
  ;}
    break;

  case 158:
#line 1699 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ArgList = 0;
  ;}
    break;

  case 159:
#line 1704 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  UnEscapeLexed(yyvsp[-5].StrVal);
  std::string FunctionName(yyvsp[-5].StrVal);
  free(yyvsp[-5].StrVal);  // Free strdup'd memory!
  
  if (!(*yyvsp[-6].TypeVal)->isFirstClassType() && *yyvsp[-6].TypeVal != Type::VoidTy)
    ThrowException("LLVM functions cannot return aggregate types!");

  std::vector<const Type*> ParamTypeList;
  if (yyvsp[-3].ArgList) {   // If there are arguments...
    for (std::vector<std::pair<PATypeHolder*,char*> >::iterator I = yyvsp[-3].ArgList->begin();
         I != yyvsp[-3].ArgList->end(); ++I)
      ParamTypeList.push_back(I->first->get());
  }

  bool isVarArg = ParamTypeList.size() && ParamTypeList.back() == Type::VoidTy;
  if (isVarArg) ParamTypeList.pop_back();

  const FunctionType *FT = FunctionType::get(*yyvsp[-6].TypeVal, ParamTypeList, isVarArg);
  const PointerType *PFT = PointerType::get(FT);
  delete yyvsp[-6].TypeVal;

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
  Fn->setCallingConv(yyvsp[-7].UIntVal);
  Fn->setAlignment(yyvsp[0].UIntVal);
  if (yyvsp[-1].StrVal) {
    Fn->setSection(yyvsp[-1].StrVal);
    free(yyvsp[-1].StrVal);
  }

  // Add all of the arguments we parsed to the function...
  if (yyvsp[-3].ArgList) {                     // Is null if empty...
    if (isVarArg) {  // Nuke the last entry
      assert(yyvsp[-3].ArgList->back().first->get() == Type::VoidTy && yyvsp[-3].ArgList->back().second == 0&&
             "Not a varargs marker!");
      delete yyvsp[-3].ArgList->back().first;
      yyvsp[-3].ArgList->pop_back();  // Delete the last entry
    }
    Function::arg_iterator ArgIt = Fn->arg_begin();
    for (std::vector<std::pair<PATypeHolder*,char*> >::iterator I = yyvsp[-3].ArgList->begin();
         I != yyvsp[-3].ArgList->end(); ++I, ++ArgIt) {
      delete I->first;                          // Delete the typeholder...

      setValueName(ArgIt, I->second);           // Insert arg into symtab...
      InsertValue(ArgIt);
    }

    delete yyvsp[-3].ArgList;                     // We're now done with the argument list
  }
;}
    break;

  case 162:
#line 1791 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.FunctionVal = CurFun.CurrentFunction;

  // Make sure that we keep track of the linkage type even if there was a
  // previous "declare".
  yyval.FunctionVal->setLinkage(yyvsp[-2].Linkage);
;}
    break;

  case 165:
#line 1801 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.FunctionVal = yyvsp[-1].FunctionVal;
;}
    break;

  case 166:
#line 1805 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { CurFun.isDeclare = true; ;}
    break;

  case 167:
#line 1805 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  yyval.FunctionVal = CurFun.CurrentFunction;
  CurFun.FunctionDone();
;}
    break;

  case 168:
#line 1814 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {    // A reference to a direct constant
    yyval.ValIDVal = ValID::create(yyvsp[0].SInt64Val);
  ;}
    break;

  case 169:
#line 1817 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::create(yyvsp[0].UInt64Val);
  ;}
    break;

  case 170:
#line 1820 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                     // Perhaps it's an FP constant?
    yyval.ValIDVal = ValID::create(yyvsp[0].FPVal);
  ;}
    break;

  case 171:
#line 1823 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::create(ConstantBool::True);
  ;}
    break;

  case 172:
#line 1826 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::create(ConstantBool::False);
  ;}
    break;

  case 173:
#line 1829 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::createNull();
  ;}
    break;

  case 174:
#line 1832 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::createUndef();
  ;}
    break;

  case 175:
#line 1835 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 176:
#line 1859 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValIDVal = ValID::create(yyvsp[0].ConstVal);
  ;}
    break;

  case 177:
#line 1866 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {  // Is it an integer reference...?
    yyval.ValIDVal = ValID::create(yyvsp[0].SIntVal);
  ;}
    break;

  case 178:
#line 1869 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                   // Is it a named reference...?
    yyval.ValIDVal = ValID::create(yyvsp[0].StrVal);
  ;}
    break;

  case 181:
#line 1880 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValueVal = getVal(*yyvsp[-1].TypeVal, yyvsp[0].ValIDVal); delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 182:
#line 1884 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.FunctionVal = yyvsp[-1].FunctionVal;
  ;}
    break;

  case 183:
#line 1887 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { // Do not allow functions with 0 basic blocks   
    yyval.FunctionVal = yyvsp[-1].FunctionVal;
  ;}
    break;

  case 184:
#line 1895 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    setValueName(yyvsp[0].TermInstVal, yyvsp[-1].StrVal);
    InsertValue(yyvsp[0].TermInstVal);

    yyvsp[-2].BasicBlockVal->getInstList().push_back(yyvsp[0].TermInstVal);
    InsertValue(yyvsp[-2].BasicBlockVal);
    yyval.BasicBlockVal = yyvsp[-2].BasicBlockVal;
  ;}
    break;

  case 185:
#line 1904 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyvsp[-1].BasicBlockVal->getInstList().push_back(yyvsp[0].InstVal);
    yyval.BasicBlockVal = yyvsp[-1].BasicBlockVal;
  ;}
    break;

  case 186:
#line 1908 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BasicBlockVal = CurBB = getBBVal(ValID::create((int)CurFun.NextBBNum++), true);

    // Make sure to move the basic block to the correct location in the
    // function, instead of leaving it inserted wherever it was first
    // referenced.
    Function::BasicBlockListType &BBL = 
      CurFun.CurrentFunction->getBasicBlockList();
    BBL.splice(BBL.end(), BBL, yyval.BasicBlockVal);
  ;}
    break;

  case 187:
#line 1918 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BasicBlockVal = CurBB = getBBVal(ValID::create(yyvsp[0].StrVal), true);

    // Make sure to move the basic block to the correct location in the
    // function, instead of leaving it inserted wherever it was first
    // referenced.
    Function::BasicBlockListType &BBL = 
      CurFun.CurrentFunction->getBasicBlockList();
    BBL.splice(BBL.end(), BBL, yyval.BasicBlockVal);
  ;}
    break;

  case 188:
#line 1929 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {              // Return with a result...
    yyval.TermInstVal = new ReturnInst(yyvsp[0].ValueVal);
  ;}
    break;

  case 189:
#line 1932 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                                       // Return with no result...
    yyval.TermInstVal = new ReturnInst();
  ;}
    break;

  case 190:
#line 1935 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {                         // Unconditional Branch...
    yyval.TermInstVal = new BranchInst(getBBVal(yyvsp[0].ValIDVal));
  ;}
    break;

  case 191:
#line 1938 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {  
    yyval.TermInstVal = new BranchInst(getBBVal(yyvsp[-3].ValIDVal), getBBVal(yyvsp[0].ValIDVal), getVal(Type::BoolTy, yyvsp[-6].ValIDVal));
  ;}
    break;

  case 192:
#line 1941 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 193:
#line 1955 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    SwitchInst *S = new SwitchInst(getVal(yyvsp[-6].PrimType, yyvsp[-5].ValIDVal), getBBVal(yyvsp[-2].ValIDVal), 0);
    yyval.TermInstVal = S;
  ;}
    break;

  case 194:
#line 1960 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 195:
#line 2012 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TermInstVal = new UnwindInst();
  ;}
    break;

  case 196:
#line 2015 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.TermInstVal = new UnreachableInst();
  ;}
    break;

  case 197:
#line 2021 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.JumpTable = yyvsp[-5].JumpTable;
    Constant *V = cast<Constant>(getValNonImprovising(yyvsp[-4].PrimType, yyvsp[-3].ValIDVal));
    if (V == 0)
      ThrowException("May only switch on a constant pool value!");

    yyval.JumpTable->push_back(std::make_pair(V, getBBVal(yyvsp[0].ValIDVal)));
  ;}
    break;

  case 198:
#line 2029 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.JumpTable = new std::vector<std::pair<Constant*, BasicBlock*> >();
    Constant *V = cast<Constant>(getValNonImprovising(yyvsp[-4].PrimType, yyvsp[-3].ValIDVal));

    if (V == 0)
      ThrowException("May only switch on a constant pool value!");

    yyval.JumpTable->push_back(std::make_pair(V, getBBVal(yyvsp[0].ValIDVal)));
  ;}
    break;

  case 199:
#line 2039 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
  // Is this definition named?? if so, assign the name...
  setValueName(yyvsp[0].InstVal, yyvsp[-1].StrVal);
  InsertValue(yyvsp[0].InstVal);
  yyval.InstVal = yyvsp[0].InstVal;
;}
    break;

  case 200:
#line 2046 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {    // Used for PHI nodes
    yyval.PHIList = new std::list<std::pair<Value*, BasicBlock*> >();
    yyval.PHIList->push_back(std::make_pair(getVal(*yyvsp[-5].TypeVal, yyvsp[-3].ValIDVal), getBBVal(yyvsp[-1].ValIDVal)));
    delete yyvsp[-5].TypeVal;
  ;}
    break;

  case 201:
#line 2051 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.PHIList = yyvsp[-6].PHIList;
    yyvsp[-6].PHIList->push_back(std::make_pair(getVal(yyvsp[-6].PHIList->front().first->getType(), yyvsp[-3].ValIDVal),
                                 getBBVal(yyvsp[-1].ValIDVal)));
  ;}
    break;

  case 202:
#line 2057 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {    // Used for call statements, and memory insts...
    yyval.ValueList = new std::vector<Value*>();
    yyval.ValueList->push_back(yyvsp[0].ValueVal);
  ;}
    break;

  case 203:
#line 2061 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.ValueList = yyvsp[-2].ValueList;
    yyvsp[-2].ValueList->push_back(yyvsp[0].ValueVal);
  ;}
    break;

  case 205:
#line 2067 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { yyval.ValueList = 0; ;}
    break;

  case 206:
#line 2069 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = true;
  ;}
    break;

  case 207:
#line 2072 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = false;
  ;}
    break;

  case 208:
#line 2078 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 209:
#line 2091 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!(*yyvsp[-3].TypeVal)->isIntegral() &&
	!(*yyvsp[-3].TypeVal)->isIntegralVector())
      ThrowException("Logical operator requires integral operands!");
    yyval.InstVal = BinaryOperator::create(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;}
    break;

  case 210:
#line 2100 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new SetCondInst(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;}
    break;

  case 211:
#line 2106 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new SetCondInst(yyvsp[-4].BinaryOpVal, getVal(*yyvsp[-3].TypeVal, yyvsp[-2].ValIDVal), getVal(*yyvsp[-3].TypeVal, yyvsp[0].ValIDVal));
    if (yyval.InstVal == 0)
      ThrowException("binary operator returned null!");
    delete yyvsp[-3].TypeVal;
  ;}
    break;

  case 212:
#line 2112 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    std::cerr << "WARNING: Use of eliminated 'not' instruction:"
              << " Replacing with 'xor'.\n";

    Value *Ones = ConstantIntegral::getAllOnesValue(yyvsp[0].ValueVal->getType());
    if (Ones == 0)
      ThrowException("Expected integral type for not instruction!");

    yyval.InstVal = BinaryOperator::create(Instruction::Xor, yyvsp[0].ValueVal, Ones);
    if (yyval.InstVal == 0)
      ThrowException("Could not create a xor instruction!");
  ;}
    break;

  case 213:
#line 2124 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[0].ValueVal->getType() != Type::UByteTy)
      ThrowException("Shift amount must be ubyte!");
    if (!yyvsp[-2].ValueVal->getType()->isInteger() &&
	!yyvsp[-2].ValueVal->getType()->isIntegerVector())
      ThrowException("Shift requires integer operand!");
    yyval.InstVal = new ShiftInst(yyvsp[-3].OtherOpVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;}
    break;

  case 214:
#line 2132 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!yyvsp[0].TypeVal->get()->isFirstClassType())
      ThrowException("cast instruction to a non-primitive type: '" +
                     yyvsp[0].TypeVal->get()->getDescription() + "'!");
    yyval.InstVal = new CastInst(yyvsp[-2].ValueVal, *yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;}
    break;

  case 215:
#line 2139 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[-4].ValueVal->getType() != Type::BoolTy)
      ThrowException("select condition must be boolean!");
    if (yyvsp[-2].ValueVal->getType() != yyvsp[0].ValueVal->getType())
      ThrowException("select value types should match!");
    yyval.InstVal = new SelectInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;}
    break;

  case 216:
#line 2146 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!yyvsp[-4].ValueVal->getType()->isBooleanVector())
      ThrowException("vselect condition must be boolean vector!");
    if (yyvsp[-2].ValueVal->getType() != yyvsp[0].ValueVal->getType())
      ThrowException("vselect value types should match!");
    if (!isa<VectorType>(yyvsp[-2].ValueVal->getType()))
      ThrowException("vselect value must be a vector!");
    yyval.InstVal = new VSelectInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;}
    break;

  case 217:
#line 2155 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    NewVarArgs = true;
    yyval.InstVal = new VAArgInst(yyvsp[-2].ValueVal, *yyvsp[0].TypeVal);
    delete yyvsp[0].TypeVal;
  ;}
    break;

  case 218:
#line 2160 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    ObsoleteVarArgs = true;
    const Type* ArgTy = yyvsp[-2].ValueVal->getType();
    Function* NF = CurModule.CurrentModule->
      getOrInsertFunction("llvm.va_copy", ArgTy, ArgTy, (Type *)0);

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
  ;}
    break;

  case 219:
#line 2179 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    ObsoleteVarArgs = true;
    const Type* ArgTy = yyvsp[-2].ValueVal->getType();
    Function* NF = CurModule.CurrentModule->
      getOrInsertFunction("llvm.va_copy", ArgTy, ArgTy, (Type *)0);

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
  ;}
    break;

  case 220:
#line 2201 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 221:
#line 2215 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 222:
#line 2272 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = yyvsp[0].InstVal;
  ;}
    break;

  case 223:
#line 2278 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { 
    yyval.ValueList = yyvsp[0].ValueList; 
  ;}
    break;

  case 224:
#line 2280 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    { 
    yyval.ValueList = new std::vector<Value*>(); 
  ;}
    break;

  case 225:
#line 2284 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = true;
  ;}
    break;

  case 226:
#line 2287 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = false;
  ;}
    break;

  case 227:
#line 2291 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = true;
  ;}
    break;

  case 228:
#line 2294 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.BoolVal = false;
  ;}
    break;

  case 229:
#line 2299 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new MallocInst(*yyvsp[-1].TypeVal, 0, yyvsp[0].UIntVal);
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 230:
#line 2303 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new MallocInst(*yyvsp[-4].TypeVal, getVal(yyvsp[-2].PrimType, yyvsp[-1].ValIDVal), yyvsp[0].UIntVal);
    delete yyvsp[-4].TypeVal;
  ;}
    break;

  case 231:
#line 2307 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new AllocaInst(*yyvsp[-1].TypeVal, 0, yyvsp[0].UIntVal);
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 232:
#line 2311 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    yyval.InstVal = new AllocaInst(*yyvsp[-4].TypeVal, getVal(yyvsp[-2].PrimType, yyvsp[-1].ValIDVal), yyvsp[0].UIntVal);
    delete yyvsp[-4].TypeVal;
  ;}
    break;

  case 233:
#line 2315 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!isa<PointerType>(yyvsp[0].ValueVal->getType()))
      ThrowException("Trying to free nonpointer type " + 
                     yyvsp[0].ValueVal->getType()->getDescription() + "!");
    yyval.InstVal = new FreeInst(yyvsp[0].ValueVal);
  ;}
    break;

  case 234:
#line 2322 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!isa<PointerType>(yyvsp[-1].TypeVal->get()))
      ThrowException("Can't load from nonpointer type: " +
                     (*yyvsp[-1].TypeVal)->getDescription());
    if (!cast<PointerType>(yyvsp[-1].TypeVal->get())->getElementType()->isFirstClassType())
      ThrowException("Can't load from pointer of non-first-class type: " +
                     (*yyvsp[-1].TypeVal)->getDescription());
    yyval.InstVal = new LoadInst(getVal(*yyvsp[-1].TypeVal, yyvsp[0].ValIDVal), "", yyvsp[-3].BoolVal);
    delete yyvsp[-1].TypeVal;
  ;}
    break;

  case 235:
#line 2333 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 236:
#line 2347 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 237:
#line 2369 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 238:
#line 2384 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Length of vimm must be unsigned int!");
    yyval.InstVal = new VImmInst(yyvsp[-2].ValueVal, yyvsp[0].ValueVal, yyvsp[-4].BoolVal);
  ;}
    break;

  case 239:
#line 2390 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 240:
#line 2402 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!isa<VectorType>(yyvsp[-2].ValueVal->getType()))
      ThrowException("First operand of extractelement must be a vector!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Second operand of extractelement must be a uint!");
    yyval.InstVal = new ExtractElementInst(yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;}
    break;

  case 241:
#line 2410 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;

  case 242:
#line 2422 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
    {
    if (!isa<VectorType>(yyvsp[-4].ValueVal->getType()))
      ThrowException("First operand of combineelement must be a vector!");
    if (yyvsp[-2].ValueVal->getType() != cast<VectorType>(yyvsp[-4].ValueVal->getType())->getElementType())
      ThrowException("Second operand of combineelement must be vector element type!");
    if (yyvsp[0].ValueVal->getType() != Type::UIntTy)
      ThrowException("Third operand of combineelement must be a uint!");
    yyval.InstVal = new CombineElementInst(yyvsp[-4].ValueVal, yyvsp[-2].ValueVal, yyvsp[0].ValueVal);
  ;}
    break;

  case 243:
#line 2432 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"
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
  ;}
    break;


    }

/* Line 999 of yacc.c.  */
#line 4766 "llvmAsmParser.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 2465 "/home/vadve/bocchino/llvm/src/lib/AsmParser/llvmAsmParser.y"

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

