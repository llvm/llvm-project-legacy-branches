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
/* Line 1240 of yacc.c.  */
#line 300 "llvmAsmParser.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE llvmAsmlval;



