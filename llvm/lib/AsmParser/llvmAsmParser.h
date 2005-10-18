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


extern YYSTYPE llvmAsmlval;
