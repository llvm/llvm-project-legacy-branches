//===- LLToken.h - Token Codes for LLVM Assembly Files ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the enums for the .ll lexer.
//
//===----------------------------------------------------------------------===//

#ifndef LIBS_ASMPARSER_LLTOKEN_H
#define LIBS_ASMPARSER_LLTOKEN_H

namespace llvm {
namespace lltok {
  enum Kind {
    // Markers
    Eof, Error,

    // Tokens with no info.
    dotdotdot,         // ...
    equal,   comma,    // =  ,
    star,              // *
    lsquare, rsquare,  // [  ]
    lbrace,  rbrace,   // {  }
    less,    greater,  // <  >
    lparen,  rparen,   // (  )
    backslash,         // \    (not /)
    exclaim,           // !

    kw_x,
    kw_begin,   kw_end,
    kw_true,    kw_false,
    kw_declare, kw_define,
    kw_global,  kw_constant,

    // Linkage tokens.
    kw_appending,
    kw_available_externally,
    kw_common,
    kw_dllexport,
    kw_dllimport,
    kw_extern_weak,
    kw_external,
    kw_internal,
    kw_linker_private,
    kw_linker_private_weak,
    kw_linker_private_weak_def_auto,
    kw_linkonce,
    kw_linkonce_odr,
    kw_private,
    kw_weak,
    kw_weak_odr,

    // Visibility tokens.
    kw_default,
    kw_hidden,
    kw_protected,

    kw_addrspace,
    kw_alias,
    kw_align,
    kw_alignstack,
    kw_asm,
    kw_c,
    kw_datalayout,
    kw_deplibs,
    kw_exact,
    kw_inbounds,
    kw_gc,
    kw_module,
    kw_nsw,
    kw_null,
    kw_nuw,
    kw_section,
    kw_sideeffect,
    kw_tail,
    kw_target,
    kw_thread_local,
    kw_triple,
    kw_to,
    kw_undef,
    kw_volatile,
    kw_zeroinitializer,

    // Exception handling tokens.
    kw_catchall,
    kw_catches,
    kw_personality,

    // Calling convention tokens.
    kw_arm_aapcs_vfpcc,
    kw_arm_aapcscc,
    kw_arm_apcscc,
    kw_cc,
    kw_ccc,
    kw_coldcc,
    kw_fastcc,
    kw_msp430_intrcc,
    kw_ptx_kernel,
    kw_ptx_device,
    kw_x86_fastcallcc,
    kw_x86_stdcallcc,
    kw_x86_thiscallcc,

    // Attribute tokens.
    kw_alwaysinline,
    kw_byval,
    kw_inlinehint,
    kw_inreg,
    kw_naked,
    kw_nest,
    kw_noalias,
    kw_nocapture,
    kw_noimplicitfloat,
    kw_noinline,
    kw_noredzone,
    kw_noreturn,
    kw_nounwind,
    kw_optsize,
    kw_readnone,
    kw_readonly,
    kw_signext,
    kw_sret,
    kw_ssp,
    kw_sspreq,
    kw_zeroext,
    kw_hotpatch,

    // Type tokens.
    kw_opaque,
    kw_type,

    // Binary operator tokens.
    kw_eq,  kw_ne,  kw_slt, kw_sgt, kw_sle, kw_sge, kw_ult, kw_ugt, kw_ule,
    kw_uge, kw_oeq, kw_one, kw_olt, kw_ogt, kw_ole, kw_oge, kw_ord, kw_uno,
    kw_ueq, kw_une,

    // Instruction Opcodes (Opcode in UIntVal).
    kw_add,  kw_fadd, kw_sub,  kw_fsub, kw_mul,  kw_fmul,
    kw_udiv, kw_sdiv, kw_fdiv,
    kw_urem, kw_srem, kw_frem, kw_shl,  kw_lshr, kw_ashr,
    kw_and,  kw_or,   kw_xor,  kw_icmp, kw_fcmp,

    kw_bitcast,
    kw_call,
    kw_indirectbr,
    kw_fpext,
    kw_fptosi,
    kw_fptoui,
    kw_fptrunc,
    kw_inttoptr,
    kw_ptrtoint,
    kw_select,
    kw_sext,
    kw_sitofp,
    kw_uitofp,
    kw_va_arg,
    kw_zext,

    // Terminator instruction tokens.
    kw_br,
    kw_invoke,
    kw_phi,
    kw_ret,
    kw_switch,
    kw_trunc,
    kw_unreachable,
    kw_unwind,

    // Memory instruction tokens.
    kw_alloca,
    kw_blockaddress,
    kw_free,
    kw_getelementptr,
    kw_load,
    kw_malloc,
    kw_store,

    // Vector instruction tokens.
    kw_extractelement,
    kw_extractvalue,
    kw_getresult,
    kw_insertelement,
    kw_insertvalue,
    kw_shufflevector,

    // Unsigned Valued tokens (UIntVal).
    GlobalID,          // @42
    LocalVarID,        // %42

    // String valued tokens (StrVal).
    LabelStr,          // foo:
    GlobalVar,         // @foo @"foo"
    LocalVar,          // %foo %"foo"
    MetadataVar,       // !foo
    StringConstant,    // "foo"

    // Type valued tokens (TyVal).
    Type,

    APFloat,           // APFloatVal
    APSInt             // APSInt
  };
} // end namespace lltok
} // end namespace llvm

#endif
