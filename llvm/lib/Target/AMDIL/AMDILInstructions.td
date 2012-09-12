//===-- AMDILInstructions.td ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

let isReMaterializable = 0, isAsCheapAsAMove = 1, loadconst = 1 in {
  defm LOADCONST  : ILConstant<"mov $dst, $val">;
  defm LOADFI     : ILFrameIndex<"mov $dst, $val">;
  }
defm OR         : BinaryOpMCInt<IL_OP_I_OR, or>;
defm AND        : BinaryOpMCInt<IL_OP_AND, and>;
defm SELECT     : SelectMC<IL_OP_CMOV_LOGICAL>;
defm SMUL       : BinaryOpMCi32<IL_OP_I_MUL, mul>;
defm SMULHI     : BinaryOpMCi32<IL_OP_I_MUL_HIGH, mulhs>;
defm XOR        : BinaryOpMCInt<IL_OP_I_XOR, xor>;
defm CUSTOM_ADD : BinaryOpMCi32<IL_OP_I_ADD, IL_add>;
defm EADD   : BinaryOpMCi32<IL_OP_I_ADD, adde>;
// Integer offsets for addressing
def ADDpr       : ILFormat<IL_OP_I_ADD, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, GPRI32:$offset),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $ptr, $offset"),
          [(set GPRI32:$dst,
        (IL_addaddrri ADDR:$ptr,
          (i32 GPRI32:$offset)))]>;
def ADDrp       : ILFormat<IL_OP_I_ADD, (outs GPRI32:$dst),
      (ins GPRI32:$offset,  MEM3232:$ptr),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $offset, $ptr"),
          [(set GPRI32:$dst,
        (IL_addaddrir
          (i32 GPRI32:$offset), ADDR:$ptr))]>;
def ADDi64pr       : ILFormat<IL_OP_I64_ADD, (outs GPRI64:$dst),
      (ins MEM6464:$ptr, GPRI64:$offset),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $ptr, $offset"),
          [(set GPRI64:$dst,
        (IL_addaddrri ADDR64:$ptr,
          (i64 GPRI64:$offset)))]>;
def ADDi64rp       : ILFormat<IL_OP_I64_ADD, (outs GPRI64:$dst),
      (ins GPRI64:$offset,  MEM6464:$ptr),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $offset, $ptr"),
          [(set GPRI64:$dst,
        (IL_addaddrir
          (i64 GPRI64:$offset), ADDR64:$ptr))]>;

let mayLoad = 0, mayStore = 0 in {
defm ABS : UnaryIntMCInt<IL_OP_ABS, int_AMDIL_abs>;
defm BITCOUNT : UnaryIntMCInt<IL_OP_IBIT_COUNT, int_AMDIL_bit_count_i32>;
defm FFB_LO : UnaryIntMCInt<IL_OP_I_FFB_LO, int_AMDIL_bit_find_first_lo>;
defm FFB_HI : UnaryIntMCInt<IL_OP_I_FFB_HI, int_AMDIL_bit_find_first_hi>;
defm FFB_SGN : UnaryIntMCInt<IL_OP_I_FFB_SGN,
        int_AMDIL_bit_find_first_sgn>;
defm IMULHI  : BinaryIntMCi32<IL_OP_I_MUL_HIGH, int_AMDIL_mulhi_i32>;
let Predicates = [HasHWSign24Bit] in {
defm IMUL24 : BinaryIntMCi32<IL_OP_I_MUL24, int_AMDIL_mul24_i32>;
defm IMULHI24 : BinaryIntMCi32<IL_OP_I_MULHI24, int_AMDIL_mulhi24_i32>;
defm IMAD24  : TernaryIntMCInt<IL_OP_I_MAD24, int_AMDIL_mad24_i32>;
}
defm CARRY  : BinaryIntMCi32<IL_OP_I_CARRY, int_AMDIL_carry_i32>;
defm BORROW  : BinaryIntMCi32<IL_OP_I_BORROW, int_AMDIL_borrow_i32>;
defm IMIN  : BinaryIntMCi32<IL_OP_I_MIN, int_AMDIL_min_i32>;
defm IMAX  : BinaryIntMCi32<IL_OP_I_MAX, int_AMDIL_max_i32>;
defm CMOV_LOG  : TernaryIntMCInt<IL_OP_CMOV_LOGICAL, int_AMDIL_cmov_logical>;
defm IBIT_EXTRACT : TernaryIntMCInt<IL_OP_IBIT_EXTRACT,
          int_AMDIL_bit_extract_i32>;
defm IMAD  : TernaryIntMCInt<IL_OP_I_MAD, int_AMDIL_mad_i32>;
defm SAD   : TernaryIntMCInt<IL_OP_SAD, int_AMDIL_media_sad>;
defm SADHI : TernaryIntMCInt<IL_OP_SAD_HI, int_AMDIL_media_sad_hi>;
}
defm SAD4  : TernaryIntMCFull<IL_OP_SAD4, int_AMDIL_media_sad4,
             i32,   GPRI32,             v4i32, GPRV4I32, v4i32imm, imm,
             v4i32, GPRV4I32, v4i32imm, imm, i32,   GPRI32,   i32imm, imm>;
defm FTOV4U8 : UnaryIntMCFull<IL_OP_F2U4, int_AMDIL_media_convert_f2v4u8,
                              i32, GPRI32, v4f32, GPRV4F32, v4f32imm, fpimm>;
//===---------------------------------------------------------------------===//
// Unsigned 32bit integer math instructions start here
//===---------------------------------------------------------------------===//
defm UMUL       : BinaryOpMCi32<IL_OP_U_MUL, IL_umul>;
defm UMULHI     : BinaryOpMCi32<IL_OP_U_MUL_HIGH, mulhu>;
defm UDIV       : BinaryOpMCi32<IL_OP_U_DIV, udiv>;
defm NATIVE_UDIV  : BinaryIntMCi32<IL_OP_U_DIV, int_AMDIL_udiv>;
let mayLoad=0, mayStore=0 in {
defm UBIT_REVERSE : UnaryIntMCInt<IL_OP_UBIT_REVERSE,
        int_AMDIL_bit_reverse_u32>;
defm UMULHI_INT : BinaryIntMCi32<IL_OP_U_MUL_HIGH, int_AMDIL_mulhi_u32>;
defm UMULHI24   : BinaryIntMCi32<IL_OP_U_MULHI24, int_AMDIL_mulhi24_u32>;
defm UMUL24     : BinaryIntMCi32<IL_OP_U_MUL24, int_AMDIL_mul24_u32>;
defm UMIN  : BinaryIntMCi32<IL_OP_U_MIN, int_AMDIL_min_u32>;
defm UMAX  : BinaryIntMCi32<IL_OP_U_MAX, int_AMDIL_max_u32>;
defm UBIT_EXTRACT : TernaryIntMCInt<IL_OP_UBIT_EXTRACT,
          int_AMDIL_bit_extract_u32>;
let swizzle = 6 in {
defm UBIT_INSERT : QuaternaryIntMCInt<IL_OP_UBIT_INSERT,
          int_AMDIL_bit_insert_u32>;
}
defm BFI : TernaryIntMCInt<IL_OP_BFI, int_AMDIL_bfi>;
defm BFM : BinaryIntMCi32<IL_OP_BFM, int_AMDIL_bfm>;
defm UMAD  : TernaryIntMCInt<IL_OP_U_MAD, int_AMDIL_mad_u32>;
defm UMAD24  : TernaryIntMCInt<IL_OP_U_MAD24, int_AMDIL_mad24_u32>;
defm U4LERP  : TernaryIntMCInt<IL_OP_U4_LERP,
          int_AMDIL_media_lerp_u4>;
defm BITALIGN : TernaryIntMCInt<IL_OP_BIT_ALIGN, int_AMDIL_media_bitalign>;
defm BYTEALIGN : TernaryIntMCInt<IL_OP_BYTE_ALIGN, int_AMDIL_media_bytealign>;
}
let Predicates = [HasHW64Bit] in {
defm CUSTOM_ADD    : BinaryOpMCi64<IL_OP_I64_ADD, IL_add>;
defm ADD    : BinaryOpMCi64<IL_OP_I64_ADD, add>;
defm IMIN64 : BinaryIntMCi64<IL_OP_I64_MIN, int_AMDIL_min_i32>;
defm UMIN64 : BinaryIntMCi64<IL_OP_U64_MIN, int_AMDIL_min_u32>;
defm IMAX64 : BinaryIntMCi64<IL_OP_I64_MAX, int_AMDIL_max_i32>;
defm UMAX64 : BinaryIntMCi64<IL_OP_U64_MAX, int_AMDIL_max_u32>;
}
defm SHL  : BinaryNCOpMCi32Shift<IL_OP_I_SHL, shl>;
defm SHR  : BinaryNCOpMCi32Shift<IL_OP_I_SHR, sra>;
defm USHR : BinaryNCOpMCi32Shift<IL_OP_U_SHR, srl>;
let Predicates = [HasHW64Bit] in {
defm SHL  : BinaryNCOpMCi64Shift<IL_OP_I64_SHL, shl>;
defm SHR  : BinaryNCOpMCi64Shift<IL_OP_I64_SHR, sra>;
defm USHR : BinaryNCOpMCi64Shift<IL_OP_U64_SHR, srl>;
}


let Predicates = [HasTmrRegister] in {
  def Tmr : ILFormat<IL_OP_MOV, (outs GPRXYI64:$tmr),
      (ins), !strconcat(IL_OP_MOV.Text, " $tmr, Tmr.xyxy"),
      [(set GPRXYI64:$tmr, (int_AMDIL_get_cycle_count))]>;
}
let Predicates = [IsEGOrLaterDevice] in {
def CU_ID : ILFormat<IL_OP_CU_ID, (outs GPRI32:$id), (ins),
    !strconcat(IL_OP_CU_ID.Text, " $id"),
    [(set GPRI32:$id, (int_AMDIL_compute_unit_id))]>;
def WAVE_ID : ILFormat<IL_OP_WAVE_ID, (outs GPRI32:$id), (ins),
    !strconcat(IL_OP_WAVE_ID.Text, " $id"),
    [(set GPRI32:$id, (int_AMDIL_wavefront_id))]>;
}

let ieee = 1 in {
  defm MUL_IEEE  : BinaryOpMCf32<IL_OP_MUL_IEEE, fmul>,
  BinaryOpMCf64<IL_OP_D_MUL, fmul>;
}
defm ADD : BinaryOpMCf32<IL_OP_ADD, fadd>,
BinaryOpMCi32<IL_OP_I_ADD, add>,
BinaryOpMCf64<IL_OP_D_ADD, fadd>;

//===---------------------------------------------------------------------===//
// float math instructions start here
//===---------------------------------------------------------------------===//
let mayLoad=0, mayStore=0 in {
defm ABS : UnaryIntMCf32<IL_OP_ABS, int_AMDIL_fabs>;
defm FRAC : UnaryIntMCf32<IL_OP_FRC, int_AMDIL_fraction>;
defm PIREDUCE : UnaryIntMCf32<IL_OP_PI_REDUCE, int_AMDIL_pireduce>;
defm ROUND_NEAREST : UnaryIntMCf32<IL_OP_ROUND_NEAR,
          int_AMDIL_round_nearest>;
defm ROUND_NEGINF : UnaryIntMCf32<IL_OP_ROUND_NEG_INF,
          int_AMDIL_round_neginf>;
defm ROUND_POSINF : UnaryIntMCf32<IL_OP_ROUND_POS_INF,
          int_AMDIL_round_posinf>;
defm ROUND_ZERO : UnaryIntMCf32<IL_OP_ROUND_ZERO,
          int_AMDIL_round_zero>;
defm ACOS : UnaryIntMCf32Scalar<IL_OP_ACOS, int_AMDIL_acos>;
defm ATAN : UnaryIntMCf32Scalar<IL_OP_ATAN, int_AMDIL_atan>;
defm ASIN : UnaryIntMCf32Scalar<IL_OP_ASIN, int_AMDIL_asin>;
defm TAN : UnaryIntMCf32Scalar<IL_OP_TAN, int_AMDIL_tan>;
defm SIN : UnaryIntMCf32Scalar<IL_OP_SIN, int_AMDIL_sin>;
defm COS : UnaryIntMCf32Scalar<IL_OP_COS, int_AMDIL_cos>;
defm SQRT : UnaryIntMCf32Scalar<IL_OP_SQRT, int_AMDIL_sqrt>;
defm EXP : UnaryIntMCf32Scalar<IL_OP_EXP, int_AMDIL_exp>;
defm EXPVEC : UnaryIntMCf32<IL_OP_EXP_VEC, int_AMDIL_exp_vec>;
defm SQRTVEC : UnaryIntMCf32<IL_OP_SQRT_VEC, int_AMDIL_sqrt_vec>;
defm COSVEC : UnaryIntMCf32<IL_OP_COS_VEC, int_AMDIL_cos_vec>;
defm SINVEC : UnaryIntMCf32<IL_OP_SIN_VEC, int_AMDIL_sin_vec>;
defm LOGVEC : UnaryIntMCf32<IL_OP_LOG_VEC, int_AMDIL_log_vec>;
defm RSQVEC : UnaryIntMCf32<IL_OP_RSQ_VEC, int_AMDIL_rsq_vec>;
defm EXN : UnaryIntMCf32Scalar<IL_OP_EXN, int_AMDIL_exn>;
defm SIGN : UnaryIntMCf32<IL_OP_SGN, int_AMDIL_sign>;
defm LENGTH : UnaryIntMCf32<IL_OP_LEN, int_AMDIL_length>;
defm POW : BinaryIntMCf32<IL_OP_POW, int_AMDIL_pow>;
}

let ieee = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm MIN  : BinaryIntMCf32<IL_OP_MIN, int_AMDIL_min>;
defm MAX  : BinaryIntMCf32<IL_OP_MAX, int_AMDIL_max>;
defm MAD  : TernaryIntMCf32<IL_OP_MAD, int_AMDIL_mad>;
  }
defm MOD  : BinaryOpMCf32<IL_OP_MOD, frem>;
}
let zeroop = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm LN  : UnaryIntMCf32Scalar<IL_OP_LN, int_AMDIL_ln>;
defm LOG : UnaryIntMCf32Scalar<IL_OP_LOG, int_AMDIL_log>;
defm RSQ : UnaryIntMCf32Scalar<IL_OP_RSQ, int_AMDIL_rsq>;
defm DIV_INT  : BinaryIntMCf32<IL_OP_DIV, int_AMDIL_div>;
defm DIV  : BinaryOpMCf32<IL_OP_DIV, fdiv>;
defm DIV_PRECISE  : BinaryIntMCf32<IL_OP_DIV_PRECISE, int_AMDIL_div_precise>;
  }
}
  let mayLoad = 0, mayStore=0 in {
defm CLAMP : TernaryIntMCf32<IL_OP_CLAMP, int_AMDIL_clamp>;
defm FMA  : TernaryIntMCf32<IL_OP_FMA, int_AMDIL_fma>;
defm LERP  : TernaryIntMCf32<IL_OP_LERP, int_AMDIL_lerp>;
  }
defm SUB   : BinaryOpMCf32<IL_OP_SUB, fsub>;
defm FABS  : UnaryOpMCf32<IL_OP_ABS, fabs>;
defm NEARBY : UnaryOpMCf32<IL_OP_ROUND_NEAR, fnearbyint>;
defm TRUNC  : UnaryOpMCf32<IL_OP_ROUND_ZERO, ftrunc>;
defm CEIL   : UnaryOpMCf32<IL_OP_ROUND_POS_INF, fceil>;
defm FLOOR  : UnaryOpMCf32<IL_OP_ROUND_NEG_INF, ffloor>;

let ieee = 1 in {
  defm DP2ADD : TernaryIntMCFull<IL_OP_DP2_ADD, int_AMDIL_dp2_add, f32, GPRF32,
       v2f32, GPRV2F32, v2f32imm, fpimm, v2f32, GPRV2F32, v2f32imm, fpimm, 
       f32,   GPRF32,   f32imm, fpimm>;
  defm DP2    : BinaryIntMCFull<IL_OP_DP2,      int_AMDIL_dp2, f32, GPRF32,
       v2f32, GPRV2F32, v2f32imm, fpimm, v2f32, GPRV2F32, v2f32imm, fpimm>;
  defm DP3    : BinaryIntMCFull<IL_OP_DP3,      int_AMDIL_dp3, f32, GPRF32,
       v4f32, GPRV4F32, v4f32imm, fpimm, v4f32, GPRV4F32, v4f32imm, fpimm>;
  defm DP4    : BinaryIntMCFull<IL_OP_DP4,      int_AMDIL_dp4, f32, GPRF32,
       v4f32, GPRV4F32, v4f32imm, fpimm, v4f32, GPRV4F32, v4f32imm, fpimm>;
  defm FTZ : FTZMC<IL_OP_MUL_IEEE, int_AMDIL_ftz>;
}

defm UNPACK_B0 : IntrConvertI32TOF32<IL_OP_UNPACK_0, int_AMDIL_media_unpack_byte_0>;
defm UNPACK_B1 : IntrConvertI32TOF32<IL_OP_UNPACK_1, int_AMDIL_media_unpack_byte_1>;
defm UNPACK_B2 : IntrConvertI32TOF32<IL_OP_UNPACK_2, int_AMDIL_media_unpack_byte_2>;
defm UNPACK_B3 : IntrConvertI32TOF32<IL_OP_UNPACK_3, int_AMDIL_media_unpack_byte_3>;
defm FTOI_FLR  : IntrConvertF32TOI32<IL_OP_FTOI_FLR, int_AMDIL_convert_f32_i32_flr>;
defm FTOI_RPI  : IntrConvertF32TOI32<IL_OP_FTOI_RPI, int_AMDIL_convert_f32_i32_rpi>;
defm HTOF      : IntrConvertF16TOF32<IL_OP_F16_TO_F32, int_AMDIL_convert_f16_f32>;
defm FTOH      : IntrConvertF32TOF16<IL_OP_F32_TO_F16, int_AMDIL_convert_f32_f16>;
defm FTOH_NEAR     : IntrConvertF32TOF16<IL_OP_F32_TO_F16_NEAR, int_AMDIL_convert_f32_f16_near>;
defm FTOH_NEG_INF  : IntrConvertF32TOF16<IL_OP_F32_TO_F16_NEG_INF, int_AMDIL_convert_f32_f16_neg_inf>;
defm FTOH_PLUS_INF : IntrConvertF32TOF16<IL_OP_F32_TO_F16_PLUS_INF, int_AMDIL_convert_f32_f16_plus_inf>;
defm NEG : UnaryNegMCf32<IL_OP_MOV,   fneg>;
defm SUB : BinaryNegMCf64<IL_OP_D_ADD, fsub>;
defm NEG : UnaryNegMCf64<IL_OP_MOV,   fneg>;
defm NOT : NotMCReg<IL_OP_I_NOT, not, vnot>;
def vineg : PatFrag<(ops node:$in), (sub immAllZerosV, node:$in)>;
defm NEG : UnaryPatMCi8Scalar< IL_OP_I_NEGATE, ineg>;
defm NEG : UnaryPatMCi8Vector< IL_OP_I_NEGATE, vineg>;
defm NEG : UnaryPatMCi16Scalar<IL_OP_I_NEGATE, ineg>;
defm NEG : UnaryPatMCi16Vector<IL_OP_I_NEGATE, vineg>;
defm NEG : UnaryPatMCi32Scalar<IL_OP_I_NEGATE, ineg>;
defm NEG : UnaryPatMCi32Vector<IL_OP_I_NEGATE, vineg>;
defm NEG : UnaryPatMCi64Scalar<IL_OP_I64_NEGATE, ineg>;
defm NEG : UnaryPatMCi64Vector<IL_OP_I64_NEGATE, vineg>;

  let mayLoad = 0, mayStore=0 in {
  defm MIN  : BinaryIntMCf64<IL_OP_D_MIN, int_AMDIL_min>;
  defm MAX  : BinaryIntMCf64<IL_OP_D_MAX, int_AMDIL_max>;
  defm DIV  : BinaryIntMCf64<IL_OP_D_DIV, int_AMDIL_div>;
  defm MAD  : TernaryIntMCf64<IL_OP_D_MAD, int_AMDIL_mad>;
  defm DFMA : TernaryIntMCf64<IL_OP_D_MAD, int_AMDIL_fma>;
  defm FRAC : UnaryIntMCf64<IL_OP_D_FRC, int_AMDIL_fraction>;
  defm SQRT : UnaryIntMCf64<IL_OP_D_SQRT, int_AMDIL_sqrt>;
  defm RSQ  : UnaryIntMCf64Scalar<IL_OP_D_RSQ, int_AMDIL_rsq>;
  defm RCP  : UnaryIntMCf64Scalar<IL_OP_D_RCP, int_AMDIL_drcp>;
  }

defm DABS : UnaryOpMCf64<IL_OP_D_ABS, fabs>;
let mayLoad = 0, mayStore=0 in {
  defm ABS  : UnaryIntMCf64<IL_OP_D_ABS, int_AMDIL_fabs>;
}

defm FREXPf64 : UnaryIntMCFull<IL_OP_D_FREXP, int_AMDIL_frexp_f64,
     v2i64, GPRV2I64, f64, GPRF64, f64imm, fpimm>;
defm LDEXP : BinaryIntMCDDI<IL_OP_D_LDEXP, int_AMDIL_ldexp>;

def MACRO__sdiv_i16  : BinaryMacro<GPRI16, GPRI16, GPRI16, sdiv>;
def MACRO__sdiv_i32  : BinaryMacro<GPRI32, GPRI32, GPRI32, sdiv>;
def MACRO__udiv_i8   : BinaryMacro< GPRI8, GPRI8, GPRI8, udiv>;
def MACRO__udiv_i16  : BinaryMacro<GPRI16, GPRI16, GPRI16, udiv>;
def MACRO__udiv_i32  : BinaryMacro<GPRI32, GPRI32, GPRI32, udiv>;
def MACRO__smod_i8   : BinaryMacro< GPRI8, GPRI8, GPRI8, srem>;
def MACRO__smod_i16  : BinaryMacro<GPRI16, GPRI16, GPRI16, srem>;
def MACRO__smod_i32  : BinaryMacro<GPRI32, GPRI32, GPRI32, srem>;
def MACRO__umod_i8   : BinaryMacro< GPRI8, GPRI8, GPRI8, urem>;
def MACRO__umod_i16  : BinaryMacro<GPRI16, GPRI16, GPRI16, urem>;
def MACRO__umod_i32  : BinaryMacro<GPRI32, GPRI32, GPRI32, urem>;
let Predicates = [HasSWDDiv] in {
  def MACRO__ddiv_f64: BinaryMacro<GPRF64, GPRF64, GPRF64, fdiv>;
}
let Predicates = [HasHWDDiv] in {
  def MACRO__ddiv_f64_fma: BinaryMacro<GPRF64, GPRF64, GPRF64, fdiv>;
}
def MACRO__ftol_i64  : UnaryMacro<GPRI64, GPRF32, fp_to_sint>;
def MACRO__ultof_f32 : UnaryMacro<GPRF32, GPRI64, uint_to_fp>;
def MACRO__ltof_f32  : UnaryMacro<GPRF32, GPRI64, sint_to_fp>;
let Predicates = [HasSW64Mul] in {
def MACRO__mul_i64   : BinaryMacro<GPRI64, GPRI64, GPRI64, mul>;
def MACRO__mul_v2i64 : BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I64, mul>;
}
let Predicates = [HasSW64DivMod] in {
def MACRO__sdiv_i64  : BinaryMacro<GPRI64, GPRI64, GPRI64, sdiv>;
def MACRO__udiv_i64  : BinaryMacro<GPRI64, GPRI64, GPRI64, udiv>;
def MACRO__smod_i64  : BinaryMacro<GPRI64, GPRI64, GPRI64, srem>;
def MACRO__umod_i64  : BinaryMacro<GPRI64, GPRI64, GPRI64, urem>;
}

let Predicates = [HasHW64DivMod] in {
  defm SDIV : BinaryOpMCi64<IL_OP_I64_DIV, sdiv>;
  defm UDIV : BinaryOpMCi64<IL_OP_U64_DIV, udiv>;
  defm SMOD : BinaryOpMCi64<IL_OP_I64_MOD, srem>;
  defm UMOD : BinaryOpMCi64<IL_OP_U64_MOD, urem>;
}
let Predicates = [HasHW64Mul] in {
  defm SMUL       : BinaryOpMCi64<IL_OP_I64_MUL, mul>;
  defm UMUL       : BinaryOpMCi64<IL_OP_U64_MUL, IL_umul>;
}

//===---------------------------------------------------------------------===//
// Comparison Instructions
//===---------------------------------------------------------------------===//
defm EQ  : BinaryCmpMCi32<IL_OP_I_EQ,   seteq>,
     BinaryCmpMCi64<IL_OP_I64_EQ, seteq>,
     BinaryCmpMCf32<IL_OP_EQ,     seteq>,
     BinaryCmpMCf64<IL_OP_D_EQ,   seteq>;
defm OEQ : BinaryCmpMCf32<IL_OP_EQ, setoeq>,
     BinaryCmpMCf64<IL_OP_D_EQ, setoeq>;
defm GE  : BinaryCmpMCi32<IL_OP_I_GE,   setge>,
     BinaryCmpMCi64<IL_OP_I64_GE, setge>,
     BinaryCmpMCf32<IL_OP_GE,     setge>,
     BinaryCmpMCf64<IL_OP_D_GE,   setge>;
defm LT  : BinaryCmpMCi32<IL_OP_I_LT,   setlt>,
     BinaryCmpMCf32<IL_OP_LT,     setlt>,
     BinaryCmpMCi64<IL_OP_I64_LT, setlt>,
     BinaryCmpMCf64<IL_OP_D_LT,   setlt>;
defm OLT  : BinaryCmpMCf32<IL_OP_LT,     setolt>,
     BinaryCmpMCf64<IL_OP_D_LT,   setolt>;
defm OGE : BinaryCmpMCf32<IL_OP_GE,   setoge>,
     BinaryCmpMCf64<IL_OP_D_GE, setoge>;
defm ULT : BinaryCmpMCi32<IL_OP_U_LT,   setult>,
     BinaryCmpMCi64<IL_OP_U64_LT, setult>;
defm UGE : BinaryCmpMCi32<IL_OP_U_GE,   setuge>,
     BinaryCmpMCi64<IL_OP_U64_GE, setuge>;
defm NE  : BinaryCmpMCi32<IL_OP_I_NE,   setne>,
     BinaryCmpMCi64<IL_OP_I64_NE, setne>,
     BinaryCmpMCf32<IL_OP_NE,     setne>,
     BinaryCmpMCf64<IL_OP_D_NE,   setne>;
defm UNE : BinaryCmpMCf32<IL_OP_NE, setune>,
     BinaryCmpMCf64<IL_OP_D_NE, setune>;
/**/
//   Uncomment this if LLVM doesn't support expansion of these.
defm GT  : BinaryInvCmpMCi32<IL_OP_I_GE,setle>;
defm LE  : BinaryInvCmpMCi32<IL_OP_I_LT,setgt>;
defm UGT : BinaryInvCmpMCi32<IL_OP_U_GE,setule>;
defm ULE : BinaryInvCmpMCi32<IL_OP_U_LT,setugt>;
defm GT  : BinaryInvCmpMCi64<IL_OP_I64_GE,setle>;
defm LE  : BinaryInvCmpMCi64<IL_OP_I64_LT,setgt>;
defm UGT : BinaryInvCmpMCi64<IL_OP_U64_GE,setule>;
defm ULE : BinaryInvCmpMCi64<IL_OP_U64_LT,setugt>;
defm GT  : BinaryInvCmpMCf32<IL_OP_GE,  setle>;
defm LE  : BinaryInvCmpMCf32<IL_OP_LT,  setgt>;
defm GT  : BinaryInvCmpMCf64<IL_OP_D_GE,setle>;
defm LE  : BinaryInvCmpMCf64<IL_OP_D_LT,setgt>;
defm OGT  : BinaryInvCmpMCf32<IL_OP_GE,  setole>;
defm OLE  : BinaryInvCmpMCf32<IL_OP_LT,  setogt>;
defm OGT  : BinaryInvCmpMCf64<IL_OP_D_GE,setole>;
defm OLE  : BinaryInvCmpMCf64<IL_OP_D_LT,setogt>;
defm UGT  : BinaryInvCmpMCf32<IL_OP_GE,  setule>;
defm ULE  : BinaryInvCmpMCf32<IL_OP_LT,  setugt>;
defm UGT  : BinaryInvCmpMCf64<IL_OP_D_GE,setule>;
defm ULE  : BinaryInvCmpMCf64<IL_OP_D_LT,setugt>;
/* */


//===---------------------------------------------------------------------===//
// Scalar ==> Scalar conversion functions
//===---------------------------------------------------------------------===//
defm FTOD        : ConvertOpMC<IL_OP_F_2_D,         fextend,    f64,  GPRF64, f32, GPRF32, f32imm, fpimm>;
defm DTOF        : ConvertOpMC<IL_OP_D_2_F,         IL_d2f,     f32, GPRF32, f64, GPRF64, f64imm, fpimm>;
defm FTOIi32    : ConvertOpMC<IL_OP_FTOI,          fp_to_sint, i32, GPRI32, f32, GPRF32, f32imm, fpimm>;
defm FTOIv2i32  : ConvertOpMC<IL_OP_FTOI,          fp_to_sint, v2i32, GPRV2I32, v2f32, GPRV2F32, v2f32imm, fpimm>;
defm FTOIv4i32  : ConvertOpMC<IL_OP_FTOI,          fp_to_sint, v4i32, GPRV4I32, v4f32, GPRV4F32, v4f32imm, fpimm>;
defm ITOFf32    : ConvertOpMC<IL_OP_ITOF,          sint_to_fp, f32, GPRF32, i32, GPRI32, i32imm, imm>;
defm ITOFv2f32  : ConvertOpMC<IL_OP_ITOF,          sint_to_fp, v2f32, GPRV2F32, v2i32, GPRV2I32, v2i32imm, imm>;
defm ITOFv4f32  : ConvertOpMC<IL_OP_ITOF,          sint_to_fp, v4f32, GPRV4F32, v4i32, GPRV4I32, v4i32imm, imm>;
defm FTOUi32   : ConvertOpMC<IL_OP_FTOU,          fp_to_uint, i32, GPRI32, f32, GPRF32, f32imm, fpimm>;
defm FTOUv2i32  : ConvertOpMC<IL_OP_FTOU,          fp_to_uint, v2i32, GPRV2I32, v2f32, GPRV2F32, v2f32imm, fpimm>;
defm FTOUv4i32  : ConvertOpMC<IL_OP_FTOU,          fp_to_uint, v4i32, GPRV4I32, v4f32, GPRV4F32, v4f32imm, fpimm>;
defm UTOFf32   : ConvertOpMC<IL_OP_UTOF,          uint_to_fp, f32, GPRF32, i32, GPRI32, i32imm, imm>;
defm UTOFv2f32  : ConvertOpMC<IL_OP_UTOF,          uint_to_fp, v2f32, GPRV2F32, v2i32, GPRV2I32, v2i32imm, imm>;
defm UTOFv4f32  : ConvertOpMC<IL_OP_UTOF,          uint_to_fp, v4f32, GPRV4F32, v4i32, GPRV4I32, v4i32imm, imm>;
defm DTOI        : ConvertOpMC<IL_OP_DTOI,          fp_to_sint, i32, GPRI32, f64, GPRF64, f64imm, fpimm>;
defm ITOD        : ConvertOpMC<IL_OP_ITOD,          sint_to_fp, f64, GPRF64, i32, GPRI32, i32imm, imm>;
defm DTOU        : ConvertOpMC<IL_OP_DTOU,          fp_to_uint, i32, GPRI32, f64, GPRF64, f64imm, fpimm>;
defm UTOD        : ConvertOpMC<IL_OP_UTOD,          uint_to_fp, f64, GPRF64, i32, GPRI32, i32imm, imm>;
let swizzle = 2 in {
  def DHIf64r     : ILFormat<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRF64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_dcomphi GPRF64:$src))]>;
  def DHIv2f64r  : ILFormat<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2F64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_dcomphi2 GPRV2F64:$src))]>;
  def DLOf64r         : ILFormat<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRF64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_dcomplo GPRF64:$src))]>;
  def DLOv2f64r   : ILFormat<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2F64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_dcomplo2 GPRV2F64:$src))]>;
}
let swizzle = 6 in {
  def DCREATEf64rr   : ILFormat<IL_OP_I_ADD, (outs GPRF64:$dst),
                (ins GPRI32:$src0, GPRI32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRF64:$dst, (IL_dcreate GPRI32:$src0, GPRI32:$src1))]>;
  def DCREATEv2f64rr : ILFormat<IL_OP_I_ADD, (outs GPRV2F64:$dst),
                (ins GPRV2I32:$src0, GPRV2I32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRV2F64:$dst,
                    (IL_dcreate2 GPRV2I32:$src0, GPRV2I32:$src1))]>;
}
let swizzle = 2 in {
  def LHIi64r         : ILFormat<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRI64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_lcomphi GPRI64:$src))]>;
  def LHIv2i64r         : ILFormat<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2I64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_lcomphi2 GPRV2I64:$src))]>;
  def LLOi64r        : ILFormat<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRI64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_lcomplo GPRI64:$src))]>;
  def LLOv2i64r         : ILFormat<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2I64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_lcomplo2 GPRV2I64:$src))]>;
}
let swizzle = 6 in {
  let swizzle = 1 in {
    def HILO_BITORv4i16rr : ILFormat<IL_OP_I_OR, (outs GPRI32:$dst),
                (ins GPRI32:$src, GPRI32:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
    def HILO_BITORv2i32rr : ILFormat<IL_OP_I_OR, (outs GPRI32:$dst),
                (ins GPRI32:$src, GPRI32:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
  }
  def HILO_BITORv2i64rr : ILFormat<IL_OP_I_OR, (outs GPRI64:$dst),
                (ins GPRI64:$src, GPRI64:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
}
let swizzle = 6 in {
def LCREATEi64rr     : ILFormat<IL_OP_I_ADD, (outs GPRI64:$dst),
                (ins GPRI32:$src0, GPRI32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRI64:$dst, (IL_lcreate GPRI32:$src0, GPRI32:$src1))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LCREATEv2i64rr     : ILFormat<IL_OP_I_ADD, (outs GPRV2I64:$dst),
                (ins GPRV2I32:$src0, GPRV2I32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRV2I64:$dst,
                    (IL_lcreate2 GPRV2I32:$src0, GPRV2I32:$src1))]>;
}
//===---------------------------------------------------------------------===//
// Scalar ==> Vector conversion functions
//===---------------------------------------------------------------------===//
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
defm VCREATE          : VectorCreate<IL_OP_MOV, IL_vbuild>;

//===---------------------------------------------------------------------===//
// Vector ==> Scalar conversion functions
//===---------------------------------------------------------------------===//

// This opcode has custom swizzle pattern encoded in Swizzle Encoder
defm VEXTRACT         : VectorExtract<IL_vextract>;

//===---------------------------------------------------------------------===//
// Vector ==> Vector conversion functions
//===---------------------------------------------------------------------===//
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
defm VINSERT          : VectorInsert<IL_vinsert>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
defm VCONCAT      : VectorConcat<IL_vconcat>;

//===---------------------------------------------------------------------===//
// Bit conversion functions
//===---------------------------------------------------------------------===//
defm IL_ASCHAR     : BitConversion<IL_OP_MOV, IL_bitconv, i8,    GPRI8>;
defm IL_ASSHORT    : BitConversion<IL_OP_MOV, IL_bitconv, i16,   GPRI16>;
defm IL_ASINT      : BitConversion<IL_OP_MOV, IL_bitconv, i32,   GPRI32>;
defm IL_ASFLOAT    : BitConversion<IL_OP_MOV, IL_bitconv, f32,   GPRF32>;
defm IL_ASDOUBLE   : BitConversion<IL_OP_MOV, IL_bitconv, f64,   GPRF64>;
defm IL_ASLONG     : BitConversion<IL_OP_MOV, IL_bitconv, i64,   GPRI64>;
defm IL_ASV2CHAR   : BitConversion<IL_OP_MOV, IL_bitconv, v2i8,  GPRV2I8>;
defm IL_ASV2SHORT  : BitConversion<IL_OP_MOV, IL_bitconv, v2i16, GPRV2I16>;
defm IL_ASV2INT    : BitConversion<IL_OP_MOV, IL_bitconv, v2i32, GPRV2I32>;
defm IL_ASV2FLOAT  : BitConversion<IL_OP_MOV, IL_bitconv, v2f32, GPRV2F32>;
defm IL_ASV2DOUBLE : BitConversion<IL_OP_MOV, IL_bitconv, v2f64, GPRV2F64>;
defm IL_ASV2LONG   : BitConversion<IL_OP_MOV, IL_bitconv, v2i64, GPRV2I64>;
defm IL_ASV4CHAR   : BitConversion<IL_OP_MOV, IL_bitconv, v4i8,  GPRV4I8>;
defm IL_ASV4SHORT  : BitConversion<IL_OP_MOV, IL_bitconv, v4i16, GPRV4I16>;
defm IL_ASV4INT    : BitConversion<IL_OP_MOV, IL_bitconv, v4i32, GPRV4I32>;
defm IL_ASV4FLOAT  : BitConversion<IL_OP_MOV, IL_bitconv, v4f32, GPRV4F32>;

//===---------------------------------------------------------------------===//
// Custom Inserter for Branches and returns, this eventually will be a
// seperate pass
//===---------------------------------------------------------------------===//
let isTerminator = 1 in {
  defm BRANCH : BranchMC<IL_brcond>;
}
//===---------------------------------------------------------------------===//
// return instructions
//===---------------------------------------------------------------------===//
let isTerminator = 1, isReturn = 1, isBarrier = 1, hasCtrlDep = 1 in {
  let swizzle = 63 in {
  def RETURN          : ILFormat<IL_OP_RET,(outs), (ins variable_ops),
      IL_OP_RET.Text, []>;
  def RETDYN      : ILFormat<IL_OP_RET_DYN, (outs), (ins variable_ops),
      IL_OP_RET_DYN.Text, [(IL_retflag)]>;
}
}
//===---------------------------------------------------------------------===//
// Lower and raise the stack x amount
//===---------------------------------------------------------------------===//
def ADJCALLSTACKDOWN : ILFormat<IL_PSEUDO_INST, (outs), (ins i32imm:$amt),
    "; begin of call sequence $amt",
    [(IL_callseq_start timm:$amt)]>;
def ADJCALLSTACKUP : ILFormat<IL_PSEUDO_INST, (outs), (ins i32imm:$amt1,
    i32imm:$amt2),
    "; end of call sequence $amt1 $amt2",
    [(IL_callseq_end timm:$amt1, timm:$amt2)]>;

//===---------------------------------------------------------------------===//
// Handle a function call
//===---------------------------------------------------------------------===//
let isCall = 1 in {
  let Uses = [
     R1,  R2,  R3,  R4,  R5,  R6,  R7,  R8,  R9, R10, R11, R12, R13, R14, R15, R16,
    R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31, R32
    ] in {
      let swizzle = 2 in {
      def CALL : ILFormat<IL_OP_CALL, (outs),
      (ins calltarget:$dst, variable_ops),
      !strconcat(IL_OP_CALL.Text, " $dst"), []>;
    }
}
}


//===---------------------------------------------------------------------===//
// Flow and Program control Instructions
//===---------------------------------------------------------------------===//
let isTerminator=1 in {
  def SWITCH      : ILFormat<IL_OP_SWITCH, (outs), (ins GPRI32:$src),
  !strconcat(IL_OP_SWITCH.Text, " $src"), []>;
  def CASE        : ILFormat<IL_OP_CASE, (outs), (ins GPRI32:$src),
      !strconcat(IL_OP_CASE.Text, " $src"), []>;
  def BREAK       : ILFormat<IL_OP_BREAK, (outs), (ins),
      IL_OP_BREAK.Text, []>;
  def CONTINUE    : ILFormat<IL_OP_CONTINUE, (outs), (ins),
      IL_OP_CONTINUE.Text, []>;
  def DEFAULT     : ILFormat<IL_OP_DEFAULT, (outs), (ins),
      IL_OP_DEFAULT.Text, []>;
  def ELSE        : ILFormat<IL_OP_ELSE, (outs), (ins),
      IL_OP_ELSE.Text, []>;
  def ENDSWITCH   : ILFormat<IL_OP_ENDSWITCH, (outs), (ins),
      IL_OP_ENDSWITCH.Text, []>;
  def ENDMAIN     : ILFormat<IL_OP_ENDMAIN, (outs), (ins),
      IL_OP_ENDMAIN.Text, []>;
  def END         : ILFormat<IL_OP_END, (outs), (ins),
      IL_OP_END.Text, []>;
  def ENDFUNC     : ILFormat<IL_OP_ENDFUNC, (outs), (ins),
      IL_OP_ENDFUNC.Text, []>;
  def ENDIF       : ILFormat<IL_OP_ENDIF, (outs), (ins),
      IL_OP_ENDIF.Text, []>;
  def WHILELOOP   : ILFormat<IL_OP_WHILE, (outs), (ins),
      IL_OP_WHILE.Text, []>;
  def ENDLOOP     : ILFormat<IL_OP_ENDLOOP, (outs), (ins),
      IL_OP_ENDLOOP.Text, []>;
  def FUNC        : ILFormat<IL_OP_FUNC, (outs), (ins),
      IL_OP_FUNC.Text, []>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm IF_LOGICALNZ  : BranchInstr<IL_OP_IF_LOGICALNZ>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm IF_LOGICALZ   : BranchInstr<IL_OP_IF_LOGICALZ>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm BREAK_LOGICALNZ : BranchInstr<IL_OP_BREAK_LOGICALNZ>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm BREAK_LOGICALZ : BranchInstr<IL_OP_BREAK_LOGICALZ>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm CONTINUE_LOGICALNZ : BranchInstr<IL_OP_CONTINUE_LOGICALNZ>;
  // This opcode has custom swizzle pattern encoded in Swizzle Encoder
  defm CONTINUE_LOGICALZ : BranchInstr<IL_OP_CONTINUE_LOGICALZ>;
  defm IFC         : BranchInstr2<IL_OP_IFC>;
  defm BREAKC      : BranchInstr2<IL_OP_BREAKC>;
  defm CONTINUEC   : BranchInstr2<IL_OP_CONTINUEC>;
}
let isTerminator = 1, isBarrier = 1, hasCtrlDep = 1 in {
  def TRAP : ILFormat<IL_OP_NOP, (outs), (ins), IL_OP_NOP.Text, [(trap)]>;
}

//===---------------------------------------------------------------------===//
//----------------- Work Item Functions - OpenCL 6.11.1 ---------------------//
//===---------------------------------------------------------------------===//
let isCall=1, isAsCheapAsAMove = 1 in {
  def GET_WORK_DIM : ILFormat<IL_OP_MOV, (outs GPRXI32:$dst), (ins),
      !strconcat(IL_OP_MOV.Text, " $dst, cb0[0].w"),
      [(set GPRXI32:$dst, (int_AMDIL_get_work_dim))]>;

  def GET_GLOBAL_ID : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, r1021.xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_global_id))]>;

  def GET_LOCAL_ID : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, r1022.xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_local_id))]>;

  def GET_GROUP_ID : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, r1023.xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_group_id))]>;

  def GET_GLOBAL_SIZE : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[0].xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_global_size))]>;

  def GET_LOCAL_SIZE : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[1].xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_local_size))]>;

  def GET_NUM_GROUPS : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[2].xyz0"),
      [(set GPRV4I32:$dst, (int_AMDIL_get_num_groups))]>;

  let Predicates = [Has32BitPtr] in {
    def GET_GLOBAL_OFFSET : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
        (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[9].xyz0"),
        [(set GPRV4I32:$dst, (int_AMDIL_get_global_offset))]>;
  }

  let Predicates = [Has64BitPtr] in {
    def GET_GLOBAL_OFFSET64 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
        (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[9].yzw0"),
        [(set GPRV4I32:$dst, (int_AMDIL_get_global_offset))]>;
  }
}
//===---------------------------------------------------------------------===//
//------------- Synchronization Functions - OpenCL 6.11.9 -------------------//
//===---------------------------------------------------------------------===//
let isCall=1 in {
  // The 'S' suffix is for the '_gws' flag.
  // The 'L' suffix is for the '_lds' flag.
  // The 'M' suffix is for the '_memory' flag.
  // The 'G' suffix is for the '_gds' flag.
  // The 'RO' suffix is for the '_read_only' flag.
  // The 'WO' suffix is for the '_write_only' flag.
  let gws = 1 in {
      defm FENCE_S : Fence<IL_OP_FENCE_S, int_AMDIL_gws>;
      defm FENCE_LS : Fence<IL_OP_FENCE_LS, int_AMDIL_gws_local>;
      defm FENCE_MS : Fence<IL_OP_FENCE_MS, int_AMDIL_gws_global>;
      defm FENCE_GS : Fence<IL_OP_FENCE_GS, int_AMDIL_gws_region>;
      defm FENCE_LMS : Fence<IL_OP_FENCE_LMS, int_AMDIL_gws_global_local>;
      defm FENCE_MGS : Fence<IL_OP_FENCE_MGS, int_AMDIL_gws_global_region>;
      defm FENCE_LGS : Fence<IL_OP_FENCE_LGS, int_AMDIL_gws_local_region>;
  }

  defm FENCE       : Fence<IL_OP_FENCE_LMG,
      int_AMDIL_fence>;
  defm FENCE_L     : Fence<IL_OP_FENCE_L,
      int_AMDIL_fence_local>;
  defm FENCE_M     : Fence<IL_OP_FENCE_M,
      int_AMDIL_fence_global>;
  defm FENCE_G     : Fence<IL_OP_FENCE_G,
      int_AMDIL_fence_region>;
  defm FENCE_LM    : Fence<IL_OP_FENCE_LM,
      int_AMDIL_fence_global_local>;
  defm FENCE_LG    : Fence<IL_OP_FENCE_LG,
      int_AMDIL_fence_region_local>;
  defm FENCE_MG    : Fence<IL_OP_FENCE_MG,
      int_AMDIL_fence_region_global>;
  defm FENCE_RO    : Fence<IL_OP_FENCE_RO_LMG,
      int_AMDIL_read_fence>;
  defm FENCE_RO_L  : Fence<IL_OP_FENCE_RO_L,
      int_AMDIL_read_fence_local>;
  defm FENCE_RO_M  : Fence<IL_OP_FENCE_RO_M,
      int_AMDIL_read_fence_global>;
  defm FENCE_RO_G  : Fence<IL_OP_FENCE_RO_G,
      int_AMDIL_read_fence_region>;
  defm FENCE_RO_LM : Fence<IL_OP_FENCE_RO_LM,
      int_AMDIL_read_fence_global_local>;
  defm FENCE_RO_LG : Fence<IL_OP_FENCE_RO_LG,
      int_AMDIL_read_fence_region_local>;
  defm FENCE_RO_MG : Fence<IL_OP_FENCE_RO_MG,
      int_AMDIL_read_fence_region_global>;
  defm FENCE_WO    : Fence<IL_OP_FENCE_WO_LMG,
      int_AMDIL_write_fence>;
  defm FENCE_WO_L  : Fence<IL_OP_FENCE_WO_L,
      int_AMDIL_write_fence_local>;
  defm FENCE_WO_M  : Fence<IL_OP_FENCE_WO_M,
      int_AMDIL_write_fence_global>;
  defm FENCE_WO_G  : Fence<IL_OP_FENCE_WO_G,
      int_AMDIL_write_fence_region>;
  defm FENCE_WO_LM : Fence<IL_OP_FENCE_WO_LM,
      int_AMDIL_write_fence_global_local>;
  defm FENCE_WO_LG : Fence<IL_OP_FENCE_WO_LG,
      int_AMDIL_write_fence_region_local>;
  defm FENCE_WO_MG : Fence<IL_OP_FENCE_WO_MG,
      int_AMDIL_write_fence_region_global>;
}

let isReturn = 1 in {
  defm EARLY_EXIT : UnaryIntMCVoid<IL_OP_RET_LOGICALNZ, int_AMDIL_early_exit,
       i32, GPRI32, i32imm, imm, " $src0">;
}
defm MEDIA_UNPACK_0 : ConvertIntMC<IL_OP_UNPACK_0,
     int_AMDIL_media_unpack_byte_0, v4f32, GPRV4F32,
     v4i32, GPRV4I32, v4i32imm, imm>;
defm MEDIA_UNPACK_1 : ConvertIntMC<IL_OP_UNPACK_1,
     int_AMDIL_media_unpack_byte_1, v4f32, GPRV4F32,
     v4i32, GPRV4I32, v4i32imm, imm>;
defm MEDIA_UNPACK_2 : ConvertIntMC<IL_OP_UNPACK_2,
     int_AMDIL_media_unpack_byte_2, v4f32, GPRV4F32,
     v4i32, GPRV4I32, v4i32imm, imm>;
defm MEDIA_UNPACK_3 : ConvertIntMC<IL_OP_UNPACK_3,
     int_AMDIL_media_unpack_byte_3, v4f32, GPRV4F32,
     v4i32, GPRV4I32, v4i32imm, imm>;

let ieee = 1 in {
  defm MIN3 : TernaryIntMCf32<IL_OP_MIN3, int_AMDIL_min3>;
  defm MED3 : TernaryIntMCf32<IL_OP_MED3, int_AMDIL_med3>;
  defm MAX3 : TernaryIntMCf32<IL_OP_MAX3, int_AMDIL_max3>;
  defm IMIN3 : TernaryIntMCInt<IL_OP_I_MIN3, int_AMDIL_min3_i32>;
  defm IMED3 : TernaryIntMCInt<IL_OP_I_MED3, int_AMDIL_med3_i32>;
  defm IMAX3 : TernaryIntMCInt<IL_OP_I_MAX3, int_AMDIL_max3_i32>;
  defm UMIN3 : TernaryIntMCInt<IL_OP_U_MIN3, int_AMDIL_min3_u32>;
  defm UMED3 : TernaryIntMCInt<IL_OP_U_MED3, int_AMDIL_med3_u32>;
  defm UMAX3 : TernaryIntMCInt<IL_OP_U_MAX3, int_AMDIL_max3_u32>;
}

defm CLASS : ClassMC<int_AMDIL_class>;

defm FREXP_EXP : IntrConvertF32TOI32<IL_OP_FREXP_EXP, int_AMDIL_frexp_exp>;
defm DFREXP_EXP : IntrConvertF64TOI32<IL_OP_D_FREXP_EXP, int_AMDIL_frexp_exp>;

defm FREXP_MANT : UnaryIntMCf32<IL_OP_FREXP_MANT, int_AMDIL_frexp_mant>;
defm SAD16  : TernaryIntMCInt<IL_OP_SAD_U16, int_AMDIL_media_sad16>;
defm SAD32  : TernaryIntMCInt<IL_OP_SAD_U32, int_AMDIL_media_sad32>;

let zeroop = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm DDIV_INT  : BinaryIntMCf64<IL_OP_D_DIV, int_AMDIL_div>;
defm DDIV  : BinaryOpMCf64<IL_OP_D_DIV, fdiv>;
  }
}

defm FREXP_MANT : UnaryIntMCf64<IL_OP_D_FREXP_MANT, int_AMDIL_frexp_mant>;

defm DTRIG_PREOPf64 : BinaryIntMCFull<IL_OP_D_TRIG_PREOP, 
     int_AMDIL_trig_preop_f64, f64, GPRF64, 
     f64, GPRF64, f64imm, fpimm, f32, GPRF32, f32imm, fpimm>;

defm LDEXP : BinaryIntMCFFI<IL_OP_LDEXP, int_AMDIL_ldexp>;
defm MSAD  : TernaryIntMCInt<IL_OP_MSAD, int_AMDIL_media_msad>;
defm QSAD  : TernaryIntLLIL64Scalar<IL_OP_QSAD, int_AMDIL_media_qsad>;
defm MQSAD : TernaryIntLLIL64Scalar<IL_OP_MQSAD, int_AMDIL_media_mqsad>;

defm ADD_RTE : BinaryIntMCf32<IL_OP_ADD_RTE, int_AMDIL_add_rte>;
defm ADD_RTP : BinaryIntMCf32<IL_OP_ADD_RTP, int_AMDIL_add_rtp>;
defm ADD_RTN : BinaryIntMCf32<IL_OP_ADD_RTN, int_AMDIL_add_rtn>;
defm ADD_RTZ : BinaryIntMCf32<IL_OP_ADD_RTZ, int_AMDIL_add_rtz>;
defm SUB_RTE : BinaryIntMCf32<IL_OP_SUB_RTE, int_AMDIL_sub_rte>;
defm SUB_RTP : BinaryIntMCf32<IL_OP_SUB_RTP, int_AMDIL_sub_rtp>;
defm SUB_RTN : BinaryIntMCf32<IL_OP_SUB_RTN, int_AMDIL_sub_rtn>;
defm SUB_RTZ : BinaryIntMCf32<IL_OP_SUB_RTZ, int_AMDIL_sub_rtz>;
defm MUL_RTE : BinaryIntMCf32<IL_OP_MUL_RTE, int_AMDIL_mul_rte>;
defm MUL_RTP : BinaryIntMCf32<IL_OP_MUL_RTP, int_AMDIL_mul_rtp>;
defm MUL_RTN : BinaryIntMCf32<IL_OP_MUL_RTN, int_AMDIL_mul_rtn>;
defm MUL_RTZ : BinaryIntMCf32<IL_OP_MUL_RTZ, int_AMDIL_mul_rtz>;
defm MAD_RTE : TernaryIntMCf32<IL_OP_MAD_RTE, int_AMDIL_mad_rte>;
defm MAD_RTP : TernaryIntMCf32<IL_OP_MAD_RTP, int_AMDIL_mad_rtp>;
defm MAD_RTN : TernaryIntMCf32<IL_OP_MAD_RTN, int_AMDIL_mad_rtn>;
defm MAD_RTZ : TernaryIntMCf32<IL_OP_MAD_RTZ, int_AMDIL_mad_rtz>;
defm FMA_RTE : TernaryIntMCf32<IL_OP_FMA_RTE, int_AMDIL_fma_rte>;
defm FMA_RTP : TernaryIntMCf32<IL_OP_FMA_RTP, int_AMDIL_fma_rtp>;
defm FMA_RTN : TernaryIntMCf32<IL_OP_FMA_RTN, int_AMDIL_fma_rtn>;
defm FMA_RTZ : TernaryIntMCf32<IL_OP_FMA_RTZ, int_AMDIL_fma_rtz>;
defm ADD_RTE : BinaryIntMCf64Scalar<IL_OP_D_ADD_RTE, int_AMDIL_add_rte>;
defm ADD_RTP : BinaryIntMCf64Scalar<IL_OP_D_ADD_RTP, int_AMDIL_add_rtp>;
defm ADD_RTN : BinaryIntMCf64Scalar<IL_OP_D_ADD_RTN, int_AMDIL_add_rtn>;
defm ADD_RTZ : BinaryIntMCf64Scalar<IL_OP_D_ADD_RTZ, int_AMDIL_add_rtz>;
defm SUB_RTE : BinaryIntMCf64Scalar<IL_OP_D_SUB_RTE, int_AMDIL_sub_rte>;
defm SUB_RTP : BinaryIntMCf64Scalar<IL_OP_D_SUB_RTP, int_AMDIL_sub_rtp>;
defm SUB_RTN : BinaryIntMCf64Scalar<IL_OP_D_SUB_RTN, int_AMDIL_sub_rtn>;
defm SUB_RTZ : BinaryIntMCf64Scalar<IL_OP_D_SUB_RTZ, int_AMDIL_sub_rtz>;
defm MUL_RTE : BinaryIntMCf64Scalar<IL_OP_D_MUL_RTE, int_AMDIL_mul_rte>;
defm MUL_RTP : BinaryIntMCf64Scalar<IL_OP_D_MUL_RTP, int_AMDIL_mul_rtp>;
defm MUL_RTN : BinaryIntMCf64Scalar<IL_OP_D_MUL_RTN, int_AMDIL_mul_rtn>;
defm MUL_RTZ : BinaryIntMCf64Scalar<IL_OP_D_MUL_RTZ, int_AMDIL_mul_rtz>;
defm MAD_RTE : TernaryIntMCf64Scalar<IL_OP_D_MAD_RTE, int_AMDIL_mad_rte>;
defm MAD_RTP : TernaryIntMCf64Scalar<IL_OP_D_MAD_RTP, int_AMDIL_mad_rtp>;
defm MAD_RTN : TernaryIntMCf64Scalar<IL_OP_D_MAD_RTN, int_AMDIL_mad_rtn>;
defm MAD_RTZ : TernaryIntMCf64Scalar<IL_OP_D_MAD_RTZ, int_AMDIL_mad_rtz>;

def PRINT_GET_ITEM_OFFSET :  ILFormat<IL_OP_MOV, (outs GPRXI32:$dst),
        (ins), !strconcat(IL_OP_MOV.Text, " $dst, r1040.x"),
        [(set GPRXI32:$dst, (int_AMDIL_get_printf_item_offset))]>;
let isCall = 1 in {
def PRINT_INC_ITEM_OFFSETr :  ILFormat<IL_OP_I_ADD, (outs),
        (ins GPRI32:$src), !strconcat(IL_OP_I_ADD.Text, " r1040.x, r1040.x, $src"),
        [(int_AMDIL_inc_printf_item_offset GPRI32:$src)]>;
def PRINT_INC_ITEM_OFFSETi :  ILFormat<IL_OP_I_ADD, (outs),
        (ins i32imm:$src), !strconcat(IL_OP_I_ADD.Text, " r1040.x, r1040.x, $src"),
        [(int_AMDIL_inc_printf_item_offset (i32 imm:$src))]>;
}
