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

let isReMaterializable = 0, isAsCheapAsAMove = 1 in {
  defm LOADCONST  : ILConstant<"mov $dst, $val">;
  defm MOVE       : UnaryOpMC<IL_OP_MOV, IL_mov>;
  defm PHIMOVE    : UnaryOpMC<IL_OP_MOV, IL_phimov>;
}
defm BINARY_NOT : UnaryOpMC<IL_OP_I_NOT, IL_not>;
defm BINARY_OR  : BinaryOpMC<IL_OP_I_OR, IL_or>;
defm BINARY_AND : BinaryOpMC<IL_OP_AND, IL_and>;
defm BINARY_XOR : BinaryOpMC<IL_OP_I_XOR, IL_xor>;
defm AND        : BinaryOpMCInt<IL_OP_AND, and>;
defm CMOV       : BinaryOpMC<IL_OP_CMOV, IL_cmov>;
defm DIV_INF    : BinaryOpMC<IL_OP_DIV_INF, IL_div_inf>;
defm SMAX       : BinaryOpMCInt<IL_OP_I_MAX, IL_smax>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder for 64bit
// instructions
defm CMOVLOG    : TernaryOpMC<IL_OP_CMOV_LOGICAL, IL_cmov_logical>;
defm SELECTBIN  : TernaryOpMCScalar<IL_OP_CMOV_LOGICAL, select>;
//===---------------------------------------------------------------------===//
// Signed 8bit integer math instructions start here
//===---------------------------------------------------------------------===//
def INTTOANY_i8 : OneInOneOut<IL_OP_MOV, (outs GPRI8:$dst), (ins GPRI32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, $src0"),
    [(set GPRI8:$dst, (IL_inttoany GPRI32:$src0))]>;
//===---------------------------------------------------------------------===//
// Signed 16bit integer math instructions start here
//===---------------------------------------------------------------------===//
def INTTOANY_i16: OneInOneOut<IL_OP_MOV, (outs GPRI16:$dst), (ins GPRI32:$src0),
    !strconcat(IL_OP_MOV.Text," $dst, $src0"),
    [(set GPRI16:$dst, (IL_inttoany GPRI32:$src0))]>;
//===---------------------------------------------------------------------===//
// Signed 32bit integer math instructions start here
//===---------------------------------------------------------------------===//
defm NEGATE     : UnaryOpMCi32<IL_OP_I_NEGATE, IL_inegate>;
defm SMUL       : BinaryOpMCi32<IL_OP_I_MUL, mul>;
defm SMULHI     : BinaryOpMCi32<IL_OP_I_MUL_HIGH, mulhs>;
defm SHL        : BinaryOpMCi32Const<IL_OP_I_SHL, shl>;
defm SHR        : BinaryOpMCi32Const<IL_OP_I_SHR, sra>;
let Predicates = [Has64BitPtr] in {
defm SHL        : BinaryOpMCi64Const<IL_OP_I_SHL, shl>;
defm SHR        : BinaryOpMCi64Const<IL_OP_I_SHR, sra>;
defm USHR       : BinaryOpMCi64Const<IL_OP_U_SHR, srl>;
}
defm SHLVEC     : BinaryOpMCi32<IL_OP_I_SHL, shl>;
defm SHRVEC     : BinaryOpMCi32<IL_OP_I_SHR, sra>;
defm ADD        : BinaryOpMCi32<IL_OP_I_ADD, add>;
defm CUSTOM_XOR : BinaryOpMCInt<IL_OP_I_XOR, xor>;
// get rid of the addri via the tablegen instead of custom lowered instruction
defm CUSTOM_ADD : BinaryOpMCi32<IL_OP_I_ADD, IL_add>;
defm EADD   : BinaryOpMCi32<IL_OP_I_ADD, adde>;
def INTTOANY_i32: OneInOneOut<IL_OP_MOV, (outs GPRI32:$dst), (ins GPRI32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, $src0"),
    [(set GPRI32:$dst, (IL_inttoany GPRI32:$src0))]>;
// Integer offsets for addressing
def ADDir       : TwoInOneOut<IL_OP_I_ADD, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, GPRI32:$offset),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $ptr, $offset"),
          [(set GPRI32:$dst,
        (IL_addaddrri ADDR:$ptr,
          (i32 GPRI32:$offset)))]>;
def ADDri       : TwoInOneOut<IL_OP_I_ADD, (outs GPRI32:$dst),
      (ins GPRI32:$offset,  MEM3232:$ptr),
          !strconcat(IL_OP_I_ADD.Text, " $dst, $offset, $ptr"),
          [(set GPRI32:$dst,
        (IL_addaddrir
          (i32 GPRI32:$offset), ADDR:$ptr))]>;

defm IFFB_HI    : UnaryOpMCi32<IL_OP_I_FFB_HI, IL_ffb_hi>;
defm IFFB_LO    : UnaryOpMCi32<IL_OP_I_FFB_LO, IL_ffb_lo>;
let mayLoad = 0, mayStore = 0 in {
defm ABS : UnaryIntrinsicInt<IL_OP_ABS, int_AMDIL_abs>;
defm BITCOUNT : UnaryIntrinsicInt<IL_OP_IBIT_COUNT, int_AMDIL_bit_count_i32>;
defm FFB_LO : UnaryIntrinsicInt<IL_OP_I_FFB_LO, int_AMDIL_bit_find_first_lo>;
defm FFB_HI : UnaryIntrinsicInt<IL_OP_I_FFB_HI, int_AMDIL_bit_find_first_hi>;
defm FFB_SGN : UnaryIntrinsicInt<IL_OP_I_FFB_SGN,
        int_AMDIL_bit_find_first_sgn>;
defm IMULHI  : BinaryIntrinsicInt<IL_OP_I_MUL_HIGH, int_AMDIL_mulhi_i32>;
let Predicates = [HasHWSign24Bit] in {
defm IMUL24 : BinaryIntrinsicInt<IL_OP_I_MUL24, int_AMDIL_mul24_i32>;
defm IMULHI24 : BinaryIntrinsicInt<IL_OP_I_MULHI24, int_AMDIL_mulhi24_i32>;
defm IMAD24  : TernaryIntrinsicInt<IL_OP_I_MAD24, int_AMDIL_mad24_i32>;
}
defm CARRY  : BinaryIntrinsicInt<IL_OP_I_CARRY, int_AMDIL_carry_i32>;
defm BORROW  : BinaryIntrinsicInt<IL_OP_I_BORROW, int_AMDIL_borrow_i32>;
defm IMIN  : BinaryIntrinsicInt<IL_OP_I_MIN, int_AMDIL_min_i32>;
defm IMAX  : BinaryIntrinsicInt<IL_OP_I_MAX, int_AMDIL_max_i32>;
defm CMOV_LOG  : TernaryIntrinsicInt<IL_OP_CMOV_LOGICAL,
          int_AMDIL_cmov_logical>;
defm IBIT_EXTRACT : TernaryIntrinsicInt<IL_OP_IBIT_EXTRACT,
          int_AMDIL_bit_extract_i32>;
defm IMAD  : TernaryIntrinsicInt<IL_OP_I_MAD, int_AMDIL_mad_i32>;
defm SAD  : TernaryIntrinsicInt<IL_OP_SAD, int_AMDIL_media_sad>;
defm SADHI  : TernaryIntrinsicInt<IL_OP_SAD_HI,
          int_AMDIL_media_sad_hi>;
}
def SAD4_i32  : ThreeInOneOut<IL_OP_SAD4, (outs GPRI32:$dst),
      (ins GPRV4I32:$src, GPRV4I32:$src1, GPRI32:$src2),
      !strconcat(IL_OP_SAD4.Text, " $dst, $src, $src1, $src2"),
      [(set GPRI32:$dst,
      (int_AMDIL_media_sad4 GPRV4I32:$src, GPRV4I32:$src1,
      GPRI32:$src2))]>;
def FTOV4U8_i32 : OneInOneOut<IL_OP_F2U4, (outs GPRI32:$dst),
      (ins GPRV4F32:$src),
      !strconcat(IL_OP_F2U4.Text, " $dst, $src"),
      [(set GPRI32:$dst,
      (int_AMDIL_media_convert_f2v4u8 GPRV4F32:$src))]>;
//===---------------------------------------------------------------------===//
// Unsigned 32bit integer math instructions start here
//===---------------------------------------------------------------------===//
defm UMUL       : BinaryOpMCi32<IL_OP_U_MUL, IL_umul>;
defm UMULHI     : BinaryOpMCi32<IL_OP_U_MUL_HIGH, mulhu>;
defm USHR       : BinaryOpMCi32Const<IL_OP_U_SHR, srl>;
defm USHRVEC    : BinaryOpMCi32<IL_OP_U_SHR, srl>;
defm UDIV       : BinaryOpMCi32<IL_OP_U_DIV, udiv>;
defm NATIVE_UDIV  : BinaryIntrinsicInt<IL_OP_U_DIV, int_AMDIL_udiv>;
let mayLoad=0, mayStore=0 in {
defm UBIT_REVERSE : UnaryIntrinsicInt<IL_OP_UBIT_REVERSE,
        int_AMDIL_bit_reverse_u32>;
defm UMULHI_INT : BinaryIntrinsicInt<IL_OP_U_MUL_HIGH, int_AMDIL_mulhi_u32>;
defm UMULHI24   : BinaryIntrinsicInt<IL_OP_U_MULHI24, int_AMDIL_mulhi24_u32>;
defm UMUL24     : BinaryIntrinsicInt<IL_OP_U_MUL24, int_AMDIL_mul24_u32>;
defm UMIN  : BinaryIntrinsicInt<IL_OP_U_MIN, int_AMDIL_min_u32>;
defm UMAX  : BinaryIntrinsicInt<IL_OP_U_MAX, int_AMDIL_max_u32>;
defm UBIT_EXTRACT : TernaryIntrinsicInt<IL_OP_UBIT_EXTRACT,
          int_AMDIL_bit_extract_u32>;
defm UBIT_INSERT : QuaternaryIntrinsicInt<IL_OP_UBIT_INSERT,
          int_AMDIL_bit_insert_u32>;
defm BFI : TernaryIntrinsicInt<IL_OP_BFI, int_AMDIL_bfi>;
defm BFM : BinaryIntrinsicInt<IL_OP_BFM, int_AMDIL_bfm>;
defm UMAD  : TernaryIntrinsicInt<IL_OP_U_MAD, int_AMDIL_mad_u32>;
defm UMAD24  : TernaryIntrinsicInt<IL_OP_U_MAD24, int_AMDIL_mad24_u32>;
defm U4LERP  : TernaryIntrinsicInt<IL_OP_U4_LERP,
          int_AMDIL_media_lerp_u4>;
defm BITALIGN : TernaryIntrinsicInt<IL_OP_BIT_ALIGN, int_AMDIL_media_bitalign>;
defm BYTEALIGN : TernaryIntrinsicInt<IL_OP_BYTE_ALIGN, int_AMDIL_media_bytealign>;
}
//===---------------------------------------------------------------------===//
// Signed 64bit integer math instructions start here
//===---------------------------------------------------------------------===//
def LNEGATE     : OneInOneOut<IL_OP_I64_NEGATE,  (outs GPRI64:$dst), (ins GPRI64:$src),
                !strconcat(IL_OP_I64_NEGATE.Text, " $dst, $src"),
                [(set GPRI64:$dst, (IL_inegate GPRI64:$src))]>;
def LNEGATE_v2i64: OneInOneOut<IL_OP_I64_NEGATE,  (outs GPRV2I64:$dst),
                (ins GPRV2I64:$src),
                !strconcat(IL_OP_I64_NEGATE.Text, " $dst, $src"),
                [(set GPRV2I64:$dst, (IL_inegate GPRV2I64:$src))]>;
let Predicates = [HasHW64Bit] in {
def LADD_i64        : TwoInOneOut<IL_OP_I64_ADD, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI64:$src2),
                  !strconcat(IL_OP_I64_ADD.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (IL_add GPRI64:$src1, GPRI64:$src2))]>;
def LADD_v2i64        : TwoInOneOut<IL_OP_I64_ADD, (outs GPRV2I64:$dst),
                  (ins GPRV2I64:$src1, GPRV2I64:$src2),
                  !strconcat(IL_OP_I64_ADD.Text, " $dst, $src1, $src2"),
                [(set GPRV2I64:$dst, (IL_add GPRV2I64:$src1, GPRV2I64:$src2))]>;
defm IMIN64 : BinaryIntrinsicLong<IL_OP_I64_MIN, int_AMDIL_min_i32>;
defm UMIN64 : BinaryIntrinsicLong<IL_OP_U64_MIN, int_AMDIL_min_u32>;
defm IMAX64 : BinaryIntrinsicLong<IL_OP_I64_MAX, int_AMDIL_max_i32>;
defm UMAX64 : BinaryIntrinsicLong<IL_OP_U64_MAX, int_AMDIL_max_u32>;
}
let Predicates = [HasHW64Bit] in {
def LSHR        : TwoInOneOut<IL_OP_I64_SHR, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI32:$src2),
                  !strconcat(IL_OP_I64_SHR.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (sra GPRI64:$src1, GPRI32:$src2))]>;
def LSHL       : TwoInOneOut<IL_OP_I64_SHL, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI32:$src2),
                  !strconcat(IL_OP_I64_SHL.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (shl GPRI64:$src1, GPRI32:$src2))]>;
// Apple requires a pattern since they pass down the shift operand as
// a 64bit value, although the lower 6 bits are all that are used.
def LSHR_APPLE        : TwoInOneOut<IL_OP_I64_SHR, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI64:$src2),
                  !strconcat(IL_OP_I64_SHR.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (sra GPRI64:$src1, GPRI64:$src2))]>;
def LSHL_APPLE       : TwoInOneOut<IL_OP_I64_SHL, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI64:$src2),
                  !strconcat(IL_OP_I64_SHL.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (shl GPRI64:$src1, GPRI64:$src2))]>;
}


//===---------------------------------------------------------------------===//
// Unsigned 64bit integer math instructions start here
//===---------------------------------------------------------------------===//
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
let Predicates = [HasHW64Bit] in {
def LUSHR        : TwoInOneOut<IL_OP_U64_SHR, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI32:$src2),
                  !strconcat(IL_OP_U64_SHR.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (srl GPRI64:$src1, GPRI32:$src2))]>;
// Apple requires a pattern since they pass down the shift operand as
// a 64bit value, although the lower 6 bits are all that are used.
def LUSHR_APPLE        : TwoInOneOut<IL_OP_U64_SHR, (outs GPRI64:$dst),
                  (ins GPRI64:$src1, GPRI64:$src2),
                  !strconcat(IL_OP_U64_SHR.Text, " $dst, $src1, $src2"),
                [(set GPRI64:$dst, (srl GPRI64:$src1, GPRI64:$src2))]>;
}


//===---------------------------------------------------------------------===//
// Generic Float Instructions
//===---------------------------------------------------------------------===//
let hasIEEEFlag = 1 in {
defm MUL_IEEE  : BinaryOpMCFloat<IL_OP_MUL_IEEE, IL_OP_D_MUL, fmul>;
}
defm ADD  : BinaryOpMCFloat<IL_OP_ADD, IL_OP_D_ADD, fadd>;
//===---------------------------------------------------------------------===//
// float math instructions start here
//===---------------------------------------------------------------------===//
let mayLoad=0, mayStore=0 in {
defm ABS : UnaryIntrinsicFloat<IL_OP_ABS, int_AMDIL_fabs>;
defm FRAC : UnaryIntrinsicFloat<IL_OP_FRC, int_AMDIL_fraction>;
defm PIREDUCE : UnaryIntrinsicFloat<IL_OP_PI_REDUCE, int_AMDIL_pireduce>;
defm ROUND_NEAREST : UnaryIntrinsicFloat<IL_OP_ROUND_NEAR,
          int_AMDIL_round_nearest>;
defm ROUND_NEGINF : UnaryIntrinsicFloat<IL_OP_ROUND_NEG_INF,
          int_AMDIL_round_neginf>;
defm ROUND_POSINF : UnaryIntrinsicFloat<IL_OP_ROUND_POS_INF,
          int_AMDIL_round_posinf>;
defm ROUND_ZERO : UnaryIntrinsicFloat<IL_OP_ROUND_ZERO,
          int_AMDIL_round_zero>;
defm ACOS : UnaryIntrinsicFloatScalar<IL_OP_ACOS, int_AMDIL_acos>;
defm ATAN : UnaryIntrinsicFloatScalar<IL_OP_ATAN, int_AMDIL_atan>;
defm ASIN : UnaryIntrinsicFloatScalar<IL_OP_ASIN, int_AMDIL_asin>;
defm TAN : UnaryIntrinsicFloatScalar<IL_OP_TAN, int_AMDIL_tan>;
defm SIN : UnaryIntrinsicFloatScalar<IL_OP_SIN, int_AMDIL_sin>;
defm COS : UnaryIntrinsicFloatScalar<IL_OP_COS, int_AMDIL_cos>;
defm SQRT : UnaryIntrinsicFloatScalar<IL_OP_SQRT, int_AMDIL_sqrt>;
defm EXP : UnaryIntrinsicFloatScalar<IL_OP_EXP, int_AMDIL_exp>;
defm EXPVEC : UnaryIntrinsicFloat<IL_OP_EXP_VEC, int_AMDIL_exp_vec>;
defm SQRTVEC : UnaryIntrinsicFloat<IL_OP_SQRT_VEC, int_AMDIL_sqrt_vec>;
defm COSVEC : UnaryIntrinsicFloat<IL_OP_COS_VEC, int_AMDIL_cos_vec>;
defm SINVEC : UnaryIntrinsicFloat<IL_OP_SIN_VEC, int_AMDIL_sin_vec>;
defm LOGVEC : UnaryIntrinsicFloat<IL_OP_LOG_VEC, int_AMDIL_log_vec>;
defm RSQVEC : UnaryIntrinsicFloat<IL_OP_RSQ_VEC, int_AMDIL_rsq_vec>;
defm EXN : UnaryIntrinsicFloatScalar<IL_OP_EXN, int_AMDIL_exn>;
defm SIGN : UnaryIntrinsicFloat<IL_OP_SGN, int_AMDIL_sign>;
defm LENGTH : UnaryIntrinsicFloat<IL_OP_LEN, int_AMDIL_length>;
defm POW : BinaryIntrinsicFloat<IL_OP_POW, int_AMDIL_pow>;
}

let hasIEEEFlag = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm MIN  : BinaryIntrinsicFloat<IL_OP_MIN, int_AMDIL_min>;
defm MAX  : BinaryIntrinsicFloat<IL_OP_MAX, int_AMDIL_max>;
defm MAD  : TernaryIntrinsicFloat<IL_OP_MAD, int_AMDIL_mad>;
  }
defm MOD  : BinaryOpMCf32<IL_OP_MOD, frem>;
}
let hasZeroOpFlag = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm LN  : UnaryIntrinsicFloatScalar<IL_OP_LN, int_AMDIL_ln>;
defm LOG : UnaryIntrinsicFloatScalar<IL_OP_LOG, int_AMDIL_log>;
defm RSQ : UnaryIntrinsicFloatScalar<IL_OP_RSQ, int_AMDIL_rsq>;
defm DIV_INT  : BinaryIntrinsicFloat<IL_OP_DIV, int_AMDIL_div>;
defm DIV  : BinaryOpMCf32<IL_OP_DIV, fdiv>;
defm DIV_PRECISE  : BinaryIntrinsicFloat<IL_OP_DIV_PRECISE, int_AMDIL_div_precise>;
  }
}
  let mayLoad = 0, mayStore=0 in {
defm CLAMP : TernaryIntrinsicFloat<IL_OP_CLAMP, int_AMDIL_clamp>;
defm FMA  : TernaryIntrinsicFloat<IL_OP_FMA, int_AMDIL_fma>;
defm LERP  : TernaryIntrinsicFloat<IL_OP_LERP, int_AMDIL_lerp>;
  }
defm SUB   : BinaryOpMCf32<IL_OP_SUB, fsub>;
defm FABS  : UnaryOpMCf32<IL_OP_ABS, fabs>;
defm FMAD  : TernaryOpMCf32<IL_OP_MAD, IL_mad>;
defm NEARBY : UnaryOpMCf32<IL_OP_ROUND_NEAR, fnearbyint>;
defm TRUNC  : UnaryOpMCf32<IL_OP_ROUND_ZERO, ftrunc>;
defm CEIL   : UnaryOpMCf32<IL_OP_ROUND_POS_INF, fceil>;
defm FLOOR  : UnaryOpMCf32<IL_OP_ROUND_NEG_INF, ffloor>;

def NEG_f32         : OneInOneOut<IL_OP_MOV, (outs GPRF32:$dst),
  (ins GPRF32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, ${src0}_neg(xyzw)"),
    [(set GPRF32:$dst, (fneg GPRF32:$src0))]>;
def INTTOANY_f32    : OneInOneOut<IL_OP_MOV, (outs GPRF32:$dst),
  (ins GPRI32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, $src0"),
    [(set GPRF32:$dst, (IL_inttoany GPRI32:$src0))]>;
let hasIEEEFlag = 1 in {
def DP2ADD_f32 : ThreeInOneOut<IL_OP_DP2_ADD, (outs GPRF32:$dst),
    (ins GPRV2F32:$src0, GPRV2F32:$src1, GPRF32:$src2),
    !strconcat(IL_OP_DP2_ADD.Text, " $dst, $src0, $src1, $src2"),
    [(set GPRF32:$dst,
    (int_AMDIL_dp2_add GPRV2F32:$src0,
    GPRV2F32:$src1, GPRF32:$src2))]>;
def DP2_f32 : TwoInOneOut<IL_OP_DP2, (outs GPRF32:$dst),
    (ins GPRV2F32:$src0, GPRV2F32:$src1),
    !strconcat(IL_OP_DP2.Text, " $dst, $src0, $src1"),
    [(set GPRF32:$dst,
    (int_AMDIL_dp2 GPRV2F32:$src0, GPRV2F32:$src1))]>;
def DP3_f32 : TwoInOneOut<IL_OP_DP3, (outs GPRF32:$dst),
    (ins GPRV4F32:$src0, GPRV4F32:$src1),
    !strconcat(IL_OP_DP3.Text, " $dst, $src0, $src1"),
    [(set GPRF32:$dst,
    (int_AMDIL_dp3 GPRV4F32:$src0, GPRV4F32:$src1))]>;
def DP4_f32 : TwoInOneOut<IL_OP_DP4, (outs GPRF32:$dst),
    (ins GPRV4F32:$src0, GPRV4F32:$src1),
    !strconcat(IL_OP_DP4.Text, " $dst, $src0, $src1"),
    [(set GPRF32:$dst,
    (int_AMDIL_dp4 GPRV4F32:$src0, GPRV4F32:$src1))]>;
def FTZ_f32 : OneInOneOut<IL_OP_MUL_IEEE, (outs GPRF32:$dst),
    (ins GPRF32:$src), !strconcat(IL_OP_MUL_IEEE.Text, " $dst, $src, r0.1"),
    [(set GPRF32:$dst,
        (int_AMDIL_ftz GPRF32:$src))]>;
def FTZ_v2f32 : OneInOneOut<IL_OP_MUL_IEEE, (outs GPRV2F32:$dst),
    (ins GPRV2F32:$src), !strconcat(IL_OP_MUL_IEEE.Text, " $dst, $src, r0.1"),
    [(set GPRV2F32:$dst,
        (int_AMDIL_ftz GPRV2F32:$src))]>;
def FTZ_v4f32 : OneInOneOut<IL_OP_MUL_IEEE, (outs GPRV4F32:$dst),
    (ins GPRV4F32:$src), !strconcat(IL_OP_MUL_IEEE.Text, " $dst, $src, r0.1"),
    [(set GPRV4F32:$dst,
        (int_AMDIL_ftz GPRV4F32:$src))]>;
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
//===---------------------------------------------------------------------===//
// float math instructions end here
//===---------------------------------------------------------------------===//

//===---------------------------------------------------------------------===//
// float2 math instructions start here
//===---------------------------------------------------------------------===//
def NEG_v2f32       : OneInOneOut<IL_OP_MOV, (outs GPRV2F32:$dst),
  (ins GPRV2F32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, ${src0}_neg(xyzw)"),
    [(set GPRV2F32:$dst, (fneg GPRV2F32:$src0))]>;
//===---------------------------------------------------------------------===//
// float2 math instructions end here
//===---------------------------------------------------------------------===//

//===---------------------------------------------------------------------===//
// float4 math instructions start here
//===---------------------------------------------------------------------===//
def NEG_v4f32 : OneInOneOut<IL_OP_MOV, (outs GPRV4F32:$dst),
  (ins GPRV4F32:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, ${src0}_neg(xyzw)"),
    [(set GPRV4F32:$dst, (fneg GPRV4F32:$src0))]>;
//===---------------------------------------------------------------------===//
// float4 math instructions end here
//===---------------------------------------------------------------------===//

//===---------------------------------------------------------------------===//
// double math instructions start here
//===---------------------------------------------------------------------===//
def  SUB_f64       : TwoInOneOut<IL_OP_D_ADD, (outs GPRF64:$dst),
  (ins GPRF64:$src0, GPRF64:$src1),
     !strconcat(IL_OP_D_ADD.Text, " $dst, $src0, ${src1}_neg(yw)"),
     [(set GPRF64:$dst, (fsub GPRF64:$src0, GPRF64:$src1))]>;
def  SUB_v2f64      : TwoInOneOut<IL_OP_D_ADD, (outs GPRV2F64:$dst),
  (ins GPRV2F64:$src0, GPRV2F64:$src1),
     !strconcat(IL_OP_D_ADD.Text, " $dst, $src0, ${src1}_neg(yw)"),
     [(set GPRV2F64:$dst, (fsub GPRV2F64:$src0, GPRV2F64:$src1))]>;
def NEG_f64       : OneInOneOut<IL_OP_MOV, (outs GPRF64:$dst),
  (ins GPRF64:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, ${src0}_neg(yw)"),
    [(set GPRF64:$dst, (fneg GPRF64:$src0))]>;
def NEG_v2f64       : OneInOneOut<IL_OP_MOV, (outs GPRV2F64:$dst),
  (ins GPRV2F64:$src0),
    !strconcat(IL_OP_MOV.Text, " $dst, ${src0}_neg(yw)"),
    [(set GPRV2F64:$dst, (fneg GPRV2F64:$src0))]>;
  let mayLoad = 0, mayStore=0 in {
defm MIN  : BinaryIntrinsicDouble<IL_OP_D_MIN, int_AMDIL_min>;
defm MAX  : BinaryIntrinsicDouble<IL_OP_D_MAX, int_AMDIL_max>;
defm DIV  : BinaryIntrinsicDouble<IL_OP_D_DIV, int_AMDIL_div>;
defm MAD  : TernaryIntrinsicDouble<IL_OP_D_MAD, int_AMDIL_mad>;
defm DFMA : TernaryIntrinsicDouble<IL_OP_D_MAD, int_AMDIL_fma>;
defm FRAC : UnaryIntrinsicDouble<IL_OP_D_FRC, int_AMDIL_fraction>;
defm SQRT : UnaryIntrinsicDouble<IL_OP_D_SQRT, int_AMDIL_sqrt>;
defm RSQ  : UnaryIntrinsicDoubleScalar<IL_OP_D_RSQ, int_AMDIL_rsq>;
defm RCP  : UnaryIntrinsicDoubleScalar<IL_OP_D_RCP, int_AMDIL_drcp>;
defm DMAD : TernaryOpMCf64<IL_OP_D_MAD, IL_mad>;
  }
let Predicates = [HasHWDoubleAbs] in {
defm DABS : UnaryOpMCf64<IL_OP_D_ABS, fabs>;
  let mayLoad = 0, mayStore=0 in {
defm ABS  : UnaryIntrinsicDouble<IL_OP_D_ABS, int_AMDIL_fabs>;
  }
} 
let Predicates = [HasSWDoubleAbs] in {
def SWDABS_f64 : OneInOneOut<IL_OP_D_ABS, (outs GPRF64:$dst),
      (ins GPRF64:$src),
      !strconcat(IL_OP_D_FREXP.Text," $dst, ${src}_abs(yw)"),
      [(set GPRF64:$dst, (fabs GPRF64:$src))]>;
  let mayLoad = 0, mayStore=0 in {
def SWABS_f64 : OneInOneOut<IL_OP_D_ABS, (outs GPRF64:$dst),
      (ins GPRF64:$src),
      !strconcat(IL_OP_D_FREXP.Text," $dst, ${src}_abs(yw)"),
      [(set GPRF64:$dst, (int_AMDIL_fabs GPRF64:$src))]>;
  }
}
def FREXP_f64 : OneInOneOut<IL_OP_D_FREXP, (outs GPRV2I64:$dst),
      (ins GPRF64:$src),
      !strconcat(IL_OP_D_FREXP.Text," $dst, $src"),
      [(set GPRV2I64:$dst,
      (int_AMDIL_frexp_f64 GPRF64:$src))]>;
def LDEXP_f64 : TwoInOneOut<IL_OP_D_LDEXP, (outs GPRF64:$dst),
      (ins GPRF64:$src, GPRI32:$src1),
      !strconcat(IL_OP_D_LDEXP.Text, " $dst, $src, $src1"),
      [(set GPRF64:$dst,
      (int_AMDIL_ldexp GPRF64:$src, GPRI32:$src1))]>;
def LDEXP_v2f64 : TwoInOneOut<IL_OP_D_LDEXP, (outs GPRV2F64:$dst),
      (ins GPRV2F64:$src, GPRV2I32:$src1),
      !strconcat(IL_OP_D_LDEXP.Text, " $dst, $src, $src1"),
      [(set GPRV2F64:$dst,
      (int_AMDIL_ldexp GPRV2F64:$src, GPRV2I32:$src1))]>;
//===---------------------------------------------------------------------===//
// double math instructions end here
//===---------------------------------------------------------------------===//
//===---------------------------------------------------------------------===//
// Various Macros
//===---------------------------------------------------------------------===//
def MACRO__sdiv_i8   : BinaryMacro< GPRI8, GPRI8, GPRI8, sdiv>;
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
// Apple requires a pattern since they pass down the shift operand as
// a 64bit value, although the lower 6 bits are all that are used.
// vector 2 use the software emulated mode since SC only supports
// scalar 64bit ops.
def MACRO__shr_apple_v2i64: BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I64, srl>;
def MACRO__shl_apple_v2i64: BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I64, shl>;
def MACRO__sra_apple_v2i64: BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I64, sra>;
def MACRO__shr_v2i64 : BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I32, srl>;
def MACRO__shl_v2i64 : BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I32, shl>;
def MACRO__sra_v2i64 : BinaryMacro<GPRV2I64, GPRV2I64, GPRV2I32, sra>;

let Predicates = [HasSW64Bit] in {
def MACRO__shr_apple_i64: BinaryMacro<GPRI64, GPRI64, GPRI64, srl>;
def MACRO__shl_apple_i64: BinaryMacro<GPRI64, GPRI64, GPRI64, shl>;
def MACRO__sra_apple_i64: BinaryMacro<GPRI64, GPRI64, GPRI64, sra>;
def MACRO__shr_i64   : BinaryMacro<GPRI64, GPRI64, GPRI32, srl>;
def MACRO__shl_i64   : BinaryMacro<GPRI64, GPRI64, GPRI32, shl>;
def MACRO__sra_i64   : BinaryMacro<GPRI64, GPRI64, GPRI32, sra>;
}

//===---------------------------------------------------------------------===//
// Comparison Instructions
//===---------------------------------------------------------------------===//
let usesCustomInserter = 1 in {
    defm CMP : Compare<"Pseudo comparison instr">;
}
//===---------------------------------------------------------------------===//
// 32-bit floating point operations
//===---------------------------------------------------------------------===//
def FEQ         : TwoInOneOut<IL_OP_EQ, (outs GPRF32:$dst),
        (ins GPRF32:$lhs, GPRF32:$rhs),
        !strconcat(IL_OP_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def FGE         : TwoInOneOut<IL_OP_GE, (outs GPRF32:$dst),
        (ins GPRF32:$lhs, GPRF32:$rhs),
        !strconcat(IL_OP_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def FLT         : TwoInOneOut<IL_OP_LT, (outs GPRF32:$dst),
        (ins GPRF32:$lhs, GPRF32:$rhs),
        !strconcat(IL_OP_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def FLT_v2f32 : TwoInOneOut<IL_OP_LT, (outs GPRV2F32:$dst),
        (ins GPRV2F32:$lhs, GPRV2F32:$rhs),
        !strconcat(IL_OP_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def FLT_v4f32 : TwoInOneOut<IL_OP_LT, (outs GPRV4F32:$dst),
        (ins GPRV4F32:$lhs, GPRV4F32:$rhs),
        !strconcat(IL_OP_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def FNE         : TwoInOneOut<IL_OP_NE, (outs GPRF32:$dst),
        (ins GPRF32:$lhs, GPRF32:$rhs),
        !strconcat(IL_OP_NE.Text, " $dst, $lhs, $rhs")
        , []>;

//===---------------------------------------------------------------------===//
//TODO: need to correctly define comparison instructions
//===---------------------------------------------------------------------===//
def DEQ        : TwoInOneOut<IL_OP_D_EQ, (outs GPRF64:$dst),
        (ins GPRF64:$lhs, GPRF64:$rhs),
        !strconcat(IL_OP_D_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def DEQ_v2f64        : TwoInOneOut<IL_OP_D_EQ, (outs GPRV2F64:$dst),
        (ins GPRV2F64:$lhs, GPRV2F64:$rhs),
        !strconcat(IL_OP_D_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def DGE        : TwoInOneOut<IL_OP_D_GE, (outs GPRF64:$dst),
        (ins GPRF64:$lhs, GPRF64:$rhs),
        !strconcat(IL_OP_D_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def DLT        : TwoInOneOut<IL_OP_D_LT, (outs GPRF64:$dst),
        (ins GPRF64:$lhs, GPRF64:$rhs),
        !strconcat(IL_OP_D_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def DNE        : TwoInOneOut<IL_OP_D_NE, (outs GPRF64:$dst),
        (ins GPRF64:$lhs, GPRF64:$rhs),
        !strconcat(IL_OP_D_NE.Text, " $dst, $lhs, $rhs")
        , []>;

//===---------------------------------------------------------------------===//
//TODO: need to correctly define comparison instructions
//===---------------------------------------------------------------------===//
def IEQ        : TwoInOneOut<IL_OP_I_EQ, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def IEQ_v2i32        : TwoInOneOut<IL_OP_I_EQ, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def IEQ_v4i32        : TwoInOneOut<IL_OP_I_EQ, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def IGE        : TwoInOneOut<IL_OP_I_GE, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def IGE_v2i32        : TwoInOneOut<IL_OP_I_GE, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def IGE_v4i32        : TwoInOneOut<IL_OP_I_GE, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def ILT        : TwoInOneOut<IL_OP_I_LT, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def ILT_v2i32        : TwoInOneOut<IL_OP_I_LT, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def ILT_v4i32        : TwoInOneOut<IL_OP_I_LT, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def INE        : TwoInOneOut<IL_OP_I_NE, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
def INE_v2i32        : TwoInOneOut<IL_OP_I_NE, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
def INE_v4i32        : TwoInOneOut<IL_OP_I_NE, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
let Predicates = [HasHW64Bit] in {
def LEQ        : TwoInOneOut<IL_OP_I64_EQ, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def LGE        : TwoInOneOut<IL_OP_I64_GE, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def LLE        : TwoInOneOut<IL_OP_I64_GE, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_GE.Text, " $dst, $rhs, $lhs")
        , []>;
def LGT        : TwoInOneOut<IL_OP_I64_LT, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_LT.Text, " $dst, $rhs, $lhs")
        , []>;
def LLT        : TwoInOneOut<IL_OP_I64_LT, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def LNE        : TwoInOneOut<IL_OP_I64_NE, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_I64_NE.Text, " $dst, $lhs, $rhs")
        , []>;
}

//===---------------------------------------------------------------------===//
// Unsigned Integer Operations
//===---------------------------------------------------------------------===//

//===---------------------------------------------------------------------===//
//TODO: need to correctly define comparison instructions
//===---------------------------------------------------------------------===//
def UEQ        : TwoInOneOut<IL_OP_I_EQ, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def UEQ_v2i32        : TwoInOneOut<IL_OP_I_EQ, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def UEQ_v4i32        : TwoInOneOut<IL_OP_I_EQ, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_EQ.Text, " $dst, $lhs, $rhs")
        , []>;
def ULE        : TwoInOneOut<IL_OP_U_GE, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def ULE_v2i32        : TwoInOneOut<IL_OP_U_GE, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def ULE_v4i32        : TwoInOneOut<IL_OP_U_GE, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def UGT        : TwoInOneOut<IL_OP_U_LT, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def UGT_v2i32        : TwoInOneOut<IL_OP_U_LT, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def UGT_v4i32        : TwoInOneOut<IL_OP_U_LT, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def UGE        : TwoInOneOut<IL_OP_U_GE, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def UGE_v2i32        : TwoInOneOut<IL_OP_U_GE, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def UGE_v4i32        : TwoInOneOut<IL_OP_U_GE, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_U_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def ULT        : TwoInOneOut<IL_OP_U_LT, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def ULT_v2i32        : TwoInOneOut<IL_OP_U_LT, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def ULT_v4i32        : TwoInOneOut<IL_OP_U_LT, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_U_LT.Text, " $dst, $lhs, $rhs")
        , []>;
def UNE        : TwoInOneOut<IL_OP_I_NE, (outs GPRI32:$dst),
        (ins GPRI32:$lhs, GPRI32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
def UNE_v2i32        : TwoInOneOut<IL_OP_I_NE, (outs GPRV2I32:$dst),
        (ins GPRV2I32:$lhs, GPRV2I32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
def UNE_v4i32        : TwoInOneOut<IL_OP_I_NE, (outs GPRV4I32:$dst),
        (ins GPRV4I32:$lhs, GPRV4I32:$rhs),
        !strconcat(IL_OP_I_NE.Text, " $dst, $lhs, $rhs")
        , []>;
let Predicates = [HasHW64Bit] in {
def ULLE        : TwoInOneOut<IL_OP_U64_GE, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_U64_GE.Text, " $dst, $rhs, $lhs")
        , []>;
def ULGT        : TwoInOneOut<IL_OP_U64_LT, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_U64_LT.Text, " $dst, $rhs, $lhs")
        , []>;
def ULGE        : TwoInOneOut<IL_OP_U64_GE, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_U64_GE.Text, " $dst, $lhs, $rhs")
        , []>;
def ULLT        : TwoInOneOut<IL_OP_U64_LT, (outs GPRI64:$dst),
        (ins GPRI64:$lhs, GPRI64:$rhs),
        !strconcat(IL_OP_U64_LT.Text, " $dst, $lhs, $rhs")
        , []>;
}
//===---------------------------------------------------------------------===//
// Scalar ==> Scalar conversion functions
//===---------------------------------------------------------------------===//
// f32 ==> f64
def FTOD        : UnaryOp<IL_OP_F_2_D,         fextend,     GPRF64, GPRF32>;
// f64 ==> f32
def DTOF        : UnaryOp<IL_OP_D_2_F,         IL_d2f,     GPRF32, GPRF64>;
// f32 ==> i32 signed
def FTOI        : UnaryOp<IL_OP_FTOI,          fp_to_sint, GPRI32, GPRF32>;
def FTOI_v2i32  : UnaryOp<IL_OP_FTOI,          fp_to_sint, GPRV2I32, GPRV2F32>;
def FTOI_v4i32  : UnaryOp<IL_OP_FTOI,          fp_to_sint, GPRV4I32, GPRV4F32>;
// i32 ==> f32 signed
def ITOF        : UnaryOp<IL_OP_ITOF,          sint_to_fp, GPRF32, GPRI32>;
def ITOF_v2f32  : UnaryOp<IL_OP_ITOF,          sint_to_fp, GPRV2F32, GPRV2I32>;
def ITOF_v4f32  : UnaryOp<IL_OP_ITOF,          sint_to_fp, GPRV4F32, GPRV4I32>;
// f32 ==> i32 unsigned
def FTOU        : UnaryOp<IL_OP_FTOU,          fp_to_uint, GPRI32, GPRF32>;
def FTOU_v2i32  : UnaryOp<IL_OP_FTOU,          fp_to_uint, GPRV2I32, GPRV2F32>;
def FTOU_v4i32  : UnaryOp<IL_OP_FTOU,          fp_to_uint, GPRV4I32, GPRV4F32>;
// i32 ==> f32 unsigned
def UTOF        : UnaryOp<IL_OP_UTOF,          uint_to_fp, GPRF32, GPRI32>;
def UTOF_v2f32  : UnaryOp<IL_OP_UTOF,          uint_to_fp, GPRV2F32, GPRV2I32>;
def UTOF_v4f32  : UnaryOp<IL_OP_UTOF,          uint_to_fp, GPRV4F32, GPRV4I32>;
let Predicates = [HasHWDoubleConv] in {
  // f64 ==> i32 signed
  def DTOI        : UnaryOp<IL_OP_DTOI,          fp_to_sint, GPRI32, GPRF64>;
  // i32 ==> f64 signed
  def ITOD        : UnaryOp<IL_OP_ITOD,          sint_to_fp, GPRF64, GPRI32>;
  // f64 ==> i32 unsigned
  def DTOU        : UnaryOp<IL_OP_DTOU,          fp_to_uint, GPRI32, GPRF64>;
  // i32 ==> f64 unsigned
  def UTOD        : UnaryOp<IL_OP_UTOD,          uint_to_fp, GPRF64, GPRI32>;
}
// Get upper 32 bits of f64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DHI         : OneInOneOut<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRF64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_dcomphi GPRF64:$src))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DHI_v2f64   : OneInOneOut<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2F64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_dcomphi2 GPRV2F64:$src))]>;
// Get lower 32 bits of f64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DLO         : OneInOneOut<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRF64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_dcomplo GPRF64:$src))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DLO_v2f64   : OneInOneOut<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2F64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_dcomplo2 GPRV2F64:$src))]>;
// Convert two 32 bit integers into a f64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DCREATE     : TwoInOneOut<IL_OP_I_ADD, (outs GPRF64:$dst),
                (ins GPRI32:$src0, GPRI32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRF64:$dst, (IL_dcreate GPRI32:$src0, GPRI32:$src1))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def DCREATE_v2f64 : TwoInOneOut<IL_OP_I_ADD, (outs GPRV2F64:$dst),
                (ins GPRV2I32:$src0, GPRV2I32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRV2F64:$dst,
                    (IL_dcreate2 GPRV2I32:$src0, GPRV2I32:$src1))]>;
// Get upper 32 bits of i64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LHI         : OneInOneOut<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRI64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_lcomphi GPRI64:$src))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LHI_v2i64         : OneInOneOut<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2I64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_lcomphi2 GPRV2I64:$src))]>;
// Get lower 32 bits of i64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LLO         : OneInOneOut<IL_OP_MOV,  (outs GPRI32:$dst),
                (ins GPRI64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRI32:$dst, (IL_lcomplo GPRI64:$src))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LLO_v2i64         : OneInOneOut<IL_OP_MOV,  (outs GPRV2I32:$dst),
                (ins GPRV2I64:$src),
                !strconcat(IL_OP_MOV.Text, " $dst, $src"),
                [(set GPRV2I32:$dst, (IL_lcomplo2 GPRV2I64:$src))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def HILO_BITOR_v4i16 : TwoInOneOut<IL_OP_I_OR, (outs GPRI32:$dst),
                (ins GPRI32:$src, GPRI32:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def HILO_BITOR_v2i32 : TwoInOneOut<IL_OP_I_OR, (outs GPRI32:$dst),
                (ins GPRI32:$src, GPRI32:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def HILO_BITOR_v2i64 : TwoInOneOut<IL_OP_I_OR, (outs GPRI64:$dst),
                (ins GPRI64:$src, GPRI64:$src2),
                !strconcat(IL_OP_I_OR.Text, " $dst, $src, $src2"), []>;
// Convert two 32 bit integers into a i64
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LCREATE     : TwoInOneOut<IL_OP_I_ADD, (outs GPRI64:$dst),
                (ins GPRI32:$src0, GPRI32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRI64:$dst, (IL_lcreate GPRI32:$src0, GPRI32:$src1))]>;
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
def LCREATE_v2i64     : TwoInOneOut<IL_OP_I_ADD, (outs GPRV2I64:$dst),
                (ins GPRV2I32:$src0, GPRV2I32:$src1),
                !strconcat(IL_OP_I_ADD.Text, " $dst, $src0, $src1"),
                [(set GPRV2I64:$dst,
                    (IL_lcreate2 GPRV2I32:$src0, GPRV2I32:$src1))]>;
//===---------------------------------------------------------------------===//
// Scalar ==> Vector conversion functions
//===---------------------------------------------------------------------===//
// This opcode has custom swizzle pattern encoded in Swizzle Encoder
defm VCREATE          : UnaryOpMCVec<IL_OP_MOV, IL_vbuild>;

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
defm IL_ASCHAR  : BitConversion<IL_OP_MOV, GPRI8, IL_bitconv>;
defm IL_ASSHORT : BitConversion<IL_OP_MOV, GPRI16, IL_bitconv>;
defm IL_ASINT   : BitConversion<IL_OP_MOV, GPRI32, IL_bitconv>;
defm IL_ASFLOAT : BitConversion<IL_OP_MOV, GPRF32, IL_bitconv>;
defm IL_ASDOUBLE : BitConversion<IL_OP_MOV, GPRF64, IL_bitconv>;
defm IL_ASLONG  : BitConversion<IL_OP_MOV, GPRI64, IL_bitconv>;
defm IL_ASV2CHAR  : BitConversion<IL_OP_MOV, GPRV2I8, IL_bitconv>;
defm IL_ASV2SHORT : BitConversion<IL_OP_MOV, GPRV2I16, IL_bitconv>;
defm IL_ASV2INT   : BitConversion<IL_OP_MOV, GPRV2I32, IL_bitconv>;
defm IL_ASV2FLOAT : BitConversion<IL_OP_MOV, GPRV2F32, IL_bitconv>;
defm IL_ASV2DOUBLE : BitConversion<IL_OP_MOV, GPRV2F64, IL_bitconv>;
defm IL_ASV2LONG  : BitConversion<IL_OP_MOV, GPRV2I64, IL_bitconv>;
defm IL_ASV4CHAR  : BitConversion<IL_OP_MOV, GPRV4I8, IL_bitconv>;
defm IL_ASV4SHORT : BitConversion<IL_OP_MOV, GPRV4I16, IL_bitconv>;
defm IL_ASV4INT   : BitConversion<IL_OP_MOV, GPRV4I32, IL_bitconv>;
defm IL_ASV4FLOAT : BitConversion<IL_OP_MOV, GPRV4F32, IL_bitconv>;

//===---------------------------------------------------------------------===//
// Custom Inserter for Branches and returns, this eventually will be a
// seperate pass
//===---------------------------------------------------------------------===//
let isTerminator = 1 in {
  def BRANCH : ILFormat<IL_PSEUDO_INST, (outs), (ins brtarget:$target),
      "; Pseudo unconditional branch instruction",
      [(br bb:$target)]>;
  defm BRANCH_COND : BranchConditional<IL_brcond>;
}
//===---------------------------------------------------------------------===//
// return instructions
//===---------------------------------------------------------------------===//
let isTerminator = 1, isReturn = 1, isBarrier = 1, hasCtrlDep = 1 in {
  def RETURN          : ILFormat<IL_OP_RET,(outs), (ins variable_ops),
      IL_OP_RET.Text, []>;
  def RETDYN      : ILFormat<IL_OP_RET_DYN, (outs), (ins variable_ops),
      IL_OP_RET_DYN.Text, [(IL_retflag)]>;
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
      def CALL : UnaryOpNoRet<IL_OP_CALL, (outs),
      (ins calltarget:$dst, variable_ops),
      !strconcat(IL_OP_CALL.Text, " $dst"), []>;
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
  def TRAP : ILFormat<IL_OP_NOP, (outs), (ins),
      IL_OP_NOP.Text, [(trap)]>;
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
  def FENCE : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_lds_memory_gds",
      [(int_AMDIL_fence GPRI32:$flag)]>;

  def FENCE_LOCAL : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_lds",
      [(int_AMDIL_fence_local GPRI32:$flag)]>;

  def FENCE_GLOBAL : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_memory",
      [(int_AMDIL_fence_global GPRI32:$flag)]>;

  def FENCE_GLOBAL_LOCAL : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_memory_lds",
      [(int_AMDIL_fence_global_local GPRI32:$flag)]>;

  def FENCE_REGION : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_gds",
      [(int_AMDIL_fence_region GPRI32:$flag)]>;

  def FENCE_REGION_LOCAL : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_gds_lds",
      [(int_AMDIL_fence_region_local GPRI32:$flag)]>;

  def FENCE_REGION_GLOBAL : BinaryOpNoRet<IL_OP_FENCE, (outs), (ins GPRI32:$flag),
      "fence_gds_memory",
      [(int_AMDIL_fence_region_global GPRI32:$flag)]>;

  def FENCE_READ_ONLY : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_lds_memory_gds_mem_read_only",
      [(int_AMDIL_read_fence GPRI32:$flag)]>;

  def FENCE_READ_ONLY_LOCAL : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_lds_mem_read_only",
      [(int_AMDIL_read_fence_local GPRI32:$flag)]>;

  def FENCE_READ_ONLY_GLOBAL : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_memory_mem_read_only",
      [(int_AMDIL_read_fence_global GPRI32:$flag)]>;

  def FENCE_READ_ONLY_GLOBAL_LOCAL : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_memory_lds_mem_read_only",
      [(int_AMDIL_read_fence_global_local GPRI32:$flag)]>;

  def FENCE_READ_ONLY_REGION : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_mem_read_only",
      [(int_AMDIL_read_fence_region GPRI32:$flag)]>;

  def FENCE_READ_ONLY_REGION_LOCAL : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_lds_mem_read_only",
      [(int_AMDIL_read_fence_region_local GPRI32:$flag)]>;

  def FENCE_READ_ONLY_REGION_GLOBAL : BinaryOpNoRet<IL_OP_FENCE_READ_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_memory_mem_read_only",
      [(int_AMDIL_read_fence_region_global GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_lds_memory_gds_mem_write_only",
      [(int_AMDIL_write_fence GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_LOCAL : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_lds_mem_write_only",
      [(int_AMDIL_write_fence_local GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_GLOBAL : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_memory_mem_write_only",
      [(int_AMDIL_write_fence_global GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_GLOBAL_LOCAL : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_memory_lds_mem_write_only",
      [(int_AMDIL_write_fence_global_local GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_REGION : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_mem_write_only",
      [(int_AMDIL_write_fence_region GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_REGION_LOCAL : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_lds_mem_write_only",
      [(int_AMDIL_write_fence_region_local GPRI32:$flag)]>;

  def FENCE_WRITE_ONLY_REGION_GLOBAL : BinaryOpNoRet<IL_OP_FENCE_WRITE_ONLY, (outs), (ins GPRI32:$flag),
      "fence_gds_memory_mem_write_only",
      [(int_AMDIL_write_fence_region_global GPRI32:$flag)]>;

}

let isReturn = 1 in {
  def EARLY_EXIT : UnaryOpNoRet<IL_OP_RET_LOGICALNZ, (outs),
      (ins GPRI32:$flag),
      !strconcat(IL_OP_RET_LOGICALNZ.Text, " $flag"),
      [(int_AMDIL_early_exit GPRI32:$flag)]>;
}
def MEDIA_UNPACK_0 : OneInOneOut<IL_OP_UNPACK_0, (outs GPRV4F32:$dst),
    (ins GPRV4I32:$src),
    !strconcat(IL_OP_UNPACK_0.Text, " $dst, $src"),
    [(set GPRV4F32:$dst,
        (v4f32 (int_AMDIL_media_unpack_byte_0 GPRV4I32:$src)))]>;
def MEDIA_UNPACK_1 : OneInOneOut<IL_OP_UNPACK_1, (outs GPRV4F32:$dst),
    (ins GPRV4I32:$src),
    !strconcat(IL_OP_UNPACK_1.Text, " $dst, $src"),
    [(set GPRV4F32:$dst,
        (v4f32 (int_AMDIL_media_unpack_byte_1 GPRV4I32:$src)))]>;
def MEDIA_UNPACK_2 : OneInOneOut<IL_OP_UNPACK_2, (outs GPRV4F32:$dst),
    (ins GPRV4I32:$src),
    !strconcat(IL_OP_UNPACK_2.Text, " $dst, $src"),
    [(set GPRV4F32:$dst,
        (v4f32 (int_AMDIL_media_unpack_byte_2 GPRV4I32:$src)))]>;
def MEDIA_UNPACK_3 : OneInOneOut<IL_OP_UNPACK_3, (outs GPRV4F32:$dst),
    (ins GPRV4I32:$src),
    !strconcat(IL_OP_UNPACK_3.Text, " $dst, $src"),
    [(set GPRV4F32:$dst,
        (v4f32 (int_AMDIL_media_unpack_byte_3 GPRV4I32:$src)))]>;

def SEMAPHORE_INIT : BinaryOpNoRet<IL_OP_SEMAPHORE_INIT, (outs),
    (ins MEM3232:$ptr, GPRI32:$val),
    !strconcat(IL_OP_SEMAPHORE_INIT.Text, "_id($ptr)_value($val)"),
    [(int_AMDIL_semaphore_init ADDR:$ptr, GPRI32:$val)]>;

def SEMAPHORE_WAIT : UnaryOpNoRet<IL_OP_SEMAPHORE_WAIT, (outs),
    (ins MEM3232:$ptr),
    !strconcat(IL_OP_SEMAPHORE_WAIT.Text, "_id($ptr)"),
    [(int_AMDIL_semaphore_wait ADDR:$ptr)]>;

def SEMAPHORE_SIGNAL : UnaryOpNoRet<IL_OP_SEMAPHORE_SIGNAL, (outs),
    (ins MEM3232:$ptr),
    !strconcat(IL_OP_SEMAPHORE_SIGNAL.Text, "_id($ptr)"),
    [(int_AMDIL_semaphore_signal ADDR:$ptr)]>;

let hasIEEEFlag = 1 in {
  defm MIN3 : TernaryIntrinsicFloat<IL_OP_MIN3, int_AMDIL_min3>;
  defm MED3 : TernaryIntrinsicFloat<IL_OP_MED3, int_AMDIL_med3>;
  defm MAX3 : TernaryIntrinsicFloat<IL_OP_MAX3, int_AMDIL_max3>;
  defm IMIN3 : TernaryIntrinsicInt<IL_OP_I_MIN3, int_AMDIL_min3_i32>;
  defm IMED3 : TernaryIntrinsicInt<IL_OP_I_MED3, int_AMDIL_med3_i32>;
  defm IMAX3 : TernaryIntrinsicInt<IL_OP_I_MAX3, int_AMDIL_max3_i32>;
  defm UMIN3 : TernaryIntrinsicInt<IL_OP_U_MIN3, int_AMDIL_min3_u32>;
  defm UMED3 : TernaryIntrinsicInt<IL_OP_U_MED3, int_AMDIL_med3_u32>;
  defm UMAX3 : TernaryIntrinsicInt<IL_OP_U_MAX3, int_AMDIL_max3_u32>;
}

def CLASS_f32 : TwoInOneOut<IL_OP_CLASS, (outs GPRI32:$dst),
    (ins GPRF32:$src, GPRI32:$flag),
    !strconcat(IL_OP_CLASS.Text, " $dst, $src, $flag"),
    [(set GPRI32:$dst,
        (int_AMDIL_class GPRF32:$src, GPRI32:$flag))]>;
def CLASS_v2f32 : TwoInOneOut<IL_OP_CLASS, (outs GPRV2I32:$dst),
    (ins GPRV2F32:$src, GPRV2I32:$flag),
    !strconcat(IL_OP_CLASS.Text, " $dst, $src, $flag"),
    [(set GPRV2I32:$dst,
        (int_AMDIL_class GPRV2F32:$src, GPRV2I32:$flag))]>;
def CLASS_v4f32 : TwoInOneOut<IL_OP_CLASS, (outs GPRV4I32:$dst),
    (ins GPRV4F32:$src, GPRV4I32:$flag),
    !strconcat(IL_OP_CLASS.Text, " $dst, $src, $flag"),
    [(set GPRV4I32:$dst,
        (int_AMDIL_class GPRV4F32:$src, GPRV4I32:$flag))]>;
def CLASS_f64 : TwoInOneOut<IL_OP_D_CLASS, (outs GPRI32:$dst),
    (ins GPRF64:$src, GPRI32:$flag),
    !strconcat(IL_OP_D_CLASS.Text, " $dst, $src, $flag"),
    [(set GPRI32:$dst,
        (int_AMDIL_class GPRF64:$src, GPRI32:$flag))]>;
def CLASS_v2f64 : TwoInOneOut<IL_OP_D_CLASS, (outs GPRV2I32:$dst),
    (ins GPRV2F64:$src, GPRV2I32:$flag),
    !strconcat(IL_OP_D_CLASS.Text, " $dst, $src, $flag"),
    [(set GPRV2I32:$dst,
        (int_AMDIL_class GPRV2F64:$src, GPRV2I32:$flag))]>;


defm FREXP_EXP : IntrConvertF32TOI32<IL_OP_FREXP_EXP, int_AMDIL_frexp_exp>;
def FREXP_EXP_f64 : OneInOneOut<IL_OP_D_FREXP_EXP, (outs GPRI32:$dst),
    (ins GPRF64:$src), 
    !strconcat(IL_OP_D_FREXP_EXP.Text, " $dst, $src"),
    [(set GPRI32:$dst,
        (int_AMDIL_frexp_exp GPRF64:$src))]>;
def FREXP_EXP_v2f64 : OneInOneOut<IL_OP_D_FREXP_EXP, (outs GPRV2I32:$dst),
    (ins GPRV2F64:$src), 
    !strconcat(IL_OP_D_FREXP_EXP.Text, " $dst, $src"),
    [(set GPRV2I32:$dst,
        (int_AMDIL_frexp_exp GPRV2F64:$src))]>;

defm FREXP_MANT : UnaryIntrinsicFloat<IL_OP_FREXP_MANT, int_AMDIL_frexp_mant>;
defm SAD16  : TernaryIntrinsicInt<IL_OP_SAD_U16, int_AMDIL_media_sad16>;
defm SAD32  : TernaryIntrinsicInt<IL_OP_SAD_U32, int_AMDIL_media_sad32>;

let hasZeroOpFlag = 1 in {
  let mayLoad = 0, mayStore=0 in {
defm DDIV_INT  : BinaryIntrinsicDouble<IL_OP_D_DIV, int_AMDIL_div>;
defm DDIV  : BinaryOpMCf64<IL_OP_D_DIV, fdiv>;
  }
}

defm FREXP_MANT : UnaryIntrinsicDouble<IL_OP_D_FREXP_MANT, int_AMDIL_frexp_mant>;

def DTRIG_PREOP : TwoInOneOut<IL_OP_D_TRIG_PREOP, (outs GPRF64:$dst),
    (ins GPRF64:$src0, GPRF32:$src1),
    !strconcat(IL_OP_D_TRIG_PREOP.Text, " $dst, $src0, $src1"),
    [(set GPRF64:$dst, 
        (int_AMDIL_trig_preop_f64 GPRF64:$src0, GPRF32:$src1))]>;


def LDEXP_f32 : TwoInOneOut<IL_OP_LDEXP, (outs GPRF32:$dst),
    (ins GPRF32:$src, GPRI32:$src1),
    !strconcat(IL_OP_LDEXP.Text, " $dst, $src, $src1"),
    [(set GPRF32:$dst,
        (int_AMDIL_ldexp GPRF32:$src, GPRI32:$src1))]>;

def LDEXP_v2f32 : TwoInOneOut<IL_OP_LDEXP, (outs GPRV2F32:$dst),
    (ins GPRV2F32:$src, GPRV2I32:$src1),
    !strconcat(IL_OP_LDEXP.Text, " $dst, $src, $src1"),
    [(set GPRV2F32:$dst,
        (int_AMDIL_ldexp GPRV2F32:$src, GPRV2I32:$src1))]>;

def LDEXP_v4f32 : TwoInOneOut<IL_OP_LDEXP, (outs GPRV4F32:$dst),
    (ins GPRV4F32:$src, GPRV4I32:$src1),
    !strconcat(IL_OP_LDEXP.Text, " $dst, $src, $src1"),
    [(set GPRV4F32:$dst,
        (int_AMDIL_ldexp GPRV4F32:$src, GPRV4I32:$src1))]>;
defm MSAD  : TernaryIntrinsicInt<IL_OP_MSAD, int_AMDIL_media_msad>;

def QSAD_i64  : ThreeInOneOut<IL_OP_QSAD, (outs GPRI64:$dst),
    (ins GPRI64:$src0, GPRI32:$src1, GPRI64:$src2),
    !strconcat(IL_OP_QSAD.Text, " $dst, $src0, $src1, $src2"),
    [(set GPRI64:$dst,
        (int_AMDIL_media_qsad GPRI64:$src0, GPRI32:$src1, GPRI64:$src2))]>;

def MQSAD_i64  : ThreeInOneOut<IL_OP_MQSAD, (outs GPRI64:$dst),
    (ins GPRI64:$src0, GPRI32:$src1, GPRI64:$src2),
    !strconcat(IL_OP_MQSAD.Text, " $dst, $src0, $src1, $src2"),
    [(set GPRI64:$dst,
        (int_AMDIL_media_mqsad GPRI64:$src0, GPRI32:$src1, GPRI64:$src2))]>;

defm ADD_RTE : BinaryIntrinsicFloat<IL_OP_ADD_RTE, int_AMDIL_add_rte>;
defm ADD_RTP : BinaryIntrinsicFloat<IL_OP_ADD_RTP, int_AMDIL_add_rtp>;
defm ADD_RTN : BinaryIntrinsicFloat<IL_OP_ADD_RTN, int_AMDIL_add_rtn>;
defm ADD_RTZ : BinaryIntrinsicFloat<IL_OP_ADD_RTZ, int_AMDIL_add_rtz>;
defm SUB_RTE : BinaryIntrinsicFloat<IL_OP_SUB_RTE, int_AMDIL_sub_rte>;
defm SUB_RTP : BinaryIntrinsicFloat<IL_OP_SUB_RTP, int_AMDIL_sub_rtp>;
defm SUB_RTN : BinaryIntrinsicFloat<IL_OP_SUB_RTN, int_AMDIL_sub_rtn>;
defm SUB_RTZ : BinaryIntrinsicFloat<IL_OP_SUB_RTZ, int_AMDIL_sub_rtz>;
defm MUL_RTE : BinaryIntrinsicFloat<IL_OP_MUL_RTE, int_AMDIL_mul_rte>;
defm MUL_RTP : BinaryIntrinsicFloat<IL_OP_MUL_RTP, int_AMDIL_mul_rtp>;
defm MUL_RTN : BinaryIntrinsicFloat<IL_OP_MUL_RTN, int_AMDIL_mul_rtn>;
defm MUL_RTZ : BinaryIntrinsicFloat<IL_OP_MUL_RTZ, int_AMDIL_mul_rtz>;
defm MAD_RTE : TernaryIntrinsicFloat<IL_OP_MAD_RTE, int_AMDIL_mad_rte>;
defm MAD_RTP : TernaryIntrinsicFloat<IL_OP_MAD_RTP, int_AMDIL_mad_rtp>;
defm MAD_RTN : TernaryIntrinsicFloat<IL_OP_MAD_RTN, int_AMDIL_mad_rtn>;
defm MAD_RTZ : TernaryIntrinsicFloat<IL_OP_MAD_RTZ, int_AMDIL_mad_rtz>;
defm FMA_RTE : TernaryIntrinsicFloat<IL_OP_FMA_RTE, int_AMDIL_fma_rte>;
defm FMA_RTP : TernaryIntrinsicFloat<IL_OP_FMA_RTP, int_AMDIL_fma_rtp>;
defm FMA_RTN : TernaryIntrinsicFloat<IL_OP_FMA_RTN, int_AMDIL_fma_rtn>;
defm FMA_RTZ : TernaryIntrinsicFloat<IL_OP_FMA_RTZ, int_AMDIL_fma_rtz>;
defm ADD_RTE : BinaryIntrinsicDoubleScalar<IL_OP_D_ADD_RTE, int_AMDIL_add_rte>;
defm ADD_RTP : BinaryIntrinsicDoubleScalar<IL_OP_D_ADD_RTP, int_AMDIL_add_rtp>;
defm ADD_RTN : BinaryIntrinsicDoubleScalar<IL_OP_D_ADD_RTN, int_AMDIL_add_rtn>;
defm ADD_RTZ : BinaryIntrinsicDoubleScalar<IL_OP_D_ADD_RTZ, int_AMDIL_add_rtz>;
defm SUB_RTE : BinaryIntrinsicDoubleScalar<IL_OP_D_SUB_RTE, int_AMDIL_sub_rte>;
defm SUB_RTP : BinaryIntrinsicDoubleScalar<IL_OP_D_SUB_RTP, int_AMDIL_sub_rtp>;
defm SUB_RTN : BinaryIntrinsicDoubleScalar<IL_OP_D_SUB_RTN, int_AMDIL_sub_rtn>;
defm SUB_RTZ : BinaryIntrinsicDoubleScalar<IL_OP_D_SUB_RTZ, int_AMDIL_sub_rtz>;
defm MUL_RTE : BinaryIntrinsicDoubleScalar<IL_OP_D_MUL_RTE, int_AMDIL_mul_rte>;
defm MUL_RTP : BinaryIntrinsicDoubleScalar<IL_OP_D_MUL_RTP, int_AMDIL_mul_rtp>;
defm MUL_RTN : BinaryIntrinsicDoubleScalar<IL_OP_D_MUL_RTN, int_AMDIL_mul_rtn>;
defm MUL_RTZ : BinaryIntrinsicDoubleScalar<IL_OP_D_MUL_RTZ, int_AMDIL_mul_rtz>;
defm MAD_RTE : TernaryIntrinsicDoubleScalar<IL_OP_D_MAD_RTE, int_AMDIL_mad_rte>;
defm MAD_RTP : TernaryIntrinsicDoubleScalar<IL_OP_D_MAD_RTP, int_AMDIL_mad_rtp>;
defm MAD_RTN : TernaryIntrinsicDoubleScalar<IL_OP_D_MAD_RTN, int_AMDIL_mad_rtn>;
defm MAD_RTZ : TernaryIntrinsicDoubleScalar<IL_OP_D_MAD_RTZ, int_AMDIL_mad_rtz>;


