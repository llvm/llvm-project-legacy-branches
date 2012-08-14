//===-- AMDILInstrPatterns.td ---------------------------------------------===//
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

def : Pat<(i32 (or GPRI32:$src0, GPRI32:$src1)),
          (i32 (BINARY_OR_i32 GPRI32:$src0, GPRI32:$src1))>;

// integer subtraction
// a - b ==> a + (-b)
def SUB_i8 : Pat<(sub GPRI8:$src0, GPRI8:$src1),
    (ADD_i8 GPRI8:$src0, (NEGATE_i8 GPRI8:$src1))>;
def SUB_v2i8 : Pat<(sub GPRV2I8:$src0, GPRV2I8:$src1),
    (ADD_v2i8 GPRV2I8:$src0, (NEGATE_v2i8 GPRV2I8:$src1))>;
def SUB_v4i8 : Pat<(sub GPRV4I8:$src0, GPRV4I8:$src1),
    (ADD_v4i8 GPRV4I8:$src0, (NEGATE_v4i8 GPRV4I8:$src1))>;
def SUB_i16 : Pat<(sub GPRI16:$src0, GPRI16:$src1),
    (ADD_i16 GPRI16:$src0, (NEGATE_i16 GPRI16:$src1))>;
def SUB_v2i16 : Pat<(sub GPRV2I16:$src0, GPRV2I16:$src1),
    (ADD_v2i16 GPRV2I16:$src0, (NEGATE_v2i16 GPRV2I16:$src1))>;
def SUB_v4i16 : Pat<(sub GPRV4I16:$src0, GPRV4I16:$src1),
    (ADD_v4i16 GPRV4I16:$src0, (NEGATE_v4i16 GPRV4I16:$src1))>;
def SUB_i32 : Pat<(sub GPRI32:$src0, GPRI32:$src1),
    (ADD_i32 GPRI32:$src0, (NEGATE_i32 GPRI32:$src1))>;
def SUB_v2i32 : Pat<(sub GPRV2I32:$src0, GPRV2I32:$src1),
    (ADD_v2i32 GPRV2I32:$src0, (NEGATE_v2i32 GPRV2I32:$src1))>;
def SUB_v4i32 : Pat<(sub GPRV4I32:$src0, GPRV4I32:$src1),
    (ADD_v4i32 GPRV4I32:$src0, (NEGATE_v4i32 GPRV4I32:$src1))>;

// Convert between float -> ulong efficiently
// static ulong
// cf2ul(float f)
// {
//     float fh = f * 0x1.0p-32f;
//     uint uh = (uint)fh;
//     float fuh = (float)uh;
//     float fl = mad(-0x1.0p+32f, fuh, f);
//     uint ul = (uint)fl;
//     return as_ulong((uint2)(ul, uh));
// }
def FTOUL_i64 : Pat<(i64 (fp_to_uint GPRF32:$src0)),
    (LCREATE
     (FTOU 
      (FMAD_f32 
       (IL_ASFLOAT_i32 (LOADCONST_i32 0xcf800000)),
       (UTOF 
        (FTOU 
         (MUL_IEEE_f32 GPRF32:$src0, 
          (IL_ASFLOAT_i32 (LOADCONST_i32 0x2f800000))
         )
        )
       ),
       GPRF32:$src0)
     ),
     (FTOU 
      (MUL_IEEE_f32 GPRF32:$src0, 
       (IL_ASFLOAT_i32 (LOADCONST_i32 0x2f800000))
      )
     )
    )>;

// static ulong2
// cf22ul2(float2 f)
// {
//     float2 fh = f * 0x1.0p-32f;
//     uint2 uh = convert_uint2(fh);
//     float2 fuh = convert_float2(uh);
//     float2 fl = mad(-0x1.0p+32f, fuh, f);
//     uint2 ul = convert_uint2(fl);
//     return as_ulong2((uint4)(ul, uh));
// }
def FTOUL_v2i64 : Pat<(v2i64 (fp_to_uint GPRV2F32:$src0)),
    (LCREATE_v2i64
     (FTOU_v2i32
      (FMAD_v2f32
       (VCREATE_v2f32 
        (IL_ASFLOAT_i32 (LOADCONST_i32 0xcf800000))),
       (UTOF_v2f32 (FTOU_v2i32 
                    (MUL_IEEE_v2f32 GPRV2F32:$src0, 
                     (VCREATE_v2f32 
                      (IL_ASFLOAT_i32 (LOADCONST_i32 0x2f800000)))))),
       GPRV2F32:$src0)),
     (FTOU_v2i32 (MUL_IEEE_v2f32 GPRV2F32:$src0, 
                  (VCREATE_v2f32 
                   (IL_ASFLOAT_i32 (LOADCONST_i32 0x2f800000))))))>;

// LLVM isn't lowering this correctly, so writing a pattern that
// matches it isntead.
def : Pat<(build_vector (f32 fpimm:$src)),
    (VCREATE_v4f32 (LOADCONST_f32 fpimm:$src))>;
def : Pat<(build_vector (i32 imm:$src)),
    (VCREATE_v4i32 (LOADCONST_i32 imm:$src))>;
def : Pat<(build_vector (i16 imm:$src)),
    (VCREATE_v4i16 (LOADCONST_i16 imm:$src))>;
def : Pat<(build_vector (i8 imm:$src)),
    (VCREATE_v4i8 (LOADCONST_i8 imm:$src))>;
def : Pat<(build_vector (f64 fpimm:$src)),
    (VCREATE_v2f64 (LOADCONST_f64 fpimm:$src))>;
def : Pat<(build_vector (f32 fpimm:$src)),
    (VCREATE_v2f32 (LOADCONST_f32 fpimm:$src))>;
def : Pat<(build_vector (i64 imm:$src)),
    (VCREATE_v2i64 (LOADCONST_i64 imm:$src))>;
def : Pat<(build_vector (i32 imm:$src)),
    (VCREATE_v2i32 (LOADCONST_i32 imm:$src))>;
def : Pat<(build_vector (i16 imm:$src)),
    (VCREATE_v2i16 (LOADCONST_i16 imm:$src))>;
def : Pat<(build_vector (i8 imm:$src)),
    (VCREATE_v2i8 (LOADCONST_i8 imm:$src))>;

// Correctly lower shl with 32bit left hand side immediate
def : Pat<(i32 (shl imm:$src, GPRI64:$shift)),
    (SHL_i32 (LOADCONST_i32 imm:$src), (LLO GPRI64:$shift))>;
def : Pat<(i32 (sra imm:$src, GPRI64:$shift)),
    (SHL_i32 (LOADCONST_i32 imm:$src), (LLO GPRI64:$shift))>;
def : Pat<(i32 (srl imm:$src, GPRI64:$shift)),
    (SHL_i32 (LOADCONST_i32 imm:$src), (LLO GPRI64:$shift))>;
def : Pat<(i32 (shl GPRI32:$src, GPRI64:$shift)),
    (SHL_i32 GPRI32:$src, (LLO GPRI64:$shift))>;
def : Pat<(i32 (sra GPRI32:$src, GPRI64:$shift)),
    (SHL_i32 GPRI32:$src, (LLO GPRI64:$shift))>;
def : Pat<(i32 (srl GPRI32:$src, GPRI64:$shift)),
    (SHL_i32 GPRI32:$src, (LLO GPRI64:$shift))>;
// Correctly lower shl with 64bit right hand side immediate
def : Pat<(i32 (shl GPRI32:$src, (i64 imm:$shift))),
    (SHL_i32 GPRI32:$src, (LLO (LOADCONST_i64 imm:$shift)))>;
def : Pat<(i32 (sra GPRI32:$src, (i64 imm:$shift))),
    (SHL_i32 GPRI32:$src, (LLO (LOADCONST_i64 imm:$shift)))>;
def : Pat<(i32 (srl GPRI32:$src, (i64 imm:$shift))),
    (SHL_i32 GPRI32:$src, (LLO (LOADCONST_i64 imm:$shift)))>;

// Calls:
def : Pat<(IL_call tglobaladdr:$dst),
    (CALL tglobaladdr:$dst)>;
def : Pat<(IL_call texternalsym:$dst),
    (CALL texternalsym:$dst)>;
def : Pat<(IL_call tconstpool:$dst),
  (CALL tconstpool:$dst)>;

include "AMDILConversions.td"

/// Bitfield Insert pattern fragments
def isLoadConstantAllOnes : PatLeaf<(timm),
    [{
      return N->isAllOnesValue();
    }]>;

/// Pattern 1: (lhs & bitpat) | (rhs & ~bitpat)
def bfi_pat1 : PatFrag<(ops node:$lhs, node:$rhs, node:$bitpat),
    (or 
     (and node:$lhs, node:$bitpat), 
     (and node:$rhs, (not node:$bitpat)))>;

/// Pattern 1b: (lhs & bitpat) | (rhs & ~bitpat)
/// FIXME: This pattern needs to be removed, but requires cleanup of IL_or
def bfi_pat1b : PatFrag<(ops node:$lhs, node:$rhs, node:$bitpat),
    (IL_or 
     (and node:$lhs, node:$bitpat), 
     (and node:$rhs, (not node:$bitpat)))>;

/// Pattern 2: (lhs & bitpat) | (rhs & (bitpat ^ -1))
def bfi_pat2 : PatFrag<(ops node:$lhs, node:$rhs, node:$bitpat),
    (or 
     (and node:$lhs, node:$bitpat),
     (and node:$rhs, 
      (xor node:$bitpat, isLoadConstantAllOnes) ))>;

/// Pattern 2b: (lhs & bitpat) | (rhs & (bitpat ^ -1))
/// FIXME: This pattern needs to be removed, but requires cleanup of IL_or
def bfi_pat2b : PatFrag<(ops node:$lhs, node:$rhs, node:$bitpat),
    (IL_or 
     (and node:$lhs, node:$bitpat),
     (and node:$rhs, 
      (xor node:$bitpat, isLoadConstantAllOnes) ))>;

/// Pattern 3: (rhs ^ ((rhs ^ lhs) & bitpat))
def bfi_pat3 : PatFrag<(ops node:$lhs, node:$rhs, node:$bitpat),
    (xor node:$rhs, 
     (and (xor node:$rhs, node:$lhs),
       node:$bitpat))>;

/// Bitfield Insert pattern fragments
def isLoadConstantOne : PatLeaf<(timm),
    [{
      return N->isOne();
    }]>;

def is0x1FConstant : PatLeaf<(timm),
    [{
      return N->getZExtValue() == 0x1F;
    }]>;

def is0x3EConstant : PatLeaf<(timm),
    [{
      return N->getZExtValue() == 0x3E;
    }]>;

def is0x1FConstantOrLess : PatLeaf<(timm),
    [{
      return N->getZExtValue() <= 0x1F;
    }]>;

def bitmask_5bits : PatFrag<(ops node:$mask),
  (and node:$mask, (i32 0x1f))>;

/// Bitfield mask instruction patterns.
/// Pattern 1: ((1 << (width & 0x1F)) + 0xFFFFFFFF) << (offset & 0x1F)
def bfm_pat1 : PatFrag<(ops node:$width, node:$offset),
    (shl (add (shl (i32 1), (bitmask_5bits node:$width)), (i32 0xFFFFFFFF)),
     (bitmask_5bits node:$offset))>;

/// Pattern 1: ((1 << (width & 0x1F)) + 0xFFFFFFFF) << (offset & 0x1F)
/// FIXME: Need to remove this pattern, but requires clean up of IL_add pattern.
def bfm_pat1b : PatFrag<(ops node:$width, node:$offset),
    (shl (IL_add (shl (i32 1), (bitmask_5bits node:$width)), (i32 0xFFFFFFFF)),
     (bitmask_5bits node:$offset))>;

let Predicates = [HasHWBitFieldInst] in {
defm BFI_PAT1A : TernaryPatFragI32<IL_OP_BFI, bfi_pat1>;
defm BFI_PAT1B : TernaryPatFragI32<IL_OP_BFI, bfi_pat1b>;
defm BFI_PAT2A : TernaryPatFragI32<IL_OP_BFI, bfi_pat2>;
defm BFI_PAT2B : TernaryPatFragI32<IL_OP_BFI, bfi_pat2b>;
defm BFI_PAT3  : TernaryPatFragI32<IL_OP_BFI, bfi_pat3>;
defm BFM_PAT1A  : BinaryPatFragI32<IL_OP_BFM, bfm_pat1>;
defm BFM_PAT1B  : BinaryPatFragI32<IL_OP_BFM, bfm_pat1b>;
}

//
// bitalign
// dst = (src0 << (32 - src2[4:0])) | (src1 >> src2[4:0])

// A.  src2 is constant
def bitalign_1 : PatFrag<(ops node:$src0, node:$src1, node:$src2, node:$src3),
    (or (shl  node:$src0, node:$src3), (srl  node:$src1, node:$src2)),
    [{
      SDNode *N_or1 = N->getOperand(1).getNode();
      SDNode *N_src2 = N_or1->getOperand(1).getNode();
      ConstantSDNode* CN_src2 = dyn_cast<ConstantSDNode>(N_src2);
      if (!CN_src2) {
        return false;
      }

      SDNode *N_or0 = N->getOperand(0).getNode();
      SDNode *N_src3 = N_or0->getOperand(1).getNode();
      ConstantSDNode* CN_src3 = dyn_cast<ConstantSDNode>(N_src3);
      if (!CN_src3) {
        return false;
      }

      uint32_t csrc2 = CN_src2->getZExtValue();
      uint32_t csrc3 = CN_src3->getZExtValue();
      return (csrc3 == (32 - csrc2));
    }]>;

def bitalign_1b : PatFrag<(ops node:$src0, node:$src1, node:$src2, node:$src3),
    (IL_or (shl  node:$src0, node:$src3), (srl  node:$src1, node:$src2)),
    [{
      SDNode *N_or1 = N->getOperand(1).getNode();
      SDNode *N_src2 = N_or1->getOperand(1).getNode();
      ConstantSDNode* CN_src2 = dyn_cast<ConstantSDNode>(N_src2);
      if (!CN_src2) {
        return false;
      }

      SDNode *N_or0 = N->getOperand(0).getNode();
      SDNode *N_src3 = N_or0->getOperand(1).getNode();
      ConstantSDNode* CN_src3 = dyn_cast<ConstantSDNode>(N_src3);
      if (!CN_src3) {
        return false;
      }

      uint32_t csrc2 = CN_src2->getZExtValue();
      uint32_t csrc3 = CN_src3->getZExtValue();
      return (csrc3 == (32 - csrc2));
    }]>;

def bitalign_2 : PatFrag<(ops node:$src0, node:$src1, node:$src2, node:$src3),
    (or (srl  node:$src1, node:$src2), (shl  node:$src0, node:$src3)),
    [{
      SDNode *N_or0 = N->getOperand(0).getNode();
      SDNode *N_src2 = N_or0->getOperand(1).getNode();
      ConstantSDNode* CN_src2 = dyn_cast<ConstantSDNode>(N_src2);
      if (!CN_src2) {
        return false;
      }

      SDNode *N_or1 = N->getOperand(1).getNode();
      SDNode *N_src3 = N_or1->getOperand(1).getNode();
      ConstantSDNode* CN_src3 = dyn_cast<ConstantSDNode>(N_src3);
      if (!CN_src3) {
        return false;
      }

      uint32_t csrc2 = CN_src2->getZExtValue();
      uint32_t csrc3 = CN_src3->getZExtValue();
      return (csrc3 == (32 - csrc2));
    }]>;

def bitalign_2b : PatFrag<(ops node:$src0, node:$src1, node:$src2, node:$src3),
    (IL_or (srl  node:$src1, node:$src2), (shl  node:$src0, node:$src3)),
    [{
      SDNode *N_or0 = N->getOperand(0).getNode();
      SDNode *N_src2 = N_or0->getOperand(1).getNode();
      ConstantSDNode* CN_src2 = dyn_cast<ConstantSDNode>(N_src2);
      if (!CN_src2) {
        return false;
      }
        
      SDNode *N_or1 = N->getOperand(1).getNode();
      SDNode *N_src3 = N_or1->getOperand(1).getNode();
      ConstantSDNode* CN_src3 = dyn_cast<ConstantSDNode>(N_src3);
      if (!CN_src3) {
        return false;
      }

      uint32_t csrc2 = CN_src2->getZExtValue();
      uint32_t csrc3 = CN_src3->getZExtValue();
      return (csrc3 == (32 - csrc2));
    }]>;

// B.  src2 is a variable

def bitalign_3 : PatFrag<(ops node:$src0, node:$src1, node:$src2),
    (or (shl  node:$src0,
                (bitmask_5bits (sub (i32 0), node:$src2))),
        (srl  node:$src1, (bitmask_5bits node:$src2)))>;

def bitalign_3b : PatFrag<(ops node:$src0, node:$src1, node:$src2),
    (IL_or (shl  node:$src0,
                 (bitmask_5bits (sub (i32 0), node:$src2))),
           (srl  node:$src1, (bitmask_5bits node:$src2)))>;

// TODO: Using FourInOneOut requires four inputs, but bitalign is actually
// three inputs... Need to improve this.
multiclass BitAlignPatFragCI32<ILOpCode opc, PatFrag node> {
  def _i32 : FourInOneOut<opc, (outs GPRI32:$dst),
      (ins GPRI32:$src0, GPRI32:$src1, GPRI32:$src2, GPRI32:$src3),
      !strconcat(opc.Text, " $dst, $src0, $src1, $src2"),
      [(set GPRI32:$dst, (node GPRI32:$src0, GPRI32:$src1, GPRI32:$src2, GPRI32:$src3))]>;
}

multiclass BitAlignPatFragSI32<ILOpCode opc, PatFrag node> {
  def _i32 : ThreeInOneOut<opc, (outs GPRI32:$dst),
      (ins GPRI32:$src0, GPRI32:$src1, GPRI32:$src2),
      !strconcat(opc.Text, " $dst, $src0, $src1, $src2"),
      [(set GPRI32:$dst, (node GPRI32:$src0, GPRI32:$src1, GPRI32:$src2))]>;
}

// Do bitalign pattern recognization if device is EG or later.
let Predicates = [IsEGOrLaterDevice] in {
defm BITALIGN_PAT_1  : BitAlignPatFragCI32<IL_OP_BIT_ALIGN, bitalign_1>;
defm BITALIGN_PAT_1B : BitAlignPatFragCI32<IL_OP_BIT_ALIGN, bitalign_1b>;
defm BITALIGN_PAT_2  : BitAlignPatFragCI32<IL_OP_BIT_ALIGN, bitalign_2>;
defm BITALIGN_PAT_2B : BitAlignPatFragCI32<IL_OP_BIT_ALIGN, bitalign_2b>;
defm BITALIGN_PAT_3  : BitAlignPatFragSI32<IL_OP_BIT_ALIGN, bitalign_3>;
defm BITALIGN_PAT_3B : BitAlignPatFragSI32<IL_OP_BIT_ALIGN, bitalign_3b>;
}

// unpack[0-3] dst, src

def unpack0 : PatFrag<(ops node:$src),
    (uint_to_fp (and node:$src, (i32 0xFF)))>;
def unpack0_1 : PatFrag<(ops node:$src),
    (uint_to_fp (i32 (int_AMDIL_bit_extract_u32 (i32 8), (i32 0), node:$src)))>;
def unpack1 : PatFrag<(ops node:$src),
    (uint_to_fp (and (srl node:$src, (i32 8)), (i32 0xFF)))>;
def unpack1_1 : PatFrag<(ops node:$src),
    (uint_to_fp (i32 (int_AMDIL_bit_extract_u32 (i32 8), (i32 8), node:$src)))>;
def unpack2 : PatFrag<(ops node:$src),
    (uint_to_fp (and (srl node:$src, (i32 16)), (i32 0xFF)))>;
def unpack2_1 : PatFrag<(ops node:$src),
    (uint_to_fp (i32 (int_AMDIL_bit_extract_u32 (i32 8), (i32 16), node:$src)))>;
def unpack3 : PatFrag<(ops node:$src), (uint_to_fp (srl node:$src, (i32 24)))>;
def unpack3_1 : PatFrag<(ops node:$src),
    (uint_to_fp (i32 (int_AMDIL_bit_extract_u32 (i32 8), (i32 24), node:$src)))>;

multiclass UnpackPatFrag<ILOpCode opc, PatFrag node> {
  def _i32 : OneInOneOut<opc, (outs GPRF32:$dst),
      (ins GPRI32:$src),
      !strconcat(opc.Text, " $dst, $src"),
      [(set GPRF32:$dst, (node GPRI32:$src))]>;
}

let Predicates = [IsEGOrLaterDevice] in {
defm UNPACK_PAT0   : UnpackPatFrag<IL_OP_UNPACK_0, unpack0>;
defm UNPACK_PAT0_1 : UnpackPatFrag<IL_OP_UNPACK_0, unpack0_1>;
defm UNPACK_PAT1   : UnpackPatFrag<IL_OP_UNPACK_1, unpack1>;
defm UNPACK_PAT1_1 : UnpackPatFrag<IL_OP_UNPACK_1, unpack1_1>;
defm UNPACK_PAT2   : UnpackPatFrag<IL_OP_UNPACK_2, unpack2>;
defm UNPACK_PAT2_1 : UnpackPatFrag<IL_OP_UNPACK_2, unpack2_1>;
defm UNPACK_PAT3   : UnpackPatFrag<IL_OP_UNPACK_3, unpack3>;
defm UNPACK_PAT3_1 : UnpackPatFrag<IL_OP_UNPACK_3, unpack3_1>;
}

