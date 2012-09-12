//===-- AMDILConversions.td -----------------------------------------------===//
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

def actosi16rr:Pat < (i16 (anyext GPRI8:$src)),
(IL_ASSHORTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def uctosi16rr:Pat < (i16 (zext GPRI8:$src)),
(IL_ASSHORTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def sctosi16rr:Pat < (i16 (sext GPRI8:$src)),
(IL_ASSHORTi32r
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def actoii32rr:Pat < (i32 (anyext GPRI8:$src)),
(IL_ASINTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def uctoii32rr:Pat < (i32 (zext GPRI8:$src)),
(IL_ASINTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def sctoii32rr:Pat < (i32 (sext GPRI8:$src)),
(IL_ASINTi32r
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))) >;


def actoli64rr:Pat < (i64 (anyext GPRI8:$src)),
(LCREATEi64rr
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24)),
 (LOADCONSTi32 0)) >;


def uctoli64rr:Pat < (i64 (zext GPRI8:$src)),
(LCREATEi64rr
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24)),
 (LOADCONSTi32 0)) >;


def sctoli64rr:Pat < (i64 (sext GPRI8:$src)),
(LCREATEi64rr
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24)),
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 31))) >;


def astoii32rr:Pat < (i32 (anyext GPRI16:$src)),
(IL_ASINTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16))) >;


def ustoii32rr:Pat < (i32 (zext GPRI16:$src)),
(IL_ASINTi32r
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16))) >;


def sstoii32rr:Pat < (i32 (sext GPRI16:$src)),
(IL_ASINTi32r
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16))) >;


def astoli64rr:Pat < (i64 (anyext GPRI16:$src)),
(LCREATEi64rr
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16)),
 (LOADCONSTi32 0)) >;


def ustoli64rr:Pat < (i64 (zext GPRI16:$src)),
(LCREATEi64rr
 (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16)),
 (LOADCONSTi32 0)) >;


def sstoli64rr:Pat < (i64 (sext GPRI16:$src)),
(LCREATEi64rr
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16)),
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 31))) >;


def aitoli64rr:Pat < (i64 (anyext GPRI32:$src)),
(LCREATEi64rr
(IL_ASINTi32r GPRI32:$src),
 (LOADCONSTi32 0)) >;


def uitoli64rr:Pat < (i64 (zext GPRI32:$src)),
(LCREATEi64rr
(IL_ASINTi32r GPRI32:$src),
 (LOADCONSTi32 0)) >;


def sitoli64rr:Pat < (i64 (sext GPRI32:$src)),
(LCREATEi64rr
(IL_ASINTi32r GPRI32:$src),
 (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi32r GPRI32:$src),
   (LOADCONSTi32 0)),
  (LOADCONSTi32 31))) >;



def sctoff32rr:Pat < (f32 (sint_to_fp GPRI8:$src)),
(f32
 (ITOFf32r
  (SHRi32i32rr
   (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
    (LOADCONSTi32 24)),
   (LOADCONSTi32 24)))) >;


def uctoff32rr:Pat < (f32 (uint_to_fp GPRI8:$src)),
(f32
 (UTOFf32r
  (USHRi32i32rr
   (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
    (LOADCONSTi32 24)),
   (LOADCONSTi32 24)))) >;


def ftosci8rr:Pat < (i8 (fp_to_sint GPRF32:$src)),
(i8
 (IL_ASCHARi32r
  (ANDi32rr
(FTOIi32r GPRF32:$src),
   (LOADCONSTi32 0x000000FF)))) >;


def ftouci8rr:Pat < (i8 (fp_to_uint GPRF32:$src)),
(i8
 (IL_ASCHARi32r
  (ANDi32rr
(FTOUi32r GPRF32:$src),
   (LOADCONSTi32 0x000000FF)))) >;


def sctodf64rr:Pat < (f64 (sint_to_fp GPRI8:$src)),
(f64 (FTODr
      (ITOFf32r
       (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))))) >;


def uctodf64rr:Pat < (f64 (uint_to_fp GPRI8:$src)),
(f64 (FTODr
      (UTOFf32r
       (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi8r GPRI8:$src),
   (LOADCONSTi32 24)),
  (LOADCONSTi32 24))))) >;


def dtosci8rr:Pat < (i8 (fp_to_sint GPRF64:$src)),
(i8
 (IL_ASCHARi32r
  (ANDi32rr
(FTOIi32r (DTOFr GPRF64:$src)),
   (LOADCONSTi32 0x000000FF)))) >;


def dtouci8rr:Pat < (i8 (fp_to_uint GPRF64:$src)),
(i8
 (IL_ASCHARi32r
  (ANDi32rr
(FTOUi32r (DTOFr GPRF64:$src)),
   (LOADCONSTi32 0x000000FF)))) >;


def sstoff32rr:Pat < (f32 (sint_to_fp GPRI16:$src)),
(f32
 (ITOFf32r
  (SHRi32i32rr
   (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
    (LOADCONSTi32 16)),
   (LOADCONSTi32 16)))) >;


def ustoff32rr:Pat < (f32 (uint_to_fp GPRI16:$src)),
(f32
 (UTOFf32r
  (USHRi32i32rr
   (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
    (LOADCONSTi32 16)),
   (LOADCONSTi32 16)))) >;


def ftossi16rr:Pat < (i16 (fp_to_sint GPRF32:$src)),
(i16
 (IL_ASSHORTi32r
  (ANDi32rr
(FTOIi32r GPRF32:$src),
   (LOADCONSTi32 0x0000FFFF)))) >;


def ftousi16rr:Pat < (i16 (fp_to_uint GPRF32:$src)),
(i16
 (IL_ASSHORTi32r
  (ANDi32rr
(FTOUi32r GPRF32:$src),
   (LOADCONSTi32 0x0000FFFF)))) >;


def sstodf64rr:Pat < (f64 (sint_to_fp GPRI16:$src)),
(f64 (FTODr
      (ITOFf32r
       (SHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16))))) >;


def ustodf64rr:Pat < (f64 (uint_to_fp GPRI16:$src)),
(f64 (FTODr
      (UTOFf32r
       (USHRi32i32rr
  (SHLi32i32rr
(IL_ASINTi16r GPRI16:$src),
   (LOADCONSTi32 16)),
  (LOADCONSTi32 16))))) >;


def dtossi16rr:Pat < (i16 (fp_to_sint GPRF64:$src)),
(i16
 (IL_ASSHORTi32r
  (ANDi32rr
(FTOIi32r (DTOFr GPRF64:$src)),
   (LOADCONSTi32 0x0000FFFF)))) >;


def dtousi16rr:Pat < (i16 (fp_to_uint GPRF64:$src)),
(i16
 (IL_ASSHORTi32r
  (ANDi32rr
(FTOUi32r (DTOFr GPRF64:$src)),
   (LOADCONSTi32 0x0000FFFF)))) >;





def stoci8rr:Pat < (i8 (trunc GPRI16:$src)),
(IL_ASCHARi32r
 (SHRi32i32rr
 (SHLi32i32rr
   (IL_ASINTi16r
(ANDi16rr GPRI16:$src,
     (LOADCONSTi16 0x000000FF))),
 (LOADCONSTi32 16)),
 (LOADCONSTi32 16))
 )>;


def itoci8rr:Pat < (i8 (trunc GPRI32:$src)),
(IL_ASCHARi32r
 (SHRi32i32rr
 (SHLi32i32rr
   (IL_ASINTi32r
(ANDi32rr GPRI32:$src,
     (LOADCONSTi32 0x000000FF)))
 , (LOADCONSTi32 24)),
 (LOADCONSTi32 24))
  ) >;


def itosi16rr:Pat < (i16 (trunc GPRI32:$src)),
(IL_ASSHORTi32r
 (SHRi32i32rr
 (SHLi32i32rr
   (IL_ASINTi32r
(ANDi32rr GPRI32:$src,
     (LOADCONSTi32 0x0000FFFF)))
 , (LOADCONSTi32 16)),
 (LOADCONSTi32 16))
  ) >;


def ltoci8rr:Pat < (i8 (trunc GPRI64:$src)),
(IL_ASCHARi32r
 (SHRi32i32rr
 (SHLi32i32rr
   (ANDi32rr
(LLOi64r GPRI64:$src),
    (LOADCONSTi32 0x000000FF))
 , (LOADCONSTi32 24)),
 (LOADCONSTi32 24))
  ) >;


def ltosi16rr:Pat < (i16 (trunc GPRI64:$src)),
(IL_ASSHORTi32r
 (SHRi32i32rr
 (SHLi32i32rr
   (ANDi32rr
(LLOi64r GPRI64:$src),
    (LOADCONSTi32 0x0000FFFF))
 , (LOADCONSTi32 16)),
 (LOADCONSTi32 16))
  ) >;


def ltoii32rr:Pat < (i32 (trunc GPRI64:$src)), (LLOi64r GPRI64:$src) >;

def actosv2i16rr:Pat < (v2i16 (anyext GPRV2I8:$src)),
(IL_ASV2SHORTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def uctosv2i16rr:Pat < (v2i16 (zext GPRV2I8:$src)),
(IL_ASV2SHORTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def sctosv2i16rr:Pat < (v2i16 (sext GPRV2I8:$src)),
(IL_ASV2SHORTv2i32r
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def actoiv2i32rr:Pat < (v2i32 (anyext GPRV2I8:$src)),
(IL_ASV2INTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def uctoiv2i32rr:Pat < (v2i32 (zext GPRV2I8:$src)),
(IL_ASV2INTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def sctoiv2i32rr:Pat < (v2i32 (sext GPRV2I8:$src)),
(IL_ASV2INTv2i32r
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24)))) >;


def actolv2i64rr:Pat < (v2i64 (anyext GPRV2I8:$src)),
(LCREATEv2i64rr
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24))),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def uctolv2i64rr:Pat < (v2i64 (zext GPRV2I8:$src)),
(LCREATEv2i64rr
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24))),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def sctolv2i64rr:Pat < (v2i64 (sext GPRV2I8:$src)),
(LCREATEv2i64rr
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 24))),
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
   (VCREATEv2i32r (LOADCONSTi32 24))),
  (VCREATEv2i32r (LOADCONSTi32 31)))) >;


def astoiv2i32rr:Pat < (v2i32 (anyext GPRV2I16:$src)),
(IL_ASV2INTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16)))) >;


def ustoiv2i32rr:Pat < (v2i32 (zext GPRV2I16:$src)),
(IL_ASV2INTv2i32r
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16)))) >;


def sstoiv2i32rr:Pat < (v2i32 (sext GPRV2I16:$src)),
(IL_ASV2INTv2i32r
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16)))) >;


def astolv2i64rr:Pat < (v2i64 (anyext GPRV2I16:$src)),
(LCREATEv2i64rr
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16))),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def ustolv2i64rr:Pat < (v2i64 (zext GPRV2I16:$src)),
(LCREATEv2i64rr
 (USHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16))),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def sstolv2i64rr:Pat < (v2i64 (sext GPRV2I16:$src)),
(LCREATEv2i64rr
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 16))),
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
   (VCREATEv2i32r (LOADCONSTi32 16))),
  (VCREATEv2i32r (LOADCONSTi32 31)))) >;


def aitolv2i64rr:Pat < (v2i64 (anyext GPRV2I32:$src)),
(LCREATEv2i64rr
(IL_ASV2INTv2i32r GPRV2I32:$src),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def uitolv2i64rr:Pat < (v2i64 (zext GPRV2I32:$src)),
(LCREATEv2i64rr
(IL_ASV2INTv2i32r GPRV2I32:$src),
 (VCREATEv2i32r (LOADCONSTi32 0))) >;


def sitolv2i64rr:Pat < (v2i64 (sext GPRV2I32:$src)),
(LCREATEv2i64rr
(IL_ASV2INTv2i32r GPRV2I32:$src),
 (SHRv2i32i32rr
  (SHLv2i32i32rr
(IL_ASV2INTv2i32r GPRV2I32:$src),
   (VCREATEv2i32r (LOADCONSTi32 0))),
  (VCREATEv2i32r (LOADCONSTi32 31)))) >;



def sctofv2f32rr:Pat < (v2f32 (sint_to_fp GPRV2I8:$src)),
(v2f32
 (ITOFv2f32r
  (SHRv2i32i32rr
   (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
    (VCREATEv2i32r (LOADCONSTi32 24))),
   (VCREATEv2i32r (LOADCONSTi32 24))))) >;


def uctofv2f32rr:Pat < (v2f32 (uint_to_fp GPRV2I8:$src)),
(v2f32
 (UTOFv2f32r
  (USHRv2i32i32rr
   (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
    (VCREATEv2i32r (LOADCONSTi32 24))),
   (VCREATEv2i32r (LOADCONSTi32 24))))) >;


def ftoscv2i8rr:Pat < (v2i8 (fp_to_sint GPRV2F32:$src)),
(v2i8
 (IL_ASV2CHARv2i32r
  (ANDv2i32rr
(FTOIv2i32r GPRV2F32:$src),
   (VCREATEv2i32r (LOADCONSTi32 0x000000FF))))) >;


def ftoucv2i8rr:Pat < (v2i8 (fp_to_uint GPRV2F32:$src)),
(v2i8
 (IL_ASV2CHARv2i32r
  (ANDv2i32rr
(FTOUv2i32r GPRV2F32:$src),
   (VCREATEv2i32r (LOADCONSTi32 0x000000FF))))) >;

def sctodv2f64rr:Pat < (v2f64 (sint_to_fp GPRV2I8:$src)),
(v2f64 
 (VINSERTv2f64rr
 (VCREATEv2f64r 
 (FTODr
  (VEXTRACTv2f32r
  (ITOFv2f32r
   (SHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
     (VCREATEv2i32r (LOADCONSTi32 24))),
    (VCREATEv2i32r (LOADCONSTi32 24)))),
  1)
 )),
 (FTODr
  (VEXTRACTv2f32r
  (ITOFv2f32r
   (SHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
     (VCREATEv2i32r (LOADCONSTi32 24))),
    (VCREATEv2i32r (LOADCONSTi32 24)))),
  2)
  ), 1, 256)
 ) >;

def uctodv2f64rr:Pat < (v2f64 (uint_to_fp GPRV2I8:$src)),
(v2f64 
 (VINSERTv2f64rr
 (VCREATEv2f64r 
 (FTODr
  (VEXTRACTv2f32r
  (UTOFv2f32r
   (USHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
     (VCREATEv2i32r (LOADCONSTi32 24))),
    (VCREATEv2i32r (LOADCONSTi32 24)))),
  1)
 )),
 (FTODr
  (VEXTRACTv2f32r
  (UTOFv2f32r
   (USHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i8r GPRV2I8:$src),
     (VCREATEv2i32r (LOADCONSTi32 24))),
    (VCREATEv2i32r (LOADCONSTi32 24)))),
  2)
  ), 1, 256)
 ) >;


def dtoscv2i8rr:Pat < (v2i8 (fp_to_sint GPRV2F64:$src)),
(v2i8
 (IL_ASV2CHARv2i32r
  (ANDv2i32rr
(FTOIv2i32r (VINSERTv2f32rr 
             (VCREATEv2f32r 
              (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 1))),
             (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 2)), 1, 256)),
   (VCREATEv2i32r (LOADCONSTi32 0x000000FF))))) >;


def dtoucv2i8rr:Pat < (v2i8 (fp_to_uint GPRV2F64:$src)),
(v2i8
 (IL_ASV2CHARv2i32r
  (ANDv2i32rr
(FTOUv2i32r (VINSERTv2f32rr 
             (VCREATEv2f32r 
              (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 1))),
             (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 2)), 1, 256)),
   (VCREATEv2i32r (LOADCONSTi32 0x000000FF))))) >;


def sstofv2f32rr:Pat < (v2f32 (sint_to_fp GPRV2I16:$src)),
(v2f32
 (ITOFv2f32r
  (SHRv2i32i32rr
   (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
    (VCREATEv2i32r (LOADCONSTi32 16))),
   (VCREATEv2i32r (LOADCONSTi32 16))))) >;


def ustofv2f32rr:Pat < (v2f32 (uint_to_fp GPRV2I16:$src)),
(v2f32
 (UTOFv2f32r
  (USHRv2i32i32rr
   (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
    (VCREATEv2i32r (LOADCONSTi32 16))),
   (VCREATEv2i32r (LOADCONSTi32 16))))) >;


def ftossv2i16rr:Pat < (v2i16 (fp_to_sint GPRV2F32:$src)),
(v2i16
 (IL_ASV2SHORTv2i32r
  (ANDv2i32rr
(FTOIv2i32r GPRV2F32:$src),
   (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF))))) >;


def ftousv2i16rr:Pat < (v2i16 (fp_to_uint GPRV2F32:$src)),
(v2i16
 (IL_ASV2SHORTv2i32r
  (ANDv2i32rr
(FTOUv2i32r GPRV2F32:$src),
   (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF))))) >;


def sstodv2f64rr:Pat < (v2f64 (sint_to_fp GPRV2I16:$src)),
(v2f64 
 (VINSERTv2f64rr
 (VCREATEv2f64r 
 (FTODr
  (VEXTRACTv2f32r
  (ITOFv2f32r
   (SHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
     (VCREATEv2i32r (LOADCONSTi32 16))),
    (VCREATEv2i32r (LOADCONSTi32 16)))),
  1)
 )),
 (FTODr
  (VEXTRACTv2f32r
  (ITOFv2f32r
   (SHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
     (VCREATEv2i32r (LOADCONSTi32 16))),
    (VCREATEv2i32r (LOADCONSTi32 16)))),
  2)
  ), 1, 256)
 ) >;

def ustodv2f64rr:Pat < (v2f64 (uint_to_fp GPRV2I16:$src)),
(v2f64 
 (VINSERTv2f64rr
 (VCREATEv2f64r 
 (FTODr
  (VEXTRACTv2f32r
  (UTOFv2f32r
   (USHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
     (VCREATEv2i32r (LOADCONSTi32 16))),
    (VCREATEv2i32r (LOADCONSTi32 16)))),
  1)
 )),
 (FTODr
  (VEXTRACTv2f32r
  (UTOFv2f32r
   (USHRv2i32i32rr
    (SHLv2i32i32rr
(IL_ASV2INTv2i16r GPRV2I16:$src),
     (VCREATEv2i32r (LOADCONSTi32 16))),
    (VCREATEv2i32r (LOADCONSTi32 16)))),
  2)
  ), 1, 256)
 ) >;


def dtossv2i16rr:Pat < (v2i16 (fp_to_sint GPRV2F64:$src)),
(v2i16
 (IL_ASV2SHORTv2i32r
  (ANDv2i32rr
(FTOIv2i32r (VINSERTv2f32rr 
             (VCREATEv2f32r 
              (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 1))),
             (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 2)), 1, 256)),
   (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF))))) >;


def dtousv2i16rr:Pat < (v2i16 (fp_to_uint GPRV2F64:$src)),
(v2i16
 (IL_ASV2SHORTv2i32r
  (ANDv2i32rr
(FTOUv2i32r (VINSERTv2f32rr 
             (VCREATEv2f32r 
              (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 1))),
             (DTOFr (VEXTRACTv2f64r GPRV2F64:$src, 2)), 1, 256)),
   (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF))))) >;

def stocv2i8rr:Pat < (v2i8 (trunc GPRV2I16:$src)),
(IL_ASV2CHARv2i32r
 (SHRv2i32i32rr
 (SHLv2i32i32rr
   (IL_ASV2INTv2i16r
(ANDv2i16rr GPRV2I16:$src,
     (VCREATEv2i16r (LOADCONSTi16 0x000000FF))))
   , (VCREATEv2i32r (LOADCONSTi32 24))),
 (VCREATEv2i32r (LOADCONSTi32 24)))
  ) >;


def itocv2i8rr:Pat < (v2i8 (trunc GPRV2I32:$src)),
(IL_ASV2CHARv2i32r
 (SHRv2i32i32rr
 (SHLv2i32i32rr
   (IL_ASV2INTv2i32r
(ANDv2i32rr GPRV2I32:$src,
     (VCREATEv2i32r (LOADCONSTi32 0x000000FF))))
   , (VCREATEv2i32r (LOADCONSTi32 24))),
 (VCREATEv2i32r (LOADCONSTi32 24)))
  ) >;


def itosv2i16rr:Pat < (v2i16 (trunc GPRV2I32:$src)),
(IL_ASV2SHORTv2i32r
 (SHRv2i32i32rr
 (SHLv2i32i32rr
   (IL_ASV2INTv2i32r
(ANDv2i32rr GPRV2I32:$src,
     (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF))))
   , (VCREATEv2i32r (LOADCONSTi32 16))),
 (VCREATEv2i32r (LOADCONSTi32 16)))
  ) >;


def ltocv2i8rr:Pat < (v2i8 (trunc GPRV2I64:$src)),
(IL_ASV2CHARv2i32r
 (SHRv2i32i32rr
 (SHLv2i32i32rr
   (ANDv2i32rr
(LLOv2i64r GPRV2I64:$src),
    (VCREATEv2i32r (LOADCONSTi32 0x000000FF)))
   , (VCREATEv2i32r (LOADCONSTi32 24))),
 (VCREATEv2i32r (LOADCONSTi32 24)))
  ) >;


def ltosv2i16rr:Pat < (v2i16 (trunc GPRV2I64:$src)),
(IL_ASV2SHORTv2i32r
 (SHRv2i32i32rr
 (SHLv2i32i32rr
   (ANDv2i32rr
(LLOv2i64r GPRV2I64:$src),
    (VCREATEv2i32r (LOADCONSTi32 0x0000FFFF)))
   , (VCREATEv2i32r (LOADCONSTi32 16))),
 (VCREATEv2i32r (LOADCONSTi32 16)))
  ) >;


def ltoiv2i32rr:Pat < (v2i32 (trunc GPRV2I64:$src)), (LLOv2i64r GPRV2I64:$src)>;



def actosv4i16rr:Pat < (v4i16 (anyext GPRV4I8:$src)),
(IL_ASV4SHORTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def uctosv4i16rr:Pat < (v4i16 (zext GPRV4I8:$src)),
(IL_ASV4SHORTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def sctosv4i16rr:Pat < (v4i16 (sext GPRV4I8:$src)),
(IL_ASV4SHORTv4i32r
 (SHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def actoiv4i32rr:Pat < (v4i32 (anyext GPRV4I8:$src)),
(IL_ASV4INTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def uctoiv4i32rr:Pat < (v4i32 (zext GPRV4I8:$src)),
(IL_ASV4INTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def sctoiv4i32rr:Pat < (v4i32 (sext GPRV4I8:$src)),
(IL_ASV4INTv4i32r
 (SHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
   (VCREATEv4i32r (LOADCONSTi32 24))),
  (VCREATEv4i32r (LOADCONSTi32 24)))) >;


def astoiv4i32rr:Pat < (v4i32 (anyext GPRV4I16:$src)),
(IL_ASV4INTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i16r GPRV4I16:$src),
   (VCREATEv4i32r (LOADCONSTi32 16))),
  (VCREATEv4i32r (LOADCONSTi32 16)))) >;


def ustoiv4i32rr:Pat < (v4i32 (zext GPRV4I16:$src)),
(IL_ASV4INTv4i32r
 (USHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i16r GPRV4I16:$src),
   (VCREATEv4i32r (LOADCONSTi32 16))),
  (VCREATEv4i32r (LOADCONSTi32 16)))) >;


def sstoiv4i32rr:Pat < (v4i32 (sext GPRV4I16:$src)),
(IL_ASV4INTv4i32r
 (SHRv4i32i32rr
  (SHLv4i32i32rr
(IL_ASV4INTv4i16r GPRV4I16:$src),
   (VCREATEv4i32r (LOADCONSTi32 16))),
  (VCREATEv4i32r (LOADCONSTi32 16)))) >;



def sctofv4f32rr:Pat < (v4f32 (sint_to_fp GPRV4I8:$src)),
(v4f32
 (ITOFv4f32r
  (SHRv4i32i32rr
   (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
    (VCREATEv4i32r (LOADCONSTi32 24))),
   (VCREATEv4i32r (LOADCONSTi32 24))))) >;


def uctofv4f32rr:Pat < (v4f32 (uint_to_fp GPRV4I8:$src)),
(v4f32
 (UTOFv4f32r
  (USHRv4i32i32rr
   (SHLv4i32i32rr
(IL_ASV4INTv4i8r GPRV4I8:$src),
    (VCREATEv4i32r (LOADCONSTi32 24))),
   (VCREATEv4i32r (LOADCONSTi32 24))))) >;


def ftoscv4i8rr:Pat < (v4i8 (fp_to_sint GPRV4F32:$src)),
(v4i8
 (IL_ASV4CHARv4i32r
  (ANDv4i32rr
(FTOIv4i32r GPRV4F32:$src),
   (VCREATEv4i32r (LOADCONSTi32 0x000000FF))))) >;


def ftoucv4i8rr:Pat < (v4i8 (fp_to_uint GPRV4F32:$src)),
(v4i8
 (IL_ASV4CHARv4i32r
  (ANDv4i32rr
(FTOUv4i32r GPRV4F32:$src),
   (VCREATEv4i32r (LOADCONSTi32 0x000000FF))))) >;


def sstofv4f32rr:Pat < (v4f32 (sint_to_fp GPRV4I16:$src)),
(v4f32
 (ITOFv4f32r
  (SHRv4i32i32rr
   (SHLv4i32i32rr
(IL_ASV4INTv4i16r GPRV4I16:$src),
    (VCREATEv4i32r (LOADCONSTi32 16))),
   (VCREATEv4i32r (LOADCONSTi32 16))))) >;


def ustofv4f32rr:Pat < (v4f32 (uint_to_fp GPRV4I16:$src)),
(v4f32
 (UTOFv4f32r
  (USHRv4i32i32rr
   (SHLv4i32i32rr
(IL_ASV4INTv4i16r GPRV4I16:$src),
    (VCREATEv4i32r (LOADCONSTi32 16))),
   (VCREATEv4i32r (LOADCONSTi32 16))))) >;


def ftossv4i16rr:Pat < (v4i16 (fp_to_sint GPRV4F32:$src)),
(v4i16
 (IL_ASV4SHORTv4i32r
  (ANDv4i32rr
(FTOIv4i32r GPRV4F32:$src),
   (VCREATEv4i32r (LOADCONSTi32 0x0000FFFF))))) >;


def ftousv4i16rr:Pat < (v4i16 (fp_to_uint GPRV4F32:$src)),
(v4i16
 (IL_ASV4SHORTv4i32r
  (ANDv4i32rr
(FTOUv4i32r GPRV4F32:$src),
   (VCREATEv4i32r (LOADCONSTi32 0x0000FFFF))))) >;





def stocv4i8rr:Pat < (v4i8 (trunc GPRV4I16:$src)),
(IL_ASV4CHARv4i32r
 (SHRv4i32i32rr
 (SHLv4i32i32rr
   (IL_ASV4INTv4i16r
(ANDv4i16rr GPRV4I16:$src,
     (VCREATEv4i16r (LOADCONSTi16 0x000000FF))))
   , (VCREATEv4i32r (LOADCONSTi32 24))),
 (VCREATEv4i32r (LOADCONSTi32 24)))
  ) >;


def itocv4i8rr:Pat < (v4i8 (trunc GPRV4I32:$src)),
(IL_ASV4CHARv4i32r
 (SHRv4i32i32rr
 (SHLv4i32i32rr
   (IL_ASV4INTv4i32r
(ANDv4i32rr GPRV4I32:$src,
     (VCREATEv4i32r (LOADCONSTi32 0x000000FF))))
   , (VCREATEv4i32r (LOADCONSTi32 24))),
 (VCREATEv4i32r (LOADCONSTi32 24)))
  ) >;


def itosv4i16rr:Pat < (v4i16 (trunc GPRV4I32:$src)),
(IL_ASV4SHORTv4i32r
 (SHRv4i32i32rr
 (SHLv4i32i32rr
   (IL_ASV4INTv4i32r
(ANDv4i32rr GPRV4I32:$src,
     (VCREATEv4i32r (LOADCONSTi32 0x0000FFFF))))
   , (VCREATEv4i32r (LOADCONSTi32 16))),
 (VCREATEv4i32r (LOADCONSTi32 16)))
  ) >;


