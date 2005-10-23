#include "VectorC.h"
#include "Intrinsics.h"

// Selects whether to use vllvm_mradds with third argument c0
// (constant 0), or vllvm_mr instead.  Using c0 may be slightly faster
// on AltiVec.  Not using c0 saves an "adds x, 0" op and may be
// slightly faster on architectures (e.g., SSE2) that don't support
// mradds as a single op.  Smarter code generation can make USE_C0 0
// just as fast on AltiVec as USE_C0 1.
//
#define USE_C0 1

// Note that both vllvm_mr and vllvm_mradds are defined in
// "Intrinsics.h" and expand to patterns of more primitive Vector LLVM
// instructions
//
void rgb2yuv_vector(unsigned char *RGB_ptr, int RGB_size,
		    unsigned char *YCC_ptr) {

  signed short  r0, r1, r2, g0, g1, g2, b0, b1, b2, c16, c128;
  unsigned char z0, tc0, tc1, tc2, tc3;
  signed short tr0, tr1, tg0, tg1, tb0, tb1;
  signed short t0, t1, t2, t3, t4, t5;
  int i;

  unsigned char vPerm1 =
    vllvm_constant_unsigned_char( 0,  3,  6,  9, 12, 15, 18, 21,
				  1,  4,  7, 10, 13, 16, 19, 22);
  unsigned char vPerm2 =
    vllvm_constant_unsigned_char( 2,  5,  8, 11, 14, 17, 20, 23,
				  0,  0,  0,  0,  0,  0,  0,  0);
  unsigned char vPerm3 =
    vllvm_constant_unsigned_char( 8, 11, 14, 17, 20, 23, 26, 29,
				  9, 12, 15, 18, 21, 24, 27, 30);
  unsigned char vPerm4 =
    vllvm_constant_unsigned_char(10, 13, 16, 19, 22, 25, 28, 31,
				 0,  0,  0,  0,  0,  0,  0,  0);

  r0 = vllvm_fixed_vimm_short(8432, 8);
  g0 = vllvm_fixed_vimm_short(16425, 8);
  b0 = vllvm_fixed_vimm_short(3176, 8);
  r1 = vllvm_fixed_vimm_short(-4818, 8);
  g1 = vllvm_fixed_vimm_short(-9527, 8);
  b1 = vllvm_fixed_vimm_short(14345, 8);
  r2 = vllvm_fixed_vimm_short(14345, 8);
  g2 = vllvm_fixed_vimm_short(-12045, 8);
  b2 = vllvm_fixed_vimm_short(-2300, 8);
  c16  = vllvm_fixed_vimm_short(16, 8);
  c128 = vllvm_fixed_vimm_short(128, 8);

#if USE_C0
  signed short c0 = vllvm_fixed_vimm_short(0, 8);
#endif

  for ( i = 0; i < (RGB_size/16); i+=3 ) {

    unsigned char v0 = vllvm_load_unsigned_char(RGB_ptr, 16, i);
    unsigned char v1 = vllvm_load_unsigned_char(RGB_ptr, 16, i+1);
    unsigned char v2 = vllvm_load_unsigned_char(RGB_ptr, 16, i+2);

    char tmp = vllvm_fixed_vimm_char(0, 32);
    tmp = vllvm_fixed_combine_unsigned_char(tmp, 32, v0, 16, 0, 1);
    tmp = vllvm_fixed_combine_unsigned_char(tmp, 32, v1, 16, 16, 1);
    tc0 = vllvm_fixed_permute_unsigned_char(tmp, 32, vPerm1, 16);

    char tmp1 = vllvm_fixed_vimm_char(0, 32);
    tmp1 = vllvm_fixed_combine_unsigned_char(tmp1, 32, v0, 16, 0, 1);
    tmp1 = vllvm_fixed_combine_unsigned_char(tmp1, 32, v1, 16, 16, 1);
    tc1 = vllvm_fixed_permute_unsigned_char(tmp1, 32, vPerm2, 16);

    char tmp2 = vllvm_fixed_vimm_char(0, 32);
    tmp2 = vllvm_fixed_combine_unsigned_char(tmp2, 32, v1, 16, 0, 1);
    tmp2 = vllvm_fixed_combine_unsigned_char(tmp2, 32, v2, 16, 16, 1);
    tc2 = vllvm_fixed_permute_unsigned_char(tmp2, 32, vPerm3, 16);

    char tmp3 = vllvm_fixed_vimm_char(0, 32);
    tmp3 = vllvm_fixed_combine_unsigned_char(tmp3, 32, v1, 16, 0, 1);
    tmp3 = vllvm_fixed_combine_unsigned_char(tmp3, 32, v2, 16, 16, 1);
    tc3 = vllvm_fixed_permute_unsigned_char(tmp3, 32, vPerm4, 16);

    tr0 = _extract_unsigned_char(tc0, 0, 1, 8);
    tg0 = _extract_unsigned_char(tc0, 8, 1, 8);
    tb0 = _extract_unsigned_char(tc1, 0, 1, 8);
    tr1 = _extract_unsigned_char(tc2, 0, 1, 8);
    tg1 = _extract_unsigned_char(tc2, 8, 1, 8);
    tb1 = _extract_unsigned_char(tc3, 0, 1, 8);

#if USE_C0
    t0 = vllvm_mradds_short( tr0, r0, c0 );
    t1 = vllvm_mradds_short( tr0, r1, c0 );
    t2 = vllvm_mradds_short( tr0, r2, c0 );
#else
    t0 = vllvm_mr_short( tr0, r0 );
    t1 = vllvm_mr_short( tr0, r1 );
    t2 = vllvm_mr_short( tr0, r2 );
#endif

    t0 = vllvm_mradds_short( tg0, g0, t0 );
    t1 = vllvm_mradds_short( tg0, g1, t1 );
    t2 = vllvm_mradds_short( tg0, g2, t2 );

    t0 = vllvm_mradds_short( tb0, b0, t0 );
    t1 = vllvm_mradds_short( tb0, b1, t1 );
    t2 = vllvm_mradds_short( tb0, b2, t2 );

#if USE_C0
    t3 = vllvm_mradds_short( tr1, r0, c0 );
    t4 = vllvm_mradds_short( tr1, r1, c0 );
    t5 = vllvm_mradds_short( tr1, r2, c0 );
#else
    t3 = vllvm_mr_short( tr1, r0 );
    t4 = vllvm_mr_short( tr1, r1 );
    t5 = vllvm_mr_short( tr1, r2 );
#endif

    t3 = vllvm_mradds_short( tg1, g0, t3 );
    t4 = vllvm_mradds_short( tg1, g1, t4 );
    t5 = vllvm_mradds_short( tg1, g2, t5 );

    t3 = vllvm_mradds_short( tb1, b0, t3 );
    t4 = vllvm_mradds_short( tb1, b1, t4 );
    t5 = vllvm_mradds_short( tb1, b2, t5 );

    t0 = vllvm_adds_short( t0, c16 );
    t3 = vllvm_adds_short( t3, c16 );
    t1 = vllvm_adds_short( t1, c128 );
    t4 = vllvm_adds_short( t4, c128 );
    t2 = vllvm_adds_short( t2, c128 );
    t5 = vllvm_adds_short( t5, c128 );

    short out0 = vllvm_fixed_vimm_short(0, 16);
    short out1 = vllvm_fixed_combine_short(out0, 16, t0, 8, 0, 1);
    short out2 = vllvm_fixed_combine_short(out1, 16, t3, 8, 8, 1);
    unsigned char out3 = vllvm_saturate_short_uchar(out2);
    vllvm_store_unsigned_char(out3, YCC_ptr, i);
    out1 = vllvm_fixed_combine_short(out0, 16, t1, 8, 0, 1);
    out2 = vllvm_fixed_combine_short(out1, 16, t4, 8, 8, 1);
    out3 = vllvm_saturate_short_uchar(out2);
    vllvm_store_unsigned_char(out3, YCC_ptr, i+1);
    out1 = vllvm_fixed_combine_short(out0, 16, t2, 8, 0, 1);
    out2 = vllvm_fixed_combine_short(out1, 16, t5, 8, 8, 1);
    out3 = vllvm_saturate_short_uchar(out2);
    vllvm_store_unsigned_char(out3, YCC_ptr, i+2);

  }
}

