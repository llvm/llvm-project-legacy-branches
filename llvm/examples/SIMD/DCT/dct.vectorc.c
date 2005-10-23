#include "Scalar.h"
#include "VectorC.h"
#include "Intrinsics.h"

// See the rgb2yuv benchmark for a description of USE_C0.  For some
// reason, USE_C0 1 seems to be slightly *faster* on SSE!  I'm
// investigating why this is.
//
#define USE_C0 1

short vllvm_adds_short(short,short);

#define MERGE(out01, out0, out1, in0, in1) \
  short out01 = vllvm_fixed_vimm_short(0, 16); \
  out01 = _fixed_combine_short(out01, 16, in0, 8, 0, 1); \
  out01 = _fixed_combine_short(out01, 16, in1, 8, 8, 1); \
  short out0 = _extract_short(out01, 0, 2, 8); \
  short out1 = _extract_short(out01, 1, 2, 8)

#define IN(x) \
  vllvm_load_short(input_scalar, 8, x)

#define STORE(out, idx) \
  vllvm_store_short(out, output_scalar, idx)

static inline void Matrix_Transpose_VectorC (short *input_scalar, short *output_scalar) {
  MERGE(b01, b0, b1, IN(0), IN(4));
  MERGE(b23, b2, b3, IN(1), IN(5));
  MERGE(b45, b4, b5, IN(2), IN(6));
  MERGE(b67, b6, b7, IN(3), IN(7));

  MERGE(a01, a0, a1, b0, b4);
  MERGE(a23, a2, a3, b1, b5);
  MERGE(a45, a4, a5, b2, b6);
  MERGE(a67, a6, a7, b3, b7);

  MERGE(out01, out0, out1, a0, a4);
  MERGE(out23, out2, out3, a1, a5);
  MERGE(out45, out4, out5, a2, a6);
  MERGE(out67, out6, out7, a3, a7);

  STORE(out0, 0);
  STORE(out1, 1);
  STORE(out2, 2);
  STORE(out3, 3);
  STORE(out4, 4);
  STORE(out5, 5);
  STORE(out6, 6);
  STORE(out7, 7);
}

static inline void DCT_Transform_VectorC ( short *x, short *y) {
  signed short t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;

  t8 = vllvm_adds_short(vllvm_load_short(x, 8, 0), vllvm_load_short(x, 8, 7));
  t9 = vllvm_subs_short(vllvm_load_short(x, 8, 0), vllvm_load_short(x, 8, 7));
  t0 = vllvm_adds_short(vllvm_load_short(x, 8, 1), vllvm_load_short(x, 8, 6));
  t7 = vllvm_subs_short(vllvm_load_short(x, 8, 1), vllvm_load_short(x, 8, 6));
  t1 = vllvm_adds_short(vllvm_load_short(x, 8, 2), vllvm_load_short(x, 8, 5));
  t6 = vllvm_subs_short(vllvm_load_short(x, 8, 2), vllvm_load_short(x, 8, 5));
  t2 = vllvm_adds_short(vllvm_load_short(x, 8, 3), vllvm_load_short(x, 8, 4));
  t5 = vllvm_subs_short(vllvm_load_short(x, 8, 3), vllvm_load_short(x, 8, 4));

  t3 = vllvm_adds_short(t8, t2);
  t4 = vllvm_subs_short(t8, t2);
  t2 = vllvm_adds_short(t0, t1);
  t8 = vllvm_subs_short(t0, t1);

  t1 = vllvm_adds_short(t7, t6);
  t0 = vllvm_subs_short(t7, t6);

  vllvm_store_short(vllvm_adds_short(t3, t2), y, 0);
  vllvm_store_short(vllvm_subs_short(t3, t2), y, 4);

  short c13573 = vllvm_fixed_vimm_short(13573, 8);
#if USE_C0
  short c0 = vllvm_fixed_vimm_short(0, 8);
#endif
  short c23170 = vllvm_fixed_vimm_short(23170, 8);
  short cneg23170 = vllvm_fixed_vimm_short(-23170, 8);
  short c6518 = vllvm_fixed_vimm_short(6518, 8);

  vllvm_store_short(vllvm_mradds_short(t8, c13573, t4), y, 2);
#if USE_C0
  t10 = vllvm_mradds_short(t4, c13573, c0);
#else
  t10 = vllvm_mr_short(t4, c13573);
#endif
  vllvm_store_short(vllvm_subs_short(t10, t8), y, 6);

  t6 = vllvm_mradds_short(t0, c23170, t5);
  t7 = vllvm_mradds_short(t0, cneg23170, t5);
  t2 = vllvm_mradds_short(t1, cneg23170, t9);
  t3 = vllvm_mradds_short(t1, c23170, t9);

  vllvm_store_short(vllvm_mradds_short(t6, c6518, t3), y, 1);
#if USE_C0
  t9 = vllvm_mradds_short(t3, c6518, c0);
#else
  t9 = vllvm_mr_short(t3, c6518);
#endif
  vllvm_store_short(vllvm_subs_short(t9, t6), y, 7);
  vllvm_store_short(vllvm_mradds_short(t2, vllvm_fixed_vimm_short(21895, 8), t7), y, 5);
  vllvm_store_short(vllvm_mradds_short(t7, vllvm_fixed_vimm_short(-21895, 8), t2), y, 3);

}

extern short *PostScalePtr;

#define STORE2(i) \
    vllvm_store_short(vllvm_mradds_short(vllvm_load_short(PostScalePtr, 8, i), \
					   vllvm_load_short(y, 8, i), \
					   vllvm_fixed_vimm_short(0, 8)), \
			output, i);

void dct_vector(short *input, short *output, short *x, short *y) {

  vllvm_store_short(vllvm_load_short(input, 8, 0), x, 0);
  vllvm_store_short(vllvm_load_short(input, 8, 1), x, 1);
  vllvm_store_short(vllvm_load_short(input, 8, 2), x, 2);
  vllvm_store_short(vllvm_load_short(input, 8, 3), x, 3);
  vllvm_store_short(vllvm_load_short(input, 8, 4), x, 4);
  vllvm_store_short(vllvm_load_short(input, 8, 5), x, 5);
  vllvm_store_short(vllvm_load_short(input, 8, 6), x, 6);
  vllvm_store_short(vllvm_load_short(input, 8, 7), x, 7);
  DCT_Transform_VectorC( x, y );
  Matrix_Transpose_VectorC( y, x );
  DCT_Transform_VectorC( x, y );

  STORE2(0);
  STORE2(1);
  STORE2(2);
  STORE2(3);
  STORE2(4);
  STORE2(5);
  STORE2(6);
  STORE2(7);

}

