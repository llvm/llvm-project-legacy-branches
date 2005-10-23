#include "VectorC.h"

#define MERGE(out01, out0, out1, in0, in1) \
  short out01 = vllvm_fixed_vimm_short(0, 16); \
  out01 = vllvm_fixed_combine_short(out01, 16, in0, 8, 0, 1); \
  out01 = vllvm_fixed_combine_short(out01, 16, in1, 8, 8, 1); \
  short out0 = vllvm_extract_short(out01, 0, 2, 8); \
  short out1 = vllvm_extract_short(out01, 1, 2, 8)

#define IN(x) \
  vllvm_load_short(input_scalar, 8, x)

#define STORE(out, idx) \
  vllvm_store_short(out, output_scalar, idx)

inline void transpose_vector (short *input_scalar, short *output_scalar) {
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

