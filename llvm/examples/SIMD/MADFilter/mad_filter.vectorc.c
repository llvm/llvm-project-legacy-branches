#include "VectorC.h"

void experimental_filter_vector(short *left_ch, short *right_ch, unsigned n) {
  unsigned i;
  for (i = 0; i < n/8; ++i) {
    short left = vllvm_load_short(left_ch, 8, i);
    short right = vllvm_load_short(right_ch, 8, i);
    short left_sub = left - (right >> 2);
    short right_sub = right - (left >> 2);
    vllvm_store_short(right_sub, right_ch, i);
    vllvm_store_short(left_sub, left_ch, i);
  }
}
