#include "VectorC.h"

void saxpy_vector(short *z, short *x, short *y, 
		  short a, unsigned n) {
  short a_vec = vllvm_fixed_vimm_short(a,8);
  int i;
  for (i = 0; i < n/8; ++i) {
    short x_vec = vllvm_load_short(x, 8, i);
    short y_vec = vllvm_load_short(y, 8, i);
    short z_vec = a_vec * x_vec + y_vec;
    vllvm_store_short(z_vec, z, i);
  }
}

