#include "VectorC.h"
#include "Intrinsics.h"

void interquant_vector(signed short* in, signed char* out, int qp ) {
  int i, j;

  short part1, part2;
  short t1, t2, t3, t4, t5;
  unsigned short u1;
  short msk;
  
  unsigned short qpd2 = vllvm_fixed_vimm_short((short) qp/2, 8);
  short v = vllvm_fixed_vimm_short((short)((int)((32768+qp)/(2*qp))), 8);
  
  short zero = vllvm_fixed_vimm_short(0, 8);
  short maxq = vllvm_fixed_vimm_short(127, 8);
  
  for (i = 0; i < 4; ++i) {
    short in_vec = vllvm_load_short(in, 8, 2*i);
    t1 = vllvm_subs_short( zero, in_vec);
    u1 = (unsigned short) vllvm_max_short( in_vec, t1 );
    t2 = vllvm_subs_ushort( u1, qpd2 );
    t3 = t2*v >> 15;
    t4 = vllvm_min_short(maxq,t3);
    msk = zero > in_vec;
    t5 = vllvm_subs_short( zero, t4 );
    part1 = vllvm_vselect_short(msk, t5, t4);

    in_vec = vllvm_load_short(in, 8, 2*i+1);
    t1 = vllvm_subs_short( zero, in_vec);
    u1 = (unsigned short) vllvm_max_short( in_vec, t1 );
    t2 = (short) vllvm_subs_ushort( u1, qpd2 );
    t3 = (t2*v) >> 15;
    t4 = vllvm_min_short(maxq,t3);
    msk = zero > in_vec;
    t5 = vllvm_subs_short( zero, t4 );
    part2 = vllvm_vselect_short(msk, t5, t4);
    
    short out_vec = vllvm_fixed_vimm_short(0, 16);
    out_vec = vllvm_fixed_combine_short(out_vec, 16, part1, 8, 0, 1);
    out_vec = vllvm_fixed_combine_short(out_vec, 16, part2, 8, 8, 1);
    vllvm_store_char(out_vec, out, i);
  }
}
