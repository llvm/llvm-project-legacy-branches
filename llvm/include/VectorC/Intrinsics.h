// VectorC intrinsics

#ifndef INTRINSICS_H
#define INTRINSICS_H

short vectorc_adds_short(short, short);
short vllvm_adds_short(short,short);

short vectorc_subs_short(short, short);
short vllvm_subs_short(short,short);

unsigned char vectorc_saturate_short_unsigned_char(short);
unsigned char vllvm_saturate_short_uchar(short);

#define vectorc_mr_short(x, y) ((((x)*(y))+vllvm_fixed_vimm_short(1<<14,8))>>15)
#define vllvm_mr_short(x, y) ((((x)*(y))+vllvm_fixed_vimm_short(1<<14,8))>>15)
#define vectorc_mradds_short(x,y,z) vectorc_adds_short(vectorc_mr_short(x,y),z)
#define vllvm_mradds_short(x,y,z) vllvm_adds_short(vllvm_mr_short(x,y),z)

short vllvm_max_short(short, short);
unsigned short vllvm_subs_ushort(unsigned short, unsigned short);
short vllvm_min_short(short, short);

#endif
