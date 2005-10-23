#include <emmintrin.h>

#define VECTOR(x) *((__m128i*) &x)
#define CONSTANT(x) _mm_set_epi16(x,x,x,x,x,x,x,x)

inline __m128i vec_mr(__m128i x, short y) {
  __m128i const_1 = _mm_set_epi16(1,1,1,1,1,1,1,1);
  __m128i y_vec = _mm_set_epi16(y, y, y, y,y, y, y, y);
  __m128i tmp_hi = _mm_mulhi_epi16(x, y_vec);
  __m128i tmp_lo = _mm_mullo_epi16(x, y_vec);
  __m128i hi = _mm_slli_epi16(tmp_hi, 2);
  __m128i lo = _mm_srli_epi16(tmp_lo, 14);
  __m128i tmp_vec = _mm_or_si128(hi, lo);
  tmp_vec = _mm_add_epi16(tmp_vec, const_1);
  tmp_vec = _mm_srai_epi16(tmp_vec, 1);
  return tmp_vec;
}

inline __m128i vec_mradds(__m128i x, short y, __m128i z) {
  return _mm_adds_epi16(vec_mr(x,y),z);
}

#define MRADDS(x,y,z) _mm_adds_epi16(vec_mr(x,y),z)

void print_quaternary(unsigned char ch) {
  unsigned i;
  for (i = 0; i < 4; ++i)
    printf("%d ", (ch >> (2*i)) & 3);
  printf("\n");
}

void print_vector_128(__m128i vec) {
  __m128i tmp = vec;
  unsigned char *p = (unsigned char*) &tmp;
  unsigned i;
  for (i = 0; i < 16; ++i)
    printf("%02X ", p[i]);
  printf("\n");
}

#define idx(idx0, idx1, idx2, idx3) \
 idx0 | (idx1 << 2) | (idx2 << 4) | (idx3 << 6)

#define extract(source, idx0, idx1, idx2, idx3) \
  _mm_shuffle_epi32(source, idx0 | (idx1 << 2) | (idx2 << 4) | (idx3 << 6)) \

#define mask(source, idx0, idx1, idx2, idx3) \
  _mm_and_si128(source, _mm_set_epi32(idx3 * ~0U, idx2 * ~0U, idx1 * ~0U, idx0 * ~0U))

#define msk(idx0, idx1, idx2, idx3) \
  _mm_set_epi32(idx3 * ~0U, idx2 * ~0U, idx1 * ~0U, idx0 * ~0U)

void rgb2yuv_vector(unsigned char *RGB_char_ptr, int RGB_size,
		    unsigned char *YCC_char_ptr) {

  __m128i* RGB_ptr = (__m128i*) RGB_char_ptr;
  __m128i zero = _mm_set_epi8(0, 0, 0, 0, 0, 0, 0, 0,
			      0, 0, 0, 0, 0, 0, 0, 0);
  __m128i constant_16 = CONSTANT(16);
  __m128i constant_128 = CONSTANT(128);

  unsigned j, i;

  __m128i in0123, in01, in0, in1, in23, in2, in3;
  __m128i in4567, in45, in4, in5, in67, in6, in7;
  __m128i in89AB, in89, in8, in9, inAB, inA, inB;

  __m128i red0, red1, red2, red_lo, red_hi;
  __m128i green0, green1, green_lo, green_hi;
  __m128i blue0, blue1, blue_lo, blue_hi;

  __m128i red_16_lo, red_16_hi;
  __m128i green_16_lo, green_16_hi;
  __m128i blue_16_lo, blue_16_hi;

  __m128i Y_lo, Y_hi;
  __m128i Cb_lo, Cb_hi;
  __m128i Cr_lo, Cr_hi;

  __m128i Ys_char, Cbs_char, Crs_char;

  for (i = 0; i < RGB_size; i += 3*16) {
    in0123 = RGB_ptr[i/16];
    in01 = _mm_unpacklo_epi8(in0123, zero);
    in0 = _mm_unpacklo_epi16(in01, zero);
    in1 = _mm_unpackhi_epi16(in01, zero);
    in23 = _mm_unpackhi_epi8(in0123, zero);
    in2 = _mm_unpacklo_epi16(in23, zero);
    in3 = _mm_unpackhi_epi16(in23, zero);
    in4567 = RGB_ptr[i/16+1];
    in45 = _mm_unpacklo_epi8(in4567, zero);
    in4 = _mm_unpacklo_epi16(in45, zero);
    in5 = _mm_unpackhi_epi16(in45, zero);
    in67 = _mm_unpackhi_epi8(in4567, zero);
    in6 = _mm_unpacklo_epi16(in67, zero);
    in7 = _mm_unpackhi_epi16(in67, zero);
    in89AB = RGB_ptr[i/16+2];
    in89 = _mm_unpacklo_epi8(in89AB, zero);
    in8 = _mm_unpacklo_epi16(in89, zero);
    in9 = _mm_unpackhi_epi16(in89, zero);
    inAB = _mm_unpackhi_epi8(in89AB, zero);
    inA = _mm_unpacklo_epi16(inAB, zero);
    inB = _mm_unpackhi_epi16(inAB, zero);

    red0 = _mm_and_si128(_mm_shuffle_epi32(in0, idx(0,3,0,0)), msk(1,1,0,0));
    red1 = _mm_and_si128(in1, msk(0,0,1,0));
    red2 = _mm_and_si128(_mm_shuffle_epi32(in2, idx(0,0,0,1)), msk(0,0,0,1));
    red_lo = _mm_or_si128(_mm_or_si128(red0, red1), red2);

    red0 = _mm_and_si128(_mm_shuffle_epi32(in3, idx(0,3,0,0)), msk(1,1,0,0));
    red1 = _mm_and_si128(in4, msk(0,0,1,0));
    red2 = _mm_and_si128(_mm_shuffle_epi32(in5, idx(0,0,0,1)), msk(0,0,0,1));
    red_hi = _mm_or_si128(_mm_or_si128(red0, red1), red2);
    red_16_lo = _mm_packs_epi32(red_lo, red_hi);

    red0 = _mm_and_si128(_mm_shuffle_epi32(in6, idx(0,3,0,0)), msk(1,1,0,0));
    red1 = _mm_and_si128(in7, msk(0,0,1,0));
    red0 = _mm_or_si128(red0, red1);
    red1 = _mm_and_si128(_mm_shuffle_epi32(in8, idx(0,0,0,1)), msk(0,0,0,1));
    red_lo = _mm_or_si128(red0, red1);

    red0 = _mm_and_si128(_mm_shuffle_epi32(in9, idx(0,3,0,0)), msk(1,1,0,0));
    red1 = _mm_and_si128(inA, msk(0,0,1,0));
    red0 = _mm_or_si128(red0, red1);
    red1 = _mm_and_si128(_mm_shuffle_epi32(inB, idx(0,0,0,1)), msk(0,0,0,1));
    red_hi = _mm_or_si128(red0, red1);
    red_16_hi = _mm_packs_epi32(red_lo, red_hi);

    green0 = mask(extract(in0, 1,0,0,0), 1, 0, 0, 0);
    green1 = mask(extract(in1, 0, 0, 3, 0), 0, 1, 1, 0);
    green0 = _mm_or_si128(green0, green1);
    green1 = mask(extract(in2, 0, 0, 0, 2), 0, 0, 0, 1);
    green_lo = _mm_or_si128(green0, green1);

    green0 = mask(extract(in3, 1,0,0,0), 1, 0, 0, 0);
    green1 = mask(extract(in4, 0, 0, 3, 0), 0, 1, 1, 0);
    green0 = _mm_or_si128(green0, green1);
    green1 = mask(extract(in5, 0, 0, 0, 2), 0, 0, 0, 1);
    green_hi = _mm_or_si128(green0, green1);
    green_16_lo = _mm_packs_epi32(green_lo, green_hi);

    green0 = mask(extract(in6, 1,0,0,0), 1, 0, 0, 0);
    green1 = mask(extract(in7, 0, 0, 3, 0), 0, 1, 1, 0);
    green0 = _mm_or_si128(green0, green1);
    green1 = mask(extract(in8, 0, 0, 0, 2), 0, 0, 0, 1);
    green_lo = _mm_or_si128(green0, green1);

    green0 = mask(extract(in9, 1,0,0,0), 1, 0, 0, 0);
    green1 = mask(extract(inA, 0, 0, 3, 0), 0, 1, 1, 0);
    green0 = _mm_or_si128(green0, green1);
    green1 = mask(extract(inB, 0, 0, 0, 2), 0, 0, 0, 1);
    green_hi = _mm_or_si128(green0, green1);
    green_16_hi = _mm_packs_epi32(green_lo, green_hi);

    blue0 = mask(extract(in0, 2,0,0,0), 1, 0, 0, 0);
    blue1 = mask(extract(in1, 0, 1, 0, 0), 0, 1, 0, 0);
    blue0 = _mm_or_si128(blue0, blue1);
    blue1 = mask(extract(in2, 0, 0, 0, 3), 0, 0, 1, 1);
    blue_lo = _mm_or_si128(blue0, blue1);

    blue0 = mask(extract(in3, 2,0,0,0), 1, 0, 0, 0);
    blue1 = mask(extract(in4, 0, 1, 0, 0), 0, 1, 0, 0);
    blue0 = _mm_or_si128(blue0, blue1);
    blue1 = mask(extract(in5, 0, 0, 0, 3), 0, 0, 1, 1);
    blue_hi = _mm_or_si128(blue0, blue1);
    blue_16_lo = _mm_packs_epi32(blue_lo, blue_hi);

    blue0 = mask(extract(in6, 2,0,0,0), 1, 0, 0, 0);
    blue1 = mask(extract(in7, 0, 1, 0, 0), 0, 1, 0, 0);
    blue0 = _mm_or_si128(blue0, blue1);
    blue1 = mask(extract(in8, 0, 0, 0, 3), 0, 0, 1, 1);
    blue_lo = _mm_or_si128(blue0, blue1);

    blue0 = mask(extract(in9, 2,0,0,0), 1, 0, 0, 0);
    blue1 = mask(extract(inA, 0, 1, 0, 0), 0, 1, 0, 0);
    blue0 = _mm_or_si128(blue0, blue1);
    blue1 = mask(extract(inB, 0, 0, 0, 3), 0, 0, 1, 1);
    blue_hi = _mm_or_si128(blue0, blue1);
    blue_16_hi = _mm_packs_epi32(blue_lo, blue_hi);

    Y_lo = vec_mr(red_16_lo, 8432);
    Y_hi = vec_mr(red_16_hi, 8432);

    Cb_lo = vec_mr(red_16_lo, -4818);
    Cb_hi = vec_mr(red_16_hi, -4818);

    Cr_lo = vec_mr(red_16_lo, 14345);
    Cr_hi = vec_mr(red_16_hi, 14345);

    Y_lo = vec_mradds(green_16_lo, 16425, Y_lo);
    Y_hi = vec_mradds(green_16_hi, 16425, Y_hi);

    Cb_lo = vec_mradds(green_16_lo, -9527, Cb_lo);
    Cb_hi = vec_mradds(green_16_hi, -9527, Cb_hi);

    Cr_lo = vec_mradds(green_16_lo, -12045, Cr_lo);
    Cr_hi = vec_mradds(green_16_hi, -12045, Cr_hi);

    Y_lo = vec_mradds(blue_16_lo, 3176, Y_lo);
    Y_hi = vec_mradds(blue_16_hi, 3176, Y_hi);

    Cb_lo = vec_mradds(blue_16_lo, 14345, Cb_lo);
    Cb_hi = vec_mradds(blue_16_hi, 14345, Cb_hi);

    Cr_lo = vec_mradds(blue_16_lo, -2300, Cr_lo);
    Cr_hi = vec_mradds(blue_16_hi, -2300, Cr_hi);

    Y_lo = _mm_adds_epi16(Y_lo, constant_16);
    Y_hi = _mm_adds_epi16(Y_hi, constant_16);

    Cb_lo = _mm_adds_epi16(Cb_lo, constant_128);
    Cb_hi = _mm_adds_epi16(Cb_hi, constant_128);

    Cr_lo = _mm_adds_epi16(Cr_lo, constant_128);
    Cr_hi = _mm_adds_epi16(Cr_hi, constant_128);

    Ys_char = _mm_packus_epi16(Y_lo, Y_hi);
    Cbs_char = _mm_packus_epi16(Cb_lo, Cb_hi);
    Crs_char = _mm_packus_epi16(Cr_lo, Cr_hi);

    for (j = 0; j < 16; ++j) {
      YCC_char_ptr[i+3*j] = ((unsigned char*) &Ys_char)[j];
      YCC_char_ptr[i+1+3*j] = ((unsigned char*) &Cbs_char)[j];
      YCC_char_ptr[i+2+3*j] = ((unsigned char*) &Crs_char)[j];
    }
  }

  malloc(0);

}
