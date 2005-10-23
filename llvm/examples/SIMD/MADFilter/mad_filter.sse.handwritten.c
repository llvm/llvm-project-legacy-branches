#include "SSE.h"

void experimental_filter_vector(short *left_ch, short *right_ch, unsigned n) {
  unsigned int i, j;
  __m128i *left_vp = (__m128i*) left_ch;
  __m128i *right_vp = (__m128i*) right_ch;

  for (i = 0; i < n/8; ++i) {
    __m128i left  = left_vp[i];
    __m128i right = right_vp[i];
    __m128i left_sub = _mm_sub_epi16(left, _mm_srai_epi16(right, 2));
    __m128i right_sub = _mm_sub_epi16(right, _mm_srai_epi16(left, 2));
    right_vp[i] = right_sub;
    left_vp[i] = left_sub;
  }
}
