#include "SSE.h"

void saxpy_vector(short *z, short *x, short *y, short a, unsigned n) {
  __m128i* x_ptr = (__m128i*) x;
  __m128i* y_ptr = (__m128i*) y;
  __m128i* z_ptr = (__m128i*) z;
  __m128i a_vec = _mm_splat_epi16(a);
  int i;
  for (i = 0; i < n/8; ++i) {
    __m128i x_vec = x_ptr[i];
    __m128i y_vec = y_ptr[i];
    __m128i z_vec = _mm_add_epi16( _mm_mullo_epi16(x_vec,a_vec),y_vec);
    z_ptr[i] = z_vec;
  }
}

