#ifndef SSE_H
#define SSE_H

#include <emmintrin.h>
#include "Scalar.h"

// Some obvious missing SSE intrinsics

#define _mm_splat_epi16(x) _mm_set_epi16(x,x,x,x,x,x,x,x)
#define _mm_splat_epi32(x) _mm_set_epi32(x,x,x,x)
#define _mm_pack_epi32(a,b) _mm_packs_epi32( _mm_srai_epi32( _mm_slli_epi32( a, 16), 16), \
  				             _mm_srai_epi32( _mm_slli_epi32( b, 16), 16) )
#define _mm_pack_epu32(a,b) _mm_packs_epi32( _mm_srai_epi32( _mm_slli_epi32( a, 16), 16), \
  				             _mm_srai_epi32( _mm_slli_epi32( b, 16), 16) )
#define _mm_pack_epi16(x,y) _mm_packs_epi16( _mm_srai_epi16( _mm_slli_epi16( x, 8), 8), \
                                             _mm_srai_epi16( _mm_slli_epi16( y, 8), 8) )
#define _mm_select_si128(msk, a, b) _mm_or_si128(_mm_and_si128(a, msk), _mm_andnot_si128(msk, b))


// Printing

inline void print_vector_short(__m128i v) {
  unsigned i;
  for (i = 0; i < 8; ++i)
    printf("%04X ", elt_short(v, i));
  printf("\n");
}

// AltiVec conversions

inline __m128i _mm_mr_epi16(__m128i x, __m128i y) {
  __m128i c = _mm_splat_epi32(1<<14);
  __m128i tmp_hi = _mm_mulhi_epi16(x, y);
  __m128i tmp_lo = _mm_mullo_epi16(x, y);
  return _mm_pack_epi32(_mm_srai_epi32(_mm_add_epi32(_mm_unpacklo_epi16(tmp_lo, tmp_hi), c), 15),
			_mm_srai_epi32(_mm_add_epi32(_mm_unpackhi_epi16(tmp_lo, tmp_hi), c), 15));
}

#define _mm_mradds_epi16(x, y, z) _mm_adds_epi16(_mm_mr_epi16(x,y),z)

#endif
