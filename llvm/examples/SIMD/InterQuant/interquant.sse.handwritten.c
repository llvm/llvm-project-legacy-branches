#include "SSE.h"

void interquant_vector ( signed short* in,
			 signed char* out,
			 int qp) {
  int i, j, k;
  short dtqp = (32768+qp)/(2*qp);
  __m128i dtqp_vec = _mm_splat_epi16(dtqp);
  __m128i zero = _mm_splat_epi16(0);
  __m128i qpd2 = _mm_splat_epi16(qp/2);
  __m128i maxq = _mm_splat_epi16(127);
  __m128i *in_vp = (__m128i*) in;
  __m128i *out_vp = (__m128i*) out;
  __m128i result[2];
  
  for (i = 0; i < 4; ++i) {
    for (j = 0; j < 2; ++j) {
      __m128i input = *in_vp++;
      __m128i t1 = _mm_subs_epi16(zero, input);
      __m128i u1 = _mm_max_epi16(input, t1);
      __m128i t2 = _mm_subs_epu16(u1, qpd2);

      // unsigned tmp = (unsigned) t2 * (unsigned) dtqp_vec
      __m128i tmp_hi = _mm_mulhi_epi16(t2, dtqp_vec);
      __m128i tmp_lo = _mm_mullo_epi16(t2, dtqp_vec);

      // short t3 = tmp >> 15
      __m128i hi = _mm_slli_epi16(tmp_hi, 1);
      __m128i lo = _mm_srli_epi16(tmp_lo, 15);
      __m128i t3 = _mm_or_si128(hi, lo);

      __m128i t4 = _mm_min_epi16(maxq, t3);
      __m128i mask = _mm_cmpgt_epi16(zero, input);
      __m128i neg = _mm_subs_epi16(zero, t4);
      result[j] = _mm_select_si128(mask, neg, t4);
    }
    *out_vp++ = _mm_pack_epi16(result[0], result[1]);
  }
}

