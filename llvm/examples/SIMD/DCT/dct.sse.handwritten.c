#include "Scalar.h"
#include "SSE.h"

extern short *PostScalePtr;

static inline void Matrix_Transpose ( short *input_scalar, short *output_scalar)
{
  __m128i *input = (__m128i*) input_scalar;
  __m128i *output = (__m128i*) output_scalar;

  __m128i a0, a1, a2, a3, a4, a5, a6, a7;
  __m128i b0, b1, b2, b3, b4, b5, b6, b7;

  b0 = _mm_unpacklo_epi16( input[0], input[4] );     /* [ 00 40 01 41 02 42 03 43 ]*/
  b1 = _mm_unpackhi_epi16( input[0], input[4] );     /* [ 04 44 05 45 06 46 07 47 ]*/
  b2 = _mm_unpacklo_epi16( input[1], input[5] );     /* [ 10 50 11 51 12 52 13 53 ]*/
  b3 = _mm_unpackhi_epi16( input[1], input[5] );     /* [ 14 54 15 55 16 56 17 57 ]*/
  b4 = _mm_unpacklo_epi16( input[2], input[6] );     /* [ 20 60 21 61 22 62 23 63 ]*/
  b5 = _mm_unpackhi_epi16( input[2], input[6] );     /* [ 24 64 25 65 26 66 27 67 ]*/
  b6 = _mm_unpacklo_epi16( input[3], input[7] );     /* [ 30 70 31 71 32 72 33 73 ]*/
  b7 = _mm_unpackhi_epi16( input[3], input[7] );     /* [ 34 74 35 75 36 76 37 77 ]*/

  a0 = _mm_unpacklo_epi16( b0, b4 );                 /* [ 00 20 40 60 01 21 41 61 ]*/
  a1 = _mm_unpackhi_epi16( b0, b4 );                 /* [ 02 22 42 62 03 23 43 63 ]*/
  a2 = _mm_unpacklo_epi16( b1, b5 );                 /* [ 04 24 44 64 05 25 45 65 ]*/
  a3 = _mm_unpackhi_epi16( b1, b5 );                 /* [ 06 26 46 66 07 27 47 67 ]*/
  a4 = _mm_unpacklo_epi16( b2, b6 );                 /* [ 10 30 50 70 11 31 51 71 ]*/
  a5 = _mm_unpackhi_epi16( b2, b6 );                 /* [ 12 32 52 72 13 33 53 73 ]*/
  a6 = _mm_unpacklo_epi16( b3, b7 );                 /* [ 14 34 54 74 15 35 55 75 ]*/
  a7 = _mm_unpackhi_epi16( b3, b7 );                 /* [ 16 36 56 76 17 37 57 77 ]*/

  output[0] = _mm_unpacklo_epi16( a0, a4 );          /* [ 00 10 20 30 40 50 60 70 ]*/
  output[1] = _mm_unpackhi_epi16( a0, a4 );          /* [ 01 11 21 31 41 51 61 71 ]*/
  output[2] = _mm_unpacklo_epi16( a1, a5 );          /* [ 02 12 22 32 42 52 62 72 ]*/
  output[3] = _mm_unpackhi_epi16( a1, a5 );          /* [ 03 13 23 33 43 53 63 73 ]*/
  output[4] = _mm_unpacklo_epi16( a2, a6 );          /* [ 04 14 24 34 44 54 64 74 ]*/
  output[5] = _mm_unpackhi_epi16( a2, a6 );          /* [ 05 15 25 35 45 55 65 75 ]*/
  output[6] = _mm_unpacklo_epi16( a3, a7 );          /* [ 06 16 26 36 46 56 66 76 ]*/
  output[7] = _mm_unpackhi_epi16( a3, a7 );          /* [ 07 17 27 37 47 57 67 77 ]*/

}

static inline void DCT_Transform ( short *x, short *y) {
  __m128i *vx = (__m128i*) x;
  __m128i *vy = (__m128i*) y;

  __m128i t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;

  __m128i c13573 = _mm_splat_epi16(13573);
  __m128i c21895 = _mm_splat_epi16(21895);
  __m128i cNeg21895 = _mm_splat_epi16(-21895);
  __m128i c23170 = _mm_splat_epi16(23170);
  __m128i cNeg23170 = _mm_splat_epi16(-23170);
  __m128i c6518 = _mm_splat_epi16(6518);

  t8 = _mm_adds_epi16(vx[0], vx[7]);
  t9 = _mm_subs_epi16(vx[0], vx[7]);
  t0 = _mm_adds_epi16(vx[1], vx[6]);
  t7 = _mm_subs_epi16(vx[1], vx[6]);
  t1 = _mm_adds_epi16(vx[2], vx[5]);
  t6 = _mm_subs_epi16(vx[2], vx[5]);
  t2 = _mm_adds_epi16(vx[3], vx[4]);
  t5 = _mm_subs_epi16(vx[3], vx[4]);

  t3 = _mm_adds_epi16(t8, t2);
  t4 = _mm_subs_epi16(t8, t2);
  t2 = _mm_adds_epi16(t0, t1);
  t8 = _mm_subs_epi16(t0, t1);

  t1 = _mm_adds_epi16(t7, t6);
  t0 = _mm_subs_epi16(t7, t6);

  vy[0] = _mm_adds_epi16(t3, t2);
  vy[4] = _mm_subs_epi16(t3, t2);

  vy[2] = _mm_mradds_epi16(t8, c13573, t4);
  t10 = _mm_mr_epi16(t4, c13573);

  vy[6] = _mm_subs_epi16(t10, t8);

  t6 = _mm_mradds_epi16(t0, c23170, t5);
  t7 = _mm_mradds_epi16(t0, cNeg23170, t5);
  t2 = _mm_mradds_epi16(t1, cNeg23170, t9);
  t3 = _mm_mradds_epi16(t1, c23170, t9);

  vy[1] = _mm_mradds_epi16(t6, c6518, t3);
  t9 = _mm_mr_epi16(t3, c6518);

  vy[7] = _mm_subs_epi16(t9, t6);
  vy[5] = _mm_mradds_epi16(t2, c21895, t7);
  vy[3] = _mm_mradds_epi16(t7, cNeg21895, t2);

}

#define STORE(i) \
     outputv[i] = _mm_mradds_epi16(PostScalev[i], yv[i], _mm_splat_epi16(0));

void dct_vector(short *input, short *output, short *x, short *y) {

  __m128i *xv = (__m128i*) x;
  __m128i *yv = (__m128i*) y;
  __m128i *inputv = (__m128i*) input;
  __m128i *outputv = (__m128i*) output;
  __m128i *PostScalev = (__m128i*) PostScalePtr;

  xv[0] = inputv[0];
  xv[1] = inputv[1];
  xv[2] = inputv[2];
  xv[3] = inputv[3];
  xv[4] = inputv[4];
  xv[5] = inputv[5];
  xv[6] = inputv[6];
  xv[7] = inputv[7];

  DCT_Transform( x, y );
  Matrix_Transpose( y, x );
  DCT_Transform( x, y );

  STORE(0);
  STORE(1);
  STORE(2);
  STORE(3);
  STORE(4);
  STORE(5);
  STORE(6);
  STORE(7);

}

