#define N 1024

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/times.h>
#include "../_malloc.h"
#include "Scalar.h"

inline void dct_scalar(short*, short*);
void dct_vector(short*, short*, short*, short*);

short *in;
short *out_vector;
short *out_scalar;

static short PostScaleArray[64] = {
    4095, 5681, 5351, 4816, 4095, 4816, 5351, 5681,
    5681, 7880, 7422, 6680, 5681, 6680, 7422, 7880,
    5351, 7422, 6992, 6292, 5351, 6292, 6992, 7422,
    4816, 6680, 6292, 5663, 4816, 5663, 6292, 6680,
    4095, 5681, 5351, 4816, 4095, 4816, 5351, 5681,
    4816, 6680, 6292, 5663, 4816, 5663, 6292, 6680,
    5351, 7422, 6992, 6292, 5351, 6292, 6992, 7422,
    5681, 7880, 7422, 6680, 5681, 6680, 7422, 7880
};

short *PostScalePtr;

void init() {
  int i;

  // Force 16-byte alignment
  //
  in = (short*) _malloc(N*sizeof(short));
  out_vector = (short*) _malloc(N*sizeof(short));
  out_scalar = (short*) _malloc(N*sizeof(short));
  PostScalePtr = (short*) _malloc(64*sizeof(short));
  memcpy(PostScalePtr, PostScaleArray, 64*sizeof(short));
  
  // Populate in with a range of values
  //
  for (i = 0; i < N; ++i) {
    in[i] = N/2-i;
  }
  
}

float run() {
  long t0, t1, t2;
  int i,j;
  struct tms buf_s, buf_e;
  long scalar_time = 0, vector_time = 0;

  times(&buf_s);
  for (i = 0; i < 100000; ++i)
    for (j = 0; j < N; j +=64)
      dct_scalar(in+j, out_scalar+j);
  times(&buf_e);
  scalar_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("scalar time=%d, ", scalar_time);
  
  short *x = (short*) _malloc(64*sizeof(short));
  short *y = (short*) _malloc(64*sizeof(short));
  times(&buf_s);
  for (i = 0; i < 100000; ++i)
    for (j = 0; j < N; j +=64)
      dct_vector(in+j, out_vector+j, x, y);
  times(&buf_e);
  vector_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("vector time=%d, ", vector_time);
  
  float speedup = ((float) scalar_time)/vector_time;
  printf("speedup=%f\n", speedup);
  
  for (i = 0; i < N; i++) {
    if (out_vector[i] != out_scalar[i]) {
      printf("FAILED\n");
      exit(1);
    }
  }
  
  return speedup;
}

int main (void) {
  unsigned i;

  init();

  float best = 0;
  for (i = 0; i < NRUNS; ++i) {
    float speedup = run();
    if (speedup > best)
      best = speedup;
  }
  printf("best speedup=%f\n", best);

  printf ("PASSED\n");
  return 0;

}

static inline void Matrix_Transpose ( short *input, short *output) {
  unsigned i;
  for (i = 0; i < 8; ++i) {
    output[i] = input[8*i];
    output[8+i] = input[8*i+1];
    output[16+i] = input[8*i+2];
    output[24+i] = input[8*i+3];
    output[32+i] = input[8*i+4];
    output[40+i] = input[8*i+5];
    output[48+i] = input[8*i+6];
    output[56+i] = input[8*i+7];
  }
}

static inline void DCT_Transform ( short *x, short *y) {
  signed short t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;

  unsigned i;
  for (i = 0; i < 8; ++i) {
    t8 = adds_short(x[i], x[56+i]);
    t9 = subs_short(x[i], x[56+i]);
    t0 = adds_short(x[8+i], x[48+i]);
    t7 = subs_short(x[8+i], x[48+i]);
    t1 = adds_short(x[16+i], x[40+i]);
    t6 = subs_short(x[16+i], x[40+i]);
    t2 = adds_short(x[24+i], x[32+i]);
    t5 = subs_short(x[24+i], x[32+i]);

    t3 = adds_short(t8, t2);
    t4 = subs_short(t8, t2);
    t2 = adds_short(t0, t1);
    t8 = subs_short(t0, t1);
    
    t1 = adds_short(t7, t6);
    t0 = subs_short(t7, t6);

    y[i] = adds_short(t3, t2);
    y[32+i] = subs_short(t3, t2);
    y[16+i] = mradds_short(t8, 13573, t4);
    t10 = mradds_short(t4, 13573, 0);
    y[48+i] = subs_short(t10, t8);

    t6 = mradds_short(t0, 23170, t5);
    t7 = mradds_short(t0, -23170, t5);
    t2 = mradds_short(t1, -23170, t9);
    t3 = mradds_short(t1, 23170, t9);

    y[8+i] = mradds_short(t6, 6518, t3);
    t9 = mradds_short(t3, 6518, 0);
    y[56+i] = subs_short(t9, t6);
    y[40+i] = mradds_short(t2, 21895, t7);
    y[24+i] = mradds_short(t7, -21895, t2);
  }

}

void dct_scalar(short *input, short *output) {

  short x[64], y[64];
  unsigned i, j;

  memcpy(x, input, 64*sizeof(short));
  DCT_Transform( x, y );
  Matrix_Transpose( y, x );
  DCT_Transform( x, y );

  for (i = 0; i < 8; ++i)
    for (j = 0; j < 8; ++j)
      output[8*i+j] = mradds_short(PostScaleArray[8*i+j], y[8*i+j], 0);
}

