#define N 1024

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <assert.h>
#include "../_malloc.h"

inline void transpose_scalar(short*, short*);
void transpose_vector(short*, short*);

short *in;
short *out_vector;
short *out_scalar;

void init() {
  int i;

  // Force 16-byte alignment
  //
  in = (short*) _malloc(N*sizeof(short));
  out_vector = (short*) _malloc(N*sizeof(short));
  out_scalar = (short*) _malloc(N*sizeof(short));
  
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
    for (i = 0; i < 1000000; ++i)
      for (j = 0; j < N/64; ++j)
	transpose_scalar(in+64*j, out_scalar+64*j);
    times(&buf_e);
    scalar_time = buf_e.tms_utime - buf_s.tms_utime;
    printf("scalar time=%d, ", scalar_time);
    
    times(&buf_s);
    for (i = 0; i < 1000000; ++i)
      for (j = 0; j < N/64; ++j)
	transpose_vector(in+64*j, out_vector+64*j);
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

int
main (void)
{
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

void transpose_scalar ( short *input_scalar, short *output_scalar) {
  unsigned i;
  for (i = 0; i < 8; ++i) {
    output_scalar[i] = input_scalar[8*i];
    output_scalar[8+i] = input_scalar[8*i+1];
    output_scalar[16+i] = input_scalar[8*i+2];
    output_scalar[24+i] = input_scalar[8*i+3];
    output_scalar[32+i] = input_scalar[8*i+4];
    output_scalar[40+i] = input_scalar[8*i+5];
    output_scalar[48+i] = input_scalar[8*i+6];
    output_scalar[56+i] = input_scalar[8*i+7];
  }
}
