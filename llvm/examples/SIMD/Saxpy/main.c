#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include "../_malloc.h"
#include <unistd.h>

#include "saxpy.h"

short *in1, *in2, *vector, *scalar;

void init() {
  unsigned i;
  in1 = _malloc(N*sizeof(short));
  for (i = 0; i < N; ++i)
    in1[i] = N/2 - i;
  in2 = _malloc(N*sizeof(short));
  for (i = 0; i < N; ++i)
    in2[i] = i - N/2;
  vector = _malloc(N*sizeof(short));
  scalar = _malloc(N*sizeof(short));
}

void run(long *scalar_time, long *vector_time) {
  unsigned i;
  struct tms buf_s, buf_e;

  times(&buf_s);
  for (i = 0; i < 1000000; ++i)
    saxpy_scalar(scalar, in1, in2, A, N);
  times(&buf_e);
  *scalar_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("scalar time=%d, ", *scalar_time);
  
  times(&buf_s);
  for (i = 0; i < 1000000; ++i)
    saxpy_vector (vector, in1, in2, A, N);
  times(&buf_e);
  *vector_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("vector time=%d, ", *vector_time);
  
  for (i = 0; i < N; i++) {
    if (vector[i] != scalar[i]) {
      printf ("FAILED\n");
      exit(1);
    }
  }

  float speedup = ((float) *scalar_time) / *vector_time;
  printf("speedup=%f\n", speedup);

}

int
main (void) {
  unsigned i;
  init();

  long best_scalar = -1, best_vector = -1;
  long scalar, vector;
  for (i = 0; i < NRUNS; ++i) {
    run (&scalar, &vector);
    if (best_scalar < 0 || best_scalar > scalar)
      best_scalar = scalar;
    if (best_vector < 0 || best_vector > vector)
      best_vector = vector;
  }

  printf("best scalar=%d, ", best_scalar);
  printf("best vector=%d, ", best_vector);
  printf("speedup=%f\n", ((float) best_scalar)/best_vector);
  printf ("PASSED\n");
  return 0;
}

void saxpy_scalar (short *z, const short *x, const short *y, short a, unsigned n) {
  int i;
  for (i = 0; i < n; ++i)
    *z++ = a * *x++ + *y++;
}
