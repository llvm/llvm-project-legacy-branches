#define N 1024 //2048*2
#define MAX_QP 31

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include "../_malloc.h"

void interquant_scalar(short*,signed char*,int);
void interquant_vector(short*,signed char*,int);

short *in;
char *vector;
char *scalar;

void init() {
  int i;

  // Force 16-byte alignment
  //
  in = (short*) _malloc(N*sizeof(short));
  vector = (char*) _malloc(N*sizeof(short));
  scalar = (char*) _malloc(N*sizeof(short));
  
  // Populate in with a range of values
  //
  for (i = 0; i < N; ++i) {
    in[i] = -(N/2)+i;
  }
  
}

void run(long *scalar_time, long *vector_time) {
  long t0, t1, t2;
  int i,j;
  int qp = 10;
  struct tms buf_s, buf_e;

  init();
  
  times(&buf_s);
  for (j = 0; j < 100000; ++j)
    for (i = 0; i < N/64; ++i)
      interquant_scalar(in+64*i, scalar+64*i, qp);
  times(&buf_e);
  
  *scalar_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("scalar time=%d, ", *scalar_time);
  
  times(&buf_s);
  for (j = 0; j < 100000; ++j)
    for (i = 0; i < N/64; ++i)
      interquant_vector(in+64*i, vector+64*i, qp);
  times(&buf_e);
  
  *vector_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("vector time=%d, ", *vector_time);
  
  for (i = 0; i < N; i++) {
    if (vector[i] != scalar[i]) {
      printf("FAILED\n");
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

void interquant_scalar( signed short* in,  signed char* out, int qp) {
  int i;
  int qpd2 = (32768+qp)/(2*qp);

  for (i = 0; i < 64; ++i) {
    short input = in[i];
    short t1 = (input == -32768) ? 32767 : -input;
    unsigned short u1 = (unsigned short) ((input > t1) ? input : t1); 
    short t2 = (short) (u1 - (qp/2));
    t2 = (t2 > 0) ? t2 : 0;
    //int t3 = (t2 * ((32768+qp)/(2*qp))) / 32768;
    int t3 = (t2 * qpd2) /32768;
    t3 = (t3 > 32767) ? 32767 : t3;
    t3 = (t3 < -32768) ? -32768 : t3;
    short t4 = (t3 < 127) ? t3 : 127;
    out[i] = (input < 0) ? -t4 : t4;
  }
}

