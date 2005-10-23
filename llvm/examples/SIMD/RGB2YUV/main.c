#define N 4800

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <assert.h>
#include "../_malloc.h"

void rgb2yuv_scalar(unsigned char*, int, unsigned char*);
void rgb2yuv_vector(unsigned char*, int, unsigned char*);

char *in;
char *out;
char *ref;

void init() {
  int i;

  // Force 16-byte alignment
  //
  in = (char*) _malloc(N*sizeof(char));
  out = (char*) _malloc(N*sizeof(char));
  ref = (char*) _malloc(N*sizeof(char));
  
  // Populate in with a range of values
  //
  for (i = 0; i < N; ++i) {
    in[i] = -(N/2)+i;
    out[i] = 1;
    ref[i] = 2;
  }
  
}

void run(long *scalar_time, long *vector_time) {
  long t0, t1, t2;
  int i,j;
  
  struct tms buf_s, buf_e;
  
  times(&buf_s);
  for (i = 0; i < 100000; ++i)
    rgb2yuv_scalar(in, N, ref);
  times(&buf_e);
  *scalar_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("scalar time=%d, ", *scalar_time);
    
  times(&buf_s);
  for (i = 0; i < 100000; ++i)
    rgb2yuv_vector(in, N, out);
  times(&buf_e);
  *vector_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("vector time=%d, ", *vector_time);
    
  for (i = 0; i < N; i++) {
    if (out[i] != ref[i]) {
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

inline short saturate(int a) {
  if (a > 32767)
    return 32767;
  if (a < -32768)
    return -32768;
  return a;
}

inline short mradds(short a, short b, short c) {
  int aint = a, bint = b, cint = c;
  assert(((aint*bint)+(1<<14))>>15 == (((short)((aint*bint)>>14))+1)>>1);
  return saturate(((aint*bint+(1 << 14)) >> 15) + cint);
}

inline short adds(short a, short b) {
  return saturate(a+b);
}

inline unsigned char saturate_uchar(unsigned short a) {
  if (a > 255)
    return 255;
  return a;
}

void rgb2yuv_scalar(unsigned char *RGB_char_ptr, int RGB_size,
		    unsigned char *YCC_char_ptr) {
  short red, green, blue;
  short Y, Cb, Cr;
  unsigned j, i;

  for (i = 0; i < RGB_size; i += 3*16) {
    for (j = 0; j < 16; ++j) {
      red = RGB_char_ptr[i+3*j];
      green = RGB_char_ptr[i+3*j+1];
      blue = RGB_char_ptr[i+3*j+2];
    
      Y = mradds(red, 8432, 0);
      Cb = mradds(red, -4818, 0);
      Cr = mradds(red, 14345, 0);
      
      Y = mradds(green, 16425, Y);
      Cb = mradds(green, -9527, Cb);
      Cr = mradds(green, -12045, Cr);
    
      Y = mradds(blue, 3176, Y);
      Cb = mradds(blue, 14345, Cb);
      Cr = mradds(blue, -2300, Cr);
      
      Y = adds(Y, 16);
      Cb = adds(Cb, 128);
      Cr = adds(Cr, 128);
      
      YCC_char_ptr[i+j] = saturate_uchar(Y);
      YCC_char_ptr[i+j+16] = saturate_uchar(Cb);
      YCC_char_ptr[i+j+32] = saturate_uchar(Cr);
    }
  }
}

