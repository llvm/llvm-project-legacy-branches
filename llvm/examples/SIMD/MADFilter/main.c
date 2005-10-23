/*
 * This program adapted from
 * mad - MPEG audio decoder
 * Copyright (C) 2000-2001 Robert Leslie
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * $Id$
 */

# include <stdlib.h>
# include <stdio.h>

# include "mad.h"
#include "../_malloc.h"
#include <sys/time.h>
#include <sys/times.h>

void experimental_filter_scalar(short*, short*, unsigned);
void experimental_filter_vector(short*, short*, unsigned);
void init_channel(short*, unsigned);
int compare_channels(short *ch1, short *ch2, unsigned);

short *scalar_left, *scalar_right;
short *vector_left, *vector_right;

#define CHANNEL_SIZE 1152
#define ARRAY_SIZE CHANNEL_SIZE*sizeof(short)

void run(long *scalar_time, long *vector_time) {
 struct tms buf_s, buf_e;
  unsigned i;
  
  scalar_left = _malloc(ARRAY_SIZE);
  scalar_right = _malloc(ARRAY_SIZE);
  vector_left = _malloc(ARRAY_SIZE);
  vector_right = _malloc(ARRAY_SIZE);
  
  init_channel(scalar_left, CHANNEL_SIZE);
  init_channel(scalar_right, CHANNEL_SIZE);

  times(&buf_s);
  for (i = 0; i < 1000000; ++i)
    experimental_filter_scalar(scalar_left, scalar_right, CHANNEL_SIZE);
  times(&buf_e);
  *scalar_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("scalar time=%d, ", *scalar_time);
  
  init_channel(vector_left, CHANNEL_SIZE);
  init_channel(vector_right, CHANNEL_SIZE);

  times(&buf_s);
  for (i = 0; i < 1000000; ++i)
    experimental_filter_vector(vector_left, vector_right, CHANNEL_SIZE);
  times(&buf_e);
  *vector_time = buf_e.tms_utime - buf_s.tms_utime;
  printf("vector time=%d, ", *vector_time);
 
  float speedup = (float) *scalar_time / *vector_time;
  printf("speedup=%f\n", speedup);

  if (!compare_channels(scalar_left, vector_left, CHANNEL_SIZE) ||
      !compare_channels(scalar_right, vector_right, CHANNEL_SIZE)) {
    printf("FAILED\n");
    exit(1);
  }

}

int
main (void) {
  unsigned i;

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

void init_channel(short *channel, unsigned n) {
  unsigned i, j;
  for (i = 0; i < n; ++i)
    channel[i] = i;
}

int compare_channels(short *ch1, short *ch2, unsigned n) {
  unsigned i, j;
  for (i = 0; i < n; ++i)
    if(ch1[i] != ch2[i])
      return 0;
  return 1;
}

void experimental_filter_scalar(short *left_ch, short *right_ch, unsigned n) {
  unsigned int i, j;

  for (i = 0; i < n; ++i) {
    short left, right;
    
    left  = left_ch[i];
    right = right_ch[i];
    right_ch[i] -= left >> 2;
    left_ch[i] -= right  >> 2;
  }
}

