void print_vector(vector short v) {
  unsigned i;
  short *p = ((short*) &v);
  for (i = 0; i < 8; ++i)
    printf("%04X ", p[i]);
  printf("\n");
}

inline void transpose_vector ( short *input_scalar, short *output_scalar)
{
  vector signed short *input = (vector signed short*) input_scalar;
  vector signed short *output = (vector signed short*) output_scalar;

  vector signed short a0, a1, a2, a3, a4, a5, a6, a7;
  vector signed short b0, b1, b2, b3, b4, b5, b6, b7;

  b0 = vec_mergeh( input[0], input[4] );     /* [ 00 40 01 41 02 42 03 43 ]*/
  b1 = vec_mergel( input[0], input[4] );     /* [ 04 44 05 45 06 46 07 47 ]*/
  b2 = vec_mergeh( input[1], input[5] );     /* [ 10 50 11 51 12 52 13 53 ]*/
  b3 = vec_mergel( input[1], input[5] );     /* [ 14 54 15 55 16 56 17 57 ]*/
  b4 = vec_mergeh( input[2], input[6] );     /* [ 20 60 21 61 22 62 23 63 ]*/
  b5 = vec_mergel( input[2], input[6] );     /* [ 24 64 25 65 26 66 27 67 ]*/
  b6 = vec_mergeh( input[3], input[7] );     /* [ 30 70 31 71 32 72 33 73 ]*/
  b7 = vec_mergel( input[3], input[7] );     /* [ 34 74 35 75 36 76 37 77 ]*/

  a0 = vec_mergeh( b0, b4 );                 /* [ 00 20 40 60 01 21 41 61 ]*/
  a1 = vec_mergel( b0, b4 );                 /* [ 02 22 42 62 03 23 43 63 ]*/
  a2 = vec_mergeh( b1, b5 );                 /* [ 04 24 44 64 05 25 45 65 ]*/
  a3 = vec_mergel( b1, b5 );                 /* [ 06 26 46 66 07 27 47 67 ]*/
  a4 = vec_mergeh( b2, b6 );                 /* [ 10 30 50 70 11 31 51 71 ]*/
  a5 = vec_mergel( b2, b6 );                 /* [ 12 32 52 72 13 33 53 73 ]*/
  a6 = vec_mergeh( b3, b7 );                 /* [ 14 34 54 74 15 35 55 75 ]*/
  a7 = vec_mergel( b3, b7 );                 /* [ 16 36 56 76 17 37 57 77 ]*/

  output[0] = vec_mergeh( a0, a4 );          /* [ 00 10 20 30 40 50 60 70 ]*/
  output[1] = vec_mergel( a0, a4 );          /* [ 01 11 21 31 41 51 61 71 ]*/
  output[2] = vec_mergeh( a1, a5 );          /* [ 02 12 22 32 42 52 62 72 ]*/
  output[3] = vec_mergel( a1, a5 );          /* [ 03 13 23 33 43 53 63 73 ]*/
  output[4] = vec_mergeh( a2, a6 );          /* [ 04 14 24 34 44 54 64 74 ]*/
  output[5] = vec_mergel( a2, a6 );          /* [ 05 15 25 35 45 55 65 75 ]*/
  output[6] = vec_mergeh( a3, a7 );          /* [ 06 16 26 36 46 56 66 76 ]*/
  output[7] = vec_mergel( a3, a7 );          /* [ 07 17 27 37 47 57 67 77 ]*/

}
