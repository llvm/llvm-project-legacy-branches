static inline void Matrix_Transpose ( vector signed short *input, vector signed short *output)
{
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

/***************************************************************
 *
 * Copyright:   (c) Copyright Motorola Inc. 1998
 *
 * Date:        April 15, 1998
 *
 * Macro:       DCT_Transform
 *
 * Description: Discrete Cosign Transform implemented by the
 *              Scaled Chen (II) Algorithm developed by Haifa
 *              Research Lab.  The major differnce between this
 *              algorithm and the Scaled Chen (I) is that
 *              certain multiply-subtracts are replaced by
 *              multiply adds.  A full description of the
 *              Scaled Chen (I) algorithm can be found in:
 *              W.C.Chen, C.H.Smith and S.C.Fralick, "A Fast
 *              Computational Algorithm for the Discrete Cosine
 *              Transform", IEEE Transactions on Cummnuications,
 *              Vol. COM-25, No. 9, pp 1004-1009, Sept. 1997.
 *
 * Inputs:      vx     : array of vector short
 *              t1-t10 : temporary vector variables set up by caller
 *              c4     : cos(4*pi/16)
 *              mc4    : -c4
 *              a0     : c6/c2
 *              a1     : c7/c1
 *              a2     : c5/c3
 *              ma2    : -a2
 *              zero   : an array of zero elements
 *
 * Outputs:     vy     : array of vector short
 *
 **************************************************************/

#define DCT_Transform(vx,vy) \
                                                                   \
  /* 1st stage. */                                                 \
  t8 = vec_adds( vx[0], vx[7] );     /* t0 + t7 */                 \
  t9 = vec_subs( vx[0], vx[7] );     /* t0 - t7 */                 \
  t0 = vec_adds( vx[1], vx[6] );     /* t1 + t6 */                 \
  t7 = vec_subs( vx[1], vx[6] );     /* t1 - t6 */                 \
  t1 = vec_adds( vx[2], vx[5] );     /* t2 + t6 */                 \
  t6 = vec_subs( vx[2], vx[5] );     /* t2 - t6 */                 \
  t2 = vec_adds( vx[3], vx[4] );     /* t3 + t4 */                 \
  t5 = vec_subs( vx[3], vx[4] );     /* t3 - t4 */                 \
                                                                   \
  /* 2nd stage. */                                                 \
  t3 = vec_adds( t8, t2 );           /* (t0+t7) + (t3+t4) */       \
  t4 = vec_subs( t8, t2 );           /* (t0+t7) - (t3+t4) */       \
  t2 = vec_adds( t0, t1 );           /* (t1+t6) + (t2+t5) */       \
  t8 = vec_subs( t0, t1 );           /* (t1+t6) - (t2+t5) */       \
                                                                   \
  t1 = vec_adds( t7, t6 );           /* (t1-t6) + (t2-t5) */       \
  t0 = vec_subs( t7, t6 );           /* (t1-t6) - (t2-t5) */       \
                                                                   \
  /* 3rd stage */                                                  \
  vy[0] = vec_adds( t3, t2 );        /* y0 = t3 + t2 */            \
  vy[4] = vec_subs( t3, t2 );        /* y4 = t3 + t2 */            \
  vy[2] = vec_mradds( t8, a0, t4 );  /* y2 = t8 * (a0) + t4 */     \
  t10 = vec_mradds( t4, a0, zero );                                \
  vy[6]  = vec_subs( t10, t8 );       /* y6 = t4 * (a0) - t8 */    \
                                                                   \
  t6 = vec_mradds( t0, c4, t5 );     /* t6 = t0 * (c4) + t5  */    \
  t7 = vec_mradds( t0, mc4, t5 );    /* t7 = t0 * (-c4) + t5 */    \
  t2 = vec_mradds( t1, mc4, t9 );    /* t2 = t1 * (-c4) + t9 */    \
  t3 = vec_mradds( t1, c4, t9 );     /* t3 = t1 * (c4) + t9  */    \
                                                                   \
  /* 4th stage. */                                                 \
  vy[1] = vec_mradds( t6, a1, t3 );    /* y1 = t6 * (a1) + t3  */  \
  t9 = vec_mradds( t3, a1, zero );                                 \
  vy[7] = vec_subs( t9, t6 ) ;         /* y7 = t3 * (a1) - t6  */  \
  vy[5] = vec_mradds( t2, a2, t7 );    /* y5 = t2 + (a2) + t7  */  \
  vy[3] = vec_mradds( t7, ma2, t2 );   /* y3 = t7 * (-a2) + t2 */

/* Post-scaling matrix -- scaled by 1 */
vector signed short PostScale[8] = {
    (vector signed short)( 4095, 5681, 5351, 4816, 4095, 4816, 5351, 5681 ),
    (vector signed short)( 5681, 7880, 7422, 6680, 5681, 6680, 7422, 7880 ),
    (vector signed short)( 5351, 7422, 6992, 6292, 5351, 6292, 6992, 7422 ),
    (vector signed short)( 4816, 6680, 6292, 5663, 4816, 5663, 6292, 6680 ),
    (vector signed short)( 4095, 5681, 5351, 4816, 4095, 4816, 5351, 5681 ),
    (vector signed short)( 4816, 6680, 6292, 5663, 4816, 5663, 6292, 6680 ),
    (vector signed short)( 5351, 7422, 6992, 6292, 5351, 6292, 6992, 7422 ),
    (vector signed short)( 5681, 7880, 7422, 6680, 5681, 6680, 7422, 7880 )
};

/***************************************************************
 *
 * Copyright:   (c) Copyright Motorola Inc. 1998
 *
 * Date:        April 17, 1998
 *
 * Function:    DCT
 *
 * Description: Scaled Chen (II) algorithm for DCT
 *              Arithmetic is 16-bit fixed point.
 *
 * Inputs:      input - Pointer to input data (short), which
 *                      must be between -255 to +255.
 *                      It is assumed that the allocated array
 *                      has been 128-bit aligned and contains
 *                      8x8 short elements.
 *
 * Outputs:     output - Pointer to output area for the transfored
 *                       data. The output values are between -2040
 *                       and 2040. It is assumed that a 128-bit
 *                       aligned 8x8 array of short has been
 *                       pre-allocated.
 *
 * Return:      None
 *
 ***************************************************************/

void dct_vector(short *input, short *output) {

  vector signed short t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;
  vector signed short a0, a1, a2, ma2, c4, mc4, zero;
  vector signed short vx[8], vy[8];
  vector signed short *vec_ptr;  /* used for conversion between
                                    arrays of short and vector
                                    signed short array.  */

  /* load the multiplication constants */
  c4   = (vector signed short)(23170);   /* c4 = cos(4*pi/16)  */
  a0   = (vector signed short)(13573);   /* a0 = c6/c2         */
  a1   = (vector signed short)(6518);    /* a1 = c7/c1         */
  a2   = (vector signed short)(21895);   /* a2 = c5/c3         */
  mc4   = (vector signed short)(-23170); /* -c4                */
  ma2   = (vector signed short)(-21895); /* -a2                */
  zero = (vector signed short)(0);       /* 0                  */

  /* copy the rows of input data */
  vec_ptr = ( vector signed short * ) input;
  vx[0] = vec_ptr[0];
  vx[1] = vec_ptr[1];
  vx[2] = vec_ptr[2];
  vx[3] = vec_ptr[3];
  vx[4] = vec_ptr[4];
  vx[5] = vec_ptr[5];
  vx[6] = vec_ptr[6];
  vx[7] = vec_ptr[7];

  /* Perform DCT first on the 8 columns */
  DCT_Transform( vx, vy );

  /* Transpose matrix to work on rows */
  Matrix_Transpose( vy, vx );

  /* Perform DCT first on the 8 rows */
  DCT_Transform( vx, vy );

  /* Post-scale and store result. */
  vec_ptr = (vector signed short *) output;

  vec_ptr[0] = vec_mradds( PostScale[0], vy[0], zero );
  vec_ptr[1] = vec_mradds( PostScale[1], vy[1], zero );
  vec_ptr[2] = vec_mradds( PostScale[2], vy[2], zero );
  vec_ptr[3] = vec_mradds( PostScale[3], vy[3], zero );
  vec_ptr[4] = vec_mradds( PostScale[4], vy[4], zero );
  vec_ptr[5] = vec_mradds( PostScale[5], vy[5], zero );
  vec_ptr[6] = vec_mradds( PostScale[6], vy[6], zero );
  vec_ptr[7] = vec_mradds( PostScale[7], vy[7], zero );

}
