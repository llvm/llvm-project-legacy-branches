void rgb2yuv_vector(unsigned char *RGB_char_ptr, int RGB_size,
		    unsigned char *YCC_char_ptr) {

  vector unsigned char *RGB_ptr = (vector unsigned char*) RGB_char_ptr;
  vector unsigned char *YCC_ptr = (vector unsigned char*) YCC_char_ptr;

  vector signed short  r0, r1, r2, g0, g1, g2, b0, b1, b2, c0, c16, c128;
  vector unsigned char z0, tc0, tc1, tc2, tc3;
  vector signed short tr0, tr1, tg0, tg1, tb0, tb1, mask;
  vector signed short t0, t1, t2, t3, t4, t5;
  int i, j;

  vector unsigned char vPerm1 =
    (vector unsigned char)( 0,  3,  6,  9, 12, 15, 18, 21, /* R0..R7    */
                            1,  4,  7, 10, 13, 16, 19, 22  /* G0..G7    */);
  vector unsigned char vPerm2 =
    (vector unsigned char)( 2,  5,  8, 11, 14, 17, 20, 23, /* B0..B7    */
                            0,  0,  0,  0,  0,  0,  0,  0  /* dont care */);
  vector unsigned char vPerm3 =
    (vector unsigned char)( 8, 11, 14, 17, 20, 23, 26, 29, /* R8..R15   */
                            9, 12, 15, 18, 21, 24, 27, 30  /* G8..G15   */);
  vector unsigned char vPerm4 =
    (vector unsigned char)(10, 13, 16, 19, 22, 25, 28, 31, /* B8..B15   */
                            0,  0,  0,  0,  0,  0,  0,  0  /* dont care */);

  vector signed short vConst1 =
    (vector signed short)( 8432,  16425,  3176,
                          -4818,  -9527, 14345,
                              0,      0 );

  vector signed short vConst2 =
    (vector signed short)( 14345, -12045, -2300,
                              16, 128, 0, 0, 0 );

  r0 = vec_splat( vConst1, 0 ); /*  8432 */
  g0 = vec_splat( vConst1, 1 ); /* 16425 */
  b0 = vec_splat( vConst1, 2 ); /*  3176 */
  r1 = vec_splat( vConst1, 3 ); /* -4818 */
  g1 = vec_splat( vConst1, 4 ); /* -9527 */
  b1 = vec_splat( vConst1, 5 ); /* 14345 */
  r2 = vec_splat( vConst2, 0 ); /* 14345 */
  g2 = vec_splat( vConst2, 1 ); /*-12045 */
  b2 = vec_splat( vConst2, 2 ); /* -2300 */
  c16  = vec_splat( vConst2, 3 ); /*  16 */
  c128 = vec_splat( vConst2, 4 ); /* 128 */
  c0 = (vector signed short) (0); /*   0 */
  z0 = (vector unsigned char) (0); /*  0 */
  mask = (vector signed short) (0x00FF);

  vector unsigned char Ys;
  vector unsigned char Cbs;
  vector unsigned char Crs;

  for ( i = 0; i < (RGB_size/sizeof(vector unsigned char)); i+=3 ) {

    tc0 = vec_perm( RGB_ptr[i], RGB_ptr[i+1], vPerm1 );   /* R0..R7  G0..G7  */
    tc1 = vec_perm( RGB_ptr[i], RGB_ptr[i+1], vPerm2 );   /* B0..B7          */
    tc2 = vec_perm( RGB_ptr[i+1], RGB_ptr[i+2], vPerm3 ); /* R8..R15 G8..G15 */
    tc3 = vec_perm( RGB_ptr[i+1], RGB_ptr[i+2], vPerm4 ); /* B8..B15         */

    tr0 = vec_and(vec_unpackh( (vector signed char) tc0 ), mask);  /* tr0 = R0 .. R7  */
    tg0 = vec_and(vec_unpackl( (vector signed char) tc0 ), mask);  /* tg0 = G0 .. G7  */
    tb0 = vec_and(vec_unpackh( (vector signed char) tc1 ), mask);  /* tb0 = B0 .. B7  */
    tr1 = vec_and(vec_unpackh( (vector signed char) tc2 ), mask);  /* tr0 = R8 .. R15 */
    tg1 = vec_and(vec_unpackl( (vector signed char) tc2 ), mask);  /* tg0 = G8 .. G15 */
    tb1 = vec_and(vec_unpackh( (vector signed char) tc3 ), mask);  /* tb0 = B8 .. B15 */

    t0 = vec_mradds( tr0, r0, c0 ); /* (R0 .. R7) *  8432 */
    t1 = vec_mradds( tr0, r1, c0 ); /* (R0 .. R7) * -4818 */
    t2 = vec_mradds( tr0, r2, c0 ); /* (R0 .. R7) * 14345 */

    t0 = vec_mradds( tg0, g0, t0 ); /* += (G0 .. G7) *  16425 */
    t1 = vec_mradds( tg0, g1, t1 ); /* += (G0 .. G7) *  -9527 */
    t2 = vec_mradds( tg0, g2, t2 ); /* += (G0 .. G7) * -12045 */

    t0 = vec_mradds( tb0, b0, t0 ); /* += (B0 .. B7) *  3176 */
    t1 = vec_mradds( tb0, b1, t1 ); /* += (B0 .. B7) * 14345 */
    t2 = vec_mradds( tb0, b2, t2 ); /* += (B0 .. B7) * -2300 */

    /* Convert the next three input vectors. */
    t3 = vec_mradds( tr1, r0, c0 ); /* (R8 .. R15) *  8432 */
    t4 = vec_mradds( tr1, r1, c0 ); /* (R8 .. R15) * -4818 */
    t5 = vec_mradds( tr1, r2, c0 ); /* (R8 .. R15) * 14345 */

    t3 = vec_mradds( tg1, g0, t3 ); /* += (G8 .. G15) *  16425 */
    t4 = vec_mradds( tg1, g1, t4 ); /* += (G8 .. G15) *  -9527 */
    t5 = vec_mradds( tg1, g2, t5 ); /* += (G8 .. G15) * -12045 */

    t3 = vec_mradds( tb1, b0, t3 ); /* += (B8 .. B15) *  3176 */
    t4 = vec_mradds( tb1, b1, t4 ); /* += (B8 .. B15) * 14345 */
    t5 = vec_mradds( tb1, b2, t5 ); /* += (B8 .. B15) * -2300 */

    t0 = vec_adds( t0, c16 );
    t3 = vec_adds( t3, c16 );
    t1 = vec_adds( t1, c128 );
    t4 = vec_adds( t4, c128 );
    t2 = vec_adds( t2, c128 );
    t5 = vec_adds( t5, c128 );

    YCC_ptr[i] = vec_packsu( t0, t3 );  /*  Y0 .. Y15  */
    YCC_ptr[i+1] = vec_packsu( t1, t4 );  /* Cb0 .. Cb15 */
    YCC_ptr[i+2] = vec_packsu( t2, t5 );  /* Cr0 .. Cr15 */

  }
}

