/***************************************************************
 *
 * Copyright:   (c) Copyright Motorola Inc. 1998
 *
 * Date:        May 18, 1998
 *
 * Function:    INTER_Quantization
 *
 * Description: The INTER_QUANTIZATION routine will quantize
 *              the predictive frames (P-picture). Coefficients
 *              are quantized to the formula:
 *                 C' = sign(C) * ( abs(C) - QP/2 ) / ( 2 * QP ).
 *              To ensure ( abs(C) - QP/2 ) is positive, saturating
 *              unsigned subtraction is used.
 *
 * Inputs:      input - Pointer to input data (short), which
 *                      must be between -2040 and 2040 (as set
 *                      up by DCT ). It is assumed that the allocated
 *                      array has been 128-bit aligned and contains
 *                      8x8 short elements.
 *
 * Outputs:     output - Pointer to output area for the transfored
 *                       data. The output values are between -127
 *                       and 127. It is assumed that a 128-bit
 *                       aligned 8x8 array of signed char has been
 *                       pre-allocated.
 *
 * QP:          QP (quantization parameter?) ranges from 1 to 31
 *
 **************************************************************/


#define INTER_CALC( input, output ) \
     t1 = vec_subs( zero, input);\
     u1 = (vector unsigned short ) vec_max( input, t1 );     /*  ( abs(C))   */ \
     t2 = (vector signed short ) vec_subs( u1, qpd2 );/*max(0,(abs(C)-QP/2)) */ \
     t3 = vec_madds( t2, dtqp.v, zero );   /* ( (abs(C)-QP/2)/(2*QP) )>>15 ) */ \
     t4 = vec_min(maxq,t3);                /* peg value at 127 if greater    */ \
     msk = vec_cmpgt( zero, input );       /* select to find sign of input   */ \
     t5 = vec_subs( zero, t4 );\
     output = vec_sel( t4, t5, msk );      /* ensure result is same sign     */

void interquant_vector ( signed short* in,
			 signed char* out,
			 int QP )
{
  vector signed short* input = (vector signed short*) in;
  vector signed char* output = (vector signed char*) out;

     /* ensure alignment so calculated constant can be
        propagated into entire vector for calculations */
     union{
        vector signed short v;
        signed short s[8];
     } dtqp;

     vector signed short zero, minus1, maxq, parta, partb;
     vector signed short t1, t2, t3, t4, t5; /* used in macros */
     vector unsigned short qpd2, u1;
     vector bool short msk;

     /* load the calculated constant into the vector */
     dtqp.s[0] = (signed short)((int)((32768+QP)/(2*QP)));
     dtqp.s[1] = (signed short)(QP/2);
     qpd2 = (vector unsigned short) vec_splat( dtqp.v, 1);
     dtqp.v = vec_splat( dtqp.v, 0 );

     /* load the static constants used in the macros */
     zero = (vector signed short) (0);
     maxq = (vector signed short) (127);
     minus1 = (vector signed short) (-1);

     /* for all input compute: C' = sign(C) * ( (abs(C)-(QP/2) ) / 2*QP ) */
     INTER_CALC( input[0], parta );
     INTER_CALC( input[1], partb );
     output[0] = vec_pack( parta, partb );

     INTER_CALC( input[2], parta );
     INTER_CALC( input[3], partb );
     output[1] = vec_pack( parta, partb );

     INTER_CALC( input[4], parta );
     INTER_CALC( input[5], partb );
     output[2] = vec_pack( parta, partb );

     INTER_CALC( input[6], parta );
     INTER_CALC( input[7], partb );
     output[3] = vec_pack( parta, partb );

}

