SIMD Examples
=============

Rob Bocchino
October 23, 2005

This directory illustrates the enhanced support for SIMD operations
provided by Vector LLVM.  It provides several benchmarks handcoded in
Vector C, AltiVec-C, and SSE-C, together with a build environment for
running and timing all three versions.  Except for RGB2YUV (which
works only for AltiVec, because of SSE's limited support for permute
operations), the same Vector C version compiles to both AltiVec and
SSE.

To run the benchmarks on AltiVec, type make altivec in this directory.
To run the benchmarks on SSE, type make sse.  To run the benchmarks on
SSE, your platform must support SSE2.

For more information on Vector C and the AltiVec and SSE C backends,
see the documents VectorCReference.txt and SIMDCReference.txt in the
directory ../../docs.
