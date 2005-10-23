all:
	# Please specify altivec or sse!

altivec:
	make -C Saxpy altivec
	make -C InterQuant altivec
	make -C MADFilter altivec
	make -C RGB2YUV altivec
	make -C Transpose altivec
	make -C DCT altivec

sse:
	make -C Saxpy sse
	make -C InterQuant sse
	make -C MADFilter sse
# Note that RGB2YUV doesn't currently work on SSE because of 
# SSE's limited support for permute operations
	make -C Transpose sse
	make -C DCT sse

clean:
	make -C Saxpy clean
	make -C InterQuant clean
	make -C MADFilter clean
	make -C RGB2YUV clean
	make -C Transpose clean
	make -C DCT clean

