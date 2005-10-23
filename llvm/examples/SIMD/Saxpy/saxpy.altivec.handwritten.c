void saxpy_vector (vector short *z, const vector short *x, const vector short *y,
		   short a, unsigned n) {
  unsigned i;
  vector short a_vec;
  *((short*) &a_vec) = a;
  a_vec = vec_splat(a_vec, 0);

  for (i = 0; i < n/8; ++i)
    *z++ = vec_mladd(a_vec, *x++, *y++);

}

