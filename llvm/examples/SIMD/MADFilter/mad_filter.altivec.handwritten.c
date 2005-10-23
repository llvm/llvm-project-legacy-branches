void experimental_filter_vector(short *left_ch, short *right_ch, unsigned n) {
  vector signed short *left_vp = (vector signed short*) left_ch;
  vector signed short *right_vp = (vector signed short*) right_ch;
  unsigned i;
  vector unsigned short two = (vector unsigned short) (2);

  for (i = 0; i < n/8; ++i) {
    vector signed short left = left_vp[i];
    vector signed short right = right_vp[i];
    vector signed short left_sub = vec_sub(left, vec_sra(right, two));
    vector signed short right_sub = vec_sub(right, vec_sra(left, two));
    right_vp[i] = right_sub;
    left_vp[i] = left_sub;
  }
}
