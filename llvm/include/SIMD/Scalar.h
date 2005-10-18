#ifndef SCALAR_H
#define SCALAR_H

// Scalar versions of common SIMD operations

#define saturate_short(x) (((x) < -32768) ? -32768 : (((x) > 32767) ? 32767 : (x)))
#define adds_short(x, y) saturate_short(x+y)
#define subs_short(x, y) saturate_short(x-y)
#define mradds_short(x, y, z) saturate_short((((x*y) + (1<<14))>>15)+z)

// Access to scalar elements

#define elt_short(vec,i) ((short*) &vec)[i]

#endif
