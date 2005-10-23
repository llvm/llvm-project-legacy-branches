#ifndef MEMALIGN
#error MEMALIGN must be defined on compiler command line!
#endif

#if MEMALIGN
#include <malloc.h>
#endif

void *_malloc(unsigned int len) {
#if MEMALIGN
  return memalign(16, len);
#else
  return malloc(len);
#endif
}
