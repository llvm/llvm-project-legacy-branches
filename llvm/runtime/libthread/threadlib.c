#include "threadlib.h"
#include <pthread.h>

unsigned long __llvm_thread_start(void*(*fn)(void*), void* arg) {
  pthread_t pt;
  pthread_create(&pt, 0, fn, arg);
  return pt;
}

void __llvm_thread_join(unsigned long thread_id) {
  pthread_join(thread_id, 0);
}


