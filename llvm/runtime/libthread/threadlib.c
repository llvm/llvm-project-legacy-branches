#include "threadlib.h"
#include <pthread.h>

int __llvm_thread_start(void*(*fn)(void*), void* arg) {
  pthread_t pt;
  return pthread_create(&pt, 0, fn, arg);
}

void __llvm_thread_join(int thread_id) {
  pthread_join(thread_id, 0);
}


