#ifndef THREADLIB_H
#define THREADLIB_H

int __llvm_thread_start(void*(*fn)(void*), void* arg);

void __llvm_thread_join(int thread_id);

#endif
