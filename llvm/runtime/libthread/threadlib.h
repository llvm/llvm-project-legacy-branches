#ifndef THREADLIB_H
#define THREADLIB_H

unsigned long __llvm_thread_start(void*(*fn)(void*), void* arg);

void __llvm_thread_join(unsigned long thread_id);

#endif
