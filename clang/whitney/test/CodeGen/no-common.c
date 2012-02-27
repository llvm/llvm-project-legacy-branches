// RUN: %clang -emit-llvm -S -o %t %s
// RUN: grep '@x = common global' %t
// RUN: %clang -fno-common -emit-llvm -S -o %t %s
// RUN: grep '@x = global' %t

int x;

// CHECK-DEFAULT: @ABC = global
// CHECK-NOCOMMON: @ABC = global
typedef void* (*fn_t)(long a, long b, char *f, int c);
fn_t ABC __attribute__ ((nocommon));

// CHECK-DEFAULT: @y = common global
// CHECK-NOCOMMON: @y = common global
int y __attribute__((common));