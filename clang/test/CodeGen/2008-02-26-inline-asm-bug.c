// RUN: clang-cc -emit-llvm < %s | grep "\$0,\$1"

void f() {
  int d1, d2;
  asm("%0,%1": "=r" (d1) : "r" (d2));
}
