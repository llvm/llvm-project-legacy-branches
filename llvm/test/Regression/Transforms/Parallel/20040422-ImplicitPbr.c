void* __llvm_pbr(void);
void __llvm_join(void*);

void bar(void);
void quux(void);

void  foo() {
  void* p = __llvm_pbr();
  if (p)
    bar();
  else
    quux();

  __llvm_join(p);

}
