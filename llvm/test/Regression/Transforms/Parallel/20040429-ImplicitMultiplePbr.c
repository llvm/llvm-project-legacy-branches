void* __llvm_pbr(void);
void __llvm_join(void*);

void bar(void);
void baz(void);
void quux(void);
void quux2(void);

void  foo() {
  void* p = __llvm_pbr();
  if (p)
    bar();
  else
    baz();
  __llvm_join(p);

  void *q = __llvm_pbr();
  if (q)
    quux();
  else
    quuux();

  __llvm_join(q);

}
