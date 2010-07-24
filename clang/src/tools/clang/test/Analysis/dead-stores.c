// RUN: %clang_cc1 -Wunused-variable -analyze -analyzer-experimental-internal-checks -analyzer-check-objc-mem -analyzer-check-dead-stores -fblocks -verify -Wno-unreachable-code -analyzer-opt-analyze-nested-blocks %s
// RUN: %clang_cc1 -Wunused-variable -analyze -analyzer-experimental-internal-checks -analyzer-check-objc-mem -analyzer-store=basic -analyzer-constraints=basic -analyzer-check-dead-stores -fblocks -verify -Wno-unreachable-code -analyzer-opt-analyze-nested-blocks %s
// RUN: %clang_cc1 -Wunused-variable -analyze -analyzer-experimental-internal-checks -analyzer-check-objc-mem -analyzer-store=basic -analyzer-constraints=range -analyzer-check-dead-stores -fblocks -verify -Wno-unreachable-code -analyzer-opt-analyze-nested-blocks %s
// RUN: %clang_cc1 -Wunused-variable -analyze -analyzer-experimental-internal-checks -analyzer-check-objc-mem -analyzer-store=region -analyzer-constraints=basic -analyzer-check-dead-stores -fblocks -verify -Wno-unreachable-code -analyzer-opt-analyze-nested-blocks %s
// RUN: %clang_cc1 -Wunused-variable -analyze -analyzer-experimental-internal-checks -analyzer-check-objc-mem -analyzer-store=region -analyzer-constraints=range -analyzer-check-dead-stores -fblocks -verify -Wno-unreachable-code -analyzer-opt-analyze-nested-blocks %s

void f1() {
  int k, y; // expected-warning{{unused variable 'k'}} expected-warning{{unused variable 'y'}}
  int abc=1;
  long idx=abc+3*5; // expected-warning {{never read}} expected-warning{{unused variable 'idx'}}
}

void f2(void *b) {
 char *c = (char*)b; // no-warning
 char *d = b+1; // expected-warning {{never read}} expected-warning{{unused variable 'd'}}
 printf("%s", c); // expected-warning{{implicitly declaring C library function 'printf' with type 'int (char const *, ...)'}} \
 // expected-note{{please include the header <stdio.h> or explicitly provide a declaration for 'printf'}}
}

int f();

void f3() {
  int r;
  if ((r = f()) != 0) { // no-warning
    int y = r; // no-warning
    printf("the error is: %d\n", y);
  }
}

void f4(int k) {
  
  k = 1;
  
  if (k)
    f1();
    
  k = 2;  // expected-warning {{never read}}
}
  
void f5() {

  int x = 4; // no-warning
  int *p = &x; // expected-warning{{never read}} expected-warning{{unused variable 'p'}}

}

int f6() {
  
  int x = 4;
  ++x; // expected-warning{{never read}}
  return 1;
}

int f7(int *p) {  
  // This is allowed for defensive programming.
  p = 0; // no-warning  
  return 1;
}

int f7b(int *p) {  
  // This is allowed for defensive programming.
  p = (0); // no-warning  
  return 1;
}

int f7c(int *p) {  
  // This is allowed for defensive programming.
  p = (void*) 0; // no-warning  
  return 1;
}

int f7d(int *p) {  
  // This is allowed for defensive programming.
  p = (void*) (0); // no-warning  
  return 1;
}

int f8(int *p) {
  extern int *baz();
  if ((p = baz())) // expected-warning{{Although the value}}
    return 1;
  return 0;
}

int f9() {
  int x = 4;
  x = x + 10; // expected-warning{{never read}}
  return 1;
}

int f10() {
  int x = 4;
  x = 10 + x; // expected-warning{{never read}}
  return 1;
}

int f11() {
  int x = 4;
  return x++; // expected-warning{{never read}}
}

int f11b() {
  int x = 4;
  return ((((++x)))); // no-warning
}

int f12a(int y) {
  int x = y;  // expected-warning{{unused variable 'x'}}
  return 1;
}
int f12b(int y) {
  int x __attribute__((unused)) = y;  // no-warning
  return 1;
}
int f12c(int y) {
  // Allow initialiation of scalar variables by parameters as a form of
  // defensive programming.
  int x = y;  // no-warning
  x = 1;
  return x;
}

// Filed with PR 2630.  This code should produce no warnings.
int f13(void)
{
  int a = 1;
  int b, c = b = a + a;

  if (b > 0)
    return (0);

  return (a + b + c);
}

// Filed with PR 2763.
int f14(int count) {
  int index, nextLineIndex;
  for (index = 0; index < count; index = nextLineIndex+1) {
    nextLineIndex = index+1;  // no-warning
    continue;
  }
  return index;
}

// Test case for <rdar://problem/6248086>
void f15(unsigned x, unsigned y) {
  int count = x * y;   // no-warning
  int z[count]; // expected-warning{{unused variable 'z'}}
}

int f16(int x) {
  x = x * 2;
  x = sizeof(int [x = (x || x + 1) * 2]) // expected-warning{{Although the value stored to 'x' is used}}
      ? 5 : 8;
  return x;
}

// Self-assignments should not be flagged as dead stores.
void f17() {
  int x = 1;
  x = x; // expected-warning{{idempotent operation}}
}

// <rdar://problem/6506065>
// The values of dead stores are only "consumed" in an enclosing expression
// what that value is actually used.  In other words, don't say "Although the
// value stored to 'x' is used...".
int f18() {
   int x = 0; // no-warning
   if (1)
      x = 10;  // expected-warning{{Value stored to 'x' is never read}}
   while (1)
      x = 10;  // expected-warning{{Value stored to 'x' is never read}}
   do
      x = 10;   // expected-warning{{Value stored to 'x' is never read}}
   while (1);

   return (x = 10); // expected-warning{{Although the value stored to 'x' is used in the enclosing expression, the value is never actually read from 'x'}}
}

// PR 3514: false positive `dead initialization` warning for init to global
//  http://llvm.org/bugs/show_bug.cgi?id=3514
extern const int MyConstant;
int f19(void) {
  int x = MyConstant;  // no-warning
  x = 1;
  return x;
}

int f19b(void) { // This case is the same as f19.
  const int MyConstant = 0;
  int x = MyConstant; // no-warning
  x = 1;
  return x;  
}

void f20(void) {
  int x = 1; // no-warning
#pragma unused(x)
}

void halt() __attribute__((noreturn));
int f21() {
  int x = 4;
  
  ++x; // expected-warning{{never read}}
  if (1) {
    halt();
    (void)x;
  }
  return 1;
}

int j;
void f22() {
  int x = 4;
  int y1 = 4;
  int y2 = 4;
  int y3 = 4;
  int y4 = 4;
  int y5 = 4;
  int y6 = 4;
  int y7 = 4;
  int y8 = 4;
  int y9 = 4;
  int y10 = 4;
  int y11 = 4;
  int y12 = 4;
  int y13 = 4;
  int y14 = 4;
  int y15 = 4;
  int y16 = 4;
  int y17 = 4;
  int y18 = 4;
  int y19 = 4;
  int y20 = 4;

  ++x; // expected-warning{{never read}}
  ++y1;
  ++y2;
  ++y3;
  ++y4;
  ++y5;
  ++y6;
  ++y7;
  ++y8;
  ++y9;
  ++y10;
  ++y11;
  ++y12;
  ++y13;
  ++y14;
  ++y15;
  ++y16;
  ++y17;
  ++y18;
  ++y19;
  ++y20;

  switch (j) {
  case 1:
    if (0)
      (void)x;
    if (1) {
      (void)y1;
      return;
    }
    (void)x;
    break;
  case 2:
    if (0)
      (void)x;
    else {
      (void)y2;
      return;
    }
    (void)x;
    break;
  case 3:
    if (1) {
      (void)y3;
      return;
    } else
      (void)x;
    (void)x;
  break;
  case 4:
    0 ? : ((void)y4, ({ return; }));
    (void)x;
    break;
  case 5:
    1 ? : (void)x;
    0 ? (void)x : ((void)y5, ({ return; }));
    (void)x;
    break;
  case 6:
    1 ? ((void)y6, ({ return; })) : (void)x;
    (void)x;
    break;
  case 7:
    (void)(0 && x);
    (void)y7;
    (void)(0 || (y8, ({ return; }), 1));  // expected-warning {{expression result unused}}
    (void)x;
    break;
  case 8:
    (void)(1 && (y9, ({ return; }), 1));  // expected-warning {{expression result unused}}
    (void)x;
    break;
  case 9:
    (void)(1 || x);
    (void)y10;
    break;
  case 10:
    while (0) {
      (void)x;
    }
    (void)y11;
    break;
  case 11:
    while (1) {
      (void)y12;
    }
    (void)x;
    break;
  case 12:
    do {
      (void)y13;
    } while (0);
    (void)y14;
    break;
  case 13:
    do {
      (void)y15;
    } while (1);
    (void)x;
    break;
  case 14:
    for (;;) {
      (void)y16;
    }
    (void)x;    
    break;
  case 15:
    for (;1;) {
      (void)y17;
    }
    (void)x;
    break;
  case 16:
    for (;0;) {
      (void)x;
    }
    (void)y18;
    break;
  case 17:
    __builtin_choose_expr(0, (void)x, ((void)y19, ({ return; })));
    (void)x;
    break;
  case 19:
    __builtin_choose_expr(1, ((void)y20, ({ return; })), (void)x);
    (void)x;
    break;
  }
}

void f23_aux(const char* s);
void f23(int argc, char **argv) {
  int shouldLog = (argc > 1); // no-warning
  ^{ 
     if (shouldLog) f23_aux("I did too use it!\n");
     else f23_aux("I shouldn't log.  Wait.. d'oh!\n");
  }();
}

void f23_pos(int argc, char **argv) {
  int shouldLog = (argc > 1); // expected-warning{{Value stored to 'shouldLog' during its initialization is never read}} expected-warning{{unused variable 'shouldLog'}}
  ^{ 
     f23_aux("I did too use it!\n");
  }();  
}

void f24_A(int y) {
  // FIXME: One day this should be reported as dead since 'z = x + y' is dead.
  int x = (y > 2); // no-warning
  ^ {
      int z = x + y; // expected-warning{{Value stored to 'z' during its initialization is never read}} expected-warning{{unused variable 'z'}}
  }();  
}

void f24_B(int y) {
  // FIXME: One day this should be reported as dead since 'x' is just overwritten.
  __block int x = (y > 2); // no-warning
  ^{
    // FIXME: This should eventually be a dead store since it is never read either.
    x = 5; // no-warning
  }();
}

int f24_C(int y) {
  // FIXME: One day this should be reported as dead since 'x' is just overwritten.
  __block int x = (y > 2); // no-warning
  ^{ 
    x = 5; // no-warning
  }();
  return x;
}

int f24_D(int y) {
  __block int x = (y > 2); // no-warning
  ^{ 
    if (y > 4)
      x = 5; // no-warning
  }();
  return x;
}

// This example shows that writing to a variable captured by a block means that it might
// not be dead.
int f25(int y) {
  __block int x = (y > 2);
  __block int z = 0;
  void (^foo)() = ^{ z = x + y; };
  x = 4; // no-warning
  foo();
  return z; 
}

// This test is mostly the same as 'f25', but shows that the heuristic of pruning out dead
// stores for variables that are just marked '__block' is overly conservative.
int f25_b(int y) {
  // FIXME: we should eventually report a dead store here.
  __block int x = (y > 2);
  __block int z = 0;
  x = 4; // no-warning
  return z; 
}

int f26_nestedblocks() {
  int z;
  z = 1;
  __block int y = 0;
  ^{
    int k;
    k = 1; // expected-warning{{Value stored to 'k' is never read}}
    ^{
        y = z + 1;
     }();
  }();
  return y;
}

// The FOREACH macro in QT uses 'break' statements within statement expressions
// placed within the increment code of for loops.
void rdar8014335() {
  for (int i = 0 ; i != 10 ; ({ break; })) {
    for ( ; ; ({ ++i; break; })) ;
    // Note that the next value stored to 'i' is never executed
    // because the next statement to be executed is the 'break'
    // in the increment code of the first loop.
    i = i * 3; // expected-warning{{Value stored to 'i' is never read}} expected-warning{{idempotent operation}}
  }
}

