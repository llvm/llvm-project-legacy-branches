; RUN: llvm-as < %s > %t.bc
; RUN: llvm-as < %p/testlink2.ll > %t2.bc
; RUN: llvm-link %t.bc %t2.bc

%intlist = type { %intlist*, i32 }

@MyVar = external global i32
@MyIntList = global %intlist { %intlist* null, i32 17 }
@0 = external global i32
@Inte = global i32 1
@AConst = linkonce constant i32 123
@Intern1 = internal constant i32 42
@Intern2 = internal constant i32 792
@MyVarPtr = linkonce global { i32* } { i32* @MyVar }

declare i32 @foo(i32)

declare void @print(i32)

define void @main() {
  %v1 = load i32* @MyVar
  call void @print(i32 %v1)
  %idx = getelementptr %intlist* @MyIntList, i64 0, i32 1
  %v2 = load i32* %idx
  call void @print(i32 %v2)
  %1 = call i32 @foo(i32 5)
  %v3 = load i32* @MyVar
  call void @print(i32 %v3)
  %v4 = load i32* %idx
  call void @print(i32 %v4)
  ret void
}

define internal void @testintern() {
  ret void
}

define internal void @Testintern() {
  ret void
}

define void @testIntern() {
  ret void
}
