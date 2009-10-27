; RUN: opt < %s -inline -scalarrepl -S | FileCheck %s

define i32 @func(i32 %i) {
        ret i32 %i
}

define i32 @main(i32 %argc) {
        %X = call i32 @func( i32 7 )            ; <i32> [#uses=1]
        %Y = add i32 %X, %argc          ; <i32> [#uses=1]
        ret i32 %Y
}



; rdar://7339069

%T = type { i32, i32 }

; CHECK-NOT: @test2f
define internal %T* @test2f(i1 %cond, %T* %P) {
  br i1 %cond, label %T, label %F
  
T:
  %A = getelementptr %T* %P, i32 0, i32 0
  store i32 42, i32* %A
  ret %T* %P
  
F:
  ret %T* %P
}

define i32 @test2(i1 %cond) {
  %A = alloca %T
  
  %B = call %T* @test2f(i1 %cond, %T* %A)
  %C = getelementptr %T* %B, i32 0, i32 0
  %D = load i32* %C
  ret i32 %D
  
; CHECK: @test2(
; CHECK-NOT: = alloca
; CHECK: ret i32 42
}
