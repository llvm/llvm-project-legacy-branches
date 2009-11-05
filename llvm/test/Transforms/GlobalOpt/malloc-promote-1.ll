; RUN: opt < %s -globalopt -S | FileCheck %s

@G = internal global i32* null          ; <i32**> [#uses=3]
; CHECK-NOT: global

define void @init() {
        %malloccall = tail call i8* @malloc(i64 4)      ; <i8*> [#uses=1]
        %P = bitcast i8* %malloccall to i32*            ; <i32*> [#uses=1]
        store i32* %P, i32** @G
        %GV = load i32** @G             ; <i32*> [#uses=1]
        store i32 0, i32* %GV
        ret void
}

declare noalias i8* @malloc(i64)

define i32 @get() {
        %GV = load i32** @G             ; <i32*> [#uses=1]
        %V = load i32* %GV              ; <i32> [#uses=1]
        ret i32 %V
; CHECK: ret i32 0
}

