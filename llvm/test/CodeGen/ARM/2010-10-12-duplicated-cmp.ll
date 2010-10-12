;; RUN: llc < %s -march=thumb | FileCheck %s
;; PR8361

; ModuleID = '<stdin>'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i386-unknown-linux-gnu"

@.str = private constant [2 x i8] c"<\00"
@.str1 = private constant [2 x i8] c"=\00"
@.str2 = private constant [2 x i8] c">\00"

define i8* @f(i32 %x, i32 %y) nounwind readnone {
entry:
;; CHECK: cmp r0, r1
  %cmp = icmp slt i32 %x, %y
;; CHECK: blt
  br i1 %cmp, label %cond.end7, label %cond.false

cond.false:                                       ; preds = %entry
;; CHECK: cmp r0, r1
  %cmp4 = icmp eq i32 %x, %y
;; CHECK: bne
  %cond = select i1 %cmp4, i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8]* @.str2, i32 0, i32 0)
  ret i8* %cond

cond.end7:                                        ; preds = %entry
  ret i8* getelementptr inbounds ([2 x i8]* @.str, i32 0, i32 0)
}
