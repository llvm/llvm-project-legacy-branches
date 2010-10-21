; RUN: llc < %s -march=arm | FileCheck %s
; RUN: llc < %s -march=thumb | FileCheck -check-prefix=THUMB %s
; RUN: llc < %s -march=thumb -mattr=+thumb2 | FileCheck -check-prefix=T2 %s

%struct.Foo = type { i32 }

define i32 @foo(%struct.Foo* %this, i32 %mask) nounwind readonly align 2 {
entry:
  %ptr = getelementptr %struct.Foo* %this, i32 0, i32 0
	%my = load i32* %ptr

; CHECK:      ldr r0
; CHECK-NEXT: ands r0, r0, r1
; CHECK-NEXT: moveq r0, #255

; THUMB:      ldr r0
; THUMB-NEXT: ands r0, r1
; THUMB-NEXT: bne

; T2:      ldr r0
; T2-NEXT: ands r0, r1
; T2-NEXT: it eq
; T2-NEXT: moveq r0, #255
; T2-NEXT: bx lr

  %and = and i32 %my, %mask
  %tst = icmp eq i32 %and, 0
  br i1 %tst, label %sw.bb, label %sw.bb2

sw.bb:
  ret i32 255

sw.bb2:
  ret i32 %and
}

