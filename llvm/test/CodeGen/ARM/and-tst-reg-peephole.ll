; RUN: llc < %s -march=arm | FileCheck %s
; RUN: llc < %s -march=thumb | FileCheck -check-prefix=THUMB %s
; RUN: llc < %s -march=thumb -mattr=+thumb2 | FileCheck -check-prefix=T2 %s

%struct.Foo = type { i32 }

define i32 @foo(%struct.Foo* %this, i32 %mask) nounwind readonly align 2 {
entry:
  %ptr = getelementptr %struct.Foo* %this, i32 0, i32 0
	%my = load i32* %ptr

; CHECK:      tst r0, r1
; CHECK-NEXT: andne r0, r0, r1

; THUMB:      ands r0, r1
; THUMB-NEXT: tst r2, r1

; T2:      tst r0, r1
; T2-NEXT: bne

  %and = and i32 %my, %mask
  %tst = icmp eq i32 %and, 0
  br i1 %tst, label %sw.bb, label %sw.bb2

sw.bb:
  ret i32 255

sw.bb2:
; T2:      ands r0, r1
  ret i32 %and
}

