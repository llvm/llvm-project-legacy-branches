; RUN: llc < %s -march=amdil | FileCheck %s

define i32 @t1(i32 %x, i32 %y) {
; CHECK:	    ushr r65.x___, r1.x, r1.y
; CHECK-NEXT:	mov r1.x___, r65.x

	%z = lshr i32 %x, %y
	ret i32 %z
}

define i32 @t2(i32 %x) {
; CHECK:	    mov r65.x___, l12
; CHECK-NEXT:	ushr r65.x___, r1.x, r65.x
; CHECK-NEXT:	mov r1.x___, r65.x

	%z = lshr i32 %x, 3
	ret i32 %z
}

define i32 @t3(i32 %x) {
; CHECK:	    mov r65.x___, l12
; CHECK-NEXT:	ushr r65.x___, r65.x, r1.x
; CHECK-NEXT:	mov r1.x___, r65.x

	%z = lshr i32 3, %x
	ret i32 %z
}

define i32 @t4(i32 %x, i32 %y) {
; CHECK:	    ishr r65.x___, r1.x, r1.y
; CHECK-NEXT:	mov r1.x___, r65.x

	%z = ashr i32 %x, %y
	ret i32 %z
}

define i32 @t5(i32 %x) {
; CHECK:	    mov r65.x___, l12
; CHECK-NEXT:	ishr r65.x___, r1.x, r65.x

	%z = ashr i32 %x, 3
	ret i32 %z
}

define i32 @t6(i32 %x) {
; CHECK:	    mov r65.x___, l12
; CHECK-NEXT:	ishr r65.x___, r65.x, r1.x
; CHECK-NEXT:	mov r1.x___, r65.x

	%z = ashr i32 -3, %x
	ret i32 %z
}
