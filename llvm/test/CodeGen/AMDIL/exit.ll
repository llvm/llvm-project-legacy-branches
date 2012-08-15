; RUN: llc < %s -march=amdil | FileCheck %s

define void @t1(i32* %p, i32 %x) {
; CHECK:	    mov r1011, r1.y
; CHECK-NEXT:	mov r1010.x___, r1.x
; CHECK-NEXT:	ishr r1007.x___, r1010.x, l12
; CHECK-NEXT:	iand r1008.x___, r1007.x, l13
; CHECK-NEXT:	ishr r1007.x___, r1007.x, l12
; CHECK-NEXT:	switch r1008.x
; CHECK-NEXT:	default
; CHECK-NEXT:	mov x1[r1007.x].x___, r1011.x
; CHECK-NEXT:	break
; CHECK-NEXT:	case 1
; CHECK-NEXT:	mov x1[r1007.x]._y__, r1011.x
; CHECK-NEXT:	break
; CHECK-NEXT:	case 2
; CHECK-NEXT:	mov x1[r1007.x].__z_, r1011.x
; CHECK-NEXT:	break
; CHECK-NEXT:	case 3
; CHECK-NEXT:	mov x1[r1007.x].___w, r1011.x
; CHECK-NEXT:	break
; CHECK-NEXT:	endswitch

  store i32 %x, i32* %p
  ret void
}
