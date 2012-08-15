; RUN: llc < %s -march=amdil | FileCheck %s
; CHECK: il_cs_2_0
; CHECK-NEXT: dcl_cb cb0[15]
; CHECK-NEXT: dcl_literal l0, 0x00000004, 0x00000001, 0x00000002, 0x00000003
; CHECK-NEXT: dcl_literal l1, 0x00FFFFFF, 0xFFFFFFFF, 0xFFFFFFFE, 0xFFFFFFFD
; CHECK-NEXT: dcl_literal l2, 0x0000FFFF, 0xFFFFFFFE, 0x000000FF, 0xFFFFFFFC
; CHECK-NEXT: dcl_literal l3, 0x00000018, 0x00000010, 0x00000008, 0xFFFFFFFF
; CHECK-NEXT: dcl_literal l4, 0xFFFFFF00, 0xFFFF0000, 0xFF00FFFF, 0xFFFF00FF
; CHECK-NEXT: dcl_literal l5, 0x00000000, 0x00000004, 0x00000008, 0x0000000C
; CHECK-NEXT: dcl_literal l6, 0x00000020, 0x00000020, 0x00000020, 0x00000020
; CHECK-NEXT: dcl_literal l7, 0x00000018, 0x0000001F, 0x00000010, 0x0000001F
; CHECK-NEXT: dcl_literal l8, 0x80000000, 0x80000000, 0x80000000, 0x80000000

define i16 @t1_u16() {
; CHECK: mov r65.x___, l12
; CHECK: mov r1.x___, r65.x
; CHECK: ret_dyn
; CHECK: ret

	ret i16 0
}

define i32 @t1_u32() {
; CHECK: mov r65.x___, l12
; CHECK: mov r1.x___, r65.x
; CHECK: ret_dyn
; CHECK: ret

	ret i32 0
}

define i64 @t1_u64() {
; CHECK:	mov r65.xy__, l12
; CHECK:	mov r1.xy__, r65.xyxy
; CHECK:	ret_dyn
; CHECK:  ret

	ret i64 0
}

define float @t1_f32() {
; CHECK:	mov r65.x___, l12
; CHECK:	mov r1.x___, r65.x
; CHECK:	ret_dyn
; CHECK:  ret

	ret float 0.0
}

define double @t1_f64() {
; CHECK:	mov r65.xy__, l12
; CHECK:	mov r1.xy__, r65.xyxy
; CHECK:	ret_dyn
; CHECK:  ret

	ret double 0.0
}

define i16 @t2_u16(i16 %x) {
; CHECK:	ret_dyn
; CHECK:  ret

	ret i16 %x
}

define i32 @t2_u32(i32 %x) {
; CHECK:	ret_dyn
; CHECK:  ret

	ret i32 %x
}

define i64 @t2_u64(i64 %x) {
; CHECK:	ret_dyn
; CHECK:  ret

	ret i64 %x
}

define float @t3_f32(float %x) {
; CHECK:	ret_dyn
; CHECK:  ret

	ret float %x
}

define double @t3_f64(double %x) {
; CHECK:	ret_dyn
; CHECK:  ret

	ret double %x
}

