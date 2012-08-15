il_cs_2_0
dcl_cb cb0[15] ; Constant buffer that holds ABI data
dcl_literal l0, 0x00000004, 0x00000001, 0x00000002, 0x00000003
dcl_literal l1, 0x00FFFFFF, 0xFFFFFFFF, 0xFFFFFFFE, 0xFFFFFFFD
dcl_literal l2, 0x0000FFFF, 0xFFFFFFFE, 0x000000FF, 0xFFFFFFFC
dcl_literal l3, 0x00000018, 0x00000010, 0x00000008, 0xFFFFFFFF
dcl_literal l4, 0xFFFFFF00, 0xFFFF0000, 0xFF00FFFF, 0xFFFF00FF
dcl_literal l5, 0x00000000, 0x00000004, 0x00000008, 0x0000000C
dcl_literal l6, 0x00000020, 0x00000020, 0x00000020, 0x00000020
dcl_literal l7, 0x00000018, 0x0000001F, 0x00000010, 0x0000001F
dcl_literal l8, 0x80000000, 0x80000000, 0x80000000, 0x80000000
;$$$$$$$$$$
endmain
;DEBUGSTART
	.file	"/home/voliveir/work/llvm-branch-2/test/CodeGen/AMDIL/sub.ll"
	.text
.global@t1_u16
;DEBUGEND
func 1024 ; t1_u16                      ; @t1_u16
; BB#0:
	mov r66.x___, r1.y
	mov r65.x___, l12
	ishl r66.x___, r66.x, r65.x
	ishr r65.x___, r66.x, r65.x
	mov r65.x___, r65.x
	inegate r65.x___, r65.x
	iadd r65.x___, r1.x, r65.x
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t1_u16
;ARGSTART:t1_u16
;uniqueid:1024
;ARGEND:t1_u16
;DEBUGSTART

.global@t1_u32
;DEBUGEND
func 1025 ; t1_u32                      ; @t1_u32
; BB#0:
	inegate r65.x___, r1.y
	iadd r65.x___, r1.x, r65.x
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t1_u32
;ARGSTART:t1_u32
;uniqueid:1025
;ARGEND:t1_u32
;DEBUGSTART

.global@t1_f32
;DEBUGEND
func 1026 ; t1_f32                      ; @t1_f32
; BB#0:
	sub r65.x___, r1.x, r1.y
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t1_f32
;ARGSTART:t1_f32
;uniqueid:1026
;ARGEND:t1_f32
;DEBUGSTART

.global@t2_u16
;DEBUGEND
func 1027 ; t2_u16                      ; @t2_u16
; BB#0:
	mov r65.x___, l12
	iadd r65.x___, r1.x, r65.x
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t2_u16
;ARGSTART:t2_u16
;uniqueid:1027
;ARGEND:t2_u16
;DEBUGSTART

.global@t2_u32
;DEBUGEND
func 1028 ; t2_u32                      ; @t2_u32
; BB#0:
	mov r65.x___, l12
	iadd r65.x___, r1.x, r65.x
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t2_u32
;ARGSTART:t2_u32
;uniqueid:1028
;ARGEND:t2_u32
;DEBUGSTART

.global@t2_u64
;DEBUGEND
func 1029 ; t2_u64                      ; @t2_u64
; BB#0:
	mov r66.xy__, l12
	mov r65.x___, r66.y000
	mov r67.x___, r1.y000
	iadd r65.x___, r67.x, r65.x
	mov r67.x___, r66.x000
	mov r66.x___, r1.x000
	iadd r66.x___, r66.x, r67.x
	ult r67.x___, r66.x, r67.x
	inegate r67.x___, r67.x
	iadd r65.x___, r65.x, r67.x
	iadd r65.xy__, r66.x000, r65.0x00
	mov r1.xy__, r65.xyxy
	ret_dyn
ret
endfunc ; t2_u64
;ARGSTART:t2_u64
;uniqueid:1029
;ARGEND:t2_u64
;DEBUGSTART

.global@t2_f32
;DEBUGEND
func 1030 ; t2_f32                      ; @t2_f32
; BB#0:
	mov r65.x___, l12
	add r65.x___, r1.x, r65.x
	mov r1.x___, r65.x
	ret_dyn
ret
endfunc ; t2_f32
;ARGSTART:t2_f32
;uniqueid:1030
;ARGEND:t2_f32
;DEBUGSTART

;DEBUGEND

end
