; RUN: opt -std-compile-opts < %s | llc -march=amdil | FileCheck %s
target triple = "amdil-pc-amdopencl"

define i32 @get_global_id(i32) nounwind {
return:
  ret i32 1
}

define  void @ubit_opt_case_0(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_0
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 16
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 255
  %tmp11 = shl i32 %tmp10, 8
  %tmp12 = or i32 %tmp7, %tmp11
  store i32 %tmp12, i32* %d, align 4
  %tmp13 = load i32 addrspace(1)** %a.addr, align 4
  %tmp14 = load i32* %idx, align 4
  %arrayidx15 = getelementptr i32 addrspace(1)* %tmp13, i32 %tmp14
  %tmp16 = load i32* %d, align 4
  store i32 %tmp16, i32 addrspace(1)* %arrayidx15, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}


define  void @ubit_opt_case_1(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_1
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 16711680
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 255
  %tmp10 = shl i32 %tmp9, 8
  %tmp11 = or i32 %tmp6, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_2(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_2
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 17
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 65280
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_3(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_3
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 17
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = shl i32 %tmp9, 25
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_4(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_4
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = shl i32 %tmp5, 17
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 4080
  %tmp10 = shl i32 %tmp9, 5
  %tmp11 = or i32 %tmp6, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_5(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_5
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 65280
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 255
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_6(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_6
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 65280
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = shl i32 %tmp8, 16
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_7(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_7
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = shl i32 %tmp5, 9
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 255
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_8(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_8
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 8
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, -65281
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_9(i32 addrspace(1)* %greal, <4 x i32> addrspace(1)* %gimag) nounwind {
  ; CHECK: ubit_opt_case_9
  ; CHECK: bfi
entry:
  %greal.addr = alloca i32 addrspace(1)*, align 4
  %gimag.addr = alloca <4 x i32> addrspace(1)*, align 4
  %tmp = alloca <4 x i32>, align 16
  %gid = alloca i32, align 4
  store i32 addrspace(1)* %greal, i32 addrspace(1)** %greal.addr, align 4
  store <4 x i32> addrspace(1)* %gimag, <4 x i32> addrspace(1)** %gimag.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %gid, align 4
  %tmp1 = load <4 x i32> addrspace(1)** %gimag.addr, align 4
  %tmp2 = load i32* %gid, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp1, i32 %tmp2
  %tmp3 = load i32* %gid, align 4
  %tmp4 = lshr i32 %tmp3, 6
  %tmp5 = mul i32 %tmp4, 1024
  %tmp6 = load i32* %gid, align 4
  %tmp7 = and i32 %tmp6, 252
  %tmp8 = add i32 %tmp5, %tmp7
  %conv = insertelement <4 x i32> undef, i32 %tmp8, i32 0
  %conv9 = insertelement <4 x i32> %conv, i32 %tmp8, i32 1
  %conv10 = insertelement <4 x i32> %conv9, i32 %tmp8, i32 2
  %conv11 = insertelement <4 x i32> %conv10, i32 %tmp8, i32 3
  store <4 x i32> %conv11, <4 x i32>* %tmp, align 16
  %tmp12 = load <4 x i32>* %tmp, align 16
  store <4 x i32> %tmp12, <4 x i32> addrspace(1)* %arrayidx, align 16
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_10(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_10
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = shl i32 %tmp5, 10
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 63
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_11(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_11
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 896
  %tmp7 = shl i32 %tmp6, 4
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 7
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_12(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_12
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 7
  %tmp7 = shl i32 %tmp6, 7
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 56
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_13(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_13
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 63
  %tmp7 = shl i32 %tmp6, 1
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = shl i32 %tmp9, 10
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_14(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_14
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load i32 addrspace(1)** %a.addr, align 4
  %tmp4 = load i32* %idx, align 4
  %arrayidx5 = getelementptr i32 addrspace(1)* %tmp3, i32 %tmp4
  %tmp6 = load <4 x i32>* %c, align 16
  %tmp7 = extractelement <4 x i32> %tmp6, i32 0
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 3
  %tmp10 = and i32 %tmp7, %tmp9
  %tmp11 = load <4 x i32>* %c, align 16
  %tmp12 = extractelement <4 x i32> %tmp11, i32 1
  %tmp13 = load <4 x i32>* %c, align 16
  %tmp14 = extractelement <4 x i32> %tmp13, i32 3
  %tmp15 = xor i32 %tmp14, -1
  %tmp16 = and i32 %tmp12, %tmp15
  %tmp17 = or i32 %tmp10, %tmp16
  store i32 %tmp17, i32 addrspace(1)* %arrayidx5, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_15(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_15
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load i32 addrspace(1)** %a.addr, align 4
  %tmp4 = load i32* %idx, align 4
  %arrayidx5 = getelementptr i32 addrspace(1)* %tmp3, i32 %tmp4
  %tmp6 = load <4 x i32>* %c, align 16
  %tmp7 = extractelement <4 x i32> %tmp6, i32 0
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 3
  %tmp10 = and i32 %tmp7, %tmp9
  %tmp11 = load <4 x i32>* %c, align 16
  %tmp12 = extractelement <4 x i32> %tmp11, i32 1
  %tmp13 = load <4 x i32>* %c, align 16
  %tmp14 = extractelement <4 x i32> %tmp13, i32 3
  %tmp15 = xor i32 %tmp14, -1
  %tmp16 = and i32 %tmp12, %tmp15
  %tmp17 = or i32 %tmp10, %tmp16
  store i32 %tmp17, i32 addrspace(1)* %arrayidx5, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_16(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_16
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load i32 addrspace(1)** %a.addr, align 4
  %tmp4 = load i32* %idx, align 4
  %arrayidx5 = getelementptr i32 addrspace(1)* %tmp3, i32 %tmp4
  %tmp6 = load <4 x i32>* %c, align 16
  %tmp7 = extractelement <4 x i32> %tmp6, i32 3
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = load <4 x i32>* %c, align 16
  %tmp11 = extractelement <4 x i32> %tmp10, i32 1
  %tmp12 = load <4 x i32>* %c, align 16
  %tmp13 = extractelement <4 x i32> %tmp12, i32 3
  %tmp14 = xor i32 %tmp11, %tmp13
  %tmp15 = and i32 %tmp9, %tmp14
  %tmp16 = xor i32 %tmp7, %tmp15
  store i32 %tmp16, i32 addrspace(1)* %arrayidx5, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @bfm_opt_case_0(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: bfm_opt_case_0
  ; CHECK: bfm
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load i32 addrspace(1)** %a.addr, align 4
  %tmp4 = load i32* %idx, align 4
  %arrayidx5 = getelementptr i32 addrspace(1)* %tmp3, i32 %tmp4
  %tmp6 = load <4 x i32>* %c, align 16
  %tmp7 = extractelement <4 x i32> %tmp6, i32 0
  %tmp8 = and i32 %tmp7, 31
  %tmp9 = and i32 %tmp8, 31
  %tmp10 = shl i32 1, %tmp9
  %tmp11 = sub nsw i32 %tmp10, 1
  %tmp12 = load <4 x i32>* %c, align 16
  %tmp13 = extractelement <4 x i32> %tmp12, i32 1
  %tmp14 = and i32 %tmp13, 31
  %tmp15 = and i32 %tmp14, 31
  %tmp16 = shl i32 %tmp11, %tmp15
  store i32 %tmp16, i32 addrspace(1)* %arrayidx5, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_opt_case_17(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_opt_case_17
  ; CHECK: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load i32 addrspace(1)** %a.addr, align 4
  %tmp4 = load i32* %idx, align 4
  %arrayidx5 = getelementptr i32 addrspace(1)* %tmp3, i32 %tmp4
  %tmp6 = load <4 x i32>* %c, align 16
  %tmp7 = extractelement <4 x i32> %tmp6, i32 0
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = load <4 x i32>* %c, align 16
  %tmp11 = extractelement <4 x i32> %tmp10, i32 1
  %tmp12 = xor i32 %tmp9, %tmp11
  %tmp13 = load <4 x i32>* %c, align 16
  %tmp14 = extractelement <4 x i32> %tmp13, i32 2
  %tmp15 = and i32 %tmp12, %tmp14
  %tmp16 = xor i32 %tmp7, %tmp15
  store i32 %tmp16, i32 addrspace(1)* %arrayidx5, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_0(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_0
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 16
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 255
  %tmp11 = shl i32 %tmp10, 9
  %tmp12 = or i32 %tmp7, %tmp11
  store i32 %tmp12, i32* %d, align 4
  %tmp13 = load i32 addrspace(1)** %a.addr, align 4
  %tmp14 = load i32* %idx, align 4
  %arrayidx15 = getelementptr i32 addrspace(1)* %tmp13, i32 %tmp14
  %tmp16 = load i32* %d, align 4
  store i32 %tmp16, i32 addrspace(1)* %arrayidx15, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_1(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_1
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 16744448
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 255
  %tmp10 = shl i32 %tmp9, 8
  %tmp11 = or i32 %tmp6, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_2(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_2
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 15
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 65280
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_3(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_3
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 255
  %tmp7 = shl i32 %tmp6, 17
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = shl i32 %tmp9, 22
  %tmp11 = or i32 %tmp7, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_4(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_4
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = shl i32 %tmp5, 15
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 4080
  %tmp10 = shl i32 %tmp9, 5
  %tmp11 = or i32 %tmp6, %tmp10
  store i32 %tmp11, i32* %d, align 4
  %tmp12 = load i32 addrspace(1)** %a.addr, align 4
  %tmp13 = load i32* %idx, align 4
  %arrayidx14 = getelementptr i32 addrspace(1)* %tmp12, i32 %tmp13
  %tmp15 = load i32* %d, align 4
  store i32 %tmp15, i32 addrspace(1)* %arrayidx14, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_5(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_5
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 61680
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 3855
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_6(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_6
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 65280
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = shl i32 %tmp8, 12
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_7(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_7
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = shl i32 %tmp5, 9
  %tmp7 = load <4 x i32>* %c, align 16
  %tmp8 = extractelement <4 x i32> %tmp7, i32 0
  %tmp9 = and i32 %tmp8, 4088
  %tmp10 = or i32 %tmp6, %tmp9
  store i32 %tmp10, i32* %d, align 4
  %tmp11 = load i32 addrspace(1)** %a.addr, align 4
  %tmp12 = load i32* %idx, align 4
  %arrayidx13 = getelementptr i32 addrspace(1)* %tmp11, i32 %tmp12
  %tmp14 = load i32* %d, align 4
  store i32 %tmp14, i32 addrspace(1)* %arrayidx13, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

define  void @ubit_noopt_case_8(i32 addrspace(1)* %a, <4 x i32> addrspace(1)* %b) nounwind {
  ; CHECK: ubit_noopt_case_8
  ; CHECK-NOT: bfi
entry:
  %a.addr = alloca i32 addrspace(1)*, align 4
  %b.addr = alloca <4 x i32> addrspace(1)*, align 4
  %idx = alloca i32, align 4
  %c = alloca <4 x i32>, align 16
  %d = alloca i32, align 4
  store i32 addrspace(1)* %a, i32 addrspace(1)** %a.addr, align 4
  store <4 x i32> addrspace(1)* %b, <4 x i32> addrspace(1)** %b.addr, align 4
  %call = call  i32 @get_global_id(i32 0) nounwind
  store i32 %call, i32* %idx, align 4
  %tmp = load <4 x i32> addrspace(1)** %b.addr, align 4
  %tmp1 = load i32* %idx, align 4
  %arrayidx = getelementptr <4 x i32> addrspace(1)* %tmp, i32 %tmp1
  %tmp2 = load <4 x i32> addrspace(1)* %arrayidx, align 16
  store <4 x i32> %tmp2, <4 x i32>* %c, align 16
  %tmp3 = load <4 x i32>* %c, align 16
  %tmp4 = extractelement <4 x i32> %tmp3, i32 1
  store i32 %tmp4, i32* %d, align 4
  %tmp5 = load i32* %d, align 4
  %tmp6 = and i32 %tmp5, 43690
  %tmp7 = shl i32 %tmp6, 9
  %tmp8 = load <4 x i32>* %c, align 16
  %tmp9 = extractelement <4 x i32> %tmp8, i32 0
  %tmp10 = and i32 %tmp9, 13107
  %tmp11 = shl i32 %tmp10, 9
  %tmp12 = or i32 %tmp7, %tmp11
  store i32 %tmp12, i32* %d, align 4
  %tmp13 = load i32 addrspace(1)** %a.addr, align 4
  %tmp14 = load i32* %idx, align 4
  %arrayidx15 = getelementptr i32 addrspace(1)* %tmp13, i32 %tmp14
  %tmp16 = load i32* %d, align 4
  store i32 %tmp16, i32 addrspace(1)* %arrayidx15, align 4
  br label %return

return:                                           ; preds = %entry
  ret void
}

