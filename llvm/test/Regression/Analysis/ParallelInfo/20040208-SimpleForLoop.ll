declare void %llvm.join(sbyte* %x)

int %main(int %argc, sbyte** %argv) {
entry:
  %A = alloca [25 x int]
  br label %parallel
  
parallel:
  %i = phi int [ 0, %entry ], [ %inc, %indvar ]
  %x = pbr label %indvar, label %array

array:
  %tmp.4 = cast int %i to long
  %tmp.5 = getelementptr [25 x int]* %A, long 0, long %tmp.4
  store int %i, int* %tmp.5
  br label %loopexit
  
indvar:
  %inc = add int %i, 1
  %tmp.1 = setle int %inc, 24
  br bool %tmp.1, label %parallel, label %loopexit

loopexit:
  call void %llvm.join(sbyte* %x)
  ret int 0
}
