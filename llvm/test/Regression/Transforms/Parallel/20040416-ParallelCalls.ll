; RUN: llvm-as < %s | opt -pcall-thread | llvm-dis | grep __llvm_thread

declare void %llvm.join(sbyte* %x)

declare sbyte* %foo(sbyte* %f)
declare sbyte* %bar(sbyte* %b)

int %main(int %argc, sbyte** %argv) {
entry:
  br label %parallel
  
parallel:
  %x = pbr label %a, label %b

a:
  call sbyte* %foo(sbyte* null)
  br label %exit
  
b:
  call sbyte* %bar(sbyte* null)
  br label %exit

exit:
  call void %llvm.join(sbyte* %x)
  ret int 0
}
