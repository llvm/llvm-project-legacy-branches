declare void %llvm.join(sbyte *)

void %main() {
header:
  %x = pbr label %bb1, label %bb2

bb1:
  br label %sink

bb2:
  br label %sink

sink:
  call void %llvm.join(sbyte * %x)
  br label %exit

exit:
  ret void
}
