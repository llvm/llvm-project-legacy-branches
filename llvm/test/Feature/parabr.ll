declare void %llvm.join()

void %main() {
header:
  pbr label %bb1, label %bb2

bb1:
  br label %sink

bb2:
  br label %sink

sink:
  call void %llvm.join()
  br label %exit

exit:
  ret void
}
