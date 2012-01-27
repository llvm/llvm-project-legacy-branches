set(LLVM_USED_LIBS clangTooling clangBasic clangAST)

add_clang_executable(fix-llvm-style
  FixLLVMStyle.cpp
  )
