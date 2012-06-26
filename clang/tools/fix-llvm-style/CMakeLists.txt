add_clang_executable(fix-llvm-style
  FixLLVMStyle.cpp
  )

target_link_libraries(fix-llvm-style
  clangTooling clangBasic clangAST clangASTMatchers)
