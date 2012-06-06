set(LLVM_USED_LIBS clangEdit clangTooling clangBasic clangAST)

add_clang_executable(remove-cstr-calls
  RemoveCStrCalls.cpp
  )
