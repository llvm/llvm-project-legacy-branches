set(LLVM_USED_LIBS clangTooling clangBasic clangAST clangASTMatchers
    clangDynamicASTMatchers)

add_clang_executable(ast-query
  ASTQuery.cpp
  )
