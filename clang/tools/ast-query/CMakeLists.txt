add_clang_executable(ast-query
  ASTQuery.cpp
  )

target_link_libraries(ast-query
  clangTooling clangBasic clangAST clangASTMatchers
  clangDynamicASTMatchers)

