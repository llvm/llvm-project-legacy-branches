add_clang_executable(clang-rename
  ClangRename.cpp
  )

target_link_libraries(clang-rename
  clangTooling clangBasic clangAST)
