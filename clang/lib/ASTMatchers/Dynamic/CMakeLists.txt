set(LLVM_LINK_COMPONENTS support)
SET(LLVM_USED_LIBS clangBasic clangAST clangASTMatchers)

add_clang_library(clangDynamicASTMatchers
  Parser.cpp
  Registry.cpp
  )
