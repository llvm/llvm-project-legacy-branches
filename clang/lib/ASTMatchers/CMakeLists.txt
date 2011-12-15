set(LLVM_LINK_COMPONENTS support)
SET(LLVM_USED_LIBS clangBasic clangAST)

add_clang_library(clangASTMatchers
  ASTMatchFinder.cpp
  ASTMatchersInternal.cpp
  )
