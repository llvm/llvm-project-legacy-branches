set(LLVM_LINK_COMPONENTS support)
SET(LLVM_USED_LIBS clangBasic clangFrontend clangAST clangASTMatchers
                   clangRewrite)

add_clang_library(clangTooling
  Refactoring.cpp
  Tooling.cpp
  )
