#!/bin/bash

COMMON="lib/CMakeLists.txt lib/Makefile unittests/CMakeLists.txt unittests/Makefile"

AST_H="include/clang/ASTMatchers/ASTMatchFinder.h include/clang/ASTMatchers/ASTMatchers.h include/clang/ASTMatchers/ASTMatchersInternal.h include/clang/ASTMatchers/ASTMatchersMacros.h"
AST_C="lib/ASTMatchers/ASTMatchFinder.cpp lib/ASTMatchers/ASTMatchersInternal.cpp"
AST_T="unittests/ASTMatchers/ASTMatchersTest.cpp unittests/ASTMatchers/ASTMatchersTest.h"
AST_M="lib/ASTMatchers/CMakeLists.txt lib/ASTMatchers/Makefile unittests/ASTMatchers/Makefile"
AST_SRC="$AST_H $AST_C $AST_T $AST_M $COMMON"

REF_H="include/clang/Tooling/Refactoring.h"
REF_C="lib/Tooling/Refactoring.cpp"
REF_T="unittests/Tooling/RefactoringTest.cpp"
REF_M="lib/Tooling/CMakeLists.txt lib/Tooling/Makefile unittests/Tooling/Makefile"
REF_SRC="$REF_H $REF_C $REF_T $REF_M $COMMON"

#tools/CMakeLists.txt
#tools/Makefile
#tools/clang-check/Makefile
#tools/fix-llvm-style/CMakeLists.txt
#tools/fix-llvm-style/FixLLVMStyle.cpp
#tools/fix-llvm-style/Makefile
#tools/remove-cstr-calls/CMakeLists.txt
#tools/remove-cstr-calls/Makefile
#tools/remove-cstr-calls/RemoveCStrCalls.cpp
#test/CMakeLists.txt
#test/Tooling/remove-cstr-calls.cpp

function archive() {
  TMP_DIR=$(mktemp -d)
  for file in $1; do
    SRC="$file"
    DST="$TMP_DIR/$file"
    mkdir -p $(dirname "$DST")
    cp -a "$SRC" "$DST"
    echo $DST
  done
  ( cd "$TMP_DIR" && tar cvzf "$2" . )
  rm -rf "$TMP_DIR"
}

archive "$AST_SRC" "/tmp/ASTMatchers.tar.gz"
archive "$REF_SRC" "/tmp/Refactoring.tar.gz"
