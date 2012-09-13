set(LLVM_NO_RTTI 1)

add_custom_command(
  OUTPUT ${LLDB_SOURCE_DIR}/source/LLDBWrapPython.cpp
  DEPENDS ${LLDB_SOURCE_DIR}/scripts/lldb.swig
  COMMAND swig -c++ -shadow -python -I${LLDB_SOURCE_DIR}/include -I./. -outdir ${LLDB_SOURCE_DIR}/scripts/Python  -o ${LLDB_SOURCE_DIR}/source/LLDBWrapPython.cpp ${LLDB_SOURCE_DIR}/scripts/lldb.swig
  COMMENT "Building lldb pyhton wrapper")
set_source_files_properties(${LLDB_SOURCE_DIR}/source/LLDBWrapPython.cpp PROPERTIES GENERATED 1)

ADD_CUSTOM_TARGET(swig_wrapper ALL echo
      DEPENDS ${LLDB_SOURCE_DIR}/source/LLDBWrapPython.cpp
    )
