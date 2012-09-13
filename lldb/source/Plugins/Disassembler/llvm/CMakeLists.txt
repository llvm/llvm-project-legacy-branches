set(LLVM_NO_RTTI 1)

add_lldb_library(lldbPluginDisassemblerLLVM
  DisassemblerLLVM.cpp
  DisassemblerLLVMC.cpp
  )
