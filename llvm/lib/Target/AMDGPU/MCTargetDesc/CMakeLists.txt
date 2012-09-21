
add_llvm_library(LLVMAMDGPUDesc
  AMDGPUAsmBackend.cpp
  AMDGPUMCTargetDesc.cpp
  AMDGPUMCAsmInfo.cpp
  R600MCCodeEmitter.cpp
  SIMCCodeEmitter.cpp
  )

add_dependencies(LLVMAMDGPUDesc AMDGPUCommonTableGen)
