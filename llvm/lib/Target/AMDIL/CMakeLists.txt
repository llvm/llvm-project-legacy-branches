set(LLVM_TARGET_DEFINITIONS AMDIL.td)

tablegen(LLVM AMDILGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM AMDILGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM AMDILGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM AMDILGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM AMDILGenCallingConv.inc -gen-callingconv)
tablegen(LLVM AMDILGenSubtarget.inc -gen-subtarget)
tablegen(LLVM AMDILGenEDInfo.inc -gen-enhanced-disassembly-info)
tablegen(LLVM AMDILGenIntrinsics.inc -gen-tgt-intrinsic)

add_public_tablegen_target(AMDILCommonTableGen)

set(sources
  AMDIL7XXDevice.cpp
  AMDIL7XXIOExpansion.cpp
  AMDIL789IOExpansion.cpp
  AMDILAsmBackend.cpp
  AMDILBarrierDetect.cpp
  AMDILCFGStructurizer.cpp
  AMDILDevice.cpp
  AMDILDeviceInfo.cpp
  AMDILEGIOExpansion.cpp
  AMDILEvergreenDevice.cpp
  AMDILELFWriterInfo.cpp
  AMDILFrameLowering.cpp
  AMDILImageExpansion.cpp
  AMDILInliner.cpp
  AMDILInstrInfo.cpp
  AMDILIntrinsicInfo.cpp
  AMDILIOExpansion.cpp
  AMDILISelDAGToDAG.cpp
  AMDILISelLowering.cpp
  AMDILKernelManager.cpp
  AMDILLiteralManager.cpp
  AMDILMachineFunctionInfo.cpp
  AMDILMachinePeephole.cpp
  AMDILMCAsmInfo.cpp
  AMDILMCCodeEmitter.cpp
  AMDILModuleInfo.cpp
  AMDILNIDevice.cpp
  AMDILPeepholeOptimizer.cpp
  AMDILPointerManager.cpp
  AMDILPrintfConvert.cpp
  AMDILRegisterInfo.cpp
  AMDILSubtarget.cpp
  AMDILSwizzleEncoder.cpp
  AMDILTargetMachine.cpp
  AMDILUtilityFunctions.cpp
  macrodata.cpp
  AMDILSIPointerManager.cpp
  AMDILSIDevice.cpp
  AMDILSIIOExpansion.cpp
  AMDILInstPrinter.cpp
  AMDIL7XXAsmPrinter.cpp
  AMDILAsmPrinter.cpp
  AMDILEGAsmPrinter.cpp
  AMDILSIAsmPrinter.cpp
  AMDILTNDevice.cpp
  )

if( CMAKE_CL_64 )
  # A workaround for a bug in cmake 2.8.3. See PR 8885.
  if( CMAKE_VERSION STREQUAL "2.8.3" )
    include(CMakeDetermineCompilerId)
  endif()
  # end of workaround.
endif()

add_llvm_target(AMDILCodeGen ${sources})

add_subdirectory(TargetInfo)
