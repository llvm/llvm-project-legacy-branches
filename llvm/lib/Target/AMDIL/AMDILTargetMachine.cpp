//===-- AMDILTargetMachine.cpp --------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#include "AMDILTargetMachine.h"
#include "AMDILDevices.h"
#include "AMDILFrameLowering.h"
#include "AMDILMCAsmInfo.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/SchedulerRegistry.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

static MCAsmInfo* createMCAsmInfo(const Target &T, StringRef TT)
{
  Triple TheTriple(TT);
  switch (TheTriple.getOS()) {
  default:
  case Triple::UnknownOS:
    return new AMDILMCAsmInfo(TheTriple);
  }
}
extern "C" void LLVMInitializeAMDILTarget() {
  // Register the target
  RegisterTargetMachine<TheAMDILTargetMachine> X(TheAMDILTarget);

  // Register the target asm info
  RegisterMCAsmInfoFn A(TheAMDILTarget, createMCAsmInfo);

  // Register the code emitter
  //TargetRegistry::RegisterCodeEmitter(TheAMDILTarget,
  //createAMDILMCCodeEmitter);
}
TheAMDILTargetMachine::TheAMDILTargetMachine(const Target &T,
                                             StringRef TT,
                                             StringRef CPU,
                                             StringRef FS,
                                             const TargetOptions &Options,
                                             Reloc::Model RM,
                                             CodeModel::Model CM,
                                             CodeGenOpt::Level OL)
  : AMDILTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL)
{
}
/// AMDILTargetMachine ctor -
///
AMDILTargetMachine::AMDILTargetMachine(const Target &T,
                                       StringRef TT,
                                       StringRef CPU,
                                       StringRef FS,
                                       const TargetOptions &Options,
                                       Reloc::Model RM,
                                       CodeModel::Model CM,
                                       CodeGenOpt::Level OL)
  :
    LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL),
    Subtarget(TT, CPU, FS),
    DataLayout(Subtarget.getDataLayout()),
    FrameLowering(TargetFrameLowering::StackGrowsUp,
                  Subtarget.device()->getStackAlignment(), 0),
    InstrInfo(*this), //JITInfo(*this),
    TLInfo(*this),
    IntrinsicInfo(this),
    ELFWriterInfo(false, true)
{
  setAsmVerbosityDefault(true);
  setMCUseLoc(false);
}
AMDILTargetLowering*
AMDILTargetMachine::getTargetLowering() const
{
  return const_cast<AMDILTargetLowering*>(&TLInfo);
}
const AMDILInstrInfo*
AMDILTargetMachine::getInstrInfo() const
{
  return &InstrInfo;
}
const AMDILFrameLowering*
AMDILTargetMachine::getFrameLowering() const
{
  return &FrameLowering;
}
const AMDILSubtarget*
AMDILTargetMachine::getSubtargetImpl() const
{
  return &Subtarget;
}
const AMDILRegisterInfo*
AMDILTargetMachine::getRegisterInfo() const
{
  return &InstrInfo.getRegisterInfo();
}
const TargetData*
AMDILTargetMachine::getTargetData() const
{
  return &DataLayout;
}
const AMDILELFWriterInfo*
AMDILTargetMachine::getELFWriterInfo() const
{
  return Subtarget.isTargetELF() ? &ELFWriterInfo : 0;
}
TargetPassConfig*
AMDILTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new AMDILPassConfig(this, PM);
}
const AMDILIntrinsicInfo*
AMDILTargetMachine::getIntrinsicInfo() const
{
  return &IntrinsicInfo;
}
bool
AMDILPassConfig::addPreISel()
{
  // Vector Coarsening as the current implementation does not support
  // big endian yet.

  return true;
}
bool
AMDILPassConfig::addInstSelector()
{
  addPass(createAMDILBarrierDetect(getAMDILTargetMachine(),
                                   getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILPrintfConvert(getAMDILTargetMachine(),
                                   getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILInlinePass(getAMDILTargetMachine(),
                                getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILPeepholeOpt(getAMDILTargetMachine(),
                                 getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILISelDag(getAMDILTargetMachine(),
                             getAMDILTargetMachine().getOptLevel()));
  return false;
}
bool
AMDILPassConfig::addPreRegAlloc()

{
  // If debugging, reduce code motion. Use less aggressive pre-RA scheduler
  if (getOptLevel() == CodeGenOpt::None) {
    llvm::RegisterScheduler::setDefault(&llvm::createSourceListDAGScheduler);
  }

  addPass(createAMDILMachinePeephole());
  addPass(createAMDILPointerManager(getAMDILTargetMachine(),
                                    getAMDILTargetMachine().getOptLevel()));
  return false;
}
bool
AMDILPassConfig::addPostRegAlloc() {
  return false;  // -print-machineinstr should print after this.
}
/// addPreEmitPass - This pass may be implemented by targets that want to run
/// passes immediately before machine code is emitted.  This should return
/// true if -print-machineinstrs should print out the code after the passes.
bool
AMDILPassConfig::addPreEmitPass()
{
  addPass(createAMDILCFGPreparationPass());
  addPass(createAMDILCFGStructurizerPass());
  addPass(createAMDILLiteralManager(getAMDILTargetMachine(),
                                    getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILIOExpansion(getAMDILTargetMachine(),
                                 getAMDILTargetMachine().getOptLevel()));
  addPass(createAMDILSwizzleEncoder(getAMDILTargetMachine(),
                                    getAMDILTargetMachine().getOptLevel()));
  return true;
}
void
AMDILTargetMachine::dump(OSTREAM_TYPE &O)
{
  if (!mDebugMode) {
    return;
  }
  O << ";AMDIL Target Machine State Dump: \n";
}
void
AMDILTargetMachine::setDebug(bool debugMode)
{
  mDebugMode = debugMode;
}
bool
AMDILTargetMachine::getDebug() const
{
  return mDebugMode;
}
extern "C" void LLVMInitializeAMDILTargetMC() {
}
