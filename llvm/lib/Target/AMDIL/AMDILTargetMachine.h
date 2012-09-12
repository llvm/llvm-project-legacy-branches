//===-- AMDILTargetMachine.h ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the AMDIL specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef AMDILTARGETMACHINE_H_
#define AMDILTARGETMACHINE_H_

#include "AMDIL.h"
#include "AMDILELFWriterInfo.h"
#include "AMDILFrameLowering.h"
#include "AMDILInstrInfo.h"
#include "AMDILISelLowering.h"
#include "AMDILIntrinsicInfo.h"
#include "AMDILSubtarget.h"

#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetData.h"
using namespace llvm;

namespace llvm
{
class raw_ostream;

class AMDILTargetMachine : public LLVMTargetMachine
{
private:
  AMDILSubtarget Subtarget;
  const TargetData DataLayout;             // Calculates type size & alignment
  AMDILFrameLowering FrameLowering;
  AMDILInstrInfo InstrInfo;
  AMDILTargetLowering TLInfo;
  AMDILIntrinsicInfo IntrinsicInfo;
  AMDILELFWriterInfo ELFWriterInfo;
  bool mDebugMode;

protected:

public:
  AMDILTargetMachine(const Target &T,
                     StringRef TT, StringRef CPU, StringRef FS,
                     const TargetOptions &Options,
                     Reloc::Model RM, CodeModel::Model CM,
                     CodeGenOpt::Level OL);

  // Get Target/Subtarget specific information
  virtual AMDILTargetLowering* getTargetLowering() const;
  virtual const AMDILInstrInfo* getInstrInfo() const;
  virtual const AMDILFrameLowering* getFrameLowering() const;
  virtual const AMDILSubtarget* getSubtargetImpl() const;
  virtual const AMDILRegisterInfo* getRegisterInfo() const;
  virtual const TargetData* getTargetData() const;
  virtual const AMDILIntrinsicInfo *getIntrinsicInfo() const;
  virtual const AMDILELFWriterInfo *getELFWriterInfo() const;

  // Set up the pass pipeline.
  virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);

  void dump(OSTREAM_TYPE &O);
  void setDebug(bool debugMode);
  bool getDebug() const;
};     // AMDILTargetMachine

class TheAMDILTargetMachine : public AMDILTargetMachine {
public:
  TheAMDILTargetMachine(const Target &T,
                        StringRef TT, StringRef CPU, StringRef FS,
                        const TargetOptions &Options,
                        Reloc::Model RM, CodeModel::Model CM,
                        CodeGenOpt::Level OL);
};     // TheAMDILTargetMachine
} // end namespace llvm

namespace llvm {
class AMDILPassConfig : public TargetPassConfig {
public:
  AMDILPassConfig(AMDILTargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {
  }

  AMDILTargetMachine &getAMDILTargetMachine() const {
    return getTM<AMDILTargetMachine>();
  }
  const AMDILSubtarget &getAMDILSubtarget() const {
    return *getAMDILTargetMachine().getSubtargetImpl();
  }
  // Pass Pipeline Configuration
  virtual bool addPreEmitPass();
  virtual bool addPreISel();
  virtual bool addInstSelector();
  virtual bool addPreRegAlloc();
  virtual bool addPostRegAlloc();
};
} // end namespace llvm

#endif // AMDILTARGETMACHINE_H_
