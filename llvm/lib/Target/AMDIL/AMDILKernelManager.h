//===-- AMDILKernelManager.h ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Class that handles the metadata/abi management for the.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDILKERNELMANAGER_H_
#define _AMDILKERNELMANAGER_H_
#include "AMDIL.h"
#include "AMDILDevice.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/ValueMap.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include <string>
#include <set>
#include <map>
#define IMAGETYPE_2D 0
#define IMAGETYPE_3D 1
#define RESERVED_LIT_COUNT 6

namespace llvm {
class AMDILSubtarget;
class AMDILMachineFunctionInfo;
class AMDILModuleInfo;
class AMDILTargetMachine;
class AMDILAsmPrinter;
class StructType;
class Value;
class TypeSymbolTable;
class MachineFunction;
class MachineInstr;
class ConstantFP;
class PrintfInfo;

class AMDILKernelManager {
public:
  typedef enum {
    RELEASE_ONLY,
    DEBUG_ONLY,
    ALWAYS
  } ErrorMsgEnum;
  AMDILKernelManager(AMDILTargetMachine *TM);
  virtual ~AMDILKernelManager();

  /// Clear the state of the KernelManager putting it in its most initial state.
  void clear();
  void setMF(MachineFunction *MF);

  /// Process the specific kernel parsing out the parameter information for the
  /// kernel.
  void processArgMetadata(OSTREAM_TYPE &O,
                          uint32_t buf, bool kernel);

  /// Prints the header for the kernel which includes the groupsize declaration
  /// and calculation of the local/group/global id's.
  void printHeader(AMDILAsmPrinter *AsmPrinter, OSTREAM_TYPE &O,
                   const std::string &name);

  virtual void printDecls(AMDILAsmPrinter *AsmPrinter, OSTREAM_TYPE &O);
  virtual void printGroupSize(OSTREAM_TYPE &O);

  /// Copies the data from the runtime setup constant buffers into registers so
  /// that the program can correctly access memory or data that was set by the
  /// host program.
  void printArgCopies(OSTREAM_TYPE &O, AMDILAsmPrinter* RegNames);

  /// Prints out the end of the function.
  void printFooter(OSTREAM_TYPE &O);

  /// Prints out the metadata for the specific function depending if it is a
  /// kernel or not.
  void printMetaData(OSTREAM_TYPE &O, uint32_t id, bool isKernel = false);

  /// Set bool value on whether to consider the function a kernel or a normal
  /// function.
  void setKernel(bool kernel);

  /// Set the unique ID of the kernel/function.
  void setID(uint32_t id);

  /// Set the name of the kernel/function.
  void setName(const std::string &name);

  /// Flag that specifies whether this function has a kernel wrapper.
  bool wasKernel();

  void getIntrinsicSetup(AMDILAsmPrinter *AsmPrinter, OSTREAM_TYPE &O);

  // Returns whether a compiler needs to insert a write to memory or not.
  bool useCompilerWrite(const MachineInstr *MI);

  // Return whether a region_barrier is used
  bool useRegionBarrier(const MachineInstr *MI);

  // Set the flag that there exists an image write.
  void setImageWrite();
  void setOutputInst();

  void emitLiterals(OSTREAM_TYPE &O);

  // Set the uav id for the specific pointer value.  If value is NULL, then the
  // ID sets the default ID.
  void setUAVID(const Value *value, uint32_t ID);

  // Get the UAV id for the specific pointer value.
  uint32_t getUAVID(const Value *value);

private:

  /// Helper function that prints the actual metadata and should only be called
  /// by printMetaData.
  void printKernelArgs(OSTREAM_TYPE &O);
  void printCopyStructPrivate(const StructType *ST,
                              OSTREAM_TYPE &O,
                              size_t stackSize,
                              uint32_t Buffer,
                              uint32_t mLitIdx,
                              uint32_t &counter);
  virtual void
  printConstantToRegMapping(AMDILAsmPrinter *RegNames,
                            unsigned &LII,
                            OSTREAM_TYPE &O,
                            uint32_t &counter,
                            uint32_t Buffer,
                            uint32_t n,
                            const char *lit = NULL,
                            uint32_t fcall = 0,
                            bool isImage = false,
                            bool isHWCB = false);
  void updatePtrArg(llvm::Function::const_arg_iterator Ip,
                    int numWriteImages,
                    int raw_uav_buffer,
                    int counter,
                    bool isKernel,
                    const Function *F);
  /// Name of the current kernel.
  std::string mName;
  uint32_t mUniqueID;
  bool mIsKernel;
  bool mWasKernel;
  bool mCompilerWrite;
  /// Flag to specify if an image write has occured or not in order to not add a
  /// compiler specific write if no other writes to memory occured.
  bool mHasImageWrite;
  bool mHasOutputInst;
  bool mHasRegionBarrier;

  /// Map from const Value * to UAV ID.
  std::map<const Value *, uint32_t> mValueIDMap;

  AMDILTargetMachine * mTM;
  const AMDILSubtarget * mSTM;
  /// This is the global offset of the printf string id's.
  MachineFunction *mMF;
  AMDILMachineFunctionInfo *mMFI;
  AMDILModuleInfo *mAMI;
}; // class AMDILKernelManager
} // llvm namespace
#endif // _AMDILKERNELMANAGER_H_
