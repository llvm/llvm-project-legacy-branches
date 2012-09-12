//===-- AMDILMachineFunctionInfo.h ----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares AMDIL-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDILMACHINEFUNCTIONINFO_H_
#define _AMDILMACHINEFUNCTIONINFO_H_
#include "AMDIL.h"
#include "AMDILDevice.h"
#include "AMDILKernel.h"
#include "llvm/CodeGen/MachineFunction.h"
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
namespace llvm
{
class AMDILSubtarget;
class PrintfInfo {
uint32_t mPrintfID;
SmallVector<uint32_t, DEFAULT_VEC_SLOTS> mOperands;
public:
  void addOperand(size_t idx, uint32_t size);
  uint32_t getPrintfID();
  void setPrintfID(uint32_t idx);
  size_t getNumOperands();
  uint32_t getOperandID(uint32_t idx);
};   // class PrintfInfo

enum NameDecorationStyle
{
  None,
  StdCall,
  FastCall
};
typedef struct SamplerInfoRec {
  std::string name;   // The name of the sampler
  uint32_t val;   // The value of the sampler
  uint32_t idx;   // The sampler resource id
} SamplerInfo;
// Some typedefs that will help with using the various iterators
// of the machine function info class.
typedef std::map<std::pair<uint64_t, uint64_t>, uint32_t>::iterator
lit_iterator;
typedef StringMap<SamplerInfo>::iterator sampler_iterator;
typedef DenseSet<uint32_t>::iterator func_iterator;
typedef DenseSet<uint32_t>::iterator intr_iterator;
typedef DenseSet<uint32_t>::iterator uav_iterator;
typedef DenseSet<uint32_t>::iterator sema_iterator;
typedef DenseSet<uint32_t>::iterator read_image1d_iterator;
typedef DenseSet<uint32_t>::iterator write_image1d_iterator;
typedef DenseSet<uint32_t>::iterator read_image1d_array_iterator;
typedef DenseSet<uint32_t>::iterator write_image1d_array_iterator;
typedef DenseSet<uint32_t>::iterator read_image1d_buffer_iterator;
typedef DenseSet<uint32_t>::iterator write_image1d_buffer_iterator;
typedef DenseSet<uint32_t>::iterator read_image2d_iterator;
typedef DenseSet<uint32_t>::iterator write_image2d_iterator;
typedef DenseSet<uint32_t>::iterator read_image2d_array_iterator;
typedef DenseSet<uint32_t>::iterator write_image2d_array_iterator;
typedef DenseSet<uint32_t>::iterator read_image3d_iterator;
typedef DenseSet<uint32_t>::iterator write_image3d_iterator;
typedef DenseSet<const Value*>::iterator read_ptr_iterator;
typedef DenseSet<const char*>::iterator error_iterator;
typedef std::map<std::string, PrintfInfo*>::iterator printf_iterator;
typedef std::set<std::string>::iterator func_md_iterator;
typedef std::vector<std::string>::iterator kernel_md_iterator;
// AMDILMachineFunctionInfo - This class is
// derived from MachineFunction private
// amdil target-specific information for each MachineFunction
class AMDILMachineFunctionInfo : public MachineFunctionInfo
{
// CalleeSavedFrameSize - Size of the callee-saved
// register portion of the
// stack frame in bytes.
unsigned int CalleeSavedFrameSize;
// BytesToPopOnReturn - Number of bytes function pops on return.
// Used on windows platform for stdcall & fastcall name decoration
unsigned int BytesToPopOnReturn;
// DecorationStyle - If the function requires additional
// name decoration,
// DecorationStyle holds the right way to do so.
NameDecorationStyle DecorationStyle;
// ReturnAddrIndex - FrameIndex for return slot.
int ReturnAddrIndex;

// TailCallReturnAddrDelta - Delta the ReturnAddr stack slot is moved
// Used for creating an area before the register spill area
// on the stack
// the returnaddr can be savely move to this area
int TailCallReturnAddrDelta;

// SRetReturnReg - Some subtargets require that sret lowering includes
// returning the value of the returned struct in a register.
// This field holds the virtual register into which the sret
// argument is passed.
unsigned int SRetReturnReg;

// The size in bytes required to host all of the kernel arguments.
// -1 means this value has not been determined yet.
int32_t mArgSize;

// The size in bytes required to host the stack and the kernel arguments
// in private memory.
// -1 means this value has not been determined yet.
int32_t mScratchSize;

// The size in bytes required to host the the kernel arguments
// on the stack.
// -1 means this value has not been determined yet.
int32_t mStackSize;

/// A map of constant to literal mapping for all of the 128bit
/// literals in the current function.
std::map<std::pair<uint64_t, uint64_t>, uint32_t> mLits;
uint32_t addLiteral(uint64_t val_lo, uint64_t val_hi);

/// The number of literals that should be reserved.
/// TODO: Remove this when the wrapper emitter is added.
uint32_t mReservedLits;

/// A map of name to sampler information that is used to emit
/// metadata to the IL stream that the runtimes can use for
/// hardware setup.
StringMap<SamplerInfo> mSamplerMap;

/// Array of flags to specify if a specific memory type is used or not.
bool mUsedMem[AMDILDevice::MAX_IDS];

/// Set of all functions that this function calls.
DenseSet<uint32_t> mFuncs;

/// Set of all intrinsics that this function calls.
DenseSet<uint32_t> mIntrs;

/// Set of all write only 1D images.
DenseSet<uint32_t> mWO1D;
/// Set of all read only 1D images.
DenseSet<uint32_t> mRO1D;
/// Set of all write only 1D image arrays.
DenseSet<uint32_t> mWO1DA;
/// Set of all read only 1D image arrays.
DenseSet<uint32_t> mRO1DA;
/// Set of all write only 1D image buffers.
DenseSet<uint32_t> mWO1DB;
/// Set of all read only 1D image buffers.
DenseSet<uint32_t> mRO1DB;
/// Set of all write only 2D images.
DenseSet<uint32_t> mWO2D;
/// Set of all read only 2D images.
DenseSet<uint32_t> mRO2D;
/// Set of all write only 2D image arrays.
DenseSet<uint32_t> mWO2DA;
/// Set of all read only 2D image arrays.
DenseSet<uint32_t> mRO2DA;
/// Set of all read only 3D images.
DenseSet<uint32_t> mRO3D;
/// Set of all write only 3D images.
DenseSet<uint32_t> mWO3D;
/// Set of all the raw uavs.
DenseSet<uint32_t> mRawUAV;
/// Set of all the arena uavs.
DenseSet<uint32_t> mArenaUAV;

/// Set of all semaphores
DenseSet<uint32_t> mSemaphore;

/// Set of all the read-only pointers
DenseSet<const Value*> mReadPtr;

/// A set of all errors that occured in the backend for this function.
DenseSet<const char *> mErrors;

/// A mapping of printf data and the printf string
std::map<std::string, PrintfInfo*> mPrintfMap;

/// A set of all of the metadata that is used for the current function.
std::set<std::string> mMetadataFunc;

/// A set of all of the metadata that is used for the function wrapper.
std::vector<std::string> mMetadataKernel;

SmallVector<unsigned, 16> mArgRegs;

/// Information about the kernel, NULL if the function is not a kernel.
AMDILKernel *mKernel;

/// Pointer to the machine function that this information belongs to.
MachineFunction *mMF;

/// Pointer to the subtarget for this function.
const AMDILSubtarget *mSTM;
public:
  AMDILMachineFunctionInfo();
  AMDILMachineFunctionInfo(MachineFunction &MF);
  virtual ~AMDILMachineFunctionInfo();
  unsigned int
  getCalleeSavedFrameSize() const;
  void
  setCalleeSavedFrameSize(unsigned int bytes);

  unsigned int
  getBytesToPopOnReturn() const;
  void
  setBytesToPopOnReturn (unsigned int bytes);

  NameDecorationStyle
  getDecorationStyle() const;
  void
  setDecorationStyle(NameDecorationStyle style);

  int
  getRAIndex() const;
  void
  setRAIndex(int Index);

  int
  getTCReturnAddrDelta() const;
  void
  setTCReturnAddrDelta(int delta);

  unsigned int
  getSRetReturnReg() const;
  void
  setSRetReturnReg(unsigned int Reg);

#define AS_SET_GET(A) \
  private: \
    bool Uses ## A; \
    bool A ## Arg; \
  public: \
    void setUses ## A() { Uses ## A = true; \
    } \
    bool uses ## A() const { return Uses ## A; } \
    void setHas ## A ## Arg() { A ## Arg = true; setUses ## A(); } \
    bool has ## A ## Arg() { return A ## Arg; }

  AS_SET_GET(LDS)
  AS_SET_GET(GDS)
  AS_SET_GET(Scratch)
  AS_SET_GET(Constant)

  bool
  usesHWConstant(std::string name) const;
  uint32_t
  getLocal(uint32_t);
  bool
  isKernel() const;
  AMDILKernel*
  getKernel();

  std::string
  getName();

  /// Get the size in bytes that are required to host all of
  /// arguments based on the argument alignment rules in the AMDIL
  /// Metadata spec.
  uint32_t getArgSize();

  /// Get the size in bytes that are required to host all of
  /// arguments and stack memory in scratch.
  uint32_t getScratchSize();

  /// Get the size in bytes that is required to host all of
  /// the arguments on the stack.
  uint32_t getStackSize();

  ///
  /// @param val value to add the lookup table
  /// @param Opcode opcode of the literal instruction
  /// @brief adds the specified value of the type represented by the
  /// Opcode
  /// to the literal to integer and integer to literal mappings.
  ///
  /// Add a 32bit integer value to the literal table.
  uint32_t addi32Literal(uint32_t val, int Opcode = AMDIL::LOADCONSTi32);

  /// Add a 32bit floating point value to the literal table.
  uint32_t addf32Literal(const ConstantFP *CFP);

  /// Add a 32bit floating point value to the literal table.
  uint32_t addf32Literal(uint32_t val);

  /// Add a 64bit integer value to the literal table.
  uint32_t addi64Literal(uint64_t val);

  /// Add a 128 bit integer value to the literal table.
  uint32_t addi128Literal(uint64_t val_lo, uint64_t val_hi);

  /// Add a 64bit floating point literal as a 64bit integer value.
  uint32_t addf64Literal(const ConstantFP *CFP);

  /// Add a 64bit floating point literal as a 64bit integer value.
  uint32_t addf64Literal(uint64_t val);

  /// Get the number of literals that have currently been allocated.
  size_t getNumLiterals() const;

  /// Get the literal ID of an Integer literal of the given offset.
  uint32_t getLitIdx(uint32_t lit);

  /// Get the literal ID of a Long literal of the given offset.
  uint32_t getLitIdx(uint64_t lit);

  /// Add some literals to the number of reserved literals.
  void addReservedLiterals(uint32_t);

  // Functions that return iterators to the beginning and end
  // of the various literal maps.
  // Functions that return the beginning and end of the literal map
  lit_iterator lit_begin() {
    return mLits.begin();
  }
  lit_iterator lit_end() {
    return mLits.end();
  }

  // Add a sampler to the set of known samplers for the current kernel.
  uint32_t addSampler(std::string name, uint32_t value);

  // Iterators that point to the beginning and end of the sampler map.
  sampler_iterator sampler_begin() {
    return mSamplerMap.begin();
  }
  sampler_iterator sampler_end() {
    return mSamplerMap.end();
  }

  /// Set the flag for the memory ID to true for the current function.
  void setUsesMem(unsigned);
  /// Retrieve the flag for the memory ID.
  bool usesMem(unsigned);

  /// Add called functions to the set of all functions this function calls.
  void addCalledFunc(uint32_t id) {
    mFuncs.insert(id);
  }
  void eraseCalledFunc(uint32_t id) {
    mFuncs.erase(id);
  }
  size_t func_size() {
    return mFuncs.size();
  }
  bool func_empty() {
    return mFuncs.empty();
  }
  func_iterator func_begin() {
    return mFuncs.begin();
  }
  func_iterator func_end() {
    return mFuncs.end();
  }

  /// Add called intrinsics to the set of all intrinscis this function calls.
  void addCalledIntr(uint32_t id) {
    mIntrs.insert(id);
  }
  size_t intr_size() {
    return mIntrs.size();
  }
  bool intr_empty() {
    return mIntrs.empty();
  }
  intr_iterator intr_begin() {
    return mIntrs.begin();
  }
  intr_iterator intr_end() {
    return mIntrs.end();
  }

  /// Add a 1D read_only image id.
  void addROImage1D(uint32_t id) {
    mRO1D.insert(id);
  }
  size_t read_image1d_size() {
    return mRO1D.size();
  }
  read_image1d_iterator read_image1d_begin() {
    return mRO1D.begin();
  }
  read_image1d_iterator read_image1d_end() {
    return mRO1D.end();
  }

  /// Add a 1D write_only image id.
  void addWOImage1D(uint32_t id) {
    mWO1D.insert(id);
  }
  size_t write_image1d_size() {
    return mWO1D.size();
  }
  write_image1d_iterator write_image1d_begin() {
    return mWO1D.begin();
  }
  write_image1d_iterator write_image1d_end() {
    return mWO1D.end();
  }

  /// Add a 1D read_only image id.
  void addROImage1DArray(uint32_t id) {
    mRO1DA.insert(id);
  }
  size_t read_image1d_array_size() {
    return mRO1DA.size();
  }
  read_image1d_array_iterator read_image1d_array_begin() {
    return mRO1DA.begin();
  }
  read_image1d_array_iterator read_image1d_array_end() {
    return mRO1DA.end();
  }

  /// Add a 1D write_only image id.
  void addWOImage1DArray(uint32_t id) {
    mWO1DA.insert(id);
  }
  size_t write_image1d_array_size() {
    return mWO1DA.size();
  }
  write_image1d_array_iterator write_image1d_array_begin() {
    return mWO1DA.begin();
  }
  write_image1d_array_iterator write_image1d_array_end() {
    return mWO1DA.end();
  }

  /// Add a 1D read_only image id.
  void addROImage1DBuffer(uint32_t id) {
    mRO1DB.insert(id);
  }
  size_t read_image1d_buffer_size() {
    return mRO1DB.size();
  }
  read_image1d_buffer_iterator read_image1d_buffer_begin() {
    return mRO1DB.begin();
  }
  read_image1d_buffer_iterator read_image1d_buffer_end() {
    return mRO1DB.end();
  }

  /// Add a 1D write_only image id.
  void addWOImage1DBuffer(uint32_t id) {
    mWO1DB.insert(id);
  }
  size_t write_image1d_buffer_size() {
    return mWO1DB.size();
  }
  write_image1d_buffer_iterator write_image1d_buffer_begin() {
    return mWO1DB.begin();
  }
  write_image1d_buffer_iterator write_image1d_buffer_end() {
    return mWO1DB.end();
  }

  /// Add a 2D read_only image id.
  void addROImage2D(uint32_t id) {
    mRO2D.insert(id);
  }
  size_t read_image2d_size() {
    return mRO2D.size();
  }
  read_image2d_iterator read_image2d_begin() {
    return mRO2D.begin();
  }
  read_image2d_iterator read_image2d_end() {
    return mRO2D.end();
  }

  /// Add a 2D write_only image id.
  void addWOImage2D(uint32_t id) {
    mWO2D.insert(id);
  }
  size_t write_image2d_size() {
    return mWO2D.size();
  }
  write_image2d_iterator write_image2d_begin() {
    return mWO2D.begin();
  }
  write_image2d_iterator write_image2d_end() {
    return mWO2D.end();
  }

  /// Add a 2D read_only image array id.
  void addROImage2DArray(uint32_t id) {
    mRO2DA.insert(id);
  }
  size_t read_image2d_array_size() {
    return mRO2DA.size();
  }
  read_image2d_array_iterator read_image2d_array_begin() {
    return mRO2DA.begin();
  }
  read_image2d_array_iterator read_image2d_array_end() {
    return mRO2DA.end();
  }

  /// Add a 2D write_only image id.
  void addWOImage2DArray(uint32_t id) {
    mWO2DA.insert(id);
  }
  size_t write_image2d_array_size() {
    return mWO2DA.size();
  }
  write_image2d_array_iterator write_image2d_array_begin() {
    return mWO2DA.begin();
  }
  write_image2d_array_iterator write_image2d_array_end() {
    return mWO2D.end();
  }

  /// Add a 3D read_only image id.
  void addROImage3D(uint32_t id) {
    mRO3D.insert(id);
  }
  size_t read_image3d_size() {
    return mRO3D.size();
  }
  read_image3d_iterator read_image3d_begin() {
    return mRO3D.begin();
  }
  read_image3d_iterator read_image3d_end() {
    return mRO3D.end();
  }

  /// Add a 3D write_only image id.
  void addWOImage3D(uint32_t id) {
    mWO3D.insert(id);
  }
  size_t write_image3d_size() {
    return mWO3D.size();
  }
  write_image3d_iterator write_image3d_begin() {
    return mWO3D.begin();
  }
  write_image3d_iterator write_image3d_end() {
    return mWO3D.end();
  }

  size_t get_num_write_images();

  /// Add a semaphore
  void sema_insert(uint32_t id) {
    mSemaphore.insert(id);
  }
  bool sema_count(uint32_t id) {
    return mSemaphore.count(id);
  }
  size_t sema_size() {
    return mSemaphore.size();
  }
  sema_iterator sema_begin() {
    return mSemaphore.begin();
  }
  sema_iterator sema_end() {
    return mSemaphore.end();
  }

  /// Add a raw uav id.
  void uav_insert(uint32_t id) {
    mRawUAV.insert(id);
  }
  bool uav_count(uint32_t id) {
    return mRawUAV.count(id);
  }
  size_t uav_size() {
    return mRawUAV.size();
  }
  uav_iterator uav_begin() {
    return mRawUAV.begin();
  }
  uav_iterator uav_end() {
    return mRawUAV.end();
  }

  /// Add an arena uav id.
  void arena_insert(uint32_t id) {
    mArenaUAV.insert(id);
  }
  bool arena_count(uint32_t id) {
    return mArenaUAV.count(id);
  }
  size_t arena_size() {
    return mArenaUAV.size();
  }
  uav_iterator arena_begin() {
    return mArenaUAV.begin();
  }
  uav_iterator arena_end() {
    return mArenaUAV.end();
  }

  /// Add a pointer to the known set of read-only pointers
  void add_read_ptr(const Value* ptr) {
    mReadPtr.insert(ptr);
  }
  bool read_ptr_count(const Value* ptr) {
    return mReadPtr.count(ptr);
  }
  bool read_size() {
    return mReadPtr.size();
  }
  read_ptr_iterator read_ptr_begin() {
    return mReadPtr.begin();
  }
  read_ptr_iterator read_ptr_end() {
    return mReadPtr.end();
  }

  // Add an error to the output for the current function.
  typedef enum {
    RELEASE_ONLY,   /// Only emit error message in release mode.
    DEBUG_ONLY,   /// Only emit error message in debug mode.
    ALWAYS   /// Always emit the error message.
  } ErrorMsgEnum;
  /// Add an error message to the set of all error messages.
  void addErrorMsg(const char* msg, ErrorMsgEnum val = ALWAYS);
  bool errors_empty() {
    return mErrors.empty();
  }
  error_iterator errors_begin() {
    return mErrors.begin();
  }
  error_iterator errors_end() {
    return mErrors.end();
  }

  /// Add a string to the printf map
  uint32_t addPrintfString(std::string &name, unsigned offset);
  /// Add a operand to the printf string
  void addPrintfOperand(std::string &name, size_t idx, uint32_t size);
  bool printf_empty() {
    return mPrintfMap.empty();
  }
  size_t printf_size() {
    return mPrintfMap.size();
  }
  printf_iterator printf_begin() {
    return mPrintfMap.begin();
  }
  printf_iterator printf_end() {
    return mPrintfMap.end();
  }

  /// Add a string to the metadata set for a function/kernel wrapper
  void addMetadata(const char *md, bool kernelOnly = false);
  void addMetadata(std::string md, bool kernelOnly = false);
  func_md_iterator func_md_begin() {
    return mMetadataFunc.begin();
  }
  func_md_iterator func_md_end() {
    return mMetadataFunc.end();
  }
  kernel_md_iterator kernel_md_begin() {
    return mMetadataKernel.begin();
  }
  kernel_md_iterator kernel_md_end() {
    return mMetadataKernel.end();
  }

  /// Query to find out if we are a signed or unsigned integer type.
  bool isSignedIntType(const Value* ptr);

  /// Query to find out if we are a volatile pointer.
  bool isVolatilePointer(const Value* ptr);

  /// Query to find out if we are a restrict pointer.
  bool isRestrictPointer(const Value* ptr);

  /// Query to find out if we are a constant argument.
  bool isConstantArgument(const Value* ptr);

  /// add/retrieve the argument registers numbers
  void addArgReg(unsigned arg) {
    mArgRegs.push_back(arg);
  }
  unsigned getArgReg(unsigned arg) {
    return (arg < mArgRegs.size()) ? mArgRegs[arg] : arg;
  }
};
} // llvm namespace
#endif // _AMDILMACHINEFUNCTIONINFO_H_
