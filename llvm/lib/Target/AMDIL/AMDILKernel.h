//===-- AMDILKernel.h -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Definition of a AMDILKernel object and the various subclasses that are
// used.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_KERNEL_H_
#define _AMDIL_KERNEL_H_
#include "AMDIL.h"
#include "llvm/Value.h"
#include "llvm/Constant.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/CodeGen/MachineFunction.h"
namespace llvm {
class AMDILSubtarget;
class AMDILTargetMachine;
/// structure that holds information for a single local/region address array
typedef struct _AMDILArrayMemRec {
  const Value* base;
  uint32_t vecSize;   // size of each vector
  uint32_t offset;    // offset into the memory section
  uint32_t align;     // alignment
  // ID of the local buffer this array resides in.
  // Currently only used for hardware supported local buffers.
  uint32_t resourceID;
  bool isHW;          // flag to specify if HW is used or SW is used
  bool isRegion;      // flag to specify if GDS is used or not
} AMDILArrayMem;

/// structure that holds information about a constant address
/// space pointer that is a kernel argument
typedef struct _AMDILConstPtrRec {
  const llvm::Value *base;
  uint32_t size;
  uint32_t offset;
  uint32_t align;   // alignment
  uint32_t cbNum;   // value of 0 means that it does not use hw CB
  bool isArray;   // flag to specify that this is an array
  bool isArgument;   // flag to specify that this is for a kernel argument
  bool usesHardware;   // flag to specify if hardware CB is used or not
  std::string name;
} AMDILConstPtr;

/// Structure that holds information for all local/region address
/// arrays in the kernel
typedef struct _AMDILLocalArgRec {
  llvm::SmallVector<AMDILArrayMem *, DEFAULT_VEC_SLOTS> local;
  std::string name;   // Kernel Name
} AMDILLocalArg;

/// Structure that holds information for each kernel argument
typedef struct _AMDILkernelArgRec {
  uint32_t reqGroupSize[3];   // x,y,z sizes for group.
  uint32_t reqRegionSize[3];   // x,y,z sizes for region.
  llvm::SmallVector<uint32_t, DEFAULT_VEC_SLOTS> argInfo;   // information about argumetns.
  bool mHasRWG;   // true if reqd_work_group_size is specified.
  bool mHasRWR;   // true if reqd_work_region_size is specified.
} AMDILKernelAttr;

/// Structure that holds information for each kernel
class AMDILKernel {
public:
  AMDILKernel(const std::string& name, bool clKernel) :
    curSize(0),
    curRSize(0),
    curHWSize(0),
    curHWRSize(0),
    constSize(0),
    mKernel(clKernel),
    mName(name),
    sgv(NULL),
    lvgv(NULL),
    rvgv(NULL) {
    memset(constSizes, 0, sizeof(constSizes));
  }
  uint32_t curSize;     // Current size of local memory, hardware + software emulated
  uint32_t curRSize;     // Current size of region memory, hardware + software emulated
  uint32_t curHWSize;     // Current size of hardware local memory
  uint32_t curHWRSize;     // Current size of hardware region memory
  uint32_t constSize;     // Current size of software constant memory
  bool mKernel;     // Flag to specify if this represents an OpenCL kernel or not
  std::string mName;     // Name of current kernel
  AMDILKernelAttr *sgv;     // pointer to kernel attributes
  AMDILLocalArg *lvgv;     // pointer to local attributes
  AMDILLocalArg *rvgv;     // pointer to region attributes
  llvm::SmallVector<struct _AMDILConstPtrRec, DEFAULT_VEC_SLOTS> constPtr;     // vector containing constant pointer information
  uint32_t constSizes[HW_MAX_NUM_CB];     // Size of each constant buffer
  llvm::SmallSet<uint32_t, OPENCL_MAX_READ_IMAGES> readOnly;     // set that specifies the read-only images for the kernel
  llvm::SmallSet<uint32_t, OPENCL_MAX_WRITE_IMAGES> writeOnly;     // set that specifies the write-only images for the kernel
  llvm::SmallVector<std::pair<uint32_t, const llvm::Constant *>,
                    DEFAULT_VEC_SLOTS> CPOffsets; // Vector of constant pool offsets
  typedef llvm::SmallVector<struct _AMDILConstPtrRec,
                            DEFAULT_VEC_SLOTS>::iterator constptr_iterator;                               // iterator for constant pointers
  typedef llvm::SmallVector<AMDILArrayMem *,
                            DEFAULT_VEC_SLOTS>::iterator arraymem_iterator;                      // iterator for the memory array
};   // AMDILKernel
} // end llvm namespace
#endif // _AMDIL_KERNEL_H_
