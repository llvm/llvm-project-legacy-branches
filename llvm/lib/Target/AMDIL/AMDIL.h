//===-- AMDIL.h -----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the
// LLVM AMDIL back-end.
//
//===----------------------------------------------------------------------===//

#ifndef AMDIL_H_
#define AMDIL_H_
#include "AMDILLLVMPC.h"
#include "AMDILLLVMVersion.h"
#include "AMDILInstPrinter.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Target/TargetMachine.h"

#define AMD_LLVM_PUBLIC

#define AMDIL_MAJOR_VERSION 3
#define AMDIL_MINOR_VERSION 1
#define AMDIL_REVISION_NUMBER 104
#define AMDIL_20_REVISION_NUMBER 88
#define ARENA_SEGMENT_RESERVED_UAVS 12
#define DEFAULT_ARENA_UAV_ID 8
#define DEFAULT_RAW_UAV_ID 7
#define GLOBAL_RETURN_RAW_UAV_ID 11
#define HW_MAX_NUM_CB 8
#define MAX_NUM_UNIQUE_UAVS 8
#define OPENCL_MAX_NUM_ATOMIC_COUNTERS 8
#define OPENCL_MAX_READ_IMAGES 128
#define OPENCL_MAX_WRITE_IMAGES 8
#define OPENCL_MAX_SAMPLERS 16
#define OPENCL_MAX_NUM_SEMAPHORES 15

// The next two values can never be zero, as zero is the ID that is
// used to assert against.
#define DEFAULT_LDS_ID     1
#define DEFAULT_GDS_ID     1
#define DEFAULT_SCRATCH_ID 1
#define DEFAULT_VEC_SLOTS  8

#define OCL_DEVICE_RV710       0x000001
#define OCL_DEVICE_RV730       0x000002
#define OCL_DEVICE_RV770       0x000004
#define OCL_DEVICE_CEDAR       0x000008
#define OCL_DEVICE_REDWOOD     0x000010
#define OCL_DEVICE_JUNIPER     0x000020
#define OCL_DEVICE_CYPRESS     0x000040
#define OCL_DEVICE_CAICOS      0x000080
#define OCL_DEVICE_TURKS       0x000100
#define OCL_DEVICE_BARTS       0x000200
#define OCL_DEVICE_CAYMAN      0x000400
#define OCL_DEVICE_TAHITI      0x000800
#define OCL_DEVICE_PITCAIRN    0x001000
#define OCL_DEVICE_CAPEVERDE   0x002000
#define OCL_DEVICE_TRINITY     0x004000
#define OCL_DEVICE_DOGS        0x008000
#define OCL_DEVICE_CATS        0x010000
#define OCL_DEVICE_BUNNIES     0x020000
#define OCL_DEVICE_CASPER      0x040000
#define OCL_DEVICE_SLIMER      0x080000
#define OCL_DEVICE_MICE        0x100000
#define OCL_DEVICE_ALL         0xFFFFFF

/// The number of function ID's that are reserved for
/// internal compiler usage.
const unsigned int RESERVED_FUNCS = 1024;

namespace llvm {
class AMDILInstrPrinter;
class AMDILTargetMachine;
class FunctionPass;
class MCAsmInfo;
class raw_ostream;
class Target;
class TargetMachine;

/// Instruction selection passes.
FunctionPass*
createAMDILISelDag(AMDILTargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILBarrierDetect(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILPrintfConvert(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILInlinePass(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILPeepholeOpt(TargetMachine &TM, CodeGenOpt::Level OptLevel);

/// Pre regalloc passes.
FunctionPass*
createAMDILPointerManager(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILMachinePeephole();

/// Pre emit passes.
FunctionPass* createMachinePostDominatorTreePass();
FunctionPass*
createAMDILCFGPreparationPass();
FunctionPass*
createAMDILCFGStructurizerPass();
FunctionPass*
createAMDILLiteralManager(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILIOExpansion(TargetMachine &TM, CodeGenOpt::Level OptLevel);
FunctionPass*
createAMDILSwizzleEncoder(TargetMachine &TM, CodeGenOpt::Level OptLevel);

/// Instruction Emission Passes
AMDILInstPrinter *createAMDILInstPrinter(const MCAsmInfo &MAI,
                                         const MCInstrInfo &MII,
                                         const MCRegisterInfo &MRI);

extern Target TheAMDILTarget;
} // end namespace llvm;

#define GET_REGINFO_ENUM
#include "AMDILGenRegisterInfo.inc"
#define GET_INSTRINFO_ENUM
#include "AMDILGenInstrInfo.inc"

/// Include device information enumerations
#include "AMDILDeviceInfo.h"

namespace llvm {
// AMDIL Instruction descriptor flags
namespace AMDID {
enum {
  EMPTY     =  0,
  SEXTLOAD  =  1,
  ZEXTLOAD  =  2,
  LOAD      =  3,
  STORE     =  4,
  TRUNCATE  =  5,
  ATOMIC    =  6,
  ADDR64    =  7,
  GLOBAL    =  8,
  PRIVATE   =  9,
  CONSTANT  = 10,
  CPOOL     = 11,
  REGION    = 12,
  LOCAL     = 13,
  GDS       = 14,
  LDS       = 15,
  CBMEM     = 16,
  SCRATCH   = 17,
  RAWUAV    = 18,
  ARENAUAV  = 19,
  IMAGE     = 20,
  INFO0     = 21,
  INFO1     = 22,
  TXLD      = 23,
  SEMA      = 24,
  APPEND    = 25,
  SWSEXTLD  = 26,
  LOADCONST = 27,
  IEEE      = 28,
  ZEROOP    = 29,
  FLAT      = 30,
  SWZLSHFT  = 31,
  SWZLDST   = (SWZLSHFT + 0),
  SWZLSRC0  = (SWZLSHFT + 1),
  SWZLSRC1  = (SWZLSHFT + 2),
  SWZLSRC2  = (SWZLSHFT + 3),
  SWZLSRC3  = (SWZLSHFT + 4),
  SWZLSRC4  = (SWZLSHFT + 5),
  GWS       = 37,
  PACKED    = 38,
  SUB32BITS = 39,
  TYPEI16   = 40,
  TYPEV4    = 41,
  VECTOR    = 42
};   // End anonymous enum.
static const uint64_t SWZLMASK  = (1ULL << SWZLDST)  | (1ULL << SWZLSRC0)
                                  | (1ULL << SWZLSRC1) | (1ULL << SWZLSRC2)
                                  | (1ULL << SWZLSRC3) | (1ULL << SWZLSRC4);
static const uint64_t AEXTLOAD = (1ULL << SEXTLOAD) | (1ULL << ZEXTLOAD);
static const uint64_t INFO = (1ULL << INFO0) | (1ULL << INFO1);
static const uint64_t EXTLOAD  = AEXTLOAD | (1ULL << SWSEXTLD);
static const uint64_t TYPEMASK  = (1ULL << TYPEI16) | (1ULL << TYPEV4);
static const uint64_t TYPEV2I8  = 0ULL;
static const uint64_t TYPEV2I16 = (1ULL << TYPEI16);
static const uint64_t TYPEV4I8  = (1ULL << TYPEV4);
static const uint64_t TYPEV4I16 = TYPEMASK;
} // end AMDID namespace.
/// OpenCL uses address spaces to differentiate between
/// various memory regions on the hardware. On the CPU
/// all of the address spaces point to the same memory,
/// however on the GPU, each address space points to
/// a seperate piece of memory that is unique from other
/// memory locations.
namespace AMDILAS {
enum AddressSpaces {
  PRIVATE_ADDRESS  = 0, // Address space for private memory.
  GLOBAL_ADDRESS   = 1, // Address space for global memory.
  CONSTANT_ADDRESS = 2, // Address space for constant memory.
  LOCAL_ADDRESS    = 3, // Address space for local memory.
  REGION_ADDRESS   = 4, // Address space for region memory.
  GLOBAL_HOST_ADDRESS = 5, // Address space with global host endianness.
  CONSTANT_HOST_ADDRESS = 6, // Address space with constant host endianness.
  FLAT_ADDRESS     = 7, // Address space for flat memory.
  ADDRESS_NONE     = 8  // Address space for unknown memory.
};

// We are piggybacking on the CommentFlag enum in MachineInstr.h to
// set bits in AsmPrinterFlags of the MachineInstruction. We will
// start at bit 16 and allocate down while LLVM will start at bit
// 1 and allocate up.

// This union/struct combination is an easy way to read out the
// exact bits that are needed.
typedef union ResourceRec {
  struct {
#ifdef __BIG_ENDIAN__
    unsigned short CacheableRead : 1;  // Flag to specify if the read is
                                       // cacheable. (Permanent)
    unsigned short HardwareInst  : 1;  // Flag to specify that this instruction
                                       // is a hardware instruction. (Permanent)
    unsigned short ResourceID    : 10; // Flag to specify the resource ID for
                                       // the op. (Permanent)
    unsigned short PointerPath   : 1;  // Flag to specify if the op is on the
                                       // pointer path.
    unsigned short ByteStore     : 1;  // Flag to specify if the op is byte
                                       // store op.
    unsigned short ConflictPtr   : 1;  // Flag to specify that the pointer has
                                       // a conflict.
    unsigned short isImage       : 1;  // Reserved for future use.
#else
    unsigned short isImage       : 1;  // Reserved for future use/llvm.
    unsigned short ConflictPtr   : 1;  // Flag to specify that the pointer has a
                                       // conflict.
    unsigned short ByteStore     : 1;  // Flag to specify if the op is a byte
                                       // store op.
    unsigned short PointerPath   : 1;  // Flag to specify if the op is on the
                                       // pointer path.
    unsigned short ResourceID    : 10; // Flag to specify the resourece ID for
                                       // the op. (Permanent)
    unsigned short HardwareInst  : 1;  // Flag to specify that this instruction
                                       // is a hardware instruction. (Permanent)
    unsigned short CacheableRead : 1;  // Flag to specify if the read is
                                       // cacheable. (Permanent)
#endif
  } bits;
  unsigned short u16all;

  ResourceRec() {
    u16all = 0;
  }
} InstrResEnc;
} // namespace AMDILAS

// The OpSwizzle encodes a subset of all possible
// swizzle combinations into a number of bits using
// only the combinations utilized by the backend.
// The lower 128 are for source swizzles and the
// upper 128 or for destination swizzles.
// The valid mappings can be found in the
// getSrcSwizzle and getDstSwizzle functions of
// AMDILUtilityFunctions.cpp.
typedef union SwizzleRec {
  struct {
#ifdef __BIG_ENDIAN__
    unsigned char dst : 1;
    unsigned char swizzle : 7;
#else
    unsigned char swizzle : 7;
    unsigned char dst : 1;
#endif
  } bits;
  unsigned char u8all;
} OpSwizzle;
} // end namespace llvm
#endif // AMDIL_H_
