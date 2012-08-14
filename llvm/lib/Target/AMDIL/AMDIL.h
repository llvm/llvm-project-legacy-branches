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

// SC->CAL version matchings.
#define CAL_CACHED_ALIGNED_UAVS          1679
#define CAL_VERSION_SC_156               1650
#define CAL_VERSION_SC_155               1630
#define CAL_VERSION_SC_154               1624
#define CAL_VERSION_SC_153               1616
#define CAL_VERSION_SC_152               1603
#define CAL_VERSION_SC_151               1589
#define CAL_VERSION_SC_150               1561
#define CAL_VERSION_SC_149               CAL_VERSION_SC_150
#define CAL_VERSION_SC_148               1525
#define CAL_VERSION_SC_147               CAL_VERSION_SC_148
#define CAL_VERSION_SC_146               CAL_VERSION_SC_148
#define CAL_VERSION_SC_145               1451
#define CAL_VERSION_SC_144               CAL_VERSION_SC_145
#define CAL_VERSION_SC_143               1441
#define CAL_VERSION_SC_142               CAL_VERSION_SC_142
#define CAL_VERSION_SC_141               1420
#define CAL_VERSION_SC_140               1400
#define CAL_VERSION_SC_139               1387
#define CAL_VERSION_SC_138               CAL_VERSION_SC_139
#define CAL_APPEND_BUFFER_SUPPORT        1340
#define CAL_VERSION_SC_137               1331
#define CAL_VERSION_SC_136                982
#define CAL_VERSION_SC_135                950
#define CAL_VERSION_GLOBAL_RETURN_BUFFER  990

#define OCL_DEVICE_RV710        0x00001
#define OCL_DEVICE_RV730        0x00002
#define OCL_DEVICE_RV770        0x00004
#define OCL_DEVICE_CEDAR        0x00008
#define OCL_DEVICE_REDWOOD      0x00010
#define OCL_DEVICE_JUNIPER      0x00020
#define OCL_DEVICE_CYPRESS      0x00040
#define OCL_DEVICE_CAICOS       0x00080
#define OCL_DEVICE_TURKS        0x00100
#define OCL_DEVICE_BARTS        0x00200
#define OCL_DEVICE_CAYMAN       0x00400
#define OCL_DEVICE_TAHITI       0x00800
#define OCL_DEVICE_PITCAIRN     0x01000
#define OCL_DEVICE_CAPEVERDE    0x02000
#define OCL_DEVICE_TRINITY      0x04000
#define OCL_DEVICE_DOGS         0x08000
#define OCL_DEVICE_CATS         0x10000
#define OCL_DEVICE_BUNNIES      0x20000
#define OCL_DEVICE_ALL          0xFFFFF

/// The number of function ID's that are reserved for
/// internal compiler usage.
const unsigned int RESERVED_FUNCS = 1024;

namespace llvm
{
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
createAMDILMachinePeephole(TargetMachine &TM, CodeGenOpt::Level OptLevel);

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
AMDILInstPrinter *createAMDILInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
    const MCRegisterInfo &MRI);

extern Target TheAMDILTarget;
} // end namespace llvm;

#define GET_REGINFO_ENUM
#include "AMDILGenRegisterInfo.inc"
#define GET_INSTRINFO_ENUM
#include "AMDILGenInstrInfo.inc"

/// Include device information enumerations
#include "AMDILDeviceInfo.h"

namespace llvm
{
/// OpenCL uses address spaces to differentiate between
/// various memory regions on the hardware. On the CPU
/// all of the address spaces point to the same memory,
/// however on the GPU, each address space points to
/// a seperate piece of memory that is unique from other
/// memory locations.
namespace AMDILAS
{
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
// Enums corresponding to AMDIL condition codes for IL.  These
// values must be kept in sync with the ones in the .td file.
namespace AMDILCC
{
enum CondCodes {
  // AMDIL specific condition codes. These correspond to the IL_CC_*
  // in AMDILInstrInfo.td and must be kept in the same order.
  IL_CC_D_EQ  =  0,   // DEQ instruction.
  IL_CC_D_GE  =  1,   // DGE instruction.
  IL_CC_D_LT  =  2,   // DLT instruction.
  IL_CC_D_NE  =  3,   // DNE instruction.
  IL_CC_F_EQ  =  4,   //  EQ instruction.
  IL_CC_F_GE  =  5,   //  GE instruction.
  IL_CC_F_LT  =  6,   //  LT instruction.
  IL_CC_F_NE  =  7,   //  NE instruction.
  IL_CC_I_EQ  =  8,   // IEQ instruction.
  IL_CC_I_GE  =  9,   // IGE instruction.
  IL_CC_I_LT  = 10,   // ILT instruction.
  IL_CC_I_NE  = 11,   // INE instruction.
  IL_CC_U_GE  = 12,   // UGE instruction.
  IL_CC_U_LT  = 13,   // ULE instruction.
  // Pseudo IL Comparison instructions here.
  IL_CC_F_GT  = 14,   //  GT instruction.
  IL_CC_U_GT  = 15,
  IL_CC_I_GT  = 16,
  IL_CC_D_GT  = 17,
  IL_CC_F_LE  = 18,   //  LE instruction
  IL_CC_U_LE  = 19,
  IL_CC_I_LE  = 20,
  IL_CC_D_LE  = 21,
  IL_CC_F_UNE = 22,
  IL_CC_F_UEQ = 23,
  IL_CC_F_ULT = 24,
  IL_CC_F_UGT = 25,
  IL_CC_F_ULE = 26,
  IL_CC_F_UGE = 27,
  IL_CC_F_ONE = 28,
  IL_CC_F_OEQ = 29,
  IL_CC_F_OLT = 30,
  IL_CC_F_OGT = 31,
  IL_CC_F_OLE = 32,
  IL_CC_F_OGE = 33,
  IL_CC_D_UNE = 34,
  IL_CC_D_UEQ = 35,
  IL_CC_D_ULT = 36,
  IL_CC_D_UGT = 37,
  IL_CC_D_ULE = 38,
  IL_CC_D_UGE = 39,
  IL_CC_D_ONE = 40,
  IL_CC_D_OEQ = 41,
  IL_CC_D_OLT = 42,
  IL_CC_D_OGT = 43,
  IL_CC_D_OLE = 44,
  IL_CC_D_OGE = 45,
  IL_CC_U_EQ  = 46,
  IL_CC_U_NE  = 47,
  IL_CC_F_O   = 48,
  IL_CC_D_O   = 49,
  IL_CC_F_UO  = 50,
  IL_CC_D_UO  = 51,
  IL_CC_L_LE  = 52,
  IL_CC_L_GE  = 53,
  IL_CC_L_EQ  = 54,
  IL_CC_L_NE  = 55,
  IL_CC_L_LT  = 56,
  IL_CC_L_GT  = 57,
  IL_CC_UL_LE = 58,
  IL_CC_UL_GE = 59,
  IL_CC_UL_EQ = 60,
  IL_CC_UL_NE = 61,
  IL_CC_UL_LT = 62,
  IL_CC_UL_GT = 63,
  COND_ERROR  = 64
};

} // end namespace AMDILCC
} // end namespace llvm
#endif // AMDIL_H_
