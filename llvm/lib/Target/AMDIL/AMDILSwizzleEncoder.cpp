//===-- AMDILSwizzleEncoder.cpp -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The implementation of the AMDIL Swizzle Encoder. The swizzle encoder goes
// through all instructions in a machine function and all operands and encodes swizzle
// information in the operands. The AsmParser can then use the swizzle information to
// print out the swizzles correctly.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swizzleencoder"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME (false)
#endif
#include "AMDILSwizzleEncoder.h"
#include "AMDILAlgorithms.tpp"
#include "AMDILUtilityFunctions.h"
#include "AMDILRegisterInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;
/// Encode all of the swizzles for the instructions in the machine operand.
static void encodeSwizzles(MachineFunction &MF, bool mDebug);
/// Get the swizzle id for the src swizzle that corresponds to the
/// current operand.
static OpSwizzle getSrcSwizzleID(MachineInstr *MI, unsigned opNum);

/// Get the swizzle id for the dst swizzle that corresponds to the
/// current instruction.
static OpSwizzle getDstSwizzleID(MachineInstr *MI);

/// Determine if the custom source swizzle or the
/// default swizzle for the specified operand should be used.
static bool isCustomSrcInst(MachineInstr *MI, unsigned opNum);

/// Get the custom source swizzle that corresponds to the specified
/// operand for the instruction.
static OpSwizzle getCustomSrcSwizzle(MachineInstr *MI, unsigned opNum);

/// Determine if the custom destination swizzle or the
/// default swizzle should be used for the instruction.
static bool isCustomDstInst(MachineInstr *MI);

/// Get the custom destination swizzle that corresponds tothe
/// instruction.
static OpSwizzle getCustomDstSwizzle(MachineInstr *MI);

/// Encode the new swizzle for the vector instruction.
static void encodeVectorInst(MachineInstr *MI, bool mDebug);
/// Helper function to dump the operand for the machine instruction
/// and the relevant target flags.
static void dumpOperand(MachineInstr *MI, unsigned opNum);
namespace llvm {
FunctionPass*
createAMDILSwizzleEncoder(TargetMachine &TM, CodeGenOpt::Level OptLevel)
{
  return new AMDILSwizzleEncoder(TM, OptLevel);
}
}

AMDILSwizzleEncoder::AMDILSwizzleEncoder(TargetMachine &tm,
                                         CodeGenOpt::Level OptLevel) :
  MachineFunctionPass(ID)
{
  mDebug = DEBUGME;
  opt = OptLevel;
}
const char* AMDILSwizzleEncoder::getPassName() const
{
  return "AMD IL Swizzle Encoder Pass";
}
bool AMDILSwizzleEncoder::runOnMachineFunction(MachineFunction &MF)
{
  // Encode swizzles in instruction operands.
  encodeSwizzles(MF, mDebug);
  return true;
}
/// Dump the operand swizzle information to the dbgs() stream.
void dumpOperand(MachineInstr *MI, unsigned opNum)
{
  OpSwizzle swizID;
  swizID.u8all = MI->getOperand(opNum).getTargetFlags();
  dbgs() << "\t" << (swizID.bits.dst ? "Dst" : "Src")
         << " Operand: " << opNum << " SwizID: "
         << (unsigned)swizID.bits.swizzle
         << " Swizzle: " << (swizID.bits.dst
                      ? getDstSwizzle(swizID.bits.swizzle)
                      : getSrcSwizzle(swizID.bits.swizzle)) << "\n";
}
// This function checks for instructions that don't have
// normal swizzle patterns to their source operands. These have to be
// handled on a case by case basis.
bool isCustomSrcInst(MachineInstr *MI, unsigned opNum) {
  if (!MI->getDesc().getNumOperands()) return false;
  unsigned opcode = MI->getOpcode();
  unsigned regClass = MI->getDesc().OpInfo[0].RegClass;
  if ((isPtrLoadInst(MI) || isPtrStoreInst(MI))
      && (isScratchInst(MI)
          || isCBInst(MI)
          || isUAVArenaInst(MI))
      && (regClass == AMDIL::GPRI16RegClassID
          || regClass == AMDIL::GPRI8RegClassID
          || regClass == AMDIL::GPRI32RegClassID
          || regClass == AMDIL::GPRF32RegClassID
          )
      && !isExtLoadInst(MI)
      && !isTruncStoreInst(MI)) {
    return true;
  }
  uint32_t maskVal =
    (MI->getDesc().TSFlags & AMDID::SWZLMASK) >> AMDID::SWZLSHFT;
  return (maskVal ? (maskVal & (1ULL << opNum)) : false);
}
#define GENERATE_1ARG_CASE(A) \
case A ## r: \
case A ## i:
#define GENERATE_2ARG_CASE(A) \
  GENERATE_1ARG_CASE(A ## r) \
  GENERATE_1ARG_CASE(A ## i)
#define GENERATE_3ARG_CASE(A) \
  GENERATE_2ARG_CASE(A ## r) \
  GENERATE_2ARG_CASE(A ## i)
#define GENERATE_4ARG_CASE(A) \
  GENERATE_3ARG_CASE(A ## r) \
  GENERATE_3ARG_CASE(A ## i)

// This function returns the OpSwizzle with the custom swizzle set
// correclty for source operands.
OpSwizzle getCustomSrcSwizzle(MachineInstr *MI, unsigned opNum) {
  OpSwizzle opSwiz;
  opSwiz.u8all = 0;
  unsigned opcode = MI->getOpcode();
  unsigned reg = (MI->getOperand(opNum).isReg()
                  ? MI->getOperand(opNum).getReg() : 0);
  unsigned regClass = MI->getDesc().OpInfo[0].RegClass;
  if ((isPtrLoadInst(MI) || isPtrStoreInst(MI))
      && (isScratchInst(MI)
          || isCBInst(MI)
          || isUAVArenaInst(MI))
      && (regClass == AMDIL::GPRI16RegClassID
          || regClass == AMDIL::GPRI8RegClassID
          || regClass == AMDIL::GPRI32RegClassID
          || regClass == AMDIL::GPRF32RegClassID
          )
      && !isExtLoadInst(MI)
      && !isTruncStoreInst(MI)) {
    if (isUAVArenaInst(MI)) {
      if (isXComponentReg(reg)) {
        opSwiz.bits.swizzle = AMDIL_SRC_XXXX;
      } else if (isYComponentReg(reg)) {
        opSwiz.bits.swizzle = AMDIL_SRC_YYYY;
      } else if (isZComponentReg(reg)) {
        opSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
      } else if (isWComponentReg(reg)) {
        opSwiz.bits.swizzle = AMDIL_SRC_WWWW;
      }
      if (opNum != 1) {
        opSwiz.bits.swizzle = AMDIL_SRC_DFLT;
      }
    } else {
      opSwiz.bits.swizzle = (opNum == 1)
                            ? AMDIL_SRC_XXXX : AMDIL_SRC_DFLT;
    }
    return opSwiz;
  }
  if (isSemaphoreInst(MI)
      || isAppendInst(MI)
      || opcode == AMDIL::CALL
      || opcode == AMDIL::RETURN
      || opcode == AMDIL::RETDYN) {
    opSwiz.bits.swizzle = AMDIL_SRC_DFLT;
    return opSwiz;
  }
  switch (opcode) {
  default:
    break;
    GENERATE_3ARG_CASE(AMDIL::SELECTf64)
    GENERATE_3ARG_CASE(AMDIL::SELECTi64)
    assert(opNum == 1 && "Only operand number 1 is custom!");
    if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
    } else {
      opSwiz.bits.swizzle = AMDIL_SRC_XXXX;
    }
    break;
  case AMDIL::DHIf64r:
  case AMDIL::LLOi64r:
    if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_Z000;
    } else {
      opSwiz.bits.swizzle = AMDIL_SRC_X000;
    }
    reg = MI->getOperand(0).getReg();
    if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle += 1;
    } else if (isZComponentReg(reg)
               || isZWComponentReg(reg)) {
      opSwiz.bits.swizzle += 2;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle += 3;
    }
    break;
  case AMDIL::DHIv2f64r:
  case AMDIL::LLOv2i64r:
    opSwiz.bits.swizzle = AMDIL_SRC_XZXZ;
    break;
  case AMDIL::DLOf64r:
  case AMDIL::LHIi64r:
    if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_W000;
    } else {
      opSwiz.bits.swizzle = AMDIL_SRC_Y000;
    }
    reg = MI->getOperand(0).getReg();
    if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle += 1;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle += 2;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle += 3;
    } else if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle += 2;
    }
    break;
  case AMDIL::DLOv2f64r:
  case AMDIL::LHIv2i64r:
    opSwiz.bits.swizzle = AMDIL_SRC_YWYW;
    break;
  case AMDIL::DCREATEf64rr:
  {
    unsigned swiz = AMDIL_SRC_X000;
    if (isWComponentReg(reg)) {
      swiz = AMDIL_SRC_W000;
    } else if (isYComponentReg(reg)) {
      swiz = AMDIL_SRC_Y000;
    } else if (isZComponentReg(reg)) {
      swiz = AMDIL_SRC_Z000;
    }
    reg = MI->getOperand(0).getReg();
    if (isZWComponentReg(reg)) {
      swiz += 2;
    }
    opSwiz.bits.swizzle = swiz + (opNum == 1);
  }
  break;
  case AMDIL::DCREATEv2f64rr:
    opSwiz.bits.swizzle = (opNum == 1)
                          ? AMDIL_SRC_0X0Y : AMDIL_SRC_X0Y0;
    break;
  case AMDIL::LCREATEi64rr:
  {
    unsigned swiz1 = (opNum == 1) ? AMDIL_SRC_X000 : AMDIL_SRC_0X00;
    if (MI->getOperand(opNum).isReg()) {
      reg = MI->getOperand(opNum).getReg();
      if (isWComponentReg(reg)) {
        swiz1 += 12;
      } else if (isYComponentReg(reg)) {
        swiz1 += 4;
      } else if (isZComponentReg(reg)) {
        swiz1 += 8;
      }
    }
    reg = MI->getOperand(0).getReg();
    if (isZWComponentReg(reg)) {
      swiz1 += 2;
    }
    opSwiz.bits.swizzle = swiz1;
  }
  break;
  case AMDIL::LCREATEv2i64rr:
    if (isXYComponentReg(reg)) {
      opSwiz.bits.swizzle = opNum + AMDIL_SRC_YWYW;
    } else {
      opSwiz.bits.swizzle = opNum + AMDIL_SRC_YZW0;
    }
    break;
  case AMDIL::CONTINUE_LOGICALNZf64r:
  case AMDIL::BREAK_LOGICALNZf64r:
  case AMDIL::IF_LOGICALNZf64r:
  case AMDIL::CONTINUE_LOGICALZf64r:
  case AMDIL::BREAK_LOGICALZf64r:
  case AMDIL::IF_LOGICALZf64r:
  case AMDIL::CONTINUE_LOGICALNZi64r:
  case AMDIL::BREAK_LOGICALNZi64r:
  case AMDIL::IF_LOGICALNZi64r:
  case AMDIL::CONTINUE_LOGICALZi64r:
  case AMDIL::BREAK_LOGICALZi64r:
  case AMDIL::IF_LOGICALZi64r:
    assert(opNum == 0
           && "Only operand numbers 0 is custom!");
  case AMDIL::SWITCH:
    if (isXYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_XXXX;
    } else if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
    } else {
      assert(!"Found a case we don't handle!");
    }
    break;
    GENERATE_4ARG_CASE(AMDIL::UBIT_INSERTi32)
    assert((opNum == 1 || opNum == 2)
           && "Only operand numbers 1 or 2 is custom!");
    if (isXComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_XXXX;
    } else if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_YYYY;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_WWWW;
    }
    break;
    GENERATE_4ARG_CASE(AMDIL::UBIT_INSERTv2i32)
    assert((opNum == 1 || opNum == 2)
           && "Only operand numbers 1 or 2 is custom!");
    if (isXYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_XYXY;
    } else if (isZWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_ZWZW;
    }
    break;
    GENERATE_4ARG_CASE(AMDIL::UBIT_INSERTv4i32)
    assert((opNum == 1 || opNum == 2)
           && "Only operand numbers 1 or 2 is custom!");
    opSwiz.bits.swizzle = AMDIL_SRC_DFLT;
    break;
  case AMDIL::HILO_BITORv4i16rr:
    opSwiz.bits.swizzle = AMDIL_SRC_XZXZ + (opNum - 1);
    break;
  case AMDIL::HILO_BITORv2i32rr:
    if (isXComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_X000;
    } else if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_Y000;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_Z000;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_W000;
    }
    reg = MI->getOperand(0).getReg();
    if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle += 1;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle += 2;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle += 3;
    }
    break;
  case AMDIL::HILO_BITORv2i64rr:
  {
    unsigned offset = 0;

    if (isXYComponentReg(reg)) {
      offset = AMDIL_SRC_XY00;
    } else if (isZWComponentReg(reg)) {
      offset = AMDIL_SRC_ZW00;
    }
    reg = MI->getOperand(0).getReg();
    if (isZWComponentReg(reg)) {
      offset += 1;
    }
    opSwiz.bits.swizzle = offset;
  }
  break;
  }
  return opSwiz;
}
#undef GENERATE_3ARG_CASE
#undef GENERATE_4ARG_CASE

// This function checks for instructions that don't have
// normal swizzle patterns to their destination operand.
// These have to be handled on a case by case basis.
bool isCustomDstInst(MachineInstr *MI) {
  if (!MI->getDesc().getNumOperands()) return false;
  unsigned opcode = MI->getOpcode();
  unsigned regClass = MI->getDesc().OpInfo[0].RegClass;
  if ((isPtrLoadInst(MI) || isPtrStoreInst(MI))
      && ( (isLDSInst(MI)
            && (MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI16RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI8RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI32RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI64RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRF32RegClassID))
           || isGDSInst(MI)
           || isScratchInst(MI)
           || isCBInst(MI)
           || isUAVArenaInst(MI))
      && !isExtLoadInst(MI)
      && !isTruncStoreInst(MI)) {
    return true;
  }
  return MI->getDesc().TSFlags & (1ULL << AMDID::SWZLDST);
}
// This function returns the OpSwizzle with the custom swizzle set
// correclty for destination operands.
OpSwizzle getCustomDstSwizzle(MachineInstr *MI) {
  OpSwizzle opSwiz;
  opSwiz.u8all = 0;
  unsigned opcode = MI->getOpcode();
  opSwiz.bits.dst = 1;
  unsigned reg = MI->getOperand(0).isReg() ?
                 MI->getOperand(0).getReg() : 0;
  unsigned regClass = MI->getDesc().OpInfo[0].RegClass;
  if (((isPtrLoadInst(MI) || isPtrStoreInst(MI))
       && ((isLDSInst(MI)
            && (MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI16RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI8RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRI32RegClassID
                || MI->getDesc().OpInfo[1].RegClass == AMDIL::GPRF32RegClassID))
           || isGDSInst(MI)
           || isScratchInst(MI)
           || isCBInst(MI)
           || isUAVArenaInst(MI))
       && !isExtLoadInst(MI)
       && !isTruncStoreInst(MI))
      || isSemaphoreInst(MI)
      || isAppendInst(MI)) {
    opSwiz.bits.dst = 0;
    if (isXComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_XXXX;
    } else if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_YYYY;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_SRC_WWWW;
    }
    if (isPtrStoreInst(MI) && isScratchInst(MI)) {
      if (isXYComponentReg(reg)) {
        opSwiz.bits.dst = 1;
        opSwiz.bits.swizzle = AMDIL_DST_XY__;
      } else if (isZWComponentReg(reg)) {
        opSwiz.bits.dst = 1;
        opSwiz.bits.swizzle = AMDIL_DST___ZW;
      }
    }
    return opSwiz;
  }
  switch (opcode) {
  case AMDIL::HILO_BITORv4i16rr:
  case AMDIL::HILO_BITORv2i64rr:
    if (isXYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_DST_XY__;
    } else {
      opSwiz.bits.swizzle = AMDIL_DST___ZW;
    }
    break;
  default:
    assert(0
           && "getCustomDstSwizzle hit an opcode it doesnt' understand!");
    if (isXComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_DST_X___;
    } else if (isYComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_DST__Y__;
    } else if (isZComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_DST___Z_;
    } else if (isWComponentReg(reg)) {
      opSwiz.bits.swizzle = AMDIL_DST____W;
    }
  };
  return opSwiz;
}
OpSwizzle getSrcSwizzleID(MachineInstr *MI, unsigned opNum)
{
  assert(opNum < MI->getNumOperands() &&
         "Must pass in a valid operand number.");
  OpSwizzle curSwiz;
  curSwiz.u8all = 0;
  curSwiz.bits.dst = 0; // We need to reset the dst bit.
  unsigned reg = 0;
  if (MI->getOperand(opNum).isReg()) {
    reg = MI->getOperand(opNum).getReg();
  }
  if (isCustomSrcInst(MI, opNum)) {
    curSwiz = getCustomSrcSwizzle(MI, opNum);
  } else if (isXComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_XXXX;
  } else if (isYComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_YYYY;
  } else if (isZComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_ZZZZ;
  } else if (isWComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_WWWW;
  } else if (isXYComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_XYXY;
  } else if (isZWComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_SRC_ZWZW;
  } else if (reg == AMDIL::R1011
             && MI->getOpcode() == TargetOpcode::COPY) {
    reg = MI->getOperand(0).getReg();
    if (isXComponentReg(reg) || isYComponentReg(reg)
        || isZComponentReg(reg) || isWComponentReg(reg)) {
      curSwiz.bits.swizzle = AMDIL_SRC_XXXX;
    } else if (isXYComponentReg(reg) || isZWComponentReg(reg)) {
      curSwiz.bits.swizzle = AMDIL_SRC_XYXY;
    }
  } else {
    curSwiz.bits.swizzle = AMDIL_SRC_DFLT;
  }
  return curSwiz;
}
OpSwizzle getDstSwizzleID(MachineInstr *MI)
{
  OpSwizzle curSwiz;
  curSwiz.bits.dst = 1;
  curSwiz.bits.swizzle = AMDIL_DST_DFLT;
  unsigned reg = 0;
  if (MI->getOperand(0).isReg()) {
    reg = MI->getOperand(0).getReg();
  }
  if (isCustomDstInst(MI)) {
    curSwiz = getCustomDstSwizzle(MI);
  } else if (isXComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST_X___;
  } else if (isYComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST__Y__;
  } else if (isZComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST___Z_;
  } else if (isWComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST____W;
  } else if (isXYComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST_XY__;
  } else if (isZWComponentReg(reg)) {
    curSwiz.bits.swizzle = AMDIL_DST___ZW;
  } else {
    curSwiz.bits.swizzle = AMDIL_DST_DFLT;
  }

  return curSwiz;
}
void encodeVectorInst(MachineInstr *MI, bool mDebug)
{
  assert(isVectorOpInst(MI) && "Only a vector instruction can be"
         " used to generate a new vector instruction!");
  unsigned opcode = MI->getOpcode();
  // For all of the opcodes, the destination swizzle is the same.
  OpSwizzle swizID = getDstSwizzleID(MI);
  OpSwizzle srcID;
  srcID.u8all = 0;
  MI->getOperand(0).setTargetFlags(swizID.u8all);
  unsigned offset = 0;
  unsigned reg = MI->getOperand(0).getReg();
  switch (opcode) {
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2f32)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2i32)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2i16)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2i8)
    if (isZWComponentReg(reg)) {
      offset = 2;
    }
    for (unsigned x = 1; x < 3; ++x) {
      unsigned offset2 = 0;
      if (MI->getOperand(x).isReg()) {
        reg = MI->getOperand(x).getReg();
      }
      if (isXComponentReg(reg)) {
        offset2 = 0;
      } else if (isYComponentReg(reg)) {
        offset2 = 4;
      } else if (isZComponentReg(reg)) {
        offset2 = 8;
      } else if (isWComponentReg(reg)) {
        offset2 = 12;
      }
      srcID.bits.swizzle = AMDIL_SRC_X000 + offset + (x - 1) + offset2;
      MI->getOperand(x).setTargetFlags(srcID.u8all);
    }
    break;
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2f64)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv2i64)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv4f32)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv4i32)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv4i16)
    GENERATE_2ARG_CASE(AMDIL::VCONCATv4i8)
    for (unsigned x = 1; x < 3; ++x) {
      if (MI->getOperand(x).isReg()) {
        reg = MI->getOperand(x).getReg();
      }
      if (isZWComponentReg(reg)) {
        srcID.bits.swizzle = AMDIL_SRC_ZW00 + (x - 1);
      } else {
        srcID.bits.swizzle = AMDIL_SRC_XY00 + (x - 1);
      }
      MI->getOperand(x).setTargetFlags(srcID.u8all);
    }
    break;
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2f32)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2i32)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2i16)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2i8)
    assert(MI->getOperand(2).getImm() <= 2
           && "Invalid immediate value encountered for this formula!");
    if (isXComponentReg(reg)) {
      offset = 0;
    } else if (isYComponentReg(reg)) {
      offset = 1;
    } else if (isZComponentReg(reg)) {
      offset = 2;
    } else if (isWComponentReg(reg)) {
      offset = 3;
    }
    assert(MI->getOperand(2).getImm() <= 4
           && "Invalid immediate value encountered for this formula!");
    if (MI->getOperand(1).isReg()) {
      reg = MI->getOperand(1).getReg();
    }
    if (isZWComponentReg(reg)) {
      srcID.bits.swizzle = AMDIL_SRC_Z000;
    } else {
      srcID.bits.swizzle = AMDIL_SRC_X000;
    }
    srcID.bits.swizzle += offset + (MI->getOperand(2).getImm()-1) * 4;
    MI->getOperand(1).setTargetFlags(srcID.u8all);
    MI->getOperand(2).setTargetFlags(0);
    break;
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv4f32)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv4i32)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv4i16)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv4i8)
    if (isXComponentReg(reg)) {
      offset = 0;
    } else if (isYComponentReg(reg)) {
      offset = 1;
    } else if (isZComponentReg(reg)) {
      offset = 2;
    } else if (isWComponentReg(reg)) {
      offset = 3;
    } else if (isXYComponentReg(reg)) {
      offset = 0;
    } else if (isZWComponentReg(reg)) {
      offset = 2;
    }
    assert(MI->getOperand(2).getImm() <= 4
           && "Invalid immediate value encountered for this formula!");
    srcID.bits.swizzle = ((MI->getOperand(2).getImm() - 1) * 4) + 1 + offset;
    MI->getOperand(1).setTargetFlags(srcID.u8all);
    MI->getOperand(2).setTargetFlags(0);
    break;
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2f64)
    GENERATE_1ARG_CASE(AMDIL::VEXTRACTv2i64)
    assert(MI->getOperand(2).getImm() <= 2
           && "Invalid immediate value encountered for this formula!");
    if (isZWComponentReg(reg)) {
      offset = 1;
    }
    srcID.bits.swizzle = AMDIL_SRC_XY00
                         + ((MI->getOperand(2).getImm() - 1) * 2) + offset;
    MI->getOperand(1).setTargetFlags(srcID.u8all);
    MI->getOperand(2).setTargetFlags(0);
    break;
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2f32)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2i32)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2i16)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2i8)
    {
      unsigned swizVal = (unsigned)MI->getOperand(4).getImm();
      OpSwizzle src2ID;
      src2ID.u8all = 0;
      if (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP) {
        offset = 2;
      }

      unsigned offset1 = 0;
      if (MI->getOperand(1).isReg()) {
        reg = MI->getOperand(1).getReg();
        if (isZWComponentReg(reg)) {
          offset1 = 8;
        }
      }

      unsigned offset2 = 0;
      if (MI->getOperand(2).isReg()) {
        reg = MI->getOperand(2).getReg();
        if (isYComponentReg(reg)) {
          offset2 = 4;
        } else if (isZComponentReg(reg)) {
          offset2 = 8;
        } else if (isWComponentReg(reg)) {
          offset2 = 12;
        }
      }
      if (((swizVal >> 8) & 0xFF) == 1) {
        srcID.bits.swizzle = AMDIL_SRC_X000 + offset1 + offset;
        src2ID.bits.swizzle = AMDIL_SRC_0X00 + offset2 + offset;
      } else {
        srcID.bits.swizzle = AMDIL_SRC_0Y00 + offset1 + offset;
        src2ID.bits.swizzle = AMDIL_SRC_X000 + offset2 + offset;
      }
      MI->getOperand(1).setTargetFlags(srcID.u8all);
      MI->getOperand(2).setTargetFlags(src2ID.u8all);
      MI->getOperand(3).setTargetFlags(0);
      MI->getOperand(4).setTargetFlags(0);
    }
    break;
    GENERATE_2ARG_CASE(AMDIL::VINSERTv4f32)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv4i32)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv4i16)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv4i8)
    {
      unsigned swizVal = (unsigned)MI->getOperand(4).getImm();
      OpSwizzle src2ID;
      src2ID.u8all = 0;
      if (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP) {
        offset = 2;
      }
      unsigned offset2 = 0;
      if (MI->getOperand(2).isReg()) {
        reg = MI->getOperand(2).getReg();
        if (isYComponentReg(reg)) {
          offset2 = 4;
        } else if (isZComponentReg(reg)) {
          offset2 = 8;
        } else if (isWComponentReg(reg)) {
          offset2 = 12;
        } else if (isZWComponentReg(reg)) {
          offset2 = 2;
        }
      }
      if ((swizVal >> 8 & 0xFF) == 1) {
        srcID.bits.swizzle = (!offset) ? AMDIL_SRC_X0ZW : AMDIL_SRC_XYZ0;
        src2ID.bits.swizzle = AMDIL_SRC_0X00 + offset2 + offset;
      } else if ((swizVal >> 16 & 0xFF) == 1) {
        srcID.bits.swizzle = AMDIL_SRC_XY0W;
        src2ID.bits.swizzle = AMDIL_SRC_00X0 + offset2;
      } else if ((swizVal >> 24 & 0xFF) == 1) {
        srcID.bits.swizzle = AMDIL_SRC_XYZ0;
        src2ID.bits.swizzle = AMDIL_SRC_000X + offset2;
      } else {
        srcID.bits.swizzle = (!offset) ? AMDIL_SRC_0YZW : AMDIL_SRC_XY0W;
        src2ID.bits.swizzle = AMDIL_SRC_X000 + offset2 + offset;
      }
      MI->getOperand(1).setTargetFlags(srcID.u8all);
      MI->getOperand(2).setTargetFlags(src2ID.u8all);
      MI->getOperand(3).setTargetFlags(0);
      MI->getOperand(4).setTargetFlags(0);
    }
    break;
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2f64)
    GENERATE_2ARG_CASE(AMDIL::VINSERTv2i64)
    {
      unsigned swizVal = (unsigned)MI->getOperand(4).getImm();
      OpSwizzle src2ID;
      src2ID.u8all = 0;
      if (MI->getOperand(2).isReg()) {
        reg = MI->getOperand(2).getReg();
        if (isZWComponentReg(reg)) {
          offset = 2;
        }
      }
      if (((swizVal >> 8) & 0xFF) == 1) {
        srcID.bits.swizzle = AMDIL_SRC_XY00;
        src2ID.bits.swizzle = AMDIL_SRC_00XY + offset;
      } else {
        srcID.bits.swizzle = AMDIL_SRC_00ZW;
        src2ID.bits.swizzle = AMDIL_SRC_XY00 + offset;
      }
      MI->getOperand(1).setTargetFlags(srcID.u8all);
      MI->getOperand(2).setTargetFlags(src2ID.u8all);
      MI->getOperand(3).setTargetFlags(0);
      MI->getOperand(4).setTargetFlags(0);
    }
    break;
  };
  if (mDebug) {
    for (unsigned i = 0; i < MI->getNumOperands(); ++i) {
      dumpOperand(MI, i);
    }
    dbgs() << "\n";
  }
}
// This function loops through all of the instructions, skipping function
// calls, and encodes the swizzles in the operand.
void encodeSwizzles(MachineFunction &MF, bool mDebug)
{
  for (MachineFunction::iterator MFI = MF.begin(), MFE = MF.end();
       MFI != MFE; ++MFI) {
    MachineBasicBlock *MBB = MFI;
    for (MachineBasicBlock::iterator MBI = MBB->begin(), MBE = MBB->end();
         MBI != MBE; ++MBI) {
      MachineInstr *MI = MBI;
      if (MI->getOpcode() == AMDIL::RETDYN
          || MI->getOpcode() == AMDIL::RETURN
          || MI->getOpcode() == AMDIL::DBG_VALUE) {
        continue;
      }
      if (mDebug) {
        dbgs() << "Encoding instruction: ";
        MI->print(dbgs());
      }
      if (isVectorOpInst(MI)) {
        encodeVectorInst(MI, mDebug);
        continue;
      }
      for (unsigned a = 0, z = MI->getNumOperands(); a < z; ++a) {
        OpSwizzle swizID;
        if (MI->getOperand(a).isReg() && MI->getOperand(a).isDef()) {
          swizID = getDstSwizzleID(MI);
        } else {
          swizID = getSrcSwizzleID(MI, a);
        }
        MI->getOperand(a).setTargetFlags(swizID.u8all);
        if (mDebug) {
          dumpOperand(MI, a);
        }
      }
      if (mDebug) {
        dbgs() << "\n";
      }
    }
  }
}
