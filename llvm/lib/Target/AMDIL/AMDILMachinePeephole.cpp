//===-- AMDILMachinePeephole.cpp ------------------------------------------===//
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

#define DEBUG_TYPE "machine_peephole"
#if !defined(NDEBUG) && !defined(USE_APPLE)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME (false)
#endif

#include "AMDIL.h"
#include "AMDILSubtarget.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"


using namespace llvm;
namespace
{
class AMDILMachinePeephole : public MachineFunctionPass
{
  typedef std::map<uint32_t, uint32_t> Reg2RegMap;
public:
  static char ID;
  AMDILMachinePeephole(TargetMachine &tm, CodeGenOpt::Level OL);
  // virtual ~AMDILMachinePeephole();
  virtual const char*
  getPassName() const;
  virtual bool
  runOnMachineFunction(MachineFunction &MF);
private:
  void insertFence(MachineBasicBlock::iterator &MIB);
  inline bool useSWByteShortReg(short opRegClassID);
  inline uint32_t genVReg(uint32_t regType) const;
  inline MachineInstrBuilder
  generateMachineInst(uint32_t opcode,
                      uint32_t dst,
                      MachineBasicBlock::iterator &MIB) const;
  inline MachineInstrBuilder
  generateMachineInst(uint32_t opcode,
                      uint32_t dst,
                      uint32_t src1,
                      MachineBasicBlock::iterator &MIB) const;
  inline MachineInstrBuilder
  generateMachineInst(uint32_t opcode,
                      uint32_t dst,
                      uint32_t src1,
                      uint32_t src2,
                      MachineBasicBlock::iterator &MIB) const;
  MachineInstr* findExtendInstruction(MachineOperand& op, bool isSigned);
  void getExtendOpcodes(uint32_t regClassID,
                        bool isSigned,
                        int64_t& constVal,
                        int& promoteOp,
                        int& demoteOp,
                        int& binaryAndOp,
                        int& shlOp,
                        int& shrOp,
                        uint32_t& intRegClassID);
  uint32_t addExtendInstruction(MachineBasicBlock::iterator &MIB,
                                uint32_t reg,
                                bool isSigned);
  void extendOperand(MachineBasicBlock::iterator &MIB,
                     uint32_t opIdx,
                     bool isSigned);
  void zeroExtend(MachineBasicBlock::iterator &MIB, uint32_t opIdx) {
    extendOperand(MIB, opIdx, false);
  }
  void signExtend(MachineBasicBlock::iterator &MIB, uint32_t opIdx) {
    extendOperand(MIB, opIdx, true);
  }

  TargetMachine &TM;
  MachineFunction* MFP;
  bool mDebug;
  // map from a register to its sign-extention
  Reg2RegMap sextMap;
  // map from a register to its zero-extention
  Reg2RegMap zextMap;
}; // AMDILMachinePeephole
char AMDILMachinePeephole::ID = 0;
} // anonymous namespace

namespace llvm
{
FunctionPass*
createAMDILMachinePeephole(TargetMachine &tm, CodeGenOpt::Level OL)
{
  return new AMDILMachinePeephole(tm, OL);
}
} // llvm namespace

AMDILMachinePeephole::AMDILMachinePeephole(TargetMachine &tm, CodeGenOpt::Level OL)
  : MachineFunctionPass(ID), TM(tm), MFP(NULL), sextMap(), zextMap()
{
  mDebug = DEBUGME;
}

bool
AMDILMachinePeephole::runOnMachineFunction(MachineFunction &MF)
{
  MFP = &MF;
  sextMap.clear();
  zextMap.clear();
  bool Changed = false;
  const AMDILSubtarget *STM = &TM.getSubtarget<AMDILSubtarget>();
  for (MachineFunction::iterator MBB = MF.begin(), MBE = MF.end();
       MBB != MBE; ++MBB) {
    MachineBasicBlock *mb = MBB;
    for (MachineBasicBlock::iterator MIB = mb->begin(), MIE = mb->end();
         MIB != MIE; ++MIB) {
      MachineInstr *mi = MIB;
      switch (mi->getOpcode()) {
      default:
        if (isAtomicInst(TM,mi)) {
          // If we don't support the hardware accellerated address spaces,
          // then the atomic needs to be transformed to the global atomic.
          if (strstr(TM.getInstrInfo()->getName(mi->getOpcode()), "_L_")
              && STM->device()->usesSoftware(AMDILDeviceInfo::LocalMem)) {
            BuildMI(*mb, MIB, mi->getDebugLoc(),
                    TM.getInstrInfo()->get(AMDIL::ADD_i32), AMDIL::R1011)
            .addReg(mi->getOperand(1).getReg())
            .addReg(AMDIL::T2);
            mi->getOperand(1).setReg(AMDIL::R1011);
            mi->setDesc(
              TM.getInstrInfo()->get(
                (mi->getOpcode() - AMDIL::ATOM_L_ADD) + AMDIL::ATOM_G_ADD));
          } else if (strstr(TM.getInstrInfo()->getName(mi->getOpcode()), "_R_")
                     && STM->device()->usesSoftware(AMDILDeviceInfo::RegionMem)) {
            assert(!"Software region memory is not supported!");
            mi->setDesc(
              TM.getInstrInfo()->get(
                (mi->getOpcode() - AMDIL::ATOM_R_ADD) + AMDIL::ATOM_G_ADD));
          }
        } else if ((isLoadInst(TM, mi) || isStoreInst(TM, mi)) && isVolatileInst(mi)) {
          insertFence(MIB);
        }
        continue;
        break;

        // Implement software emulated i8/i16 types by sign/zero extending
        // i8/i16 type operands of instructions.
        // To avoid generating too many sign/zero extensions, we do this only
        // where its needed:
        // sign/zero-extend i8/i16 type operands if the bits in the
        // upper bits affects the result of the instruction
        ExpandCaseToByteShortScalarTypes(AMDIL::CONTINUEC)
        ExpandCaseToByteShortScalarTypes(AMDIL::BREAKC)
        // ExpandCaseToByteShortScalarTypes(AMDIL::BRANCH_COND)
        ExpandCaseToByteShortScalarTypes(AMDIL::IFC) {
          // we are not generating the above currently:
          assert(0 && "unexpected instruction");
          break;
        }
        ExpandCaseToByteShortScalarTypes(AMDIL::BREAK_LOGICALNZ)
        ExpandCaseToByteShortScalarTypes(AMDIL::BREAK_LOGICALZ)
        ExpandCaseToByteShortScalarTypes(AMDIL::CONTINUE_LOGICALNZ)
        ExpandCaseToByteShortScalarTypes(AMDIL::CONTINUE_LOGICALZ)
        ExpandCaseToByteShortScalarTypes(AMDIL::IF_LOGICALNZ)
        ExpandCaseToByteShortScalarTypes(AMDIL::IF_LOGICALZ) {
          short opRegClassID = mi->getDesc().OpInfo[0].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            zeroExtend(MIB, 0);
          }
          break;
        }
        ExpandCaseToByteShortScalarTypes(AMDIL::SELECTBIN)
        ExpandCaseToByteShortTypes(AMDIL::CMOVLOG)
        ExpandCaseToByteShortTypes(AMDIL::CMOV)
        // ExpandCaseToByteShortTypes(AMDIL::EADD)
        // find first hi/low bit
        ExpandCaseToByteShortTypes(AMDIL::IFFB_HI)
        ExpandCaseToByteShortTypes(AMDIL::IFFB_LO)
        ExpandCaseToByteShortTypes(AMDIL::USHR)
        ExpandCaseToByteShortTypes(AMDIL::USHRVEC) {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            zeroExtend(MIB, 1);
          }
          break;
        }
        ExpandCaseToByteShortTypes(AMDIL::NEGATE)
        ExpandCaseToByteShortTypes(AMDIL::SHR)
        ExpandCaseToByteShortTypes(AMDIL::SHRVEC) {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            signExtend(MIB, 1);
          }
          break;
        }
        ExpandCaseToByteShortScalarTypes(AMDIL::MACRO__sdiv)
        ExpandCaseToByteShortScalarTypes(AMDIL::MACRO__smod)
        ExpandCaseToByteShortTypes(AMDIL::DIV_INF)
        ExpandCaseToByteShortTypes(AMDIL::SMAX)
        ExpandCaseToByteShortTypes(AMDIL::SMULHI)
        ExpandCaseToByteShortTypes(AMDIL::SMUL) {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          assert(opRegClassID == mi->getDesc().OpInfo[2].RegClass
                 && "instruction ops have different type");
          if (useSWByteShortReg(opRegClassID)) {
            signExtend(MIB, 1);
            signExtend(MIB, 2);
          }
          break;
        }
        ExpandCaseToByteShortScalarTypes(AMDIL::MACRO__udiv)
        ExpandCaseToByteShortScalarTypes(AMDIL::MACRO__umod)
        ExpandCaseToByteShortTypes(AMDIL::UDIV)
        ExpandCaseToByteShortTypes(AMDIL::UMULHI) {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          assert(opRegClassID == mi->getDesc().OpInfo[2].RegClass
                 && "instruction ops have different type");
          if (useSWByteShortReg(opRegClassID)) {
            zeroExtend(MIB, 1);
            zeroExtend(MIB, 2);
          }
          break;
        }
        // This works around a restriction in AMDIL where the
        // result of a comparison can only be in the lower
        // 2 components.
      case AMDIL::LEQ:
      case AMDIL::LGE:
      case AMDIL::LLE:
      case AMDIL::LGT:
      case AMDIL::LLT:
      case AMDIL::LNE:
      case AMDIL::ULLE:
      case AMDIL::ULGT:
      case AMDIL::ULGE:
      case AMDIL::ULLT: {
        if (isZWComponentReg(mi->getOperand(0).getReg())) {
          MachineInstr *newmi = BuildMI(MF, mi->getDebugLoc(),
                                        TM.getInstrInfo()->get(AMDIL::MOVE_i64),
                                        mi->getOperand(0).getReg()).addReg(AMDIL::Rxy1000);
          mi->getOperand(0).setReg(AMDIL::Rxy1000);
          mi->getParent()->insertAfter(MIB, newmi);
        }
      }
      break;
      }
    }
  }
  return Changed;
}

const char*
AMDILMachinePeephole::getPassName() const
{
  return "AMDIL Generic Machine Peephole Optimization Pass";
}

void
AMDILMachinePeephole::insertFence(MachineBasicBlock::iterator &MIB)
{
  MachineInstr *MI = MIB;
  MachineInstr *fence = BuildMI(*(MI->getParent()->getParent()),
                                MI->getDebugLoc(),
                                TM.getInstrInfo()->get(AMDIL::FENCE)).addReg(1);

  MI->getParent()->insert(MIB, fence);
  fence = BuildMI(*(MI->getParent()->getParent()),
                  MI->getDebugLoc(),
                  TM.getInstrInfo()->get(AMDIL::FENCE)).addReg(1);
  MIB = MI->getParent()->insertAfter(MIB, fence);
}

// returns if the given register class is software emulated byte or short
bool AMDILMachinePeephole::useSWByteShortReg(short opRegClassID)
{
  if ((opRegClassID == AMDIL::GPRI16RegClassID
       || opRegClassID == AMDIL::GPRV2I16RegClassID
       || opRegClassID == AMDIL::GPRV4I16RegClassID)
      && TM.getSubtarget<AMDILSubtarget>()
      .device()->usesSoftware(AMDILDeviceInfo::ShortOps)) {
    return true;
  }
  if ((opRegClassID == AMDIL::GPRI8RegClassID
       || opRegClassID == AMDIL::GPRV2I8RegClassID
       || opRegClassID == AMDIL::GPRV4I8RegClassID)
      && TM.getSubtarget<AMDILSubtarget>()
      .device()->usesSoftware(AMDILDeviceInfo::ByteOps)) {
    return true;
  }
  return false;
}

uint32_t AMDILMachinePeephole::genVReg(uint32_t regType) const
{
  return MFP->getRegInfo().createVirtualRegister(getRegClassFromID(regType));
}

MachineInstrBuilder
AMDILMachinePeephole::generateMachineInst(uint32_t opcode,
    uint32_t dst,
    MachineBasicBlock::iterator &MIB)
const
{
  MachineInstr* mi = MIB;
  MachineBasicBlock* mb = mi->getParent();
  return BuildMI(*mb, MIB, mi->getDebugLoc(),
                 TM.getInstrInfo()->get(opcode), dst);
}

MachineInstrBuilder
AMDILMachinePeephole::generateMachineInst(uint32_t opcode,
    uint32_t dst,
    uint32_t src1,
    MachineBasicBlock::iterator &MIB)
const
{
  return generateMachineInst(opcode, dst, MIB).addReg(src1);
}

MachineInstrBuilder
AMDILMachinePeephole::generateMachineInst(uint32_t opcode,
    uint32_t dst,
    uint32_t src1,
    uint32_t src2,
    MachineBasicBlock::iterator &MIB)
const
{
  return generateMachineInst(opcode, dst, src1, MIB).addReg(src2);
}

// Find a MachineInstr that uses the given register and has the given opcode.
// Return NULL if not found.
static inline MachineInstr* findRegUse(uint32_t reg, int opcode,
                                       const MachineRegisterInfo& MRI)
{
  for (MachineRegisterInfo::use_iterator it = MRI.use_begin(reg),
       end = MRI.use_end();
       it != end;
       ++it) {
    MachineInstr& useInst = *it;
    if (useInst.getOpcode() == opcode) {
      for (MachineInstr::mop_iterator it2 = useInst.operands_begin(),
           end2 = useInst.operands_end();
           it2 != end2; ++it2) {
        MachineOperand& op = *it2;
        if (op.isUse() && op.isReg() && op.getReg() == reg) {
          return &useInst;
        }
      }
    }
  }
  return NULL;
}

// Find a MachineInstr that uses the given register and immediate,
// and has the given opcode.
// Return NULL if not found.
static inline MachineInstr* findRegUse(uint32_t reg, int opcode, int64_t imm,
                                       const MachineRegisterInfo& MRI)
{
  for (MachineRegisterInfo::use_iterator it = MRI.use_begin(reg),
       end = MRI.use_end();
       it != end;
       ++it) {
    MachineInstr& useInst = *it;
    if (useInst.getOpcode() == opcode) {
      bool foundRegUse = false;
      bool foundImmUse = false;
      for (MachineInstr::mop_iterator it2 = useInst.operands_begin(),
           end2 = useInst.operands_end();
           it2 != end2; ++it2) {
        MachineOperand& op = *it2;
        if (op.isUse() && op.isReg() && op.getReg() == reg) {
          foundRegUse = true;
        } else if (op.isUse() && op.isImm() && op.getImm() == imm) {
          foundImmUse = true;
        }
      }
      if (foundRegUse && foundImmUse) {
        return &useInst;
      }
    }
  }
  return NULL;
}

// returns if the given MachineInstr defines exactly 1 register operand
static inline bool hasSingleRegDef(MachineInstr& inst)
{
  size_t nDefs = 0;
  for (MachineInstr::mop_iterator it = inst.operands_begin(),
       end = inst.operands_end();
       it != end; ++it) {
    MachineOperand& op = *it;
    if (!op.isDef() || !op.isReg()) {
      continue;
    }
    ++nDefs;
    if (nDefs > 1) {
      return false;
    }
  }
  return nDefs == 1;
}

// returns the first register this MachineInstr defines
static inline uint32_t firstDefReg(MachineInstr& inst)
{
  for (MachineInstr::mop_iterator it = inst.operands_begin(),
       end = inst.operands_end();
       it != end; ++it) {
    MachineOperand& op = *it;
    if (op.isDef() && op.isReg()) {
      return op.getReg();
    }
  }
  assert(0 && "should not reach");
  return 0;
}

// Find sign extension sequence such as the following:
// reg1 = IL_ASINT_i8 reg
// reg2 = SHL_i32 reg1, 24
// reg3 = SHR_i32 reg2, 24
// reg4 = IL_ASCHAR_i32 reg3
// or zero extension sequence such as the following:
// reg1 = IL_ASINT_i8 reg
// reg2 = BINARY_AND_i32 reg1, 0xff
// reg3 = IL_ASCHAR_i32 reg2
// The above sequence does sign/zero-extension to reg if reg is of type i8
// Return the last instruction in the sequence
// Return NULL if no such sequence found
MachineInstr* AMDILMachinePeephole::findExtendInstruction(MachineOperand& op,
    bool isSigned)
{
  unsigned opReg = op.getReg();
  uint32_t regClassID = MFP->getRegInfo().getRegClass(opReg)->getID();
  int64_t constVal;
  int promoteOp;
  int demoteOp;
  int binaryAndOp;
  int shlOp;
  int shrOp;
  uint32_t intRegClassID;
  getExtendOpcodes(regClassID, isSigned, constVal, promoteOp, demoteOp,
                   binaryAndOp, shlOp, shrOp, intRegClassID);
  const MachineRegisterInfo& MRI = MFP->getRegInfo();
  MachineInstr* promoteInst = findRegUse(opReg, promoteOp, MRI);
  if (promoteInst == NULL) return NULL;
  if (!hasSingleRegDef(*promoteInst)) return NULL;
  uint32_t reg1 = firstDefReg(*promoteInst);
  uint32_t reg3;
  if (isSigned) {
    MachineInstr* shlInst = findRegUse(reg1, shlOp, constVal, MRI);
    if (shlInst == NULL) return NULL;
    if (!hasSingleRegDef(*shlInst)) return NULL;
    uint32_t reg2 = firstDefReg(*shlInst);
    MachineInstr* shrInst = findRegUse(reg2, shrOp, constVal, MRI);
    if (shrInst == NULL) return NULL;
    if (!hasSingleRegDef(*shrInst)) return NULL;
    reg3 = firstDefReg(*shrInst);
  } else {
    MachineInstr* andInst = findRegUse(reg1, binaryAndOp, constVal, MRI);
    if (andInst == NULL) return NULL;
    if (!hasSingleRegDef(*andInst)) return NULL;
    reg3 = firstDefReg(*andInst);
  }
  MachineInstr* demoteInst = findRegUse(reg3, demoteOp, MRI);
  if (demoteInst == NULL) return NULL;
  if (!hasSingleRegDef(*demoteInst)) return NULL;
  return demoteInst;
}

// returns opcodes to be used to sign/zero extend the given register class
void
AMDILMachinePeephole::getExtendOpcodes(uint32_t regClassID,
                                       bool isSigned,
                                       int64_t& constVal,
                                       int& promoteOp,
                                       int& demoteOp,
                                       int& binaryAndOp,
                                       int& shlOp,
                                       int& shrOp,
                                       uint32_t& intRegClassID)
{
  switch(regClassID) {
  default:
    assert(0 && "unexpected reg class");
  case AMDIL::GPRI8RegClassID:
    constVal = isSigned ? 24 : 0xFF;
    promoteOp = AMDIL::IL_ASINT_i8;
    demoteOp = AMDIL::IL_ASCHAR_i32;
    binaryAndOp = AMDIL::BINARY_AND_i32;
    intRegClassID = AMDIL::GPRI32RegClassID;
    shlOp = AMDIL::SHL_i32;
    shrOp = AMDIL::SHR_i32;
    break;
  case AMDIL::GPRV2I8RegClassID:
    constVal = isSigned ? 24 : 0xFF;
    promoteOp = AMDIL::IL_ASV2INT_v2i8;
    demoteOp = AMDIL::IL_ASV2CHAR_v2i32;
    binaryAndOp = AMDIL::BINARY_AND_v2i32;
    intRegClassID = AMDIL::GPRV2I32RegClassID;
    shlOp = AMDIL::SHLVEC_v2i32;
    shrOp = AMDIL::SHRVEC_v2i32;
    break;
  case AMDIL::GPRV4I8RegClassID:
    constVal = isSigned ? 24 : 0xFF;
    promoteOp = AMDIL::IL_ASV4INT_v4i8;
    demoteOp = AMDIL::IL_ASV4CHAR_v4i32;
    binaryAndOp = AMDIL::BINARY_AND_v4i32;
    intRegClassID = AMDIL::GPRV4I32RegClassID;
    shlOp = AMDIL::SHLVEC_v4i32;
    shrOp = AMDIL::SHRVEC_v4i32;
    break;
  case AMDIL::GPRI16RegClassID:
    constVal = isSigned ? 16 : 0xFFFF;
    promoteOp = AMDIL::IL_ASINT_i16;
    demoteOp = AMDIL::IL_ASSHORT_i32;
    binaryAndOp = AMDIL::BINARY_AND_i32;
    intRegClassID = AMDIL::GPRI32RegClassID;
    shlOp = AMDIL::SHL_i32;
    shrOp = AMDIL::SHR_i32;
    break;
  case AMDIL::GPRV2I16RegClassID:
    constVal = isSigned ? 16 : 0xFFFF;
    promoteOp = AMDIL::IL_ASV2INT_v2i16;
    demoteOp = AMDIL::IL_ASV2SHORT_v2i32;
    binaryAndOp = AMDIL::BINARY_AND_v2i32;
    intRegClassID = AMDIL::GPRV2I32RegClassID;
    shlOp = AMDIL::SHLVEC_v2i32;
    shrOp = AMDIL::SHRVEC_v2i32;
    break;
  case AMDIL::GPRV4I16RegClassID:
    constVal = isSigned ? 16 : 0xFFFF;
    promoteOp = AMDIL::IL_ASV4INT_v4i16;
    demoteOp = AMDIL::IL_ASV4SHORT_v4i32;
    binaryAndOp = AMDIL::BINARY_AND_v4i32;
    intRegClassID = AMDIL::GPRV4I32RegClassID;
    shlOp = AMDIL::SHLVEC_v4i32;
    shrOp = AMDIL::SHRVEC_v4i32;
    break;
  }
}

// create sequence of instructions to sign/zero extend the given register
uint32_t
AMDILMachinePeephole::addExtendInstruction(MachineBasicBlock::iterator &MIB,
    uint32_t reg,
    bool isSigned)
{
  int64_t constVal;
  int promoteOp;
  int demoteOp;
  int binaryAndOp;
  int shlOp;
  int shrOp;
  uint32_t intRegClassID;
  uint32_t regClassID = MFP->getRegInfo().getRegClass(reg)->getID();
  getExtendOpcodes(regClassID, isSigned, constVal, promoteOp, demoteOp,
                   binaryAndOp, shlOp, shrOp, intRegClassID);
  uint32_t constReg = genVReg(AMDIL::GPRI32RegClassID);
  uint32_t intReg = genVReg(intRegClassID);
  uint32_t intReg2 = genVReg(intRegClassID);
  uint32_t dstReg = genVReg(regClassID);
  generateMachineInst(promoteOp, intReg, reg, MIB);
  generateMachineInst(AMDIL::LOADCONST_i32, constReg, MIB).addImm(constVal);
  if (isSigned) {
    uint32_t intReg3 = genVReg(intRegClassID);
    generateMachineInst(shlOp, intReg3, intReg, constReg, MIB);
    generateMachineInst(shrOp, intReg2, intReg3, constReg, MIB);
  } else {
    generateMachineInst(binaryAndOp, intReg2, intReg, constReg, MIB);
  }
  generateMachineInst(demoteOp, dstReg, intReg2, MIB);
  return dstReg;
}

// sign/zero extend an operand of a MachineInstr by either reuse an existing
// sequence of sign/zero extension of the operand or by creating a new sequence.
void
AMDILMachinePeephole::extendOperand(MachineBasicBlock::iterator &MIB,
                                    uint32_t opIdx,
                                    bool isSigned)
{
  MachineInstr* mi = MIB;
  DEBUG(dbgs() << (isSigned ? "sign" : "zero") << " extending operand "
        << opIdx << " for " << *mi);
  MachineOperand& op = mi->getOperand(opIdx);
  assert(op.isReg() && op.isUse() && "extending non-register or def operand");
  uint32_t opReg = op.getReg();
  uint32_t newOpReg;

  assert((unsigned)MFP->getRegInfo().getRegClass(opReg)->getID()
         == (unsigned)mi->getDesc().OpInfo[opIdx].RegClass
         && "inconsistent op reg class");

  // first check the sext/zext map to see if it already has a sign/zero
  // extention, if so, reuse it
  Reg2RegMap& map = isSigned ? sextMap : zextMap;
  Reg2RegMap::iterator it = map.find(opReg);
  if (it != map.end()) {
    DEBUG(dbgs() << "Found in map ");
    newOpReg = it->second;
  } else {
    // not in the map. See if we can find in the DFG
    MachineInstr* extendInst = findExtendInstruction(op, isSigned);
    if (extendInst && hasSingleRegDef(*extendInst)) {
      newOpReg = firstDefReg(*extendInst);
      DEBUG(dbgs() << "Found in DFG ");
    } else {
      // not in the DFG either. Create sign/zero extention.
      newOpReg = addExtendInstruction(MIB, opReg, isSigned);
      DEBUG(dbgs() << "Created ");
    }
    map[opReg] = newOpReg;
  }
  DEBUG(dbgs() << (isSigned ? "sign" : "zero") << " extension vreg"
        << TargetRegisterInfo::virtReg2Index(newOpReg) << " for vreg"
        << TargetRegisterInfo::virtReg2Index(opReg) << "\n");
  op.setReg(newOpReg);
}

