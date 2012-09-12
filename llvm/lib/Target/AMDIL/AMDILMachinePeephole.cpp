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
#include "llvm/InitializePasses.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm
{
extern void initializeAMDILMachinePeepholePass(llvm::PassRegistry&);
}

using namespace llvm;
namespace
{
class AMDILMachinePeephole : public MachineFunctionPass
{
typedef SmallVector<MachineBasicBlock*, 32> MachineBlockVec;
typedef SmallVector<MachineInstr*, 4> MachineInstVec;
typedef std::map<uint32_t, MachineInstVec*> Reg2InstsMap;

public:
  static char ID;
  AMDILMachinePeephole();
  // virtual ~AMDILMachinePeephole();
  virtual const char*
  getPassName() const;
  virtual bool
  runOnMachineFunction(MachineFunction &MF);
  virtual void
  getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesCFG();
    AU.addPreserved<MachineDominatorTree>();
    AU.addRequired<MachineDominatorTree>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
private:
  void insertFence(MachineBasicBlock::iterator &MIB);
  inline bool useSWByteShortReg(short opRegClassID);
  inline uint32_t genVReg(uint32_t regType) const;
  bool findExtendSequence(MachineOperand& op, bool isSigned,
                          MachineInstVec& extendSeq);
  void getExtendOpcodes(uint32_t regClassID,
                        bool isSigned,
                        uint32_t& nConsts,
                        int64_t* constVal,
                        int& promoteOp,
                        int& demoteOp,
                        int* extendOps,
                        uint32_t& intRegClassID);
  uint32_t addExtendInstructions(MachineBasicBlock& BB,
                                 MachineBasicBlock::instr_iterator I,
                                 uint32_t reg,
                                 bool isSigned,
                                 MachineInstVec& extendSeq);
  void moveInsts(MachineInstVec& insts,
                 MachineBasicBlock& BB,
                 MachineBasicBlock::instr_iterator I);
  void extendOperand(MachineBasicBlock::iterator &MIB,
                     uint32_t opIdx,
                     bool isSigned);
  void zeroExtend(MachineBasicBlock::iterator &MIB, uint32_t opIdx) {
    extendOperand(MIB, opIdx, false);
  }
  void signExtend(MachineBasicBlock::iterator &MIB, uint32_t opIdx) {
    extendOperand(MIB, opIdx, true);
  }
  const TargetMachine *TM;
  const MachineDominatorTree *DomTree;
  MachineFunction* MFP;
  bool mDebug;
  // map from a register to its sign-extention sequence
  Reg2InstsMap sextMap;
  // map from a register to its zero-extention sequence
  Reg2InstsMap zextMap;
};   // AMDILMachinePeephole
} // anonymous namespace

char AMDILMachinePeephole::ID = 0;
INITIALIZE_PASS_BEGIN(AMDILMachinePeephole, "amdil-machine-peephole",
                      "AMDIL Machine Peephole Optimization", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(AMDILMachinePeephole, "amdil-machine-peephole",
                    "AMDIL Machine Peephole Optimization", false, false)

namespace llvm
{
FunctionPass*
createAMDILMachinePeephole()
{
  return new AMDILMachinePeephole();
}
} // llvm namespace

AMDILMachinePeephole::AMDILMachinePeephole()
  : MachineFunctionPass(ID),
    TM(NULL), DomTree(NULL), MFP(NULL), sextMap(), zextMap()
{
  mDebug = DEBUGME;
  initializeAMDILMachinePeepholePass(*PassRegistry::getPassRegistry());
}
#define GENERATE_3ARG_CASE(A) \
case A ## rrr: \
case A ## irr: \
case A ## rir: \
case A ## iir: \
case A ## rri: \
case A ## iri: \
case A ## rii: \
case A ## iii:

#define GENERATE_SH2ARG_CASE(A) \
case A ## rr: \
case A ## ri:

#define GENERATE_2ARG_CASE(A) \
case A ## rr: \
case A ## ri: \
case A ## ii:

#define GENERATE_1ARG_CASE(A) \
case A ## r: \
case A ## i:

bool
AMDILMachinePeephole::runOnMachineFunction(MachineFunction &MF)
{
  MFP = &MF;
  TM = &MF.getTarget();
  DomTree = &getAnalysis<MachineDominatorTree>();

  bool Changed = false;
  const AMDILSubtarget *STM = &TM->getSubtarget<AMDILSubtarget>();
  for (MachineFunction::iterator MBB = MF.begin(), MBE = MF.end();
       MBB != MBE; ++MBB) {
    MachineBasicBlock *mb = MBB;
    for (MachineBasicBlock::iterator MIB = mb->begin(), MIE = mb->end();
         MIB != MIE; ++MIB) {
      MachineInstr *mi = MIB;
      switch (mi->getOpcode()) {
      default:
        if (isAtomicInst(mi)) {
          // If we don't support the hardware accellerated address spaces,
          // then the atomic needs to be transformed to the global atomic.
          if (strstr(TM->getInstrInfo()->getName(mi->getOpcode()), "_L_")
              && STM->device()->usesSoftware(AMDILDeviceInfo::LocalMem)) {
            BuildMI(*mb, MIB, mi->getDebugLoc(),
                    TM->getInstrInfo()->get(AMDIL::ADDi32rr), AMDIL::R1011)
            .addReg(mi->getOperand(1).getReg())
            .addReg(AMDIL::T2);
            mi->getOperand(1).setReg(AMDIL::R1011);
            mi->setDesc(
              TM->getInstrInfo()->get(
                (mi->getOpcode() - AMDIL::ATOM_L_ADD) + AMDIL::ATOM_G_ADD));
          } else if (strstr(TM->getInstrInfo()->getName(mi->getOpcode()), "_R_")
                     && STM->device()->usesSoftware(AMDILDeviceInfo::RegionMem))
          {
            assert(!"Software region memory is not supported!");
            mi->setDesc(
              TM->getInstrInfo()->get(
                (mi->getOpcode() - AMDIL::ATOM_R_ADD) + AMDIL::ATOM_G_ADD));
          }
        } else if (isVolatileInst(mi) &&
                   (isPtrLoadInst(mi) || isPtrStoreInst(mi))) {
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
      case AMDIL::CONTINUECi8rr:
      case AMDIL::CONTINUECi16rr:
      case AMDIL::BREAKCi8rr:
      case AMDIL::BREAKCi16rr:
      case AMDIL::IFCi8rr:
      case AMDIL::IFCi16rr:
      {
        // we are not generating the above currently:
        assert(0 && "unexpected instruction");
        break;
      }
      case AMDIL::BREAK_LOGICALNZi16r:
      case AMDIL::BREAK_LOGICALNZi8r:
      case AMDIL::BREAK_LOGICALZi16r:
      case AMDIL::BREAK_LOGICALZi8r:
      case AMDIL::CONTINUE_LOGICALNZi16r:
      case AMDIL::CONTINUE_LOGICALNZi8r:
      case AMDIL::CONTINUE_LOGICALZi16r:
      case AMDIL::CONTINUE_LOGICALZi8r:
      case AMDIL::IF_LOGICALNZi16r:
      case AMDIL::IF_LOGICALNZi8r:
      case AMDIL::IF_LOGICALZi16r:
      case AMDIL::IF_LOGICALZi8r:
      {
        short opRegClassID = mi->getDesc().OpInfo[0].RegClass;
        if (useSWByteShortReg(opRegClassID)) {
          zeroExtend(MIB, 0);
        }
        break;
      }
        GENERATE_SH2ARG_CASE(AMDIL::USHRi8i8)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i8i8)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv4i8i8)
        GENERATE_SH2ARG_CASE(AMDIL::USHRi8i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i8i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv4i8i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRi8i64)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i8i64)
        GENERATE_SH2ARG_CASE(AMDIL::USHRi16i16)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i16i16)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv4i16i16)
        GENERATE_SH2ARG_CASE(AMDIL::USHRi16i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i16i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv4i16i32)
        GENERATE_SH2ARG_CASE(AMDIL::USHRi16i64)
        GENERATE_SH2ARG_CASE(AMDIL::USHRv2i16i64)
        {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            zeroExtend(MIB, 1);
          }
          break;
        }
      case AMDIL::NEGi8r:
      case AMDIL::NEGi16r:
      case AMDIL::NEGv2i8r:
      case AMDIL::NEGv2i16r:
      case AMDIL::NEGv4i8r:
      case AMDIL::NEGv4i16r:
      case AMDIL::NOTi8r:
      case AMDIL::NOTi16r:
      case AMDIL::NOTv2i8r:
      case AMDIL::NOTv2i16r:
      case AMDIL::NOTv4i8r:
      case AMDIL::NOTv4i16r:
        GENERATE_SH2ARG_CASE(AMDIL::SHRi8i8)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i8i8)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv4i8i8)
        GENERATE_SH2ARG_CASE(AMDIL::SHRi8i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i8i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv4i8i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRi8i64)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i8i64)
        GENERATE_SH2ARG_CASE(AMDIL::SHRi16i16)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i16i16)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv4i16i16)
        GENERATE_SH2ARG_CASE(AMDIL::SHRi16i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i16i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv4i16i32)
        GENERATE_SH2ARG_CASE(AMDIL::SHRi16i64)
        GENERATE_SH2ARG_CASE(AMDIL::SHRv2i16i64)
        {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            signExtend(MIB, 1);
          }
          break;
        }
        GENERATE_2ARG_CASE(AMDIL::SMULi8)
        GENERATE_2ARG_CASE(AMDIL::SMULv2i8)
        GENERATE_2ARG_CASE(AMDIL::SMULv4i8)
        GENERATE_2ARG_CASE(AMDIL::SMULi16)
        GENERATE_2ARG_CASE(AMDIL::SMULv2i16)
        GENERATE_2ARG_CASE(AMDIL::SMULv4i16)
        GENERATE_2ARG_CASE(AMDIL::SMULHIi8)
        GENERATE_2ARG_CASE(AMDIL::SMULHIv2i8)
        GENERATE_2ARG_CASE(AMDIL::SMULHIv4i8)
        GENERATE_2ARG_CASE(AMDIL::SMULHIi16)
        GENERATE_2ARG_CASE(AMDIL::SMULHIv2i16)
        GENERATE_2ARG_CASE(AMDIL::SMULHIv4i16)
      case AMDIL::MACRO__sdiv_i16:
      case AMDIL::MACRO__smod_i8:
      case AMDIL::MACRO__smod_i16:
#define CMPEXP(A) \
  GENERATE_2ARG_CASE(A ## i8) \
  GENERATE_2ARG_CASE(A ## v2i8) \
  GENERATE_2ARG_CASE(A ## v4i8) \
  GENERATE_2ARG_CASE(A ## i16) \
  GENERATE_2ARG_CASE(A ## v2i16) \
  GENERATE_2ARG_CASE(A ## v4i16)
        CMPEXP(AMDIL::EQ)
        CMPEXP(AMDIL::NE)
        CMPEXP(AMDIL::LT)
        CMPEXP(AMDIL::GT)
        CMPEXP(AMDIL::ULT)
        CMPEXP(AMDIL::UGT)
        CMPEXP(AMDIL::LE)
        CMPEXP(AMDIL::GE)
        CMPEXP(AMDIL::ULE)
#undef CMPEXP
        {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            signExtend(MIB, 1);
            signExtend(MIB, 2);
          }
          break;
        }
      case AMDIL::MACRO__udiv_i8:
      case AMDIL::MACRO__udiv_i16:
      case AMDIL::MACRO__umod_i8:
      case AMDIL::MACRO__umod_i16:
        GENERATE_2ARG_CASE(AMDIL::UMULHIi8)
        GENERATE_2ARG_CASE(AMDIL::UMULHIv2i8)
        GENERATE_2ARG_CASE(AMDIL::UMULHIv4i8)
        GENERATE_2ARG_CASE(AMDIL::UMULHIi16)
        GENERATE_2ARG_CASE(AMDIL::UMULHIv2i16)
        GENERATE_2ARG_CASE(AMDIL::UMULHIv4i16)
        GENERATE_2ARG_CASE(AMDIL::UDIVi8)
        GENERATE_2ARG_CASE(AMDIL::UDIVv2i8)
        GENERATE_2ARG_CASE(AMDIL::UDIVv4i8)
        GENERATE_2ARG_CASE(AMDIL::UDIVi16)
        GENERATE_2ARG_CASE(AMDIL::UDIVv2i16)
        GENERATE_2ARG_CASE(AMDIL::UDIVv4i16)
        {
          short opRegClassID = mi->getDesc().OpInfo[1].RegClass;
          if (useSWByteShortReg(opRegClassID)) {
            zeroExtend(MIB, 1);
            zeroExtend(MIB, 2);
          }
          break;
        }
      }
    }
  }

  // cleanup
  for (Reg2InstsMap::iterator I = sextMap.begin(), E = sextMap.end();
       I != E; ++I) {
    MachineInstVec* vec = I->second;
    delete vec;
  }
  sextMap.clear();
  for (Reg2InstsMap::iterator I = zextMap.begin(), E = zextMap.end();
       I != E; ++I) {
    MachineInstVec* vec = I->second;
    delete vec;
  }
  zextMap.clear();
  return Changed;
}
#undef GENERATE_3ARG_CASE
#undef GENERATE_SH2ARG_CASE
#undef GENERATE_2ARG_CASE

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
                                TM->getInstrInfo()->get(AMDIL::FENCEr)).addReg(
    1);

  MI->getParent()->insert(MIB, fence);
  fence = BuildMI(*(MI->getParent()->getParent()),
                  MI->getDebugLoc(),
                  TM->getInstrInfo()->get(AMDIL::FENCEr)).addReg(1);
  MIB = MI->getParent()->insertAfter(MIB, fence);
}
// returns if the given register class is software emulated byte or short
bool AMDILMachinePeephole::useSWByteShortReg(short opRegClassID)
{
  if ((opRegClassID == AMDIL::GPRI16RegClassID
       || opRegClassID == AMDIL::GPRV2I16RegClassID
       || opRegClassID == AMDIL::GPRV4I16RegClassID)
      && TM->getSubtarget<AMDILSubtarget>()
      .device()->usesSoftware(AMDILDeviceInfo::ShortOps)) {
    return true;
  }
  if ((opRegClassID == AMDIL::GPRI8RegClassID
       || opRegClassID == AMDIL::GPRV2I8RegClassID
       || opRegClassID == AMDIL::GPRV4I8RegClassID)
      && TM->getSubtarget<AMDILSubtarget>()
      .device()->usesSoftware(AMDILDeviceInfo::ByteOps)) {
    return true;
  }
  return false;
}
uint32_t AMDILMachinePeephole::genVReg(uint32_t regType) const {
  return MFP->getRegInfo().createVirtualRegister(getRegClassFromID(regType));
}
// Find a MachineInstr that uses the given register and has the given opcode.
// Return NULL if not found.
static inline MachineInstr* findRegUse(int opcode, uint32_t reg,
                                       const MachineRegisterInfo& MRI)
{
  for (MachineRegisterInfo::use_iterator it = MRI.use_begin(reg),
       end = MRI.use_end();
       it != end;
       ++it) {
    MachineInstr& useInst = *it;
    if (useInst.getOpcode() != opcode) continue;
    assert(useInst.getNumOperands() == 2 && "unexpected # of operands");
    MachineOperand& op = useInst.getOperand(1);
    if (op.isReg() && op.getReg() == reg) {
      assert(op.isUse() && "op not use");
      return &useInst;
    }
  }
  return NULL;
}
// Find a MachineInstr that uses the given register and immediate,
// and has the given opcode.
// Return NULL if not found.
static inline MachineInstr* findRegUse(int opcode, uint32_t reg, int64_t imm,
                                       const MachineRegisterInfo& MRI)
{
  for (MachineRegisterInfo::use_iterator it = MRI.use_begin(reg),
       end = MRI.use_end();
       it != end;
       ++it) {
    MachineInstr& useInst = *it;
    if (useInst.getOpcode() != opcode) continue;
    assert(useInst.getNumOperands() == 3 && "unexpected # of operands");
    MachineOperand& op1 = useInst.getOperand(1);
    MachineOperand& op2 = useInst.getOperand(2);
    assert(op1.isReg() && op1.isUse() && op2.isImm() && "unexpected op");
    if (op1.getReg() == reg && op2.getImm() == imm) {
      return &useInst;
    }
  }
  return NULL;
}
// Find a MachineInstr that uses the given register and immediate,
// and has the given opcode.
// Return NULL if not found.
static inline MachineInstr* findRegUse(int opcode, int64_t imm1,
                                       int64_t imm2, uint32_t reg,
                                       const MachineRegisterInfo& MRI)
{
  for (MachineRegisterInfo::use_iterator it = MRI.use_begin(reg),
       end = MRI.use_end();
       it != end;
       ++it) {
    MachineInstr& useInst = *it;
    if (useInst.getOpcode() != opcode) continue;
    assert(useInst.getNumOperands() == 4 && "unexpected # of operands");
    MachineOperand& op1 = useInst.getOperand(1);
    MachineOperand& op2 = useInst.getOperand(2);
    MachineOperand& op3 = useInst.getOperand(3);
    assert(op1.isImm() && op2.isImm() && op3.isReg() && op3.isUse()
           && "unexpected op");
    if (op1.getImm() == imm1 && op2.getImm() == imm2 && op3.getReg() == reg) {
      return &useInst;
    }
  }
  return NULL;
}
// returns if the given MachineInstr defines exactly 1 register operand
// returns if the given MachineInstr defines exactly 1 register operand
static inline bool hasSingleRegDef(MachineInstr& inst)
{
  size_t nDefs = 0;
  for (MachineInstr::mop_iterator it = inst.operands_begin(),
       end = inst.operands_end();
       it != end; ++it) {
    MachineOperand& op = *it;
    if (!op.isReg() || !op.isDef()) {
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
// reg1 = IL_ASINTi8r reg
// reg2 = SHLi32rr reg1, 24
// reg3 = SHRi32rr reg2, 24
// reg4 = IL_ASCHARi32r reg3
// or zero extension sequence such as the following:
// reg1 = IL_ASINTi8r reg
// reg2 = ANDi32rr reg1, 0xff
// reg3 = IL_ASCHARi32r reg2
// The above sequence does sign/zero-extension to reg if reg is of type i8
// Return the extension sequence through "extendSeq".
// Return true if extension sequence is found, return false otherwise
bool AMDILMachinePeephole::findExtendSequence(MachineOperand& op,
                                              bool isSigned,
                                              MachineInstVec& extendSeq)
{
  unsigned opReg = op.getReg();
  uint32_t regClassID = MFP->getRegInfo().getRegClass(opReg)->getID();
  uint32_t nConsts;
  int64_t constVal[2];
  int promoteOp;
  int demoteOp;
  int extendOps[2];
  uint32_t intRegClassID;
  getExtendOpcodes(regClassID, isSigned, nConsts, constVal, promoteOp, demoteOp,
                   extendOps, intRegClassID);
  const MachineRegisterInfo& MRI = MFP->getRegInfo();
  extendSeq.clear();
  MachineInstr* promoteInst = findRegUse(promoteOp, opReg, MRI);
  if (promoteInst == NULL || !hasSingleRegDef(*promoteInst)) return false;
  extendSeq.push_back(promoteInst);
  uint32_t reg1 = firstDefReg(*promoteInst);
  for (int i = 0; i < 2; ++i) {
    if (extendOps[i] == AMDIL::INSTRUCTION_LIST_END) break;
    MachineInstr* extendInst;
    if (nConsts == 1) {
      extendInst = findRegUse(extendOps[i], reg1, constVal[0], MRI);
    } else {
      extendInst = findRegUse(extendOps[i], constVal[0], constVal[1], reg1,
                              MRI);
    }
    if (extendInst == NULL || !hasSingleRegDef(*extendInst)) {
      extendSeq.clear();
      return false;
    }
    extendSeq.push_back(extendInst);
    reg1 = firstDefReg(*extendInst);
  }
  MachineInstr* demoteInst = findRegUse(demoteOp, reg1, MRI);
  if (demoteInst == NULL || !hasSingleRegDef(*demoteInst)) {
    extendSeq.clear();
    return false;
  }
  extendSeq.push_back(demoteInst);
  return true;
}
// returns opcodes to be used to sign/zero extend the given register class
void
AMDILMachinePeephole::getExtendOpcodes(uint32_t regClassID,
                                       bool isSigned,
                                       uint32_t& nConsts,
                                       int64_t* constVal,
                                       int& promoteOp,
                                       int& demoteOp,
                                       int* extendOps,
                                       uint32_t& intRegClassID)
{
  const AMDILSubtarget *STM = &TM->getSubtarget<AMDILSubtarget>();
  switch(regClassID)
  {
  default:
    assert(0 && "unexpected reg class");
    break;
  case AMDIL::GPRI8RegClassID:
  case AMDIL::GPRV2I8RegClassID:
  case AMDIL::GPRV4I8RegClassID:
    if (STM->device()->getGeneration() <= AMDILDeviceInfo::HD4XXX) {
      constVal[0] = isSigned ? 24 : 0xFF;
      nConsts = 1;
    } else {
      constVal[0] = 8;
      constVal[1] = 0;
      nConsts = 2;
    }
    break;
  case AMDIL::GPRI16RegClassID:
  case AMDIL::GPRV2I16RegClassID:
  case AMDIL::GPRV4I16RegClassID:
    if (STM->device()->getGeneration() <= AMDILDeviceInfo::HD4XXX) {
      constVal[0] = isSigned ? 16 : 0xFFFF;
      nConsts = 1;
    } else {
      constVal[0] = 16;
      constVal[1] = 0;
      nConsts = 2;
    }
    break;
  }

  extendOps[0] = AMDIL::INSTRUCTION_LIST_END;
  extendOps[1] = AMDIL::INSTRUCTION_LIST_END;
  switch(regClassID)
  {
  default:
    break;
  case AMDIL::GPRI8RegClassID:
  case AMDIL::GPRI16RegClassID:
    intRegClassID = AMDIL::GPRI32RegClassID;
    if (STM->device()->getGeneration() <= AMDILDeviceInfo::HD4XXX) {
      extendOps[0] = isSigned ? AMDIL::SHLi32i32rr : AMDIL::ANDi32rr;
      if (isSigned) extendOps[1] = AMDIL::SHRi32i32rr;
    } else {
      extendOps[0] = isSigned ? AMDIL::IBIT_EXTRACTi32iir
                     : AMDIL::UBIT_EXTRACTi32iir;
    }
    break;
  case AMDIL::GPRV2I8RegClassID:
  case AMDIL::GPRV2I16RegClassID:
    intRegClassID = AMDIL::GPRV2I32RegClassID;
    if (STM->device()->getGeneration() <= AMDILDeviceInfo::HD4XXX) {
      extendOps[0] = isSigned ? AMDIL::SHLv2i32i32rr : AMDIL::ANDv2i32rr;
      if (isSigned) extendOps[1] = AMDIL::SHRv2i32i32rr;
    } else {
      extendOps[0] = isSigned ? AMDIL::IBIT_EXTRACTv2i32iir :
                     AMDIL::UBIT_EXTRACTv2i32iir;
    }
    break;
  case AMDIL::GPRV4I8RegClassID:
  case AMDIL::GPRV4I16RegClassID:
    intRegClassID = AMDIL::GPRV4I32RegClassID;
    if (STM->device()->getGeneration() <= AMDILDeviceInfo::HD4XXX) {
      extendOps[0] = isSigned ? AMDIL::SHLv4i32i32rr : AMDIL::ANDv4i32rr;
      if (isSigned) extendOps[1] = AMDIL::SHRv4i32i32rr;
    } else {
      extendOps[0] = isSigned ? AMDIL::IBIT_EXTRACTv4i32iir :
                     AMDIL::UBIT_EXTRACTv4i32iir;
    }
    break;
  }

  switch(regClassID)
  {
  default:
    break;
  case AMDIL::GPRI8RegClassID:
    promoteOp = AMDIL::IL_ASINTi8r;
    demoteOp = AMDIL::IL_ASCHARi32r;
    break;
  case AMDIL::GPRV2I8RegClassID:
    promoteOp = AMDIL::IL_ASV2INTv2i8r;
    demoteOp = AMDIL::IL_ASV2CHARv2i32r;
    break;
  case AMDIL::GPRV4I8RegClassID:
    promoteOp = AMDIL::IL_ASV4INTv4i8r;
    demoteOp = AMDIL::IL_ASV4CHARv4i32r;
    break;
  case AMDIL::GPRI16RegClassID:
    promoteOp = AMDIL::IL_ASINTi16r;
    demoteOp = AMDIL::IL_ASSHORTi32r;
    break;
  case AMDIL::GPRV2I16RegClassID:
    promoteOp = AMDIL::IL_ASV2INTv2i16r;
    demoteOp = AMDIL::IL_ASV2SHORTv2i32r;
    break;
  case AMDIL::GPRV4I16RegClassID:
    promoteOp = AMDIL::IL_ASV4INTv4i16r;
    demoteOp = AMDIL::IL_ASV4SHORTv4i32r;
    break;
  }
}
// create sequence of instructions to sign/zero extend the given register
uint32_t
AMDILMachinePeephole::addExtendInstructions(MachineBasicBlock& BB,
                                            MachineBasicBlock::instr_iterator I,
                                            uint32_t reg,
                                            bool isSigned,
                                            MachineInstVec& extendSeq)
{
  uint32_t nConsts;
  int64_t constVal[2];
  int promoteOp;
  int demoteOp;
  int extendOps[2];
  uint32_t intRegClassID;
  uint32_t regClassID = MFP->getRegInfo().getRegClass(reg)->getID();
  getExtendOpcodes(regClassID, isSigned, nConsts, constVal, promoteOp, demoteOp,
                   extendOps, intRegClassID);
  uint32_t constReg = genVReg(AMDIL::GPRI32RegClassID);
  uint32_t intReg = genVReg(intRegClassID);
  uint32_t intReg2;
  uint32_t dstReg = genVReg(regClassID);
  MachineInstrBuilder promoteInst
    = BuildMI(BB, I, (*I).getDebugLoc(),
              TM->getInstrInfo()->get(promoteOp), intReg).addReg(reg);
  extendSeq.clear();
  extendSeq.push_back(promoteInst);
  for (int i = 0; i < 2; ++i) {
    if (extendOps[i] == AMDIL::INSTRUCTION_LIST_END) break;
    intReg2 = genVReg(intRegClassID);
    MachineInstrBuilder extendInst
      = BuildMI(BB, I, (*I).getDebugLoc(),
                TM->getInstrInfo()->get(extendOps[i]), intReg2);
    assert((nConsts == 1 || nConsts == 2) && "unexpected instruction type");
    if ((extendOps[i] >= AMDIL::IBIT_EXTRACTi16iir
         && extendOps[i] <= AMDIL::IBIT_EXTRACTv4i8iir)
        || (extendOps[i] >= AMDIL::UBIT_EXTRACTi16iir
            && extendOps[i] <= AMDIL::UBIT_EXTRACTv4i8iir)) {
      assert(nConsts == 2 && "unexpected instruction type");
      extendInst.addImm(constVal[0]);
      extendInst.addImm(constVal[1]);
      extendInst.addReg(intReg);
    } else {
      assert(nConsts == 1 && "unexpected instruction type");
      extendInst.addReg(intReg);
      extendInst.addImm(constVal[0]);
    }
    extendSeq.push_back(extendInst);
    intReg = intReg2;
  }
  MachineInstrBuilder demoteInst
    = BuildMI(BB, I, (*I).getDebugLoc(),
              TM->getInstrInfo()->get(demoteOp), dstReg).addReg(intReg2);
  extendSeq.push_back(demoteInst);
  return dstReg;
}
// move sequence of instructions to "BB" at "I"
void AMDILMachinePeephole::moveInsts(MachineInstVec& insts,
                                     MachineBasicBlock& BB,
                                     MachineBasicBlock::instr_iterator I)
{
  for (MachineInstVec::iterator i = insts.begin(), e = insts.end();
       i != e; ++i) {
    MachineInstr* inst = *i;
    DEBUG(dbgs() << "moving " << *inst << " from B#"
                 << inst->getParent()->getNumber() << " to B#"
                 << BB.getNumber() << "\n");
    assert(DomTree->dominates(&BB, inst->getParent())
           && "def not dominate use");
    inst->removeFromParent();
    BB.insert(I, inst);
  }
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
  if (!op.isReg() || !op.isUse()) return;
  uint32_t opReg = op.getReg();
  const MachineRegisterInfo& MRI = MFP->getRegInfo();
  assert(MRI.isSSA() && "not SSA");
  MachineBasicBlock* insertBB = NULL;
  MachineBasicBlock::instr_iterator insertItr;
  if (MRI.def_empty(opReg)) {
    // if opReg is live-in, insert extension instructions in the entry BB
    insertBB = &MFP->front();
    insertItr = insertBB->instr_begin();
  } else {
    MachineInstr& opInst = *MRI.def_begin(opReg);
    insertBB = opInst.getParent();
    insertItr = MachineBasicBlock::instr_iterator(&opInst);
    ++insertItr;
  }
  // should insert the extension instructions after all PHIs in the block
  while (insertItr != insertBB->instr_end() && insertItr->isPHI()) {
    ++insertItr;
  }

  assert((unsigned)MFP->getRegInfo().getRegClass(opReg)->getID()
         == (unsigned)mi->getDesc().OpInfo[opIdx].RegClass
         && "inconsistent op reg class");

  // first check the sext/zext map to see if it already has a sign/zero
  // extention, if so, reuse it
  MachineInstVec* extendSeq = NULL;
  Reg2InstsMap& map = isSigned ? sextMap : zextMap;
  Reg2InstsMap::iterator it = map.find(opReg);
  if (it != map.end()) {
    DEBUG(dbgs() << "Found in map ");
    extendSeq = it->second;
    // if the extend sequence found does not dominate current instruction,
    // move the sequence to the operand's block
    if (!DomTree->dominates(extendSeq->back()->getParent(), mi->getParent())) {
      moveInsts(*extendSeq, *insertBB, insertItr);
    }
  } else {
    extendSeq = new MachineInstVec();
    // not in the map. See if we can find in the DFG
    bool found = findExtendSequence(op, isSigned, *extendSeq);
    if (found) {
      DEBUG(dbgs() << "Found in DFG ");
      // if the extend sequence found does not dominate current instruction,
      // move the sequence to the operand's block
      if (!DomTree->dominates(extendSeq->back()->getParent(),
                              mi->getParent())) {
        moveInsts(*extendSeq, *insertBB, insertItr);
      }
    } else {
      // not in the DFG either. Create sign/zero extention.
      addExtendInstructions(*insertBB, insertItr, opReg, isSigned, *extendSeq);
      DEBUG(dbgs() << "Created ");
    }
    map[opReg] = extendSeq;
  }
  assert(!extendSeq->empty() && "sanity");
  uint32_t newOpReg = firstDefReg(*extendSeq->back());
  DEBUG(dbgs() << (isSigned ? "sign" : "zero") << " extension vreg"
               << TargetRegisterInfo::virtReg2Index(newOpReg) << " for vreg"
               << TargetRegisterInfo::virtReg2Index(opReg) << "\n");
  op.setReg(newOpReg);
}
