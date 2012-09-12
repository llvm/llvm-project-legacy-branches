//===-- AMDILInstrInfo.cpp ------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the AMDIL implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "AMDIL.h"
#include "AMDILInstrInfo.h"
#include "AMDILUtilityFunctions.h"
#define GET_INSTRINFO_CTOR
#define GET_INSTRINFO_MC_DESC
#include "AMDILGenInstrInfo.inc"
#include "AMDILMachineFunctionInfo.h"
#include "llvm/Instructions.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
using namespace llvm;

AMDILInstrInfo::AMDILInstrInfo(AMDILTargetMachine &tm)
  : AMDILGenInstrInfo(AMDIL::ADJCALLSTACKDOWN, AMDIL::ADJCALLSTACKUP),
    RI(tm, *this),
    TM(tm) {
}
const AMDILRegisterInfo &AMDILInstrInfo::getRegisterInfo() const {
  return RI;
}
/// Return true if the instruction is a register to register move and leave the
/// source and dest operands in the passed parameters.
bool AMDILInstrInfo::isMoveInstr(const MachineInstr &MI, unsigned int &SrcReg,
                                 unsigned int &DstReg, unsigned int &SrcSubIdx,
                                 unsigned int &DstSubIdx) const {
  // FIXME: we should look for:
  //    add with 0
  //assert(0 && "is Move Instruction has not been implemented yet!");
  //return true;
  if (MI.getOpcode() == TargetOpcode::COPY) {
    return false;
  }
  if (!MI.getOperand(0).isReg() || !MI.getOperand(1).isReg()) {
    return false;
  }
  SrcReg = MI.getOperand(1).getReg();
  DstReg = MI.getOperand(0).getReg();
  DstSubIdx = 0;
  SrcSubIdx = 0;
  return true;
}
bool AMDILInstrInfo::isCoalescableExtInstr(const MachineInstr &MI,
                                           unsigned &SrcReg, unsigned &DstReg,
                                           unsigned &SubIdx) const {
  return false;
  unsigned opc = MI.getOpcode();
  SubIdx = llvm::NoSubRegister;
  switch (opc) {
  default:
    return false;
  case AMDIL::DHIf64r:
  case AMDIL::LHIi64r:
    if (MI.getOperand(0).getSubReg() || MI.getOperand(1).getSubReg())
      // Be conservative.
      return false;
    SrcReg = MI.getOperand(1).getReg();
    DstReg = MI.getOperand(0).getReg();
    SubIdx = llvm::sub_y_comp;
    break;
  case AMDIL::DLOf64r:
  case AMDIL::LLOi64r:
    if (MI.getOperand(0).getSubReg() || MI.getOperand(1).getSubReg())
      // Be conservative.
      return false;
    SrcReg = MI.getOperand(1).getReg();
    DstReg = MI.getOperand(0).getReg();
    SubIdx = llvm::sub_x_comp;
    break;
  case AMDIL::VEXTRACTv2f64r:
  case AMDIL::VEXTRACTv2i64r:
    if (MI.getOperand(0).getSubReg() || MI.getOperand(1).getSubReg())
      // Be conservative.
      return false;
    SrcReg = MI.getOperand(1).getReg();
    DstReg = MI.getOperand(0).getReg();
    assert(MI.getOperand(2).isImm()
           && "Operand 2 must be an immediate value!");
    switch (MI.getOperand(2).getImm()) {
    case 0:
      SubIdx = llvm::sub_xy_comp;
      break;
    case 1:
      SubIdx = llvm::sub_zw_comp;
      break;
    default:
      return false;
    };
  case AMDIL::VEXTRACTv2f32r:
  case AMDIL::VEXTRACTv2i32r:
  case AMDIL::VEXTRACTv2i16r:
  case AMDIL::VEXTRACTv2i8r:
  case AMDIL::VEXTRACTv4f32r:
  case AMDIL::VEXTRACTv4i32r:
  case AMDIL::VEXTRACTv4i16r:
  case AMDIL::VEXTRACTv4i8r:
    if (MI.getOperand(0).getSubReg() || MI.getOperand(1).getSubReg())
      // Be conservative.
      return false;
    SrcReg = MI.getOperand(1).getReg();
    DstReg = MI.getOperand(0).getReg();
    assert(MI.getOperand(2).isImm()
           && "Operand 2 must be an immediate value!");
    switch (MI.getOperand(2).getImm()) {
    case 0:
      SubIdx = llvm::sub_x_comp;
      break;
    case 1:
      SubIdx = llvm::sub_y_comp;
      break;
    case 2:
      SubIdx = llvm::sub_z_comp;
      break;
    case 3:
      SubIdx = llvm::sub_w_comp;
      break;
    default:
      return false;
    };
  };
  return SubIdx != llvm::NoSubRegister;
}
unsigned AMDILInstrInfo::isLoadFromStackSlot(const MachineInstr *MI,
                                             int &FrameIndex) const {
  if (isPrivateInst(MI) && isPtrLoadInst(MI) && MI->getOperand(1).isFI()) {
    FrameIndex = MI->getOperand(1).getIndex();
    return MI->getOperand(0).getReg();
  }
  return 0;
}
unsigned AMDILInstrInfo::isLoadFromStackSlotPostFE(const MachineInstr *MI,
                                                   int &FrameIndex) const {
  if (isPrivateInst(MI) && isPtrLoadInst(MI) && MI->getOperand(1).isFI()) {
    FrameIndex = MI->getOperand(1).getIndex();
    return MI->getOperand(0).getReg();
  }
  return 0;
}
bool AMDILInstrInfo::hasLoadFromStackSlot(const MachineInstr *MI,
                                          const MachineMemOperand *&MMO,
                                          int &FrameIndex) const {
  for (MachineInstr::mmo_iterator o = MI->memoperands_begin(),
       oe = MI->memoperands_end();
       o != oe;
       ++o) {
    if ((*o)->isLoad() && (*o)->getValue())
      if (const FixedStackPseudoSourceValue *Value =
            dyn_cast<const FixedStackPseudoSourceValue>((*o)->getValue())) {
        FrameIndex = Value->getFrameIndex();
        MMO = *o;
        return true;
      }
  }
  return false;
}
unsigned AMDILInstrInfo::isStoreToStackSlot(const MachineInstr *MI,
                                            int &FrameIndex) const {
  if (isPrivateInst(MI) && isPtrStoreInst(MI) && MI->getOperand(1).isFI()) {
    FrameIndex = MI->getOperand(1).getIndex();
    return MI->getOperand(0).getReg();
  }
  return 0;
}
unsigned AMDILInstrInfo::isStoreToStackSlotPostFE(const MachineInstr *MI,
                                                  int &FrameIndex) const {
  if (isPrivateInst(MI) && isPtrStoreInst(MI) && MI->getOperand(1).isFI()) {
    unsigned Reg;
    if ((Reg = isStoreToStackSlot(MI, FrameIndex))) {
      return Reg;
    }
    const MachineMemOperand *Dummy = NULL;
    return hasStoreToStackSlot(MI, Dummy, FrameIndex);
  }
  return 0;
}
bool AMDILInstrInfo::hasStoreToStackSlot(const MachineInstr *MI,
                                         const MachineMemOperand *&MMO,
                                         int &FrameIndex) const {
  for (MachineInstr::mmo_iterator o = MI->memoperands_begin(),
       oe = MI->memoperands_end();
       o != oe;
       ++o) {
    if ((*o)->isStore() && (*o)->getValue())
      if (const FixedStackPseudoSourceValue *Value =
            dyn_cast<const FixedStackPseudoSourceValue>((*o)->getValue())) {
        FrameIndex = Value->getFrameIndex();
        MMO = *o;
        return true;
      }
  }
  return false;
}
void
AMDILInstrInfo::reMaterialize(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MI,
                              unsigned DestReg, unsigned SubIdx,
                              const MachineInstr *Orig,
                              const TargetRegisterInfo &TRI) const {
  // TODO: Implement this function
}
MachineInstr*
AMDILInstrInfo::duplicate(MachineInstr *Orig,
                          MachineFunction &MF) const {
  // TODO: Implement this function
  return MF.CloneMachineInstr(Orig);
}
MachineInstr *
AMDILInstrInfo::convertToThreeAddress(MachineFunction::iterator &MFI,
                                      MachineBasicBlock::iterator &MBBI,
                                      LiveVariables *LV) const {
  // TODO: Implement this function
  return NULL;
}
MachineInstr*
AMDILInstrInfo::commuteInstruction(MachineInstr *MI,
                                   bool NewMI) const {
  // TODO: Implement this function
  return NULL;
}
bool
AMDILInstrInfo::findCommutedOpIndices(MachineInstr *MI, unsigned &SrcOpIdx1,
                                      unsigned &SrcOpIdx2) const
{
  // TODO: Implement this function
  return false;
}
bool
AMDILInstrInfo::produceSameValue(const MachineInstr *MI0,
                                 const MachineInstr *MI1,
                                 const MachineRegisterInfo *MRI) const
{
  // TODO: Implement this function
  return false;
}
bool AMDILInstrInfo::getNextBranchInstr(MachineBasicBlock::iterator &iter,
                                        MachineBasicBlock &MBB) const {
  while (iter != MBB.end()) {
    switch (iter->getOpcode()) {
    default:
      break;
    case AMDIL::BRANCHf64br:
    case AMDIL::BRANCHf32br:
    case AMDIL::BRANCHi64br:
    case AMDIL::BRANCHi32br:
    case AMDIL::BRANCHi16br:
    case AMDIL::BRANCHi8br:
    case AMDIL::BRANCHb:
      return true;
    };
    ++iter;
  }
  return false;
}
bool AMDILInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const {
  bool retVal = true;
  return retVal;
  MachineBasicBlock::iterator iter = MBB.begin();
  if (!getNextBranchInstr(iter, MBB)) {
    retVal = false;
  } else {
    MachineInstr *firstBranch = iter;
    if (!getNextBranchInstr(++iter, MBB)) {
      if (firstBranch->getOpcode() == AMDIL::BRANCHb) {
        TBB = firstBranch->getOperand(0).getMBB();
        firstBranch->eraseFromParent();
        retVal = false;
      } else {
        TBB = firstBranch->getOperand(0).getMBB();
        FBB = *(MBB.succ_begin()+1);
        if (FBB == TBB) {
          FBB = *(MBB.succ_begin());
        }
        Cond.push_back(firstBranch->getOperand(1));
        retVal = false;
      }
    } else {
      MachineInstr *secondBranch = iter;
      if (!getNextBranchInstr(++iter, MBB)) {
        if (secondBranch->getOpcode() == AMDIL::BRANCHb) {
          TBB = firstBranch->getOperand(0).getMBB();
          Cond.push_back(firstBranch->getOperand(1));
          FBB = secondBranch->getOperand(0).getMBB();
          secondBranch->eraseFromParent();
          retVal = false;
        } else {
          assert(0 && "Should not have two consecutive conditional branches");
        }
      } else {
        MBB.getParent()->viewCFG();
        assert(0 && "Should not have three branch instructions in"
               " a single basic block");
        retVal = false;
      }
    }
  }
  return retVal;
}
unsigned int AMDILInstrInfo::getBranchInstr(const MachineOperand &op) const {
  const MachineInstr *MI = op.getParent();

  switch (MI->getDesc().OpInfo->RegClass) {
  default:   // FIXME: fallthrough??
  case AMDIL::GPRI8RegClassID:  return AMDIL::BRANCHi8br;
  case AMDIL::GPRI16RegClassID: return AMDIL::BRANCHi16br;
  case AMDIL::GPRI32RegClassID: return AMDIL::BRANCHi32br;
  case AMDIL::GPRI64RegClassID: return AMDIL::BRANCHi64br;
  case AMDIL::GPRF32RegClassID: return AMDIL::BRANCHf32br;
  case AMDIL::GPRF64RegClassID: return AMDIL::BRANCHf64br;
  };
}
unsigned int
AMDILInstrInfo::InsertBranch(MachineBasicBlock &MBB,
                             MachineBasicBlock *TBB,
                             MachineBasicBlock *FBB,
                             const SmallVectorImpl<MachineOperand> &Cond,
                             DebugLoc DL) const
{
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  for (unsigned int x = 0; x < Cond.size(); ++x) {
    Cond[x].getParent()->dump();
  }
  if (FBB == 0) {
    if (Cond.empty()) {
      BuildMI(&MBB, DL, get(AMDIL::BRANCHb)).addMBB(TBB);
    } else {
      BuildMI(&MBB, DL, get(getBranchInstr(Cond[0])))
      .addMBB(TBB).addReg(Cond[0].getReg());
    }
    return 1;
  } else {
    BuildMI(&MBB, DL, get(getBranchInstr(Cond[0])))
    .addMBB(TBB).addReg(Cond[0].getReg());
    BuildMI(&MBB, DL, get(AMDIL::BRANCHb)).addMBB(FBB);
  }
  assert(0 && "Inserting two branches not supported");
  return 0;
}
unsigned int AMDILInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator I = MBB.end();
  if (I == MBB.begin()) {
    return 0;
  }
  --I;
  switch (I->getOpcode()) {
  default:
    return 0;
  case AMDIL::BRANCHf64br:
  case AMDIL::BRANCHf32br:
  case AMDIL::BRANCHi64br:
  case AMDIL::BRANCHi32br:
  case AMDIL::BRANCHi16br:
  case AMDIL::BRANCHi8br:
  case AMDIL::BRANCHb:
    I->eraseFromParent();
    break;
  }
  I = MBB.end();

  if (I == MBB.begin()) {
    return 1;
  }
  --I;
  switch (I->getOpcode()) {
  // FIXME: only one case??
  default:
    return 1;
  case AMDIL::BRANCHf64br:
  case AMDIL::BRANCHf32br:
  case AMDIL::BRANCHi64br:
  case AMDIL::BRANCHi32br:
  case AMDIL::BRANCHi16br:
  case AMDIL::BRANCHi8br:
    I->eraseFromParent();
    break;
  }
  return 2;
}
MachineBasicBlock::iterator skipFlowControl(MachineBasicBlock *MBB) {
  MachineBasicBlock::iterator tmp = MBB->end();
  if (!MBB->size()) {
    return MBB->end();
  }
  while (--tmp) {
    if (tmp->getOpcode() == AMDIL::ENDLOOP
        || tmp->getOpcode() == AMDIL::ENDIF
        || tmp->getOpcode() == AMDIL::ELSE) {
      if (tmp == MBB->begin()) {
        return tmp;
      } else {
        continue;
      }
    }  else {
      return ++tmp;
    }
  }
  return MBB->end();
}
bool
AMDILInstrInfo::copyRegToReg(MachineBasicBlock &MBB,
                             MachineBasicBlock::iterator I,
                             unsigned DestReg, unsigned SrcReg,
                             const TargetRegisterClass *DestRC,
                             const TargetRegisterClass *SrcRC,
                             DebugLoc DL) const {
  // If we are adding to the end of a basic block we can safely assume that the
  // move is caused by a PHI node since all move instructions that are non-PHI
  // have already been inserted into the basic blocks Therefor we call the skip
  // flow control instruction to move the iterator before the flow control
  // instructions and put the move instruction there.
  bool phi = (DestReg < 1025) || (SrcReg < 1025);
  int movInst = TargetOpcode::COPY;

  MachineBasicBlock::iterator iTemp = (I == MBB.end()) ? skipFlowControl(&MBB)
                                      : I;
  if (DestRC != SrcRC) {
    //int convInst;
    size_t dSize = DestRC->getSize();
    size_t sSize = SrcRC->getSize();
    if (dSize > sSize) {
      // Elements are going to get duplicated.
      BuildMI(MBB, iTemp, DL, get(movInst), DestReg).addReg(SrcReg);
    } else if (dSize == sSize) {
      // Direct copy, conversions are not handled.
      BuildMI(MBB, iTemp, DL, get(movInst), DestReg).addReg(SrcReg);
    } else if (dSize < sSize) {
      // Elements are going to get dropped.
      BuildMI(MBB, iTemp, DL, get(movInst), DestReg).addReg(SrcReg);
    }
  } else {
    BuildMI( MBB, iTemp, DL, get(movInst), DestReg).addReg(SrcReg);
  }
  return true;
}
void
AMDILInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI, DebugLoc DL,
                            unsigned DestReg, unsigned SrcReg,
                            bool KillSrc) const
{
  BuildMI(MBB, MI, DL, get(TargetOpcode::COPY), DestReg)
  .addReg(SrcReg, getKillRegState(KillSrc));
  return;
#if 0
  DEBUG(dbgs() << "Cannot copy " << RI.getName(SrcReg)
               << " to " << RI.getName(DestReg) << '\n');
  llvm_unreachable("Cannot emit physreg copy instruction");
#endif
}
void
AMDILInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MI,
                                    unsigned SrcReg, bool isKill,
                                    int FrameIndex,
                                    const TargetRegisterClass *RC,
                                    const TargetRegisterInfo *TRI) const {
  unsigned int Opc = 0;
  MachineFunction &MF = *(MBB.getParent());
  MachineFrameInfo &MFI = *MF.getFrameInfo();

  DebugLoc DL;
  switch (RC->getID()) {
  default:
    Opc = AMDIL::PRIVATESTOREv4i32r;
    break;
  case AMDIL::GPRF32RegClassID:
    Opc = AMDIL::PRIVATESTOREf32r;
    break;
  case AMDIL::GPRF64RegClassID:
    Opc = AMDIL::PRIVATESTOREf64r;
    break;
  case AMDIL::GPRI16RegClassID:
    Opc = AMDIL::PRIVATESTOREi16r;
    break;
  case AMDIL::GPRI32RegClassID:
    Opc = AMDIL::PRIVATESTOREi32r;
    break;
  case AMDIL::GPRI8RegClassID:
    Opc = AMDIL::PRIVATESTOREi8r;
    break;
  case AMDIL::GPRI64RegClassID:
    Opc = AMDIL::PRIVATESTOREi64r;
    break;
  case AMDIL::GPRV2F32RegClassID:
    Opc = AMDIL::PRIVATESTOREv2f32r;
    break;
  case AMDIL::GPRV2F64RegClassID:
    Opc = AMDIL::PRIVATESTOREv2f64r;
    break;
  case AMDIL::GPRV2I16RegClassID:
    Opc = AMDIL::PRIVATESTOREv2i16r;
    break;
  case AMDIL::GPRV2I32RegClassID:
    Opc = AMDIL::PRIVATESTOREv2i32r;
    break;
  case AMDIL::GPRV2I8RegClassID:
    Opc = AMDIL::PRIVATESTOREv2i8r;
    break;
  case AMDIL::GPRV2I64RegClassID:
    Opc = AMDIL::PRIVATESTOREv2i64r;
    break;
  case AMDIL::GPRV4F32RegClassID:
    Opc = AMDIL::PRIVATESTOREv4f32r;
    break;
  case AMDIL::GPRV4I16RegClassID:
    Opc = AMDIL::PRIVATESTOREv4i16r;
    break;
  case AMDIL::GPRV4I32RegClassID:
    Opc = AMDIL::PRIVATESTOREv4i32r;
    break;
  case AMDIL::GPRV4I8RegClassID:
    Opc = AMDIL::PRIVATESTOREv4i8r;
    break;
  }
  if (MI != MBB.end()) DL = MI->getDebugLoc();
  MachineMemOperand *MMO =
    new MachineMemOperand(
      MachinePointerInfo::getFixedStack(FrameIndex),
      MachineMemOperand::MOLoad,
      MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlignment(FrameIndex));
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }
  MachineInstr *nMI = BuildMI(MBB, MI, DL, get(Opc))
                      .addReg(SrcReg, getKillRegState(isKill))
                      .addFrameIndex(FrameIndex)
                      .addMemOperand(MMO)
                      .addImm(0);
  AMDILMachineFunctionInfo *mfinfo = MF.getInfo<AMDILMachineFunctionInfo>();
  mfinfo->setUsesScratch();
  AMDILAS::InstrResEnc curRes;
  curRes.bits.ResourceID
    = TM.getSubtargetImpl()->device()->getResourceID(AMDILDevice::SCRATCH_ID);
  setAsmPrinterFlags(nMI, curRes);
}
void
AMDILInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator MI,
                                     unsigned DestReg, int FrameIndex,
                                     const TargetRegisterClass *RC,
                                     const TargetRegisterInfo *TRI) const {
  unsigned int Opc = 0;
  MachineFunction &MF = *(MBB.getParent());
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  DebugLoc DL;
  switch (RC->getID()) {
  default:
    Opc = AMDIL::PRIVATELOADv4i32r;
    break;
  case AMDIL::GPRF32RegClassID:
    Opc = AMDIL::PRIVATELOADf32r;
    break;
  case AMDIL::GPRF64RegClassID:
    Opc = AMDIL::PRIVATELOADf64r;
    break;
  case AMDIL::GPRI16RegClassID:
    Opc = AMDIL::PRIVATELOADi16r;
    break;
  case AMDIL::GPRI32RegClassID:
    Opc = AMDIL::PRIVATELOADi32r;
    break;
  case AMDIL::GPRI8RegClassID:
    Opc = AMDIL::PRIVATELOADi8r;
    break;
  case AMDIL::GPRI64RegClassID:
    Opc = AMDIL::PRIVATELOADi64r;
    break;
  case AMDIL::GPRV2F32RegClassID:
    Opc = AMDIL::PRIVATELOADv2f32r;
    break;
  case AMDIL::GPRV2F64RegClassID:
    Opc = AMDIL::PRIVATELOADv2f64r;
    break;
  case AMDIL::GPRV2I16RegClassID:
    Opc = AMDIL::PRIVATELOADv2i16r;
    break;
  case AMDIL::GPRV2I32RegClassID:
    Opc = AMDIL::PRIVATELOADv2i32r;
    break;
  case AMDIL::GPRV2I8RegClassID:
    Opc = AMDIL::PRIVATELOADv2i8r;
    break;
  case AMDIL::GPRV2I64RegClassID:
    Opc = AMDIL::PRIVATELOADv2i64r;
    break;
  case AMDIL::GPRV4F32RegClassID:
    Opc = AMDIL::PRIVATELOADv4f32r;
    break;
  case AMDIL::GPRV4I16RegClassID:
    Opc = AMDIL::PRIVATELOADv4i16r;
    break;
  case AMDIL::GPRV4I32RegClassID:
    Opc = AMDIL::PRIVATELOADv4i32r;
    break;
  case AMDIL::GPRV4I8RegClassID:
    Opc = AMDIL::PRIVATELOADv4i8r;
    break;
  }

  MachineMemOperand *MMO =
    new MachineMemOperand(
      MachinePointerInfo::getFixedStack(FrameIndex),
      MachineMemOperand::MOLoad,
      MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlignment(FrameIndex));
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }
  AMDILMachineFunctionInfo *mfinfo = MF.getInfo<AMDILMachineFunctionInfo>();
  mfinfo->setUsesScratch();
  MachineInstr* nMI = BuildMI(MBB, MI, DL, get(Opc))
                      .addReg(DestReg, RegState::Define)
                      .addFrameIndex(FrameIndex)
                      .addMemOperand(MMO)
                      .addImm(0);
  AMDILAS::InstrResEnc curRes;
  curRes.bits.ResourceID
    = TM.getSubtargetImpl()->device()->getResourceID(AMDILDevice::SCRATCH_ID);
  setAsmPrinterFlags(nMI, curRes);
}
#if 0
MachineInstr *
AMDILInstrInfo::foldMemoryOperandImpl(MachineFunction &MF,
                                      MachineInstr *MI,
                                      const SmallVectorImpl<unsigned> &Ops,
                                      int FrameIndex) const {
  // TODO: Implement this function
  return 0;
}
MachineInstr*
AMDILInstrInfo::foldMemoryOperandImpl(MachineFunction &MF,
                                      MachineInstr *MI,
                                      const SmallVectorImpl<unsigned> &Ops,
                                      MachineInstr *LoadMI) const {
  // TODO: Implement this function
  return 0;
}
#endif

#if 0
bool
AMDILInstrInfo::canFoldMemoryOperand(const MachineInstr *MI,
                                     const SmallVectorImpl<unsigned> &Ops)
const
{
  // TODO: Implement this function
  return TargetInstrInfoImpl::canFoldMemoryOperand(MI, Ops);
}
bool
AMDILInstrInfo::unfoldMemoryOperand(MachineFunction &MF, MachineInstr *MI,
                                    unsigned Reg, bool UnfoldLoad,
                                    bool UnfoldStore,
                                    SmallVectorImpl<MachineInstr*> &NewMIs)
const {
  // TODO: Implement this function
  return false;
}
bool
AMDILInstrInfo::unfoldMemoryOperand(SelectionDAG &DAG, SDNode *N,
                                    SmallVectorImpl<SDNode*> &NewNodes) const {
  // TODO: Implement this function
  return false;
}
unsigned
AMDILInstrInfo::getOpcodeAfterMemoryUnfold(unsigned Opc,
                                           bool UnfoldLoad, bool UnfoldStore,
                                           unsigned *LoadRegIndex) const {
  // TODO: Implement this function
  return 0;
}
#endif
bool
AMDILInstrInfo::areLoadsFromSameBasePtr(SDNode *Load1, SDNode *Load2,
                                        int64_t &Offset1,
                                        int64_t &Offset2) const {
  if (!Load1->isMachineOpcode() || !Load2->isMachineOpcode()) {
    return false;
  }
  const MachineSDNode *mload1 = dyn_cast<MachineSDNode>(Load1);
  const MachineSDNode *mload2 = dyn_cast<MachineSDNode>(Load2);
  if (!mload1 || !mload2) {
    return false;
  }
  if (mload1->memoperands_empty() ||
      mload2->memoperands_empty()) {
    return false;
  }
  MachineMemOperand *memOp1 = (*mload1->memoperands_begin());
  MachineMemOperand *memOp2 = (*mload2->memoperands_begin());
  const Value *mv1 = memOp1->getValue();
  const Value *mv2 = memOp2->getValue();
  if (!memOp1->isLoad() || !memOp2->isLoad()) {
    return false;
  }
  if (getBasePointerValue(mv1) == getBasePointerValue(mv2)) {
    if (isa<GetElementPtrInst>(mv1) && isa<GetElementPtrInst>(mv2)) {
      const GetElementPtrInst *gep1 = dyn_cast<GetElementPtrInst>(mv1);
      const GetElementPtrInst *gep2 = dyn_cast<GetElementPtrInst>(mv2);
      if (!gep1 || !gep2) {
        return false;
      }
      if (gep1->getNumOperands() != gep2->getNumOperands()) {
        return false;
      }
      for (unsigned i = 0, e = gep1->getNumOperands() - 1; i < e; ++i) {
        const Value *op1 = gep1->getOperand(i);
        const Value *op2 = gep2->getOperand(i);
        if (op1 != op2) {
          // If any value except the last one is different, return false.
          return false;
        }
      }
      unsigned size = gep1->getNumOperands()-1;
      if (!isa<ConstantInt>(gep1->getOperand(size))
          || !isa<ConstantInt>(gep2->getOperand(size))) {
        return false;
      }
      Offset1 = dyn_cast<ConstantInt>(gep1->getOperand(size))->getSExtValue();
      Offset2 = dyn_cast<ConstantInt>(gep2->getOperand(size))->getSExtValue();
      return true;
    } else if (isa<Argument>(mv1) && isa<Argument>(mv2)) {
      return false;
    } else if (isa<GlobalValue>(mv1) && isa<GlobalValue>(mv2)) {
      return false;
    }
  }
  return false;
}
bool AMDILInstrInfo::shouldScheduleLoadsNear(SDNode *Load1, SDNode *Load2,
                                             int64_t Offset1, int64_t Offset2,
                                             unsigned NumLoads) const {
  LoadSDNode *LoadSD1 = dyn_cast<LoadSDNode>(Load1);
  LoadSDNode *LoadSD2 = dyn_cast<LoadSDNode>(Load2);
  if (!LoadSD1 || !LoadSD2) {
    return false;
  }
  // We only care about scheduling loads near for global address space.
  if (dyn_cast<PointerType>(LoadSD1->getSrcValue()->getType())
      ->getAddressSpace() != AMDILAS::GLOBAL_ADDRESS) {
    return false;
  }
  // We only care about scheduling loads near for global address space.
  if (dyn_cast<PointerType>(LoadSD2->getSrcValue()->getType())
      ->getAddressSpace() != AMDILAS::GLOBAL_ADDRESS) {
    return false;
  }
  assert(Offset2 > Offset1
         && "Second offset should be larger than first offset!");
  // If we have less than 16 loads in a row, and the offsets are within 16,
  // then schedule together.
  // TODO: Make the loads schedule near if it fits in a cacheline
  return (NumLoads < 16 && (Offset2 - Offset1) < 16);
}
bool AMDILInstrInfo::shouldScheduleWithNormalPriority(SDNode* instruction)
const {
  if (instruction->isMachineOpcode()) {
    unsigned int Opc = instruction->getMachineOpcode();
    switch(Opc) {
    case AMDIL::BARRIER_7XX:
    case AMDIL::BARRIER_EGNI:
    case AMDIL::BARRIER_LOCAL:
    case AMDIL::BARRIER_GLOBAL:
    case AMDIL::BARRIER_REGION:
    case AMDIL::FENCEr:
    case AMDIL::FENCE_Lr:
    case AMDIL::FENCE_Mr:
    case AMDIL::FENCE_Gr:
    case AMDIL::FENCE_LMr:
    case AMDIL::FENCE_LGr:
    case AMDIL::FENCE_MGr:
    case AMDIL::FENCE_ROr:
    case AMDIL::FENCE_RO_Lr:
    case AMDIL::FENCE_RO_Mr:
    case AMDIL::FENCE_RO_Gr:
    case AMDIL::FENCE_RO_LMr:
    case AMDIL::FENCE_RO_LGr:
    case AMDIL::FENCE_RO_MGr:
    case AMDIL::FENCE_WOr:
    case AMDIL::FENCE_WO_Lr:
    case AMDIL::FENCE_WO_Mr:
    case AMDIL::FENCE_WO_Gr:
    case AMDIL::FENCE_WO_LMr:
    case AMDIL::FENCE_WO_LGr:
    case AMDIL::FENCE_WO_MGr:
    case AMDIL::FENCE_Sr:
    case AMDIL::FENCE_MSr:
    case AMDIL::FENCE_LSr:
    case AMDIL::FENCE_GSr:
    case AMDIL::FENCE_LMSr:
    case AMDIL::FENCE_MGSr:
    case AMDIL::FENCE_LGSr:
      return true;    // Maybe other instructions will need to be added to this?
    default:
      return false;
    }
  }
  return false;
}
bool
AMDILInstrInfo::ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond)
const {
  // TODO: Implement this function
  return true;
}
void AMDILInstrInfo::insertNoop(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI) const {
  // TODO: Implement this function
}
bool AMDILInstrInfo::isPredicated(const MachineInstr *MI) const {
  // TODO: Implement this function
  return false;
}
bool AMDILInstrInfo::isUnpredicatedTerminator(const MachineInstr *MI) const {
  // TODO: Implement this function
  return false;
}
bool AMDILInstrInfo::PredicateInstruction(
  MachineInstr *MI,
  const SmallVectorImpl<MachineOperand>
  &Pred) const {
  // TODO: Implement this function
  return false;
}
bool
AMDILInstrInfo::SubsumesPredicate(const SmallVectorImpl<MachineOperand> &Pred1,
                                  const SmallVectorImpl<MachineOperand> &Pred2)
const {
  // TODO: Implement this function
  return false;
}
bool AMDILInstrInfo::DefinesPredicate(MachineInstr *MI,
                                      std::vector<MachineOperand> &Pred) const
{
  // TODO: Implement this function
  return false;
}
bool AMDILInstrInfo::isPredicable(MachineInstr *MI) const {
  // TODO: Implement this function
  return MI->getDesc().isPredicable();
}
bool
AMDILInstrInfo::isSafeToMoveRegClassDefs(const TargetRegisterClass *RC) const {
  // TODO: Implement this function
  return true;
}
unsigned AMDILInstrInfo::GetInstSizeInBytes(const MachineInstr *MI) const {
  // TODO: Implement this function
  return 0;
}
unsigned
AMDILInstrInfo::GetFunctionSizeInBytes(const MachineFunction &MF) const {
  // TODO: Implement this function
  return 0;
}
unsigned AMDILInstrInfo::getInlineAsmLength(const char *Str,
                                            const MCAsmInfo &MAI) const {
  // TODO: Implement this function
  return 0;
}
