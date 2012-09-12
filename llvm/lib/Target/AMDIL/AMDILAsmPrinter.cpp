//===-- AMDILAsmPrinter.cpp -----------------------------------------------===//
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

#define DEBUG_TYPE "asm-printer"
#if !defined(NDEBUG) && !defined(USE_APPLE)
# define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
# define DEBUGME (false)
#endif
#include "AMDILAsmPrinter.h"
#include "AMDILAlgorithms.tpp"
#include "AMDILCompilerErrors.h"
#include "AMDILDevices.h"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILModuleInfo.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/Constants.h"
#include "llvm/Metadata.h"
#include "llvm/Type.h"
#include "llvm/DebugInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/DebugLoc.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/TargetRegistry.h"
#include <sstream>
using namespace llvm;
/// createAMDILCodePrinterPass - Returns a pass that prints the AMDIL
/// assembly code for a MachineFunction to the given output stream,
/// using the given target machine description. This should work
/// regardless of whether the function is in SSA form.
///

ASMPRINTER_RETURN_TYPE
createAMDILCodePrinterPass(AMDIL_ASM_PRINTER_ARGUMENTS)
{
  const AMDILSubtarget *stm = &TM.getSubtarget<AMDILSubtarget>();
  return stm->device()->getAsmPrinter(ASM_PRINTER_ARGUMENTS);
}
#include "AMDILGenAsmWriter.inc"
// Force static initialization
extern "C" void LLVMInitializeAMDILAsmPrinter() {
  llvm::TargetRegistry::RegisterAsmPrinter(TheAMDILTarget,
                                           createAMDILCodePrinterPass);
}
AMDILInstPrinter *llvm::createAMDILInstPrinter(const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new AMDILInstPrinter(MAI, MII, MRI);
}
//
// @param name
// @brief strips KERNEL_PREFIX and KERNEL_SUFFIX from the name
// and returns that name if both of the tokens are present.
//
static
std::string Strip(const std::string &name)
{
  size_t start = name.find("__OpenCL_");
  size_t end = name.find("_kernel");
  if (start == std::string::npos
      || end == std::string::npos
      || (start == end)) {
    return name;
  } else {
    return name.substr(9, name.length()-16);
  }
}
// TODO: Add support for verbose.
AMDILAsmPrinter::AMDILAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS)
  : AsmPrinter(ASM_PRINTER_ARGUMENTS)
{
  mDebugMode = DEBUGME;
  mTM = reinterpret_cast<AMDILTargetMachine*>(&TM);
  mTM->setDebug(mDebugMode);
  mMeta = new AMDILKernelManager(mTM);
  mBuffer = 0;
  mNeedVersion = false;
  mMFI = NULL;
  mAMI = NULL;
}
AMDILAsmPrinter::~AMDILAsmPrinter()
{
  delete mMeta;
}
const char*
AMDILAsmPrinter::getPassName() const
{
  return "AMDIL Assembly Printer";
}
void
AMDILAsmPrinter::EmitInstruction(const MachineInstr *II)
{
  std::string FunStr;
  raw_string_ostream OFunStr(FunStr);
  formatted_raw_ostream O(OFunStr);
  const AMDILSubtarget *curTarget = mTM->getSubtargetImpl();
  if (mDebugMode) {
    O << ";";
    II->print(O);
  }
  if (isMacroFunc(II)) {
    emitMacroFunc(II, O);
    O.flush();
    OutStreamer.EmitRawText(StringRef(FunStr));
    return;
  }
  if (isMacroCall(II)) {
    unsigned reg = 0;
    unsigned newDst = 0;
    OpSwizzle opSwiz, oldSwiz;
    const char *name = mTM->getInstrInfo()->getName(II->getOpcode()) + 5;
    int macronum = amd::MacroDBFindMacro(name);
    O << "\t;"<< name<<"\n";
    O << "\tmcall("<<macronum<<")";
    reg = II->getOperand(0).getReg();
    newDst = AMDIL::R1000;
    oldSwiz.u8all = opSwiz.u8all =
                      II->getOperand(0).getTargetFlags();
    if (isXComponentReg(reg)) {
      newDst = AMDIL::Rx1000;
      opSwiz.bits.swizzle = AMDIL_DST_X___;
    } else if (isYComponentReg(reg)) {
      newDst = AMDIL::Ry1000;
      opSwiz.bits.swizzle = AMDIL_DST_X___;
    } else if (isZComponentReg(reg)) {
      newDst = AMDIL::Rz1000;
      opSwiz.bits.swizzle = AMDIL_DST_X___;
    } else if (isWComponentReg(reg)) {
      newDst = AMDIL::Rw1000;
      opSwiz.bits.swizzle = AMDIL_DST_X___;
    } else if (isXYComponentReg(reg)) {
      newDst = AMDIL::Rxy1000;
      opSwiz.bits.swizzle = AMDIL_DST_XY__;
    } else if (isZWComponentReg(reg)) {
      newDst = AMDIL::Rzw1000;
      opSwiz.bits.swizzle = AMDIL_DST_XY__;
    } else {
      opSwiz.bits.swizzle = AMDIL_DST_DFLT;
    }
    for (unsigned x = 0, y = II->getNumOperands(); x < y; ++x) {
      if (!x) {
        O << "(";
        O << getRegisterName(newDst);
        O << getDstSwizzle(opSwiz.bits.swizzle);
      } else {
        printOperand(II, x
                     , O
                     );
      }
      if (!x) {
        O << "), (";
      } else if (x != y - 1) {
        O << ", ";
      } else {
        O << ")\n";
      }
    }
    O << "\tmov " << getRegisterName(reg) << getDstSwizzle(oldSwiz.bits.swizzle)
      << ", " << getRegisterName(newDst);
    if (isXComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_X000);
    } else if (isYComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_0X00);
    } else if (isZComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_00X0);
    } else if (isWComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_000X);
    } else if (isXYComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_XY00);
    } else if (isZWComponentReg(reg)) {
      O << getSrcSwizzle(AMDIL_SRC_00XY);
    } else {
      O << getSrcSwizzle(AMDIL_SRC_DFLT);
    }
    O << "\n";
    if (curTarget->device()->isSupported(
          AMDILDeviceInfo::MacroDB)) {
      mMacroIDs.insert(macronum);
    } else {
      mMFI->addCalledIntr(macronum);
    }
  } else if (II->getOpcode() == AMDIL::COPY) {
    printCopy(II, O);
  } else {
    printInstruction(II, O);
  }
  O.flush();
  OutStreamer.EmitRawText(StringRef(FunStr));
}
void
AMDILAsmPrinter::printCopy(const MachineInstr *MI,
                           OSTREAM_TYPE &O)
{
  O << "\tmov ";
  printOperand(MI, 0, O);
  O << ", ";
  printOperand(MI, 1, O);
  O << "\n";
}
void
AMDILAsmPrinter::emitMacroFunc(const MachineInstr *MI,
                               OSTREAM_TYPE &O)
{
  const char *name = "unknown";
  llvm::StringRef nameRef;
  if (MI->getOperand(0).isGlobal()) {
    nameRef = MI->getOperand(0).getGlobal()->getName();
    name = nameRef.data();
  }
  emitMCallInst(MI, O, name);
}
bool
AMDILAsmPrinter::runOnMachineFunction(MachineFunction &lMF)
{
  this->MF = &lMF;
  mMeta->setMF(&lMF);
  mMFI = lMF.getInfo<AMDILMachineFunctionInfo>();
  mAMI = &(lMF.getMMI().getObjFileInfo<AMDILModuleInfo>());

  SetupMachineFunction(lMF);
  std::string kernelName = MF->getFunction()->getName();
  mName = Strip(kernelName);

  mKernelName = kernelName;
  EmitFunctionHeader();
  EmitFunctionBody();
  return false;
}
void
AMDILAsmPrinter::addCPoolLiteral(const Constant *C)
{
  if (const ConstantFP *CFP = dyn_cast<ConstantFP>(C)) {
    if (CFP->getType()->isFloatTy()) {
      mMFI->addf32Literal(CFP);
    } else {
      mMFI->addf64Literal(CFP);
    }
  } else if (const ConstantInt *CI = dyn_cast<ConstantInt>(C)) {
    int64_t val = 0;
    if (CI) {
      val = CI->getSExtValue();
    }
    if (CI->getBitWidth() == (int64_t)64) {
      mMFI->addi64Literal(val);
    } else if (CI->getBitWidth() == (int64_t)8) {
      mMFI->addi32Literal((uint32_t)val, AMDIL::LOADCONSTi8);
    } else if (CI->getBitWidth() == (int64_t)16) {
      mMFI->addi32Literal((uint32_t)val, AMDIL::LOADCONSTi16);
    } else {
      mMFI->addi32Literal((uint32_t)val, AMDIL::LOADCONSTi32);
    }
  } else if (const ConstantArray *CA = dyn_cast<ConstantArray>(C)) {
    uint32_t size = CA->getNumOperands();
    for (uint32_t x = 0; x < size; ++x) {
      addCPoolLiteral(CA->getOperand(x));
    }
  } else if (const ConstantAggregateZero *CAZ
               = dyn_cast<ConstantAggregateZero>(C)) {
    if (CAZ->isNullValue()) {
      mMFI->addi32Literal(0, AMDIL::LOADCONSTi32);
      mMFI->addi64Literal(0);
      mMFI->addf64Literal((uint64_t)0);
      mMFI->addf32Literal((uint32_t)0);
    }
  } else if (const ConstantStruct *CS = dyn_cast<ConstantStruct>(C)) {
    uint32_t size = CS->getNumOperands();
    for (uint32_t x = 0; x < size; ++x) {
      addCPoolLiteral(CS->getOperand(x));
    }
  } else if (const ConstantVector *CV = dyn_cast<ConstantVector>(C)) {
    // TODO: Make this handle vectors natively up to the correct
    // size
    uint32_t size = CV->getNumOperands();
    for (uint32_t x = 0; x < size; ++x) {
      addCPoolLiteral(CV->getOperand(x));
    }
  } else {
    // TODO: Do we really need to handle ConstantPointerNull?
    // What about BlockAddress, ConstantExpr and Undef?
    // How would these even be generated by a valid CL program?
    assert(0 && "Found a constant type that I don't know how to handle");
  }
}
void
AMDILAsmPrinter::EmitGlobalVariable(const GlobalVariable *GV)
{
  llvm::StringRef GVname = GV->getName();
  SmallString<1024> Str;
  raw_svector_ostream O(Str);
  int32_t autoSize = mAMI->getArrayOffset(GVname);
  int32_t constSize = mAMI->getConstOffset(GVname);
  O << ".global@" << GVname;
  if (autoSize != -1) {
    O << ":" << autoSize << "\n";
  } else if (constSize != -1) {
    O << ":" << constSize << "\n";
  }
  O.flush();
  OutStreamer.EmitRawText(O.str());
}
void
AMDILAsmPrinter::printOperand(const MachineInstr *MI, int opNum
                              , OSTREAM_TYPE &O
                              )
{
  const MachineOperand &MO = MI->getOperand (opNum);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    if (MO.isReg()) {
      unsigned opcode = MI->getOpcode();
      if ((signed)MO.getReg() < 0) {
        // FIXME: we need to remove all virtual register creation after register allocation.
        // This is a work-around to make sure that the virtual register range does not
        // clobber the physical register range.
        O << "r" << ((MO.getReg() & 0x7FFFFFFF)  + 2048) << getSwizzle(MI,
                                                                       opNum);
      } else if (opNum == 0
                 && isAtomicInst(MI) && isStoreInst(MI)) {
        const MachineOperand &MO = MI->getOperand(opNum);
        OpSwizzle swiz;
        unsigned reg = MI->getOperand(2).getReg();
        swiz.u8all = MO.getTargetFlags();
        O << "mem0";
        if (isXComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST_X___);
        } else if (isYComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST__Y__);
        } else if (isZComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST___Z_);
        } else if (isWComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST____W);
        } else if (isXYComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST_XY__);
        } else if (isZWComponentReg(reg)) {
          O << getDstSwizzle(AMDIL_DST___ZW);
        } else {
          O << getDstSwizzle(AMDIL_DST_DFLT);
        }
        O << ", " << getRegisterName(MO.getReg()) << getSwizzle(MI, opNum);
      } else if (opNum == 0
                 && isScratchInst(MI) && isStoreInst(MI)) {
        O << getRegisterName(MO.getReg()) << ".x]";
        uint32_t reg = MI->getOperand(1).getReg();
        // If we aren't a vector register, print the dst swizzle.
        if (reg < AMDIL::R1 || reg > AMDIL::R1012) {
          O << getSwizzle(MI, opNum);
        }
      } else {
        O << getRegisterName(MO.getReg()) << getSwizzle(MI, opNum);
      }
    } else {
      assert(0 && "Invalid Register type");
      mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
    }
    break;
  case MachineOperand::MO_Immediate:
  case MachineOperand::MO_FPImmediate:
  {
    if (isSkippedLiteral(MI, opNum)) {
    } else if (isBypassedLiteral(MI, opNum)) {
      O << MO.getImm();
    } else if (MO.isImm() || MO.isFPImm()) {
      O << "l" << MO.getImm() << getSwizzle(MI, opNum);
    } else {
      assert(0 && "Invalid literal/constant type");
      mMFI->addErrorMsg(
        amd::CompilerErrorMessage[INTERNAL_ERROR]);
    }
  }
  break;
  case MachineOperand::MO_MachineBasicBlock:
    EmitBasicBlockStart(MO.getMBB());
    return;
  case MachineOperand::MO_GlobalAddress:
  {
    int offset = 0;
    const GlobalValue *gv = MO.getGlobal();
    // Here we look up by the name for the corresponding number
    // and we print that out instead of the name or the address
    if (MI->getOpcode() == AMDIL::CALL) {
      uint32_t funcNum;
      llvm::StringRef name = gv->getName();
      funcNum = name.empty()
                ?  mAMI->getOrCreateFunctionID(gv)
                : mAMI->getOrCreateFunctionID(name);
      mMFI->addCalledFunc(funcNum);
      O << funcNum <<" ; "<< name;
    } else if((offset = mAMI->getArrayOffset(gv->getName()))
              != -1) {
      mMFI->setUsesLDS();
      O << "l" << mMFI->getLitIdx((uint32_t)offset) << ".x";
    } else if((offset = mAMI->getConstOffset(gv->getName()))
              != -1) {
      mMFI->addMetadata(";memory:datareqd");
      O << "l" << mMFI->getLitIdx((uint32_t)offset) << ".x";
      mMFI->setUsesConstant();
    } else {
      assert(0 && "GlobalAddress without a function call!");
      mMFI->addErrorMsg(
        amd::CompilerErrorMessage[MISSING_FUNCTION_CALL]);
    }
  }
  break;
  case MachineOperand::MO_ExternalSymbol:
  {
    if (MI->getOpcode() == AMDIL::CALL) {
      uint32_t funcNum = mAMI->getOrCreateFunctionID(
        std::string(MO.getSymbolName()));
      mMFI->addCalledFunc(funcNum);
      O << funcNum << " ; "<< MO.getSymbolName();
      // This is where pointers should get resolved
    } else {
      assert(0 && "ExternalSymbol without a function call!");
      mMFI->addErrorMsg(
        amd::CompilerErrorMessage[MISSING_FUNCTION_CALL]);
    }
  }
  break;
  case MachineOperand::MO_ConstantPoolIndex:
  {
    // Copies of constant buffers need to be done here
    const AMDILKernel *tmp = mAMI->getKernel(mKernelName);
    O << "l" << mMFI->getLitIdx(
      tmp->CPOffsets[MO.getIndex()].first);
  }
  break;
  default:
    O << "<unknown operand type>"; break;
  }
}
void
AMDILAsmPrinter::printMemOperand(
  const MachineInstr *MI,
  int opNum,
  OSTREAM_TYPE &O,
  const char *Modifier
  )
{
  const MachineOperand &MO = MI->getOperand (opNum);
  if (opNum != 1) {
    printOperand(MI, opNum
                 , O
                 );
  } else {
    switch (MO.getType()) {
    case MachineOperand::MO_Register:
      if (MO.isReg()) {
        unsigned opcode = MI->getOpcode();
        if ((signed)MO.getReg() < 0) {
          // FIXME: we need to remove all virtual register creation after register allocation.
          // This is a work-around to make sure that the virtual register range does not
          // clobber the physical register range.
          O << "r" << ((MO.getReg() & 0x7FFFFFFF) + 2048) << getSwizzle(MI,
                                                                        opNum);
        } else if (opNum == 0
                   && isScratchInst(MI)) {
          O << getRegisterName(MO.getReg()) << ".x]";
          uint32_t reg = MI->getOperand(1).getReg();
          // If we aren't the vector register, print the dst swizzle.
          if (reg < AMDIL::R1 || reg > AMDIL::R1012) {
            O << getSwizzle(MI, opNum);
          }
        } else {
          O << getRegisterName(MO.getReg()) << getSwizzle(MI, opNum);
        }
      }
      else {
        assert(0 && "Invalid Register type");
        mMFI->addErrorMsg(
          amd::CompilerErrorMessage[INTERNAL_ERROR]);
      }
      break;
    case MachineOperand::MO_Immediate:
    case MachineOperand::MO_FPImmediate:
    {
      if (isSkippedLiteral(MI, opNum)) {
      } else if (isBypassedLiteral(MI, opNum)) {
        O << MO.getImm();
      } else if (MO.isImm() || MO.isFPImm()) {
        O << "l" << MO.getImm() << getSwizzle(MI, opNum);
      } else {
        assert(0 && "Invalid literal/constant type");
        mMFI->addErrorMsg(
          amd::CompilerErrorMessage[INTERNAL_ERROR]);
      }
    }
    break;
    case MachineOperand::MO_ConstantPoolIndex:
    {
      // Copies of constant buffers need to be done here
      const AMDILKernel *tmp = mAMI->getKernel(mKernelName);
      O << "l" << mMFI->getLitIdx(
        tmp->CPOffsets[MO.getIndex()].first);
    }
    break;
    default:
      O << "<unknown operand type>"; break;
    };
  }
}
const char*
AMDILAsmPrinter::getSwizzle(const MachineInstr *MI, int opNum)
{
  const MachineOperand &MO = MI->getOperand(opNum);
  OpSwizzle swiz;
  swiz.u8all = MO.getTargetFlags();
  if (!swiz.bits.dst) {
    return getSrcSwizzle(swiz.bits.swizzle);
  } else {
    return getDstSwizzle(swiz.bits.swizzle);
  }
}
void
AMDILAsmPrinter::EmitStartOfAsmFile(Module &M)
{
  SmallString<1024> Str;
  raw_svector_ostream O(Str);
  const AMDILSubtarget *curTarget = mTM->getSubtargetImpl();
  curTarget->setKernelManager(mMeta);

  if (curTarget->device()->isSupported(
        AMDILDeviceInfo::MacroDB)) {
    // Since we are using the macro db, the first token must be a macro.
    // So we make up a macro that is never used.
    // I originally picked -1, but the IL text translater treats them as
    // unsigned integers.
    O << "mdef(16383)_out(1)_in(2)\n";
    O << "mov r0, in0\n";
    O << "mov r1, in1\n";
    O << "div_zeroop(infinity) r0.x___, r0.x, r1.x\n";
    O << "mov out0, r0\n";
    O << "mend\n";
  }

  // We need to increase the number of reserved literals for
  // any literals we output manually instead of via the
  // emitLiteral function. This function should never
  // have any executable code in it. Only declarations
  // and the main function patch symbol.
  if (curTarget->device()->getGeneration() == AMDILDeviceInfo::HDTEST) {
    O << "il_cs_3_0\n";
  } else {
    O << "il_cs_2_0\n";
  }
  O << "dcl_cb cb0[15] ; Constant buffer that holds ABI data\n";
  O << "dcl_literal l0, 0x00000004, 0x00000001, 0x00000002, 0x00000003\n";
  O << "dcl_literal l1, 0x00FFFFFF, 0xFFFFFFFF, 0xFFFFFFFE, 0xFFFFFFFD\n";
  O << "dcl_literal l2, 0x0000FFFF, 0xFFFFFFFE, 0x000000FF, 0xFFFFFFFC\n";
  O << "dcl_literal l3, 0x00000018, 0x00000010, 0x00000008, 0xFFFFFFFF\n";
  O << "dcl_literal l4, 0xFFFFFF00, 0xFFFF0000, 0xFF00FFFF, 0xFFFF00FF\n";
  O << "dcl_literal l5, 0x00000000, 0x00000004, 0x00000008, 0x0000000C\n";
  O << "dcl_literal l6, 0x00000020, 0x00000020, 0x00000020, 0x00000020\n";
  O << "dcl_literal l7, 0x00000018, 0x0000001F, 0x00000010, 0x0000001F\n";
  O << "dcl_literal l8, 0x80000000, 0x80000000, 0x80000000, 0x80000000\n";
  O << ";$$$$$$$$$$\n";
  O << "endmain\n";
  O << ";DEBUGSTART\n";
  OutStreamer.EmitRawText(O.str());
}
void
AMDILAsmPrinter::EmitEndOfAsmFile(Module &M)
{
  SmallString<1024> Str;
  raw_svector_ostream O(Str);
  const AMDILSubtarget *curTarget = mTM->getSubtargetImpl();
  O << ";DEBUGEND\n";
  if (curTarget->device()->isSupported(AMDILDeviceInfo::MacroDB)) {
    int lines;
    for (llvm::DenseSet<uint32_t>::iterator msb = mMacroIDs.begin()
         , mse = mMacroIDs.end(); msb != mse; ++msb) {
      int idx = *msb;
      const char* *macro = amd::MacroDBGetMacro(&lines, idx);
      for (int k = 0; k < lines; ++k) {
        O << macro[k];
      }
    }
  }
  if (mAMI) mAMI->dumpDataSection(O, mMFI);
  O << "\nend\n";
#ifdef _DEBUG
  if (mDebugMode) {
    mTM->dump(O);
  }
#endif
  OutStreamer.EmitRawText(O.str());
}
void
AMDILAsmPrinter::PrintSpecial(const MachineInstr *MI, const char *Code) const
{
  assert(0 && "When is this function hit!");
}
bool
AMDILAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned int OpNo,
                                 unsigned int AsmVariant, const char *ExtraCode)
{
  assert(0 && "When is this function hit!");
  return false;
}
bool
AMDILAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                       unsigned int OpNo,
                                       unsigned int AsmVariant,
                                       const char *ExtraCode)
{
  assert(0 && "When is this function hit!");
  return false;
}
void
AMDILAsmPrinter::EmitMachineConstantPoolValue(MachineConstantPoolValue *MCPV)
{
  assert(0 && "When is this function hit!");
}
void
AMDILAsmPrinter::printPICJumpTableSetLabel(unsigned uid,
                                           const MachineBasicBlock *MBB) const
{
  assert(0 && "When is this function hit!");
}
void
AMDILAsmPrinter::printPICJumpTableSetLabel(unsigned uid, unsigned uid2,
                                           const MachineBasicBlock *MBB) const
{
  assert(0 && "When is this function hit!");
}
void
AMDILAsmPrinter::printPICJumpTableEntry(const MachineJumpTableInfo *MJTI,
                                        const MachineBasicBlock *MBB,
                                        unsigned uid) const
{
  assert(0 && "When is this function hit!");
}
void
AMDILAsmPrinter::EmitFunctionBodyStart()
{
  SmallString<1024> Str;
  raw_svector_ostream O(Str);

  bool isKernel = false;
  O << "";
  O << ";DEBUGEND\n";
  ++mBuffer;
  isKernel = mMFI->isKernel();
  uint32_t id = mName.empty()
                ? mAMI->getOrCreateFunctionID(MF->getFunction())
                : mAMI->getOrCreateFunctionID(mName);
  mMeta->setKernel(isKernel);
  mMeta->setID(id);
  if (isKernel) {
    mMeta->printHeader(this, O, mKernelName);
    mMeta->processArgMetadata(O, mBuffer, isKernel);
    mMeta->printGroupSize(O);
    mMeta->printDecls(this, O);
    AMDILKernel &tmp = *(mMFI->getKernel());
    // add the literals for the offsets and sizes of
    // all kernel declared local arrays
    if (tmp.lvgv) {
      AMDILLocalArg *lptr = tmp.lvgv;
      llvm::SmallVector<AMDILArrayMem*, DEFAULT_VEC_SLOTS>::iterator lmb, lme;
      for (lmb = lptr->local.begin(), lme = lptr->local.end();
           lmb != lme; ++lmb) {
        mMFI->addi32Literal((*lmb)->offset);
        mMFI->addi32Literal((*lmb)->vecSize);
        mMFI->setUsesLDS();
      }
    }
    // Add the literals for the offsets and sizes of
    // all the globally scoped constant arrays
    for (StringMap<AMDILConstPtr>::iterator cmb = mAMI->consts_begin(),
         cme = mAMI->consts_end(); cmb != cme; ++cmb) {
      mMFI->addi32Literal((cmb)->second.offset);
      mMFI->addi32Literal((cmb)->second.size);
      mMFI->addMetadata(";memory:datareqd");
      mMFI->setUsesConstant();
    }

    // Add the literals for the offsets and sizes of
    // all the kernel constant arrays
    llvm::SmallVector<AMDILConstPtr,
                      DEFAULT_VEC_SLOTS>::const_iterator cpb, cpe;
    for (cpb = tmp.constPtr.begin(), cpe = tmp.constPtr.end();
         cpb != cpe; ++cpb) {
      mMFI->addi32Literal(cpb->size);
      mMFI->addi32Literal(cpb->offset);
      mMFI->setUsesConstant();
    }
    mMeta->emitLiterals(O);
    // Add 1 to the size so that the next literal is the one we want
    mMeta->printArgCopies(O, this);
    O << "call " << id << " ; " << mName << "\n";
    mMeta->printFooter(O);
    mMeta->printMetaData(O, id, isKernel);
    O << "func " << id << " ; " << mName << "\n";
  } else {
    if (mName.empty()) {
      std::stringstream ss;
      ss << "unknown_" << id;
      mName = ss.str();
    }
    mMeta->setName(mName);
    O << "func " << id << " ; " << mName << "\n";
    mMeta->processArgMetadata(O, mBuffer, false);
  }
  O.flush();
  OutStreamer.EmitRawText(O.str());
}
void
AMDILAsmPrinter::EmitFunctionBodyEnd()
{
  SmallString<1024> Str;
  raw_svector_ostream O(Str);
  uint32_t id = mName.empty()
                ? mAMI->getOrCreateFunctionID(MF->getFunction())
                : mAMI->getOrCreateFunctionID(mName);
  if (mName.empty()) {
    std::stringstream ss;
    ss << "unknown_" << id;
    mName = ss.str();
  }
  if (mAMI->isKernel(mKernelName)) {
    O << "ret\nendfunc ; " << mName << "\n";
    mMeta->setName(mName);
    mMeta->printMetaData(O, id, false);
  } else {
    O << "ret\nendfunc ; " << mName << "\n";
    mMeta->printMetaData(O, id, false);
  }
  mMeta->clear();
  O << ";DEBUGSTART\n";
  O.flush();
  OutStreamer.EmitRawText(O.str());
}
void
AMDILAsmPrinter::EmitConstantPool()
{
  if (!mAMI->getKernel(mKernelName)) {
    return;
  }
  AMDILKernel *tmp = mAMI->getKernel(mKernelName);
  if (!tmp || !tmp->mKernel) {
    return;
  }
  mAMI->calculateCPOffsets(MF, tmp);
  // Add all the constant pool offsets to the literal table
  for (uint32_t x = 0; x < tmp->CPOffsets.size(); ++x) {
    mMFI->addMetadata(";memory:datareqd");
    mMFI->addi32Literal(tmp->CPOffsets[x].first);
  }

  // Add all the constant pool constants to the literal tables
  {
    const MachineConstantPool *MCP = MF->getConstantPool();
    const std::vector<MachineConstantPoolEntry> &consts
      = MCP->getConstants();
    for (uint32_t x = 0, s = consts.size(); x < s; ++x) {
      addCPoolLiteral(consts[x].Val.ConstVal);
    }
  }
}
void
AMDILAsmPrinter::EmitFunctionEntryLabel()
{
  return;
  assert(0 && "When is this function hit!");
}
/// getDebugResourceLocation - Get resource id information encoded in
/// target flags.
uint32_t AMDILAsmPrinter::getDebugResourceID(const MachineInstr *MI) const {
  const llvm::MachineOperand& opr = MI->getOperand(MI->getNumOperands() - 1);
  assert(opr.isMetadata());
  const MDNode *Var = opr.getMetadata();
  const Value * valOfVar = Var;
  uint32_t resourceID = mMeta->getUAVID(valOfVar);
  return resourceID;
}
bool
AMDILAsmPrinter::isMacroCall(const MachineInstr *MI) {
  return !strncmp(mTM->getInstrInfo()->getName(MI->getOpcode()), "MACRO", 5);
}
bool
AMDILAsmPrinter::isMacroFunc(const MachineInstr *MI) {
  if (MI->getOpcode() != AMDIL::CALL) {
    return false;
  }
  if (!MI->getOperand(0).isGlobal()) {
    return false;
  }
  const llvm::StringRef &nameRef = MI->getOperand(0).getGlobal()->getName();
  if (nameRef.startswith("__atom_")
      || nameRef.startswith("__atomic_")) {
    mMeta->setOutputInst();
  }
  return amd::MacroDBFindMacro(nameRef.data()) != -1;
}
static const char*
getRegSwizzle(unsigned reg, bool dst)
{
  if (reg >= AMDIL::Rx1 && reg < AMDIL::Rxy1) {
    return ".x";
  } else if (reg >= AMDIL::Ry1 && reg < AMDIL::Rz1) {
    return ".y";
  } else if (reg >= AMDIL::Rz1 && reg < AMDIL::Rzw1) {
    return ".z";
  } else if (reg >= AMDIL::Rw1 && reg < AMDIL::Rx1) {
    return ".w";
  } else if (reg >= AMDIL::Rxy1 && reg < AMDIL::Ry1) {
    return ((dst) ? ".xy__" : ".xy00");
  } else if (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP) {
    return ((dst) ? ".__zw" : ".00zw");
  } else {
    return "";
  }
}
void
AMDILAsmPrinter::emitMCallInst(const MachineInstr *MI,
                               OSTREAM_TYPE &O,
                               const char *name)
{
  const AMDILSubtarget *curTarget = mTM->getSubtargetImpl();
  int macronum = amd::MacroDBFindMacro(name);
  int numIn = amd::MacroDBNumInputs(macronum);
  int numOut = amd::MacroDBNumOutputs(macronum);
  if (macronum == -1) {
    return;
  }
  if (curTarget->device()->isSupported(
        AMDILDeviceInfo::MacroDB)) {
    mMacroIDs.insert(macronum);
  } else {
    mMFI->addCalledIntr(macronum);
  }
  const TargetRegisterClass *trc = NULL;
  if (strstr(name, "4f32")
      || strstr(name, "4i32")) {
    trc = MF->getTarget()
          .getRegisterInfo()->getRegClass(AMDIL::GPRV4F32RegClassID);
  } else if (strstr(name, "2f32")
             || strstr(name, "2i32")) {
    trc = MF->getTarget()
          .getRegisterInfo()->getRegClass(AMDIL::GPRV2F32RegClassID);
  } else {
    trc = MF->getTarget()
          .getRegisterInfo()->getRegClass(AMDIL::GPRF32RegClassID);
  }
  O << "\tmcall(" << macronum << ")(";
  int x;
  for (x = 0; x < numOut - 1; ++x) {
    O << getRegisterName(trc->getRegister(x))
      << getRegSwizzle(trc->getRegister(x), true) << ", ";
  }
  O << getRegisterName(trc->getRegister(x))
    << getRegSwizzle(trc->getRegister(x), true) << "),(";
  for (x = 0; x < numIn - 1; ++x) {
    O << getRegisterName(trc->getRegister(x))
      << getRegSwizzle(trc->getRegister(x), false) << ", ";
  }
  O << getRegisterName(trc->getRegister(x))
    << getRegSwizzle(trc->getRegister(x), false) << ")";
  O << " ;" << name <<"\n";
}
#if defined(LLVM_29) || defined(USE_APPLE)
void
AMDILAsmPrinter::EmitDwarfRegOp(const MachineLocation &MLoc) const {
}
#else
void
AMDILAsmPrinter::EmitDwarfRegOp(const MachineLocation &MLoc) const {
  const TargetRegisterInfo *RI = TM.getRegisterInfo();
  unsigned reg = MLoc.getReg();
  unsigned baseReg = AMDIL::R1;
  const char* regStr = NULL;
  const char* regxStr = NULL;
  unsigned offset = 0;
  unsigned size = 32;
  const char* offStr = NULL;
  if (isXComponentReg(reg)) {
    baseReg += (reg - AMDIL::Rx1);
    regxStr = "DW_OP_regx for x component of register";
    regStr = "DW_OP_reg for x component of register";
    offset = 0;
    offStr = "DW_OP_bit_piece 32 0";
  } else if (isYComponentReg(reg)) {
    baseReg += (reg - AMDIL::Ry1);
    regxStr = "DW_OP_regx for y component of register";
    regStr = "DW_OP_reg for y component of register";
    offset = 32;
    offStr = "DW_OP_bit_piece 32 32";
  } else if (isZComponentReg(reg)) {
    baseReg += (reg - AMDIL::Rz1);
    regxStr = "DW_OP_regx for z component of register";
    regStr = "DW_OP_reg for z component of register";
    offset = 64;
    offStr = "DW_OP_bit_piece 32 64";
  } else if (isWComponentReg(reg)) {
    baseReg += (reg - AMDIL::Rw1);
    regxStr = "DW_OP_regx for w component of register";
    regStr = "DW_OP_reg for w component of register";
    offset = 96;
    offStr = "DW_OP_bit_piece 32 96";
  } else if (isXYComponentReg(reg)) {
    baseReg += (reg - AMDIL::Rxy1);
    regxStr = "DW_OP_regx for xy component of register";
    regStr = "DW_OP_reg for xy component of register";
    offset = 0;
    size = 64;
    offStr = "DW_OP_bit_piece 64 0";
  } else if (isZWComponentReg(reg)) {
    baseReg += (reg - AMDIL::Rzw1);
    regxStr = "DW_OP_regx for zw component of register";
    regStr = "DW_OP_reg for zw component of register";
    offset = 64;
    size = 64;
    offStr = "DW_OP_bit_piece 64 64";
  } else {
    baseReg = reg;
    regxStr = "DW_OP_regx for xyzw component of register";
    regStr = "DW_OP_reg for xyzw component of register";
    offset = 0;
    size = 128;
    offStr = "DW_OP_bit_piece 128 0";
  }
  baseReg = RI->getDwarfRegNum(baseReg, false);
  OutStreamer.AddComment("Loc expr size");
  unsigned OffsetSize = MCAsmInfo::getULEB128Size(size)
                        + MCAsmInfo::getULEB128Size(offset);
  if (int Offset = MLoc.getOffset()) {
    OffsetSize += Offset ?  MCAsmInfo::getSLEB128Size(Offset) : 1;
    OutStreamer.AddComment("Loc expr size");
    EmitInt16(OffsetSize);
    OutStreamer.AddComment(
      dwarf::OperationEncodingString(dwarf::DW_OP_fbreg));
    EmitInt8(dwarf::DW_OP_fbreg);
    OutStreamer.AddComment("Offset");
    EmitSLEB128(Offset);
  } else if (baseReg < 32) {
    EmitInt16(2 + OffsetSize);
    OutStreamer.AddComment(
      dwarf::OperationEncodingString(dwarf::DW_OP_reg0 + baseReg));
    EmitInt8(dwarf::DW_OP_reg0 + baseReg);
  } else {
    EmitInt16(2 +  MCAsmInfo::getULEB128Size(baseReg) + OffsetSize);
    OutStreamer.AddComment(regxStr);
    EmitInt8(dwarf::DW_OP_regx);
    OutStreamer.AddComment(Twine(baseReg));
    EmitULEB128(baseReg);
  }

  OutStreamer.AddComment(offStr);
  EmitInt8(dwarf::DW_OP_bit_piece);
  EmitULEB128(size);
  EmitULEB128(offset);
}
#endif
