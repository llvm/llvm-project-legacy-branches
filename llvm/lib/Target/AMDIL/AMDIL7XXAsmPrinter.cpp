//===-- AMDIL7XXAsmPrinter.cpp --------------------------------------------===//
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

#include "AMDIL7XXAsmPrinter.h"
#include "AMDILAlgorithms.tpp"
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
#include "llvm/Support/DebugLoc.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

// TODO: Add support for verbose.
AMDIL7XXAsmPrinter::AMDIL7XXAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS)
  : AMDILAsmPrinter(ASM_PRINTER_ARGUMENTS)
{
}
AMDIL7XXAsmPrinter::~AMDIL7XXAsmPrinter()
{
}
///
/// @param name
/// @brief strips KERNEL_PREFIX and KERNEL_SUFFIX from the name
/// and returns that name if both of the tokens are present.
///
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
void
AMDIL7XXAsmPrinter::emitMacroFunc(const MachineInstr *MI,
                                  OSTREAM_TYPE &O)
{
  const AMDILSubtarget *curTarget = mTM->getSubtargetImpl();
  const char *name = "unknown";
  llvm::StringRef nameRef;
  if (MI->getOperand(0).isGlobal()) {
    nameRef = MI->getOperand(0).getGlobal()->getName();
    name = nameRef.data();
    if (curTarget->device()->usesHardware(
          AMDILDeviceInfo::DoubleOps)
        && !::strncmp(name, "__sqrt_f64", 10) ) {
      name = "__sqrt_f64_7xx";
    }
  }
  emitMCallInst(MI, O, name);
}
bool
AMDIL7XXAsmPrinter::runOnMachineFunction(MachineFunction &lMF)
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
AMDIL7XXAsmPrinter::EmitInstruction(const MachineInstr *II)
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
    O << "\tmcall("<<macronum<<") ";
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
  } else {
    // Print the assembly for the instruction.
    // We want to make sure that we do HW constants
    // before we do arena segment
    if (mMeta->useCompilerWrite(II)) {
      // TODO: This is a hack to get around some
      // conformance failures.
      O << "\tif_logicalz cb0[0].x\n";
      O << "\tuav_raw_store_id("
        << curTarget->device()->getResourceID(AMDILDevice::RAW_UAV_ID)
        << ") ";
      O << "mem0.x___, cb0[3].x, r0.0\n";
      O << "\tendif\n";
      mMFI->addMetadata(";memory:compilerwrite");
    } else if (II->getOpcode() == AMDIL::COPY) {
      printCopy(II, O);
    } else {
      printInstruction(II, O);
    }
  }
  O.flush();
  OutStreamer.EmitRawText(StringRef(FunStr));
}
