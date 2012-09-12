//===-- AMDILMachineFunctionInfo.cpp --------------------------------------===//
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

#include "AMDILMachineFunctionInfo.h"
#include "AMDILModuleInfo.h"
#include "AMDILCompilerErrors.h"
#include "AMDILModuleInfo.h"
#include "AMDILSubtarget.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/FormattedStream.h"
#include <cstdio>
#include <ostream>
#include <algorithm>
#include <string>
#include <queue>
#include <list>
#include <utility>
using namespace llvm;

static const AMDILConstPtr *getConstPtr(const AMDILKernel *krnl,
                                        const std::string &arg) {
  if (!krnl) {
    return NULL;
  }
  llvm::SmallVector<AMDILConstPtr,
                    DEFAULT_VEC_SLOTS>::const_iterator begin, end;
  for (begin = krnl->constPtr.begin(), end = krnl->constPtr.end();
       begin != end; ++begin) {
    if (!strcmp(begin->name.data(),arg.c_str())) {
      return &(*begin);
    }
  }
  return NULL;
}
void PrintfInfo::addOperand(size_t idx, uint32_t size) {
  mOperands.resize((unsigned)(idx + 1));
  mOperands[(unsigned)idx] = size;
}
uint32_t PrintfInfo::getPrintfID() {
  return mPrintfID;
}
void PrintfInfo::setPrintfID(uint32_t id) {
  mPrintfID = id;
}
size_t PrintfInfo::getNumOperands() {
  return mOperands.size();
}
uint32_t PrintfInfo::getOperandID(uint32_t idx) {
  return mOperands[idx];
}
AMDILMachineFunctionInfo::AMDILMachineFunctionInfo()
  : CalleeSavedFrameSize(0), BytesToPopOnReturn(0),
    DecorationStyle(None), ReturnAddrIndex(0),
    TailCallReturnAddrDelta(0),
    SRetReturnReg(0), mReservedLits(11),
    UsesLDS(false), LDSArg(false),
    UsesGDS(false), GDSArg(false),
    UsesScratch(false), ScratchArg(false),
    UsesConstant(false), ConstantArg(false)
{
  memset(mUsedMem, 0, sizeof(mUsedMem));
  mMF = NULL;
  mKernel = NULL;
  mScratchSize = -1;
  mArgSize = -1;
  mStackSize = -1;
}
AMDILMachineFunctionInfo::AMDILMachineFunctionInfo(MachineFunction& MF)
  : CalleeSavedFrameSize(0), BytesToPopOnReturn(0),
    DecorationStyle(None), ReturnAddrIndex(0),
    TailCallReturnAddrDelta(0),
    SRetReturnReg(0), mReservedLits(11),
    UsesLDS(false), LDSArg(false),
    UsesGDS(false), GDSArg(false),
    UsesScratch(false), ScratchArg(false),
    UsesConstant(false), ConstantArg(false)
{
  memset(mUsedMem, 0, sizeof(mUsedMem));
  const Function *F = MF.getFunction();
  mMF = &MF;
  MachineModuleInfo &mmi = MF.getMMI();
  const AMDILTargetMachine *TM =
    reinterpret_cast<const AMDILTargetMachine*>(&MF.getTarget());
  AMDILModuleInfo *AMI = &(mmi.getObjFileInfo<AMDILModuleInfo>());
  AMI->processModule(mmi.getModule(), TM);
  for (Module::const_iterator I = F->getParent()->begin(),
       E = F->getParent()->end(); I != E; ++I) {
    // Map all the known names to a unique number
    AMI->getOrCreateFunctionID(I->getName());
  }
  mSTM = TM->getSubtargetImpl();
  mKernel = AMI->getKernel(F->getName());

  mScratchSize = -1;
  mArgSize = -1;
  mStackSize = -1;
}
AMDILMachineFunctionInfo::~AMDILMachineFunctionInfo()
{
  for (std::map<std::string, PrintfInfo*>::iterator pfb = printf_begin(),
       pfe = printf_end(); pfb != pfe; ++pfb) {
    delete pfb->second;
  }
}
unsigned int
AMDILMachineFunctionInfo::getCalleeSavedFrameSize() const
{
  return CalleeSavedFrameSize;
}
void
AMDILMachineFunctionInfo::setCalleeSavedFrameSize(unsigned int bytes)
{
  CalleeSavedFrameSize = bytes;
}
unsigned int
AMDILMachineFunctionInfo::getBytesToPopOnReturn() const
{
  return BytesToPopOnReturn;
}
void
AMDILMachineFunctionInfo::setBytesToPopOnReturn(unsigned int bytes)
{
  BytesToPopOnReturn = bytes;
}
NameDecorationStyle
AMDILMachineFunctionInfo::getDecorationStyle() const
{
  return DecorationStyle;
}
void
AMDILMachineFunctionInfo::setDecorationStyle(NameDecorationStyle style)
{
  DecorationStyle = style;
}
int
AMDILMachineFunctionInfo::getRAIndex() const
{
  return ReturnAddrIndex;
}
void
AMDILMachineFunctionInfo::setRAIndex(int index)
{
  ReturnAddrIndex = index;
}
int
AMDILMachineFunctionInfo::getTCReturnAddrDelta() const
{
  return TailCallReturnAddrDelta;
}
void
AMDILMachineFunctionInfo::setTCReturnAddrDelta(int delta)
{
  TailCallReturnAddrDelta = delta;
}
unsigned int
AMDILMachineFunctionInfo::getSRetReturnReg() const
{
  return SRetReturnReg;
}
void
AMDILMachineFunctionInfo::setSRetReturnReg(unsigned int reg)
{
  SRetReturnReg = reg;
}
bool
AMDILMachineFunctionInfo::usesHWConstant(std::string name) const
{
  const AMDILConstPtr *curConst = getConstPtr(mKernel, name);
  if (curConst) {
    return curConst->usesHardware;
  } else {
    return false;
  }
}
uint32_t
AMDILMachineFunctionInfo::getLocal(uint32_t dim)
{
  if (mKernel && mKernel->sgv) {
    AMDILKernelAttr *sgv = mKernel->sgv;
    switch (dim) {
    default: break;
    case 0:
    case 1:
    case 2:
      return sgv->reqGroupSize[dim];
      break;
    case 3:
      return sgv->reqGroupSize[0] * sgv->reqGroupSize[1] * sgv->reqGroupSize[2];
    };
  }
  switch (dim) {
  default:
    return 1;
  case 3:
    return mSTM->getDefaultSize(0) *
           mSTM->getDefaultSize(1) *
           mSTM->getDefaultSize(2);
  case 2:
  case 1:
  case 0:
    return mSTM->getDefaultSize(dim);
    break;
  };
  return 1;
}
bool
AMDILMachineFunctionInfo::isKernel() const
{
  return mKernel != NULL && mKernel->mKernel;
}
AMDILKernel*
AMDILMachineFunctionInfo::getKernel()
{
  return mKernel;
}
std::string
AMDILMachineFunctionInfo::getName()
{
  if (mMF) {
    return mMF->getFunction()->getName();
  } else {
    return "";
  }
}
uint32_t
AMDILMachineFunctionInfo::getArgSize()
{
  if (mArgSize == -1) {
    const AMDILTargetMachine *TM =
      reinterpret_cast<const AMDILTargetMachine*>(&mMF->getTarget());
    const TargetData* TD = TM->getTargetData();
    Function::const_arg_iterator I = mMF->getFunction()->arg_begin();
    Function::const_arg_iterator Ie = mMF->getFunction()->arg_end();
    uint32_t Counter = 0;
    while (I != Ie) {
      Type* curType = I->getType();
      if (curType->isIntegerTy() || curType->isFloatingPointTy()) {
        ++Counter;
      } else if (const VectorType *VT = dyn_cast<VectorType>(curType)) {
        Type *ET = VT->getElementType();
        int numEle = VT->getNumElements();
        switch (ET->getPrimitiveSizeInBits()) {
        default:
          if (numEle == 3) {
            Counter++;
          } else {
            Counter += ((numEle + 2) >> 2);
          }
          break;
        case 64:
          if (numEle == 3) {
            Counter += 2;
          } else {
            Counter += (numEle >> 1);
          }
          break;
        case 16:
        case 8:
          switch (numEle) {
          default:
            Counter += ((numEle + 2) >> 2);
          case 2:
            Counter++;
            break;
          }
          break;
        }
      } else if (const PointerType *PT = dyn_cast<PointerType>(curType)) {
        Type *CT = PT->getElementType();
        const StructType *ST = dyn_cast<StructType>(CT);
        if (ST && ST->isOpaque()) {
          bool i1d  = ST->getName().startswith("struct._image1d_t");
          bool i1da = ST->getName().startswith("struct._image1d_array_t");
          bool i1db = ST->getName().startswith("struct._image1d_buffer_t");
          bool i2d  = ST->getName().startswith("struct._image2d_t");
          bool i2da = ST->getName().startswith("struct._image2d_array_t");
          bool i3d  = ST->getName().startswith("struct._image3d_t");
          bool is_image = i1d || i1da || i1db || i2d || i2da || i3d;
          if (is_image) {
            if (mSTM->device()->isSupported(AMDILDeviceInfo::Images)) {
              Counter += 2;
            } else {
              addErrorMsg(amd::CompilerErrorMessage[NO_IMAGE_SUPPORT]);
            }
          } else {
            Counter++;
          }
        } else if (CT->isStructTy()
                   && PT->getAddressSpace() == AMDILAS::PRIVATE_ADDRESS) {
          StructType *ST = dyn_cast<StructType>(CT);
          const TargetData* TD = TM->getTargetData();
          Counter += TD->RoundUpAlignment(TD->getTypeAllocSize(ST), 16) >> 4;
        } else if (CT->isIntOrIntVectorTy()
                   || CT->isFPOrFPVectorTy()
                   || CT->isArrayTy()
                   || CT->isPointerTy()
                   || PT->getAddressSpace() != AMDILAS::PRIVATE_ADDRESS) {
          ++Counter;
        } else {
          assert(0 && "Current type is not supported!");
          addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
        }
      } else {
        assert(0 && "Current type is not supported!");
        addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
      }
      ++I;
    }
    // Convert from slots to bytes by multiplying by 16(shift by 4).
    mArgSize = Counter << 4;
  }
  return (uint32_t)mArgSize;
}
uint32_t
AMDILMachineFunctionInfo::getScratchSize()
{
  const AMDILTargetMachine *TM =
    reinterpret_cast<const AMDILTargetMachine*>(&mMF->getTarget());
  const TargetData* TD = TM->getTargetData();
  if (mScratchSize == -1) {
    mScratchSize = 0;
    Function::const_arg_iterator I = mMF->getFunction()->arg_begin();
    Function::const_arg_iterator Ie = mMF->getFunction()->arg_end();
    while (I != Ie) {
      Type *curType = I->getType();
      const TargetData* TD = TM->getTargetData();
      mScratchSize += TD->RoundUpAlignment(TD->getTypeAllocSize(curType), 16);
      ++I;
    }
    mScratchSize += ((mScratchSize + 15) & ~15);
  }
  return (uint32_t)mScratchSize;
}
uint32_t
AMDILMachineFunctionInfo::getStackSize()
{
  if (mStackSize == -1) {
    uint32_t privSize = 0;
    const MachineFrameInfo *MFI = mMF->getFrameInfo();
    privSize = MFI->getOffsetAdjustment() + MFI->getStackSize();
    const AMDILTargetMachine *TM =
      reinterpret_cast<const AMDILTargetMachine*>(&mMF->getTarget());
    bool addStackSize = TM->getOptLevel() == CodeGenOpt::None;
    Function::const_arg_iterator I = mMF->getFunction()->arg_begin();
    Function::const_arg_iterator Ie = mMF->getFunction()->arg_end();
    while (I != Ie) {
      Type *curType = I->getType();
      ++I;
      if (dyn_cast<PointerType>(curType)) {
        Type *CT = dyn_cast<PointerType>(curType)->getElementType();
        if (CT->isStructTy()
            && dyn_cast<PointerType>(curType)->getAddressSpace()
            == AMDILAS::PRIVATE_ADDRESS) {
          addStackSize = true;
        }
      }
    }
    if (addStackSize) {
      privSize += getScratchSize();
    }
    mStackSize = privSize;
  }
  return (uint32_t)mStackSize;
}
uint32_t
AMDILMachineFunctionInfo::addi32Literal(uint32_t val, int Opcode) {
  // Since we have emulated 16/8/1 bit register types with a 32bit real
  // register, we need to sign extend the constants to 32bits in order for
  // comparisons against the constants to work correctly, this fixes some issues
  // we had in conformance failing for saturation.
  if (Opcode == AMDIL::LOADCONSTi16) {
    val = (((int32_t)val << 16) >> 16);
  } else if (Opcode == AMDIL::LOADCONSTi8) {
    val = (((int32_t)val << 24) >> 24);
  }
  uint64_t val64b = ((uint64_t)val | (uint64_t)val << 32U);
  return addLiteral(val64b, val64b);
}
uint32_t
AMDILMachineFunctionInfo::addi64Literal(uint64_t val) {
  return addLiteral(val, val);
}
uint32_t
AMDILMachineFunctionInfo::addi128Literal(uint64_t val_lo, uint64_t val_hi) {
  return addLiteral(val_lo, val_hi);
}
uint32_t
AMDILMachineFunctionInfo::addLiteral(uint64_t val_lo, uint64_t val_hi) {
  std::pair<uint64_t, uint64_t> a;
  a.first = val_lo;
  a.second = val_hi;
  if (mLits.find(a) == mLits.end()) {
    mLits[a] = getNumLiterals();
  }
  return mLits[a];
}
uint32_t
AMDILMachineFunctionInfo::addf32Literal(uint32_t val) {
  uint64_t Val64b = ((uint64_t)val | ((uint64_t)val << 32));
  return addLiteral(Val64b, Val64b);
}
uint32_t
AMDILMachineFunctionInfo::addf32Literal(const ConstantFP *CFP) {
  uint32_t val = (uint32_t)CFP->getValueAPF().bitcastToAPInt().getZExtValue();
  return addf32Literal(val);
}
uint32_t
AMDILMachineFunctionInfo::addf64Literal(uint64_t val) {
  return addLiteral(val, val);
}
uint32_t
AMDILMachineFunctionInfo::addf64Literal(const ConstantFP *CFP) {
  union dtol_union {
    double d;
    uint64_t ul;
  } dval;
  const APFloat &APF = CFP->getValueAPF();
  if (&APF.getSemantics() ==
      (const llvm::fltSemantics *)&APFloat::IEEEsingle) {
    float fval = APF.convertToFloat();
    dval.d = (double)fval;
  } else {
    dval.d = APF.convertToDouble();
  }
  return addLiteral(dval.ul, dval.ul);
}
uint32_t
AMDILMachineFunctionInfo::getLitIdx(uint32_t val)
{
  uint64_t val64 = ((uint64_t)val | ((uint64_t)val << 32));
  return mLits[std::pair<uint64_t, uint64_t>(val64, val64)];
}
uint32_t
AMDILMachineFunctionInfo::getLitIdx(uint64_t val)
{
  return mLits[std::pair<uint64_t, uint64_t>(val, val)];
}
size_t
AMDILMachineFunctionInfo::getNumLiterals() const {
  return mLits.size() + mReservedLits;
}
void
AMDILMachineFunctionInfo::addReservedLiterals(uint32_t size)
{
  mReservedLits += size;
}
uint32_t
AMDILMachineFunctionInfo::addSampler(std::string name, uint32_t val)
{
  if (mSamplerMap.find(name) != mSamplerMap.end()) {
    SamplerInfo newVal = mSamplerMap[name];
    newVal.val = val;
    mSamplerMap[name] = newVal;
    return mSamplerMap[name].idx;
  } else {
    SamplerInfo curVal;
    curVal.name = name;
    curVal.val = val;
    curVal.idx = mSamplerMap.size();
    mSamplerMap[name] = curVal;
    return curVal.idx;
  }
}
void
AMDILMachineFunctionInfo::setUsesMem(unsigned id) {
  assert(id < AMDILDevice::MAX_IDS &&
         "Must set the ID to be less than MAX_IDS!");
  mUsedMem[id] = true;
}
bool
AMDILMachineFunctionInfo::usesMem(unsigned id) {
  assert(id < AMDILDevice::MAX_IDS &&
         "Must set the ID to be less than MAX_IDS!");
  return mUsedMem[id];
}
void
AMDILMachineFunctionInfo::addErrorMsg(const char *msg, ErrorMsgEnum val)
{
  if (val == DEBUG_ONLY) {
#if !defined(NDEBUG)
    mErrors.insert(msg);
#endif
  }  else if (val == RELEASE_ONLY) {
#if defined(NDEBUG)
    mErrors.insert(msg);
#endif
  } else if (val == ALWAYS) {
    mErrors.insert(msg);
  }
}
uint32_t
AMDILMachineFunctionInfo::addPrintfString(std::string &name, unsigned offset)
{
  if (mPrintfMap.find(name) != mPrintfMap.end()) {
    return mPrintfMap[name]->getPrintfID();
  } else {
    PrintfInfo *info = new PrintfInfo;
    info->setPrintfID(mPrintfMap.size() + offset);
    mPrintfMap[name] = info;
    return info->getPrintfID();
  }
}
void
AMDILMachineFunctionInfo::addPrintfOperand(std::string &name,
                                           size_t idx,
                                           uint32_t size)
{
  mPrintfMap[name]->addOperand(idx, size);
}
void
AMDILMachineFunctionInfo::addMetadata(const char *md, bool kernelOnly)
{
  addMetadata(std::string(md), kernelOnly);
}
void
AMDILMachineFunctionInfo::addMetadata(std::string md, bool kernelOnly)
{
  if (kernelOnly) {
    mMetadataKernel.push_back(md);
  } else {
    mMetadataFunc.insert(md);
  }
}
size_t
AMDILMachineFunctionInfo::get_num_write_images()
{
  return write_image3d_size() + write_image2d_size()
         + write_image2d_array_size() + write_image1d_array_size()
         + write_image1d_size() + write_image1d_buffer_size();
}
bool
AMDILMachineFunctionInfo::isSignedIntType(const Value* ptr)
{
  if (!mSTM->supportMetadata30()) return true;
  std::string signedNames = "llvm.signedOrSignedpointee.annotations.";
  std::string argName = ptr->getName();
  if (!mMF) return false;
  signedNames += mMF->getFunction()->getName();
  const GlobalVariable *GV =
    mMF->getFunction()->getParent()->getGlobalVariable(signedNames);
  if (!GV || !GV->hasInitializer()) return false;
  const ConstantArray *CA = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!CA) return false;
  for (uint32_t start = 0, stop = CA->getNumOperands(); start < stop;
       ++start) {
    const ConstantExpr *nameField = dyn_cast<ConstantExpr>(CA->getOperand(start));
    if (!nameField) continue;

    const GlobalVariable *nameGV =
      dyn_cast<GlobalVariable>(nameField->getOperand(0));
    if (!nameGV || !nameGV->hasInitializer()) continue;

    const ConstantDataArray *nameArray =
      dyn_cast<ConstantDataArray>(nameGV->getInitializer());
    if (!nameArray) continue;

    std::string nameStr = nameArray->getAsString();
    // We don't want to include the newline
    if (!nameStr.compare(0, nameStr.length()-1, argName)) return true;
  }
  return false;
}
bool
AMDILMachineFunctionInfo::isVolatilePointer(const Value* ptr)
{
  if (!mSTM->supportMetadata30()) return false;
  std::string signedNames = "llvm.volatilepointer.annotations.";
  std::string argName = ptr->getName();
  if (!mMF) return false;
  signedNames += mMF->getFunction()->getName();
  const GlobalVariable *GV =
    mMF->getFunction()->getParent()->getGlobalVariable(signedNames);
  if (!GV || !GV->hasInitializer()) return false;
  const ConstantArray *CA = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!CA) return false;
  for (uint32_t start = 0, stop = CA->getNumOperands(); start < stop;
       ++start) {
    const ConstantExpr *nameField = dyn_cast<ConstantExpr>(CA->getOperand(start));
    if (!nameField) continue;

    const GlobalVariable *nameGV =
      dyn_cast<GlobalVariable>(nameField->getOperand(0));
    if (!nameGV || !nameGV->hasInitializer()) continue;

    const ConstantDataArray *nameArray =
      dyn_cast<ConstantDataArray>(nameGV->getInitializer());
    if (!nameArray) continue;

    std::string nameStr = nameArray->getAsString();
    // We don't want to include the newline
    if (!nameStr.compare(0, nameStr.length()-1, argName)) return true;
  }
  return false;
}
bool
AMDILMachineFunctionInfo::isRestrictPointer(const Value* ptr)
{
  if (!mSTM->supportMetadata30()) return false;
  std::string signedNames = "llvm.restrictpointer.annotations.";
  std::string argName = ptr->getName();
  if (!mMF) return false;
  signedNames += mMF->getFunction()->getName();
  const GlobalVariable *GV =
    mMF->getFunction()->getParent()->getGlobalVariable(signedNames);
  if (!GV || !GV->hasInitializer()) return false;
  const ConstantArray *CA = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!CA) return false;
  for (uint32_t start = 0, stop = CA->getNumOperands(); start < stop;
       ++start) {
    const ConstantExpr *nameField = dyn_cast<ConstantExpr>(CA->getOperand(start));
    if (!nameField) continue;

    const GlobalVariable *nameGV =
      dyn_cast<GlobalVariable>(nameField->getOperand(0));
    if (!nameGV || !nameGV->hasInitializer()) continue;

    const ConstantDataArray *nameArray =
      dyn_cast<ConstantDataArray>(nameGV->getInitializer());
    if (!nameArray) continue;

    std::string nameStr = nameArray->getAsString();
    // We don't want to include the newline
    if (!nameStr.compare(0, nameStr.length()-1, argName)) return true;
  }
  return false;
}
bool
AMDILMachineFunctionInfo::isConstantArgument(const Value* ptr)
{
  if (!mSTM->supportMetadata30()) return false;
  std::string signedNames = "llvm.argtypeconst.annotations.";
  std::string argName = ptr->getName();
  if (!mMF) return false;
  signedNames += mMF->getFunction()->getName();
  const GlobalVariable *GV =
    mMF->getFunction()->getParent()->getGlobalVariable(signedNames);
  if (!GV || !GV->hasInitializer()) return false;
  const ConstantArray *CA = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!CA) return false;
  for (uint32_t start = 0, stop = CA->getNumOperands(); start < stop;
       ++start) {
    const ConstantExpr *nameField = dyn_cast<ConstantExpr>(CA->getOperand(start));
    if (!nameField) continue;

    const GlobalVariable *nameGV =
      dyn_cast<GlobalVariable>(nameField->getOperand(0));
    if (!nameGV || !nameGV->hasInitializer()) continue;

    const ConstantDataArray *nameArray =
      dyn_cast<ConstantDataArray>(nameGV->getInitializer());
    if (!nameArray) continue;

    std::string nameStr = nameArray->getAsString();
    // We don't want to include the newline
    if (!nameStr.compare(0, nameStr.length()-1, argName)) return true;
  }
  return false;
}
