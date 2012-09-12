//===-- AMDILKernelManager.cpp --------------------------------------------===//
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

#include "AMDILKernelManager.h"
#include "AMDILAlgorithms.tpp"
#include "AMDILAsmPrinter.h"
#include "AMDILDeviceInfo.h"
#include "AMDILDevices.h"
#include "AMDILCompilerErrors.h"
#include "AMDILKernel.h"
#include "AMDILMachineFunctionInfo.h"
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
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/MathExtras.h"
#include <cstdio>
#include <ostream>
#include <algorithm>
#include <string>
#include <queue>
#include <list>
#include <utility>
using namespace llvm;
#define NUM_EXTRA_SLOTS_PER_IMAGE 1

void
printRegName(AMDILAsmPrinter *RegNames,
             unsigned reg,
             OSTREAM_TYPE &O,
             bool dst,
             bool dupe = false)
{
  if (reg >= AMDIL::Rx1 && reg < AMDIL::Rxy1) {
    O << RegNames->getRegisterName(reg) << ".x,";
  } else if (reg >= AMDIL::Ry1 && reg < AMDIL::Rz1) {
    O << RegNames->getRegisterName(reg) << ".y,";
  } else if (reg >= AMDIL::Rz1 && reg < AMDIL::Rzw1) {
    O << RegNames->getRegisterName(reg) << ".z,";
  } else if (reg >= AMDIL::Rw1 && reg < AMDIL::Rx1) {
    O << RegNames->getRegisterName(reg) << ".w,";
  } else if (reg >= AMDIL::Rxy1 && reg < AMDIL::Ry1) {
    O << RegNames->getRegisterName(reg) << ((dst) ? ".xy__," :
                                            (dupe ? ".xyxy," : ".xy00,"));
  } else if (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP) {
    O << RegNames->getRegisterName(reg) << ((dst) ? ".__zw," :
                                            (dupe ? ".zwzw," : ".00zw,"));
  } else {
    O << RegNames->getRegisterName(reg) << ",";
  }
}
const char*
getFirstComponent(unsigned reg, unsigned fcall)
{
  if (reg >= AMDIL::Rx1 && reg < AMDIL::Rxy1) {
    return ".x";
  } else if (reg >= AMDIL::Ry1 && reg < AMDIL::Rz1) {
    return ".x";
  } else if (reg >= AMDIL::Rz1 && reg < AMDIL::Rzw1) {
    return ".x";
  } else if (reg >= AMDIL::Rw1 && reg < AMDIL::Rx1) {
    return ".x";
  } else if (reg >= AMDIL::Rxy1 && reg < AMDIL::Ry1) {
    switch (fcall) {
    case 1090:
    case 1091:
    case 1092:
      return ".xx";
    default:
      return ".xy";
    };
  } else if (reg >= AMDIL::Rzw1 && reg < AMDIL::SDP) {
    switch (fcall) {
    case 1090:
    case 1091:
    case 1092:
      return ".00xx";
    default:
      return ".00xy";
    };
  } else {
    switch (fcall) {
    case 1090:
    case 1091:
      return ".xxxx";
    case 1092:
    case 1093:
      return ".xxyy";
    default:
      return ".xyzw";
    };
  }
}
static bool errorPrint(const char *ptr, OSTREAM_TYPE &O) {
  if (ptr[0] == 'E') {
    O << ";error:" << ptr << "\n";
  } else {
    O << ";warning:" << ptr << "\n";
  }
  return false;
}
static bool semaPrint(uint32_t val, OSTREAM_TYPE &O) {
  O << "dcl_semaphore_id(" << val << ")\n";
  return false;
}
static bool arenaPrint(uint32_t val, OSTREAM_TYPE &O) {
  if (val >= ARENA_SEGMENT_RESERVED_UAVS) {
    O << "dcl_arena_uav_id(" << val << ")\n";
  }
  return false;
}
static bool uavPrint(uint32_t val, OSTREAM_TYPE &O) {
  if (val < 8 || val == 11) {
    O << "dcl_raw_uav_id(" << val << ")\n";
  }
  return false;
}
static bool uavPrintSI(uint32_t val, OSTREAM_TYPE &O) {
  O << "dcl_typeless_uav_id(" << val <<
  ")_stride(4)_length(4)_access(read_write)\n";
  return false;
}
static bool
printfPrint(std::pair<const std::string,
                      PrintfInfo *> &data, OSTREAM_TYPE &O) {
  O << ";printf_fmt:" << data.second->getPrintfID();
  // Number of operands
  O << ":" << data.second->getNumOperands();
  // Size of each operand
  for (size_t i = 0, e = data.second->getNumOperands(); i < e; ++i) {
    O << ":" << (data.second->getOperandID(i) >> 3);
  }
  const char *ptr = data.first.c_str();
  uint32_t size = data.first.size() - 1;
  // The format string size
  O << ":" << size << ":";
  for (size_t i = 0; i < size; ++i) {
    if (ptr[i] == '\r') {
      O << "\\r";
    } else if (ptr[i] == '\n') {
      O << "\\n";
    } else {
      O << ptr[i];
    }
  }
  O << ";\n";   // c_str() is cheap way to trim
  return false;
}
void AMDILKernelManager::updatePtrArg(Function::const_arg_iterator Ip,
                                      int numWriteImages, int raw_uav_buffer,
                                      int counter, bool isKernel,
                                      const Function *F) {
  assert(F && "Cannot pass a NULL Pointer to F!");
  assert(Ip->getType()->isPointerTy() &&
         "Argument must be a pointer to be passed into this function!\n");
  std::string ptrArg(";pointer:");
  const char *symTab = "NoSymTab";
  uint32_t ptrID = getUAVID(Ip);
  PointerType *PT = cast<PointerType>(Ip->getType());
  uint32_t Align = 4;
  const char *MemType = "uav";
  if (PT->getElementType()->isSized()) {
    Align = mTM->getTargetData()->getTypeAllocSize(PT->getElementType());
    if ((Align & (Align - 1))) Align = NextPowerOf2(Align);
  }
  ptrArg += Ip->getName().str() + ":" + getTypeName(PT, symTab, mMFI,
                                                    mMFI->isSignedIntType(Ip))
            + ":1:1:" +
            itostr(counter * 16) + ":";
  if (mSTM->overridesFlatAS()) {
    MemType = "flat";
    ptrID = 0;
  } else {
    switch (PT->getAddressSpace()) {
    case AMDILAS::FLAT_ADDRESS:
      if (!mSTM->device()->isSupported(AMDILDeviceInfo::FlatMem)) {
        mMFI->addErrorMsg(amd::CompilerErrorMessage[NO_FLAT_SUPPORT]);
      }
      MemType = "flat";
      ptrID = 0;
      break;
    case AMDILAS::ADDRESS_NONE:
      //O << "No Address space qualifier!";
      mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
      assert(1);
      break;
    case AMDILAS::GLOBAL_ADDRESS:
      if (mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment)) {
        if (ptrID >= ARENA_SEGMENT_RESERVED_UAVS) {
          ptrID = 8;
        }
      }
      mMFI->uav_insert(ptrID);
      break;
    case AMDILAS::CONSTANT_ADDRESS: {
      if (isKernel &&
          mSTM->device()->usesHardware(AMDILDeviceInfo::ConstantMem)) {
        const AMDILKernel* t = mAMI->getKernel(F->getName());
        if (mAMI->usesHWConstant(t, Ip->getName())) {
          MemType = /*(isSI) ? "uc\0" :*/ "hc\0";
          ptrID = mAMI->getConstPtrCB(t, Ip->getName());
          mMFI->setUsesConstant();
        } else {
          MemType = "c\0";
          mMFI->uav_insert(ptrID);
        }
      } else {
        MemType = "c\0";
        mMFI->uav_insert(ptrID);
      }
      break;
    }
    default:
    case AMDILAS::PRIVATE_ADDRESS:
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem)) {
        MemType = (mSTM->device()->isSupported(AMDILDeviceInfo::PrivateUAV))
                  ? "up\0" : "hp\0";
        mMFI->setUsesScratch();
      } else {
        MemType = "p\0";
        mMFI->uav_insert(ptrID);
      }
      break;
    case AMDILAS::REGION_ADDRESS:
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem)) {
        MemType = "hr\0";
        ptrID = 0;
        mMFI->setUsesGDS();
      } else {
        MemType = "r\0";
        mMFI->uav_insert(ptrID);
      }
      break;
    case AMDILAS::LOCAL_ADDRESS:
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem)) {
        MemType = "hl\0";
        // size of local mem pointed to by ptr type args are unknown,
        // so go to default lds buffer
        ptrID = DEFAULT_LDS_ID;
        mMFI->setUsesLDS();
      } else {
        MemType = "l\0";
        mMFI->uav_insert(ptrID);
      }
      break;
    };
  }
  ptrArg += std::string(MemType) + ":";
  ptrArg += itostr(ptrID) + ":";
  ptrArg += itostr(Align) + ":";
  const Value* ptr = Ip;
  if (mMFI->read_ptr_count(ptr)) {
    ptrArg += "RO";
    // FIXME: add write-only pointer detection.
    //} else if (mMFI->write_ptr_count(ptr)) {
    //  ptrArg += "WO";
  } else {
    ptrArg += "RW";
  }
  ptrArg += (mMFI->isVolatilePointer(Ip)) ? ":1" : ":0";
  ptrArg += (mMFI->isRestrictPointer(Ip)) ? ":1" : ":0";
  mMFI->addMetadata(ptrArg, true);
}
AMDILKernelManager::AMDILKernelManager(AMDILTargetMachine *TM)
{
  mTM = TM;
  mSTM = mTM->getSubtargetImpl();
  mMFI = NULL;
  mAMI = NULL;
  mMF = NULL;
  clear();
}
AMDILKernelManager::~AMDILKernelManager() {
  clear();
}
void
AMDILKernelManager::setMF(MachineFunction *MF)
{
  mMF = MF;
  mMFI = MF->getInfo<AMDILMachineFunctionInfo>();
  mAMI = &(MF->getMMI().getObjFileInfo<AMDILModuleInfo>());
}
void AMDILKernelManager::clear() {
  mUniqueID = 0;
  mWasKernel = false;
  mHasImageWrite = false;
  mHasOutputInst = false;
}
bool AMDILKernelManager::useCompilerWrite(const MachineInstr *MI) {
  return (MI->getOpcode() == AMDIL::RETURN && wasKernel() && !mHasImageWrite
          && !mHasOutputInst);
}
void AMDILKernelManager::processArgMetadata(OSTREAM_TYPE &O,
                                            uint32_t buf,
                                            bool isKernel)
{
  const Function *F = mMF->getFunction();
  const char * symTab = "NoSymTab";
  Function::const_arg_iterator Ip = F->arg_begin();
  Function::const_arg_iterator Ep = F->arg_end();

  if (F->hasStructRetAttr()) {
    assert(Ip != Ep && "Invalid struct return fucntion!");
    ++Ip;
  }
  uint32_t mCBSize = 0;
  int raw_uav_buffer = mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID);
  bool MultiUAV = mSTM->device()->isSupported(AMDILDeviceInfo::MultiUAV);
  bool ArenaSegment =
    mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment);
  int numWriteImages = mMFI->get_num_write_images();
  if (numWriteImages == OPENCL_MAX_WRITE_IMAGES || MultiUAV || ArenaSegment) {
    if (mSTM->device()->getGeneration() <= AMDILDeviceInfo::HD6XXX) {
      raw_uav_buffer = mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID);
    }
  }
  uint32_t CounterNum = 0;
  uint32_t SemaNum = 0;
  uint32_t ROArg = 0;
  uint32_t WOArg = 0;
  uint32_t NumArg = 0;
  while (Ip != Ep) {
    Type *cType = Ip->getType();
    if (cType->isIntOrIntVectorTy() || cType->isFPOrFPVectorTy()) {
      std::string argMeta(";value:");
      argMeta += Ip->getName().str() + ":" + getTypeName(cType,
                                                         symTab,
                                                         mMFI
                                                         ,
                                                         mMFI->isSignedIntType(
                                                           Ip)) + ":";
      int bitsize = cType->getPrimitiveSizeInBits();
      int numEle = 1;
      if (cType->getTypeID() == Type::VectorTyID) {
        numEle = cast<VectorType>(cType)->getNumElements();
      }
      argMeta += itostr(numEle) + ":1:" + itostr(mCBSize << 4);
      mMFI->addMetadata(argMeta, true);

      // FIXME: simplify
      if ((bitsize / numEle) < 32) {
        bitsize = numEle >> 2;
      } else {
        bitsize >>= 7;
      }
      if (!bitsize) {
        bitsize = 1;
      }

      mCBSize += bitsize;
    } else if (const PointerType *PT = dyn_cast<PointerType>(cType)) {
      Type *CT = PT->getElementType();
      const StructType *ST = dyn_cast<StructType>(CT);
      if (ST && ST->isOpaque()) {
        StringRef name = ST->getName();
        bool i1d  = name.startswith( "struct._image1d_t" );
        bool i1da = name.startswith( "struct._image1d_array_t" );
        bool i1db = name.startswith( "struct._image1d_buffer_t" );
        bool i2d  = name.startswith( "struct._image2d_t" );
        bool i2da = name.startswith( "struct._image2d_array_t" );
        bool i3d  = name.startswith( "struct._image3d_t" );
        bool c32  = name.startswith( "struct._counter32_t" );
        bool c64  = name.startswith( "struct._counter64_t" );
        bool sema = name.startswith( "struct._sema_t" );
        if (i1d || i1da || i1db || i2d | i2da || i3d) {
          if (mSTM->device()->isSupported(AMDILDeviceInfo::Images)) {
            std::string imageArg(";image:");
            imageArg += Ip->getName().str() + ":";
            if (i1d) imageArg += "1D:";
            else if (i1da) imageArg += "1DA:";
            else if (i1db) imageArg += "1DB:";
            else if (i2d) imageArg += "2D:";
            else if (i2da) imageArg += "2DA:";
            else if (i3d) imageArg += "3D:";

            if (isKernel) {
              if (mAMI->isReadOnlyImage (mMF->getFunction()->getName(),
                                         (ROArg + WOArg))) {
                imageArg += "RO:" + itostr(ROArg);
                O << "dcl_resource_id(" << ROArg << ")_type(";
                if (i1d) O << "1d";
                else if (i1da) O << "1darray";
                else if (i1db) O << "buffer";
                else if (i2d) O << "2d";
                else if (i2da) O << "2darray";
                else if (i3d) O << "3d";
                O << ")_fmtx(unknown)_fmty(unknown)"
                  << "_fmtz(unknown)_fmtw(unknown)\n";
                ++ROArg;
              } else if (mAMI->isWriteOnlyImage(mMF->getFunction()->getName(),
                                                (ROArg + WOArg))) {
                uint32_t offset = 0;
                offset += WOArg;
                imageArg += "WO:" + itostr(offset & 0x7);
                O << "dcl_uav_id(" << ((offset) & 0x7) << ")_type(";
                if (i1d) O << "1d";
                else if (i1da) O << "1darray";
                else if (i1db) O << "buffer";
                else if (i2d) O << "2d";
                else if (i2da) O << "2darray";
                else if (i3d) O << "3d";
                O << ")_fmtx(uint)\n";
                ++WOArg;
              } else {
                imageArg += "RW:" + itostr(ROArg + WOArg);
              }
            }
            imageArg += ":1:" + itostr(mCBSize * 16);
            mMFI->addMetadata(imageArg, true);
            mMFI->addi32Literal(mCBSize);
            mCBSize += NUM_EXTRA_SLOTS_PER_IMAGE + 1;
          } else {
            mMFI->addErrorMsg(amd::CompilerErrorMessage[NO_IMAGE_SUPPORT]);
          }
        } else if (c32 || c64) {
          std::string counterArg(";counter:");
          counterArg += Ip->getName().str() + ":"
                        + itostr(c32 ? 32 : 64) + ":"
                        + itostr(CounterNum++) + ":1:" + itostr(mCBSize * 16);
          mMFI->addMetadata(counterArg, true);
          ++mCBSize;
        } else if (sema) {
          std::string semaArg(";sema:");
          semaArg += Ip->getName().str() + ":" + itostr(SemaNum++)
                     + ":1:" + itostr(mCBSize * 16);
          mMFI->addMetadata(semaArg, true);
          ++mCBSize;
        } else {
          updatePtrArg(Ip, numWriteImages, raw_uav_buffer, mCBSize, isKernel,
                       F);
          ++mCBSize;
        }
      }
      else if (CT->getTypeID() == Type::StructTyID
               && PT->getAddressSpace() == AMDILAS::PRIVATE_ADDRESS) {
        const TargetData *td = mTM->getTargetData();
        const StructLayout *sl = td->getStructLayout(dyn_cast<StructType>(CT));
        int bytesize = sl->getSizeInBytes();
        int reservedsize = (bytesize + 15) & ~15;
        int numSlots = reservedsize >> 4;
        if (!numSlots) {
          numSlots = 1;
        }
        std::string structArg(";value:");
        structArg += Ip->getName().str() + ":struct:"
                     + itostr(bytesize) + ":1:" + itostr(mCBSize * 16);
        mMFI->addMetadata(structArg, true);
        mCBSize += numSlots;
      } else if (CT->isIntOrIntVectorTy()
                 || CT->isFPOrFPVectorTy()
                 || CT->getTypeID() == Type::ArrayTyID
                 || CT->getTypeID() == Type::PointerTyID
                 || PT->getAddressSpace() != AMDILAS::PRIVATE_ADDRESS) {
        updatePtrArg(Ip, numWriteImages, raw_uav_buffer, mCBSize, isKernel, F);
        ++mCBSize;
      } else {
        assert(0 && "Cannot process current pointer argument");
        mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
      }
    } else {
      assert(0 && "Cannot process current kernel argument");
      mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
    }
    if (mMFI->isConstantArgument(Ip)) {
      std::string constArg(";constarg:");
      constArg += itostr(NumArg) + ":" + Ip->getName().str();
      mMFI->addMetadata(constArg, true);
    }
    ++NumArg;
    ++Ip;
  }
}
void AMDILKernelManager::printHeader(AMDILAsmPrinter *AsmPrinter,
                                     OSTREAM_TYPE &O,
                                     const std::string &name) {
  mName = name;
  std::string kernelName;
  kernelName = (mSTM->isApple()) ? "__OpenCL_" + name + "_kernel"
               :  name;
  int kernelId = mAMI->getOrCreateFunctionID(kernelName);
  O << "func " << kernelId << " ; " << kernelName << "\n";
  if (mSTM->is64bit()) {
    O << "mov " << AsmPrinter->getRegisterName(AMDIL::SDP) << ", cb0[8].xy\n";
  } else {
    O << "mov " << AsmPrinter->getRegisterName(AMDIL::SDP) << ", cb0[8].x\n";
  }
  O << "mov " << AsmPrinter->getRegisterName(AMDIL::SP) << ", l1.0000\n";
}
void AMDILKernelManager::printGroupSize(OSTREAM_TYPE& O) {
  // The HD4XXX generation of hardware does not support a 3D launch, so we need
  // to use dcl_num_thread_per_group to specify the launch size. If the launch
  // size is specified via a kernel attribute, we print it here. Otherwise we
  // use the the default size.
  const AMDILKernel *kernel = mAMI->getKernel(mName);
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    if (kernel && kernel->sgv
        && (kernel->sgv->mHasRWG
            || !mMFI->usesLDS())) {
      // if the user has specified what the required workgroup size is then we
      // need to compile for that size and that size only.  Otherwise we compile
      // for the max workgroup size that is passed in as an option to the
      // backend.
      O << "dcl_num_thread_per_group "
        << kernel->sgv->reqGroupSize[0] << ", "
        << kernel->sgv->reqGroupSize[1] << ", "
        << kernel->sgv->reqGroupSize[2] << "\n";
    } else {
      // If the kernel uses local memory, then the kernel is being
      // compiled in single wavefront mode. So we have to generate code slightly
      // different.
      O << "dcl_num_thread_per_group "
        << mSTM->device()->getWavefrontSize()
        << ", 1, 1\n";
    }
  } else {
    // Otherwise we generate for devices that support 3D launch natively.  If
    // the reqd_workgroup_size attribute was specified, then we can specify the
    // exact launch dimensions.
    if (kernel && kernel->sgv) {
      if (kernel->sgv->mHasRWG) {
        O << "dcl_num_thread_per_group "
          << kernel->sgv->reqGroupSize[0] << ", "
          << kernel->sgv->reqGroupSize[1] << ", "
          << kernel->sgv->reqGroupSize[2] << "\n";
      } else {
        // Otherwise we specify the largest workgroup size that can be launched.
        O << "dcl_max_thread_per_group " <<
        kernel->sgv->reqGroupSize[0]
        * kernel->sgv->reqGroupSize[1]
        * kernel->sgv->reqGroupSize[2] << "\n";
      }

      if (kernel->sgv->mHasRWR) {
        O << "dcl_gws_thread_count " <<
        kernel->sgv->reqRegionSize[0]
        * kernel->sgv->reqRegionSize[1]
        * kernel->sgv->reqRegionSize[2] << "\n";
      }
    } else {
      O << "dcl_max_thread_per_group " << mSTM->device()->getWavefrontSize() <<
      "\n";
    }
  }
  // Now that we have specified the workgroup size, lets declare the local
  // memory size. If we are using hardware and we know the value at compile
  // time, then we need to declare the correct value. Otherwise we should just
  // declare the maximum size.
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem)
      && mMFI->usesLDS()) {
    size_t kernelLocalSize = (kernel->curHWSize + 3) & ~3;
    if (kernelLocalSize > mSTM->device()->getMaxLDSSize()) {
      mMFI->addErrorMsg(amd::CompilerErrorMessage[INSUFFICIENT_LOCAL_RESOURCES]);
    }
    // declare non-default local buffers
    unsigned nLocals = mAMI->numLocalBuffers();
    std::vector<unsigned> localBufferSizes(nLocals, 0);
    AMDILLocalArg* locals = kernel->lvgv;
    llvm::SmallVector<AMDILArrayMem *, DEFAULT_VEC_SLOTS>::iterator ib, ie;
    for (ib = locals->local.begin(), ie = locals->local.end(); ib != ie;
         ++ib) {
      AMDILArrayMem* local = *ib;
      if (!local->isHW || local->isRegion) {
        continue;
      }
      assert(local->resourceID != 0 && "bad resourceID");
      uint32_t size = (local->vecSize + 3) & ~3;
      localBufferSizes[local->resourceID-DEFAULT_LDS_ID] += size;
    }
    unsigned nDefSize = 0;
    for (unsigned i = 1; i < nLocals; ++i) {
      unsigned size = localBufferSizes[i];
      if (size > 0) {
        O << "dcl_lds_id(" << DEFAULT_LDS_ID + i << ") " << size << "\n";
        nDefSize += size;
      }
    }
    // If there is a local pointer as a kernel argument, we don't know the size
    // at compile time, so we reserve all of the space.
    unsigned defLocalSize = localBufferSizes[0];
    if (mMFI->hasLDSArg() || !kernelLocalSize) {
      defLocalSize = mSTM->device()->getMaxLDSSize() - nDefSize;
    }
    // decalre the default local buffer
    if (defLocalSize > 0) {
      O << "dcl_lds_id(" << DEFAULT_LDS_ID << ") " << defLocalSize << "\n";
    }
    mMFI->setUsesMem(AMDILDevice::LDS_ID);
  }
  // If the device supports the region memory extension, which maps to our
  // hardware GDS memory, then lets declare it so we can use it later on.
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem)) {
    size_t kernelGDSSize = (kernel->curHWRSize + 3) & ~3;
    if (kernelGDSSize > mSTM->device()->getMaxGDSSize()) {
      mMFI->addErrorMsg(
        amd::CompilerErrorMessage[INSUFFICIENT_REGION_RESOURCES]);
    }
    // If there is a region pointer as a kernel argument, we don't know the size
    // at compile time, so we reserved all of the space.
    if (mMFI->usesGDS() && (mMFI->hasGDSArg() || !kernelGDSSize)) {
      O << "dcl_gds_id(" << DEFAULT_GDS_ID <<
      ") " << mSTM->device()->getMaxGDSSize() << "\n";
      mMFI->setUsesMem(AMDILDevice::GDS_ID);
    } else if (kernelGDSSize) {
      // We know the size, so lets declare it.
      O << "dcl_gds_id(" << DEFAULT_GDS_ID <<
      ") " << kernelGDSSize << "\n";
      mMFI->setUsesMem(AMDILDevice::GDS_ID);
    }
  }
}
void
AMDILKernelManager::printDecls(AMDILAsmPrinter *AsmPrinter, OSTREAM_TYPE &O) {
  // If we are a HD4XXX generation device, then we only support a single uav
  // surface, so we declare it and leave
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD4XXX) {
    O << "dcl_raw_uav_id("
      << mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID)
      << ")\n";
    mMFI->setUsesMem(AMDILDevice::RAW_UAV_ID);
    getIntrinsicSetup(AsmPrinter, O);
    return;
  }
  // If we are supporting multiple uav's view the MultiUAV capability, then we
  // need to print out the declarations here. MultiUAV conflicts with write
  // images, so they only use 8 - NumWriteImages uav's. Therefor only pointers
  // with ID's < 8 will get printed.
  if (mSTM->device()->isSupported(AMDILDeviceInfo::MultiUAV)) {
    binaryForEach(mMFI->uav_begin(), mMFI->uav_end(), uavPrint, O);
    mMFI->setUsesMem(AMDILDevice::RAW_UAV_ID);
  }
  // If arena segments are supported, then we should emit them now.  Arena
  // segments are similiar to MultiUAV, except ArenaSegments are virtual and up
  // to 1024 of them can coexist. These are more compiler hints for CAL and thus
  // cannot overlap in any form.  Each ID maps to a seperate piece of memory and
  // CAL determines whether the load/stores should go to the fast path/slow path
  // based on the usage and instruction.
  if (mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment)) {
    binaryForEach(mMFI->uav_begin(), mMFI->uav_end(), arenaPrint, O);
  }

  if (mMFI->sema_size() &&
      !mSTM->device()->usesHardware(AMDILDeviceInfo::Semaphore)) {
    mMFI->addErrorMsg(amd::CompilerErrorMessage[NO_SEMAPHORE_SUPPORT]);
  } else {
    binaryForEach(mMFI->sema_begin(), mMFI->sema_end(), semaPrint, O);
  }
  // Now that we have printed out all of the arena and multi uav declaration,
  // now we must print out the default raw uav id. This always exists on HD5XXX
  // and HD6XXX hardware. The reason is that the hardware supports 12 UAV's and
  // 11 are taken up by MultiUAV/Write Images and Arena.  However, if we do not
  // have UAV 11 as the raw UAV and there are 8 write images, we must revert
  // everything to the arena and not print out the default raw uav id.
  if (mSTM->device()->getGeneration() == AMDILDeviceInfo::HD5XXX
      || mSTM->device()->getGeneration() == AMDILDeviceInfo::HD6XXX) {
    if ((mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID) < 11 &&
         mMFI->get_num_write_images()
         != OPENCL_MAX_WRITE_IMAGES
         && !mSTM->device()->isSupported(AMDILDeviceInfo::MultiUAV))
        || mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID) == 11) {
      if (!mMFI->usesMem(AMDILDevice::RAW_UAV_ID)
          && mMFI->uav_count(mSTM->device()->
                             getResourceID(AMDILDevice::RAW_UAV_ID))) {
        O << "dcl_raw_uav_id("
          << mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID);
        O << ")\n";
        mMFI->setUsesMem(AMDILDevice::RAW_UAV_ID);
      }
    }
    // If we have not printed out the arena ID yet, then do so here.
    if (!mMFI->usesMem(AMDILDevice::ARENA_UAV_ID)
        && mSTM->device()->usesHardware(AMDILDeviceInfo::ArenaUAV)) {
      O << "dcl_arena_uav_id("
        << mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID) << ")\n";
      mMFI->setUsesMem(AMDILDevice::ARENA_UAV_ID);
    }
  } else if (mSTM->device()->getGeneration() > AMDILDeviceInfo::HD6XXX
             && !mSTM->overridesFlatAS()) {
    binaryForEach(mMFI->uav_begin(), mMFI->uav_end(), uavPrintSI, O);
    mMFI->setUsesMem(AMDILDevice::RAW_UAV_ID);
  }
  getIntrinsicSetup(AsmPrinter, O);
}
void AMDILKernelManager::getIntrinsicSetup(AMDILAsmPrinter *AsmPrinter,
                                           OSTREAM_TYPE &O)
{
  O << "mov r0.__z_, vThreadGrpIdFlat0.x\n"
    << "mov r1022.xyz0, vTidInGrp0.xyz\n";
  if (mSTM->device()->getGeneration() > AMDILDeviceInfo::HD4XXX) {
    O << "mov r1023.xyz0, vThreadGrpId0.xyz\n";
  } else {
    O << "imul r0.___w, cb0[2].x, cb0[2].y\n"
    // Calculates the local id.
    // Calculates the group id.
    << "umod r1023.x___, r0.z, cb0[2].x\n"
    << "udiv r1023._y__, r0.z, cb0[2].x\n"
    << "umod r1023._y__, r1023.y, cb0[2].y\n"
    << "udiv r1023.__z_, r0.z, r0.w\n";
  }
  // Calculates the global id.
  const AMDILKernel *kernel = mAMI->getKernel(mName);
  if (kernel && kernel->sgv && kernel->sgv->mHasRWG) {
    // Anytime we declare a literal, we need to reserve it, if it is not emitted
    // in emitLiterals.
    O << "dcl_literal l9, "
      << kernel->sgv->reqGroupSize[0] << ", "
      << kernel->sgv->reqGroupSize[1] << ", "
      << kernel->sgv->reqGroupSize[2] << ", "
      << "0xFFFFFFFF\n";
    O << "imad r1021.xyz0, r1023.xyzz, l9.xyzz, r1022.xyzz\n";
  } else {
    O << "dcl_literal l9, "
      << mSTM->getDefaultSize(0) << ", "
      << mSTM->getDefaultSize(1) << ", "
      << mSTM->getDefaultSize(2) << ", "
      << "0xFFFFFFFF\n";
    // This umax is added so that on SI or later architectures, the
    // ISA generator can do value range analysis to determine that cb0[1]
    // is a positive value or not.
    if (mSTM->device()->getGeneration() > AMDILDeviceInfo::HD6XXX) {
      O << "umin r1023.xyz0, r1023.xyzz, l9.w\n";
      O << "umin r1021.xyz0, cb0[1].xyzz, l2.x\n";
      O << "imad r1021.xyz0, r1023.xyzz, r1021.xyzz, r1022.xyzz\n";
    } else {
      O << "imad r1021.xyz0, r1023.xyzz, cb0[1].xyzz, r1022.xyzz\n";
    }
  }

  // These umax's are added so that on SI or later architectures, the
  // ISA generator can do value range analysis to determine that cb0[1]
  // is a positive value or not.
  // Add the global/group offset for multi-launch support.
  if (mSTM->device()->getGeneration() > AMDILDeviceInfo::HD6XXX) {
    O << "umin r1024.xyz0, cb0[6].xyzz, l9.w\n"
      << "iadd r1021.xyz0, r1021.xyz0, r1024.xyz0\n"
      << "umin r1024.xyz0, cb0[7].xyzz, l9.w\n"
      << "iadd r1023.xyz0, r1023.xyz0, r1024.xyz0\n";
  } else {
    O << "iadd r1021.xyz0, r1021.xyz0, cb0[6].xyz0\n"
      << "iadd r1023.xyz0, r1023.xyz0, cb0[7].xyz0\n";
  }
  // moves the flat group id.
  O << "mov r1023.___w, r0.z\n";
  if (mSTM->device()->usesSoftware(AMDILDeviceInfo::LocalMem)) {
    if (mSTM->is64bit()) {
      O << "umul " << AsmPrinter->getRegisterName(AMDIL::T2)
        << ".x0__, r1023.w, cb0[4].z\n"
        << "i64add " << AsmPrinter->getRegisterName(AMDIL::T2)
        << ".xy__, " << AsmPrinter->getRegisterName(AMDIL::T2)
        << ".xyyy, cb0[4].xyyy\n";
    } else {
      O << "imad " << AsmPrinter->getRegisterName(AMDIL::T2)
        << ".x___, r1023.w, cb0[4].y, cb0[4].x\n";
    }
  }
  // Shift the flat group id to be in bytes instead of dwords.
  O << "ishl r1023.___w, r1023.w, l0.z\n";
  if (mSTM->device()->usesSoftware(AMDILDeviceInfo::PrivateMem)) {
    if (mSTM->is64bit()) {
      O << "umul " << AsmPrinter->getRegisterName(AMDIL::T1)
        << ".x0__, vAbsTidFlat.x, cb0[3].z\n"
        << "i64add " << AsmPrinter->getRegisterName(AMDIL::T1)
        << ".xy__, " << AsmPrinter->getRegisterName(AMDIL::T1)
        << ".xyyy, cb0[3].xyyy\n";
    } else {
      O << "imad " << AsmPrinter->getRegisterName(AMDIL::T1)
        << ".x___, vAbsTidFlat.x, cb0[3].y, cb0[3].x\n";
    }
  } else {
    O << "mov " << AsmPrinter->getRegisterName(AMDIL::T1) << ".x___, l0.0000\n";
  }
  if (mSTM->device()->isSupported(AMDILDeviceInfo::RegionMem)) {
    O << "udiv r1024.xyz_, r1021.xyzz, cb0[10].xyzz\n";
    if (kernel && kernel->sgv && kernel->sgv->mHasRWR) {
      O << "dcl_literal l10,"
        << kernel->sgv->reqRegionSize[0] << ", "
        << kernel->sgv->reqRegionSize[1] << ", "
        << kernel->sgv->reqRegionSize[2] << ", "
        << "0\n"
        << "imad r1025.xyz0, r1023.xyzz, l10.xyzz, r1022.xyzz\n";
    } else {
      O << "imad r1025.xyz0, r1023.xyzz, cb0[10].xyzz, r1022.xyzz\n";
    }
  }
  if (!mMFI->printf_empty()) {
    O << "mov " << AsmPrinter->getRegisterName(AMDIL::PRINTF) << ".x, l0.y\n";
  }
}
void AMDILKernelManager::printFooter(OSTREAM_TYPE &O) {
  O << "ret\n";
  if (mSTM->isApple()) {
    O << "endfunc ; __OpenCL_" << mName << "_kernel\n";
  } else {
    O << "endfunc ; " << mName << "\n";
  }
}
void
AMDILKernelManager::printMetaData(OSTREAM_TYPE &O, uint32_t id, bool kernel) {
  if (kernel) {
    int kernelId = (mSTM->isApple())
                   ? mAMI->getOrCreateFunctionID(
      "__OpenCL_" + mName + "_kernel")
                   : mAMI->getOrCreateFunctionID(mName);
    mMFI->addCalledFunc(id);
    mUniqueID = kernelId;
    mIsKernel = true;
  }
  printKernelArgs(O);
  if (kernel) {
    mIsKernel = false;
    mMFI->eraseCalledFunc(id);
    mUniqueID = id;
  }
}
void AMDILKernelManager::setKernel(bool kernel) {
  mIsKernel = kernel;
  if (kernel) {
    mWasKernel = mIsKernel;
  }
}
void AMDILKernelManager::setID(uint32_t id)
{
  mUniqueID = id;
}
void AMDILKernelManager::setName(const std::string &name) {
  mName = name;
}
bool AMDILKernelManager::wasKernel() {
  return mWasKernel;
}
void AMDILKernelManager::setImageWrite() {
  mHasImageWrite = true;
}
void AMDILKernelManager::setOutputInst() {
  mHasOutputInst = true;
}
void AMDILKernelManager::printConstantToRegMapping(
  AMDILAsmPrinter *RegNames,
  unsigned &LII,
  OSTREAM_TYPE &O,
  uint32_t &Counter,
  uint32_t Buffer,
  uint32_t n,
  const char *lit,
  uint32_t fcall,
  bool isImage,
  bool isHWCB)
{
  // TODO: This needs to be enabled or SC will never statically index into the
  // CB when a pointer is used.
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::ConstantMem) && isHWCB) {
    O << "mov ";
    printRegName(RegNames, mMFI->getArgReg(LII), O, true);
    O << " l5.x\n";
    ++LII;
    Counter++;
    return;
  }
  for (uint32_t x = 0; x < n; ++x) {
    uint32_t reg = mMFI->getArgReg(LII);
    O << "mov ";
    if (isImage) {
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " l" << mMFI->getLitIdx(Counter++) << "\n";
    } else {
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " cb" <<Buffer<< "[" <<Counter++<< "]"
        << getFirstComponent(mMFI->getArgReg(LII), fcall) << "\n";
    }
    switch(fcall) {
    case 1093:
    case 1092:
      O << "ishr ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, false);
      O << " l3.0y0y\n";
      if (!lit) {
        O << "ishl " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.z\n";
        O << "ishr " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.z\n";
      }
      break;
    case 1091:
      O << "ishr ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, false);
      O << " l3.0zyx\n";
      if (!lit) {
        O << "ishl " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.x\n";
        O << "ishr " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.x\n";
      }
      break;
    case 1090:
      O << "ishr ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, false);
      O << " l3.0z0z\n";
      if (!lit) {
        O << "ishl " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.x\n";
        O << "ishr " << RegNames->getRegisterName(reg) << ", ";
        O << RegNames->getRegisterName(reg)<< ", l3.x\n";
      }
      break;
    default:
      break;
    };
    if (lit) {
      O << "ishl ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, false, true);
      O << " " << lit << "\nishr ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, true);
      O << " ";
      printRegName(RegNames, mMFI->getArgReg(LII), O, false, true);
      O << " " << lit << "\n";
    }
    ++LII;
    if (isImage) {
      Counter += NUM_EXTRA_SLOTS_PER_IMAGE;
    }
  }
}
void
AMDILKernelManager::printCopyStructPrivate(const StructType *ST,
                                           OSTREAM_TYPE &O,
                                           size_t stackSize,
                                           uint32_t Buffer,
                                           uint32_t mLitIdx,
                                           uint32_t &Counter)
{
  size_t n = ((stackSize + 15) & ~15) >> 4;
  for (size_t x = 0; x < n; ++x) {
    if (mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateUAV)) {
      O << "uav_raw_store_id(" <<
      mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID)
        << ") mem0, r0.x, cb" << Buffer << "[" << Counter++ << "]\n";
    } else if (mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem)) {
      O << "ishr r0.y, r0.x, l0.x\n";
      O << "mov x" << mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID)
        <<"[r0.y], cb" << Buffer << "[" << Counter++ << "]\n";
    } else {
      O << "uav_raw_store_id(" <<
      mSTM->device()->getResourceID(AMDILDevice::GLOBAL_ID)
        << ") mem0, r0.x, cb" << Buffer << "[" << Counter++ << "]\n";
    }
    O << "iadd r0.x, r0.x, l" << mLitIdx << ".z\n";
  }
}
void AMDILKernelManager::printKernelArgs(OSTREAM_TYPE &O) {
  std::string version(";version:");
  version += itostr(mSTM->supportMetadata30() ? AMDIL_MAJOR_VERSION : 2) + ":"
             + itostr(AMDIL_MINOR_VERSION) + ":"
             + itostr(mSTM->supportMetadata30()
                      ? AMDIL_REVISION_NUMBER : AMDIL_20_REVISION_NUMBER);
  const AMDILKernel *kernel = mAMI->getKernel(
    (mSTM->isApple() && !mIsKernel)
    ?  "__OpenCL_" + mName + "_kernel" : mName);
  bool isKernel = (kernel) ? kernel->mKernel : false;
  if (mSTM->isApple()) {
    if (isKernel) {
      O << ";ARGSTART:__OpenCL_" <<mName<< "_kernel\n";
    } else {
      O << ";ARGSTART:" <<mName<< "\n";
    }
  } else {
    O << ";ARGSTART:" <<mName<< "\n";
  }
  if (isKernel) {
    O << version << "\n";
    O << ";device:" <<mSTM->getDeviceName() << "\n";
  }
  O << ";uniqueid:" <<mUniqueID<< "\n";

  if (kernel) {
    size_t region = kernel->curRSize;
    size_t hwregion = ((kernel->curHWRSize + 3) & (~0x3));
    bool usehwregion = mSTM->device()->usesHardware(AMDILDeviceInfo::RegionMem);
    if (!mSTM->overridesFlatAS()) {
      size_t local = kernel->curSize;
      size_t hwlocal = ((kernel->curHWSize + 3) & (~0x3));
      bool usehwlocal = mSTM->device()->usesHardware(AMDILDeviceInfo::LocalMem);
      bool usehwprivate = mSTM->device()->usesHardware(
        AMDILDeviceInfo::PrivateMem);
      bool useuavprivate = mSTM->device()->isSupported(
        AMDILDeviceInfo::PrivateUAV);
      if (isKernel) {
        O << ";memory:" << ((usehwprivate) ?
                            (useuavprivate) ? "uav" : "hw" : "" ) << "private:"
          <<(((mMFI->getStackSize() + 15) & (~0xF)))<< "\n";
      }
      O << ";memory:" << ((usehwlocal) ? "hw" : "") << "local:"
        << ((usehwlocal) ? hwlocal : hwlocal + local) << "\n";
    }
    if (mSTM->device()->isSupported(AMDILDeviceInfo::RegionMem)) {
      O << ";memory:" << ((usehwregion) ? "hw" : "") << "region:"
        << ((usehwregion) ? hwregion : hwregion + region) << "\n";
    }

    if (kernel && isKernel && kernel->sgv) {
      if (kernel->sgv->mHasRWG) {
        O << ";cws:"
          << kernel->sgv->reqGroupSize[0] << ":"
          << kernel->sgv->reqGroupSize[1] << ":"
          << kernel->sgv->reqGroupSize[2] << "\n";
      }
      if (kernel->sgv->mHasRWR) {
        O << ";crs:"
          << kernel->sgv->reqRegionSize[0] << ":"
          << kernel->sgv->reqRegionSize[1] << ":"
          << kernel->sgv->reqRegionSize[2] << "\n";
      }
    }
  }
  if (isKernel) {
    for (std::vector<std::string>::iterator ib = mMFI->kernel_md_begin(),
         ie = mMFI->kernel_md_end(); ib != ie; ++ib) {
      O << (*ib) << "\n";
    }
  }
  for (std::set<std::string>::iterator ib = mMFI->func_md_begin(),
       ie = mMFI->func_md_end(); ib != ie; ++ib) {
    O << (*ib) << "\n";
  }
  if (!mMFI->func_empty()) {
    O << ";function:" << mMFI->func_size();
    binaryForEach(mMFI->func_begin(), mMFI->func_end(), commaPrint, O);
    O << "\n";
  }

  if (!mSTM->device()->isSupported(AMDILDeviceInfo::MacroDB)
      && !mMFI->intr_empty()) {
    O << ";intrinsic:" << mMFI->intr_size();
    binaryForEach(mMFI->intr_begin(), mMFI->intr_end(), commaPrint, O);
    O << "\n";
  }

  if (!isKernel) {
    binaryForEach(mMFI->printf_begin(), mMFI->printf_end(), printfPrint, O);
    mMF->getMMI().getObjFileInfo<AMDILModuleInfo>().add_printf_offset(
      mMFI->printf_size());
  } else {
    for (StringMap<SamplerInfo>::iterator
         smb = mMFI->sampler_begin(),
         sme = mMFI->sampler_end(); smb != sme; ++smb) {
      O << ";sampler:" << (*smb).second.name << ":" << (*smb).second.idx
        << ":" << ((*smb).second.val == (uint32_t)-1 ? 0 : 1)
        << ":" << ((*smb).second.val != (uint32_t)-1 ? (*smb).second.val : 0)
        << "\n";
    }
  }
  if (mSTM->is64bit()) {
    O << ";memory:64bitABI\n";
  }

  if (!mMFI->errors_empty()) {
    binaryForEach(mMFI->errors_begin(), mMFI->errors_end(), errorPrint, O);
  }
  // This has to come last
  if (isKernel && !mSTM->overridesFlatAS()) {
    uint32_t id = (mMFI->uav_size() ? *(mMFI->uav_begin()) : 0);
    if (mSTM->device()->getGeneration() <= AMDILDeviceInfo::HD6XXX) {
      if (mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID) >
          mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID)) {
        if (mMFI->uav_size() == 1) {
          if (mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment)
              && *(mMFI->uav_begin()) >= ARENA_SEGMENT_RESERVED_UAVS) {
            id = mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID);
          }
        } else if (mMFI->uav_count(mSTM->device()->
                                   getResourceID(AMDILDevice::RAW_UAV_ID))) {
          id = mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID);
        } else {
          id = mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID);
        }
      } else if ((mMFI->get_num_write_images()) !=
                 OPENCL_MAX_WRITE_IMAGES
                 && !mSTM->device()->isSupported(AMDILDeviceInfo::ArenaSegment)
                 && mMFI->uav_count(mSTM->device()->
                                    getResourceID(AMDILDevice::RAW_UAV_ID))) {
        id = mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID);;
      } else if (mMFI->uav_size() > 1) {
        id = mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID);
      }
    } else if (mSTM->device()->getGeneration() > AMDILDeviceInfo::HD6XXX) {
      if (!mSTM->overridesFlatAS()) {
        id = mSTM->device()->getResourceID(AMDILDevice::GLOBAL_ID);
        if (mMFI->uav_size() && !mMFI->uav_count(id)) {
          id = (*mMFI->uav_begin());
        }
      }
    }
    O << ";uavid:" << id  << "\n";
  }
  if (isKernel) {
    O << ";privateid:" << mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID)
      << "\n";
  }
  if (isKernel) {
    std::string argKernel = "llvm.argtypename.annotations.";
    argKernel.append(mName);
    GlobalVariable *GV = mMF->getFunction()->getParent()
                         ->getGlobalVariable(argKernel);
    if (GV && GV->hasInitializer()) {
      const ConstantArray *nameArray
        = dyn_cast_or_null<ConstantArray>(GV->getInitializer());
      if (nameArray) {
        for (unsigned x = 0, y = nameArray->getNumOperands(); x < y; ++x) {
          const GlobalVariable *gV= dyn_cast_or_null<GlobalVariable>(
            nameArray->getOperand(x)->getOperand(0));
          const ConstantDataArray *argName =
            dyn_cast_or_null<ConstantDataArray>(gV->getInitializer());
          if (!argName) {
            continue;
          }
          std::string argStr = argName->getAsString();
          O << ";reflection:" << x << ":" <<
          argStr.substr(0, argStr.length()-1) << "\n";
        }
      }
    }
  }
  if (mSTM->isApple()) {
    if (isKernel) {
      O << ";ARGEND:__OpenCL_" << mName << "_kernel\n";
    } else {
      O << ";ARGEND:" << mName << "\n";
    }
  } else {
    O << ";ARGEND:" << mName << "\n";
  }
}
void AMDILKernelManager::printArgCopies(OSTREAM_TYPE &O,
                                        AMDILAsmPrinter *RegNames)
{
  Function::const_arg_iterator I = mMF->getFunction()->arg_begin();
  Function::const_arg_iterator Ie = mMF->getFunction()->arg_end();
  uint32_t Counter = 0;

  if (mMFI->getArgSize()) {
    O << "dcl_cb cb1";
    O << "[" << (mMFI->getArgSize() >> 4) << "]\n";
    mMFI->setUsesMem(AMDILDevice::CONSTANT_ID);
  }
  const Function *F = mMF->getFunction();
  // Get the stack size
  uint32_t stackSize = mMFI->getStackSize();
  uint32_t privateSize = mMFI->getScratchSize();
  uint32_t stackOffset = (privateSize + 15) & (~0xF);
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateMem) &&
      !mSTM->overridesFlatAS()) {
    // TODO: If the size is too large, we need to fall back to software emulated
    // instead of using the hardware capability.
    int size =
      (((((stackSize != privateSize) ? stackSize + privateSize :  stackSize)
         + 15) & (~0xF)) >> 4)
      + (mSTM->device()->isSupported(AMDILDeviceInfo::Debug) ? 1 : 0);
    if (size > 4096) {
      mMFI->addErrorMsg(amd::CompilerErrorMessage[
                          INSUFFICIENT_PRIVATE_RESOURCES]);
    }
    if (size) {
      // For any stack variables, we need to declare the literals for them so that
      // we can use them when we copy our data to the stack.
      // Anytime we declare a literal, we need to reserve it, if it is not emitted
      // in emitLiterals.
      uint32_t resid =  mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID);
      if (mSTM->device()->usesHardware(AMDILDeviceInfo::PrivateUAV)) {
        O << "dcl_typeless_uav_id(" << resid
          << ")_stride(4)_length(" << (size << 4 )<< ")_access(private)\n";
        if (mSTM->device()->isSupported(AMDILDeviceInfo::Debug)) {
          int newSize = (size - 1) << 4;
          mMFI->addReservedLiterals(1);
          O << "dcl_literal l" << mMFI->getNumLiterals() << ", "
            << newSize << ","
            << newSize << ","
            << newSize << ","
            << newSize << "\n";
          O << "uav_raw_store_id(" << resid << ") mem0, l"
            << mMFI->getNumLiterals()<< ", r1023\n";
        }
      } else {
        O << "dcl_indexed_temp_array x"
          << mSTM->device()->getResourceID(AMDILDevice::SCRATCH_ID) << "["
          << size << "]\n";
        if (mSTM->device()->isSupported(AMDILDeviceInfo::Debug)) {
          O << "mov x" << resid << "[" << size - 1 << "], r1023\n";
        }
      }
      mMFI->addReservedLiterals(1);
      O << "dcl_literal l" << mMFI->getNumLiterals() << ", " << stackSize <<
      ", "
        << privateSize << ", 16, " <<
      ((stackSize == privateSize) ? 0 : stackOffset) << "\n"
        << "iadd r0.x, " << RegNames->getRegisterName(AMDIL::T1) << ".x, l"
        << mMFI->getNumLiterals() << ".w\n";

      O << "mov " << RegNames->getRegisterName(AMDIL::FP)
        << ", l" << mMFI->getNumLiterals() << ".0\n";
    }
  }
  I = mMF->getFunction()->arg_begin();
  int32_t count = 0;
  unsigned curReg = 0;
  for (I = mMF->getFunction()->arg_begin(); I != Ie; ++I) {
    Type *curType = I->getType();
    unsigned int Buffer = 1;
    O << "; Kernel arg setup: " << I->getName().str() << "\n";
    if (curType->isIntegerTy() || curType->isFloatingPointTy()) {
      switch (curType->getPrimitiveSizeInBits()) {
      default:
        printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1);
        break;
      case 16:
        printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1,
                                  "l3.y" );
        break;
      case 8:
        printConstantToRegMapping(RegNames,
                                  curReg,
                                  O,
                                  Counter,
                                  Buffer,
                                  1,
                                  "l3.x" );
        break;
      }
    } else if (const VectorType *VT = dyn_cast<VectorType>(curType)) {
      Type *ET = VT->getElementType();
      int numEle = VT->getNumElements();
      switch (ET->getPrimitiveSizeInBits()) {
      default:
        printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer,
                                  (numEle+2) >> 2);
        break;
      case 64:
        if (numEle == 3) {
          O << "mov ";
          printRegName(RegNames, mMFI->getArgReg(curReg++), O, true);
          O << " cb" << Buffer << "[" << Counter << "].xy\n";
          O << "mov ";
          printRegName(RegNames, mMFI->getArgReg(curReg++), O, true);
          O << " cb" << Buffer << "[" << Counter << "].zw\n";
          ++Counter;
          O << "mov ";
          printRegName(RegNames, mMFI->getArgReg(curReg++), O, true);
          O << " cb" << Buffer << "[" << Counter << "].xy\n";
          Counter++;
        } else {
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer,
                                    (numEle) >> 1);
        }
        break;
      case 16:
      {
        switch (numEle) {
        default:
          printConstantToRegMapping(RegNames, curReg, O, Counter,
                                    Buffer, (numEle+2) >> 2, "l3.y", 1093);
          break;
        case 2:
          printConstantToRegMapping(RegNames, curReg, O, Counter,
                                    Buffer, 1, "l3.y", 1092);
          break;
        }
        break;
      }
      case 8:
      {
        switch (numEle) {
        default:
          printConstantToRegMapping(RegNames, curReg, O, Counter,
                                    Buffer, (numEle+2) >> 2, "l3.x", 1091);
          break;
        case 2:
          printConstantToRegMapping(RegNames, curReg, O, Counter,
                                    Buffer, 1, "l3.x", 1090);
          break;
        }
        break;
      }
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
            printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer,
                                      1, NULL, 0, is_image);
          } else {
            mMFI->addErrorMsg(
              amd::CompilerErrorMessage[NO_IMAGE_SUPPORT]);
            ++curReg;
          }
        } else {
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1);
        }
      } else if (CT->isStructTy()
                 && PT->getAddressSpace() == AMDILAS::PRIVATE_ADDRESS) {
        StructType *ST = dyn_cast<StructType>(CT);
        bool i1d  = ST->getName().startswith("struct._image1d_t");
        bool i1da = ST->getName().startswith("struct._image1d_array_t");
        bool i1db = ST->getName().startswith("struct._image1d_buffer_t");
        bool i2d  = ST->getName().startswith("struct._image2d_t");
        bool i2da = ST->getName().startswith("struct._image2d_array_t");
        bool i3d  = ST->getName().startswith("struct._image3d_t");
        bool is_image = i1d || i1da || i1db || i2d || i2da || i3d;
        if (is_image) {
          if (mSTM->device()->isSupported(AMDILDeviceInfo::Images)) {
            printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer,
                                      1, NULL, 0, is_image);
          } else {
            mMFI->addErrorMsg(amd::CompilerErrorMessage[NO_IMAGE_SUPPORT]);
            ++curReg;
          }
        } else {
          const TargetData* TD = mTM->getTargetData();
          size_t structSize
            = TD->RoundUpAlignment(TD->getTypeAllocSize(ST), 16);

          stackOffset += structSize;
          O << "mov ";
          printRegName(RegNames, mMFI->getArgReg(curReg), O, true);
          O << " r0.x\n";
          printCopyStructPrivate(ST,
                                 O,
                                 structSize,
                                 Buffer,
                                 mMFI->getNumLiterals(),
                                 Counter);
          ++curReg;
        }
      } else if (CT->isIntOrIntVectorTy()
                 || CT->isFPOrFPVectorTy()
                 || CT->isArrayTy()
                 || CT->isPointerTy()
                 || PT->getAddressSpace() != AMDILAS::PRIVATE_ADDRESS) {
        if (PT->getAddressSpace() == AMDILAS::CONSTANT_ADDRESS) {
          const AMDILKernel* krnl = mAMI->getKernel(F->getName());
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer,
                                    1, NULL, 0, false,
                                    mAMI->usesHWConstant(krnl, I->getName()));
        } else if (PT->getAddressSpace() == AMDILAS::REGION_ADDRESS) {
          // TODO: If we are region address space, the first region pointer, no
          // array pointers exist, and hardware RegionMem is enabled then we can
          // zero out register as the initial offset is zero.
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1);
        } else if (PT->getAddressSpace() == AMDILAS::LOCAL_ADDRESS) {
          // TODO: If we are local address space, the first local pointer, no
          // array pointers exist, and hardware LocalMem is enabled then we can
          // zero out register as the initial offset is zero.
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1);
        } else {
          printConstantToRegMapping(RegNames, curReg, O, Counter, Buffer, 1);
        }
      } else {
        assert(0 && "Current type is not supported!");
        mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
        ++curReg;
      }
    } else {
      assert(0 && "Current type is not supported!");
      mMFI->addErrorMsg(amd::CompilerErrorMessage[INTERNAL_ERROR]);
      ++curReg;
    }
  }
  if (mSTM->device()->usesHardware(AMDILDeviceInfo::ConstantMem)) {
    const AMDILKernel* krnl = mAMI->getKernel(F->getName());
    uint32_t constNum = 0;
    for (uint32_t x = 0; x < mSTM->device()->getMaxNumCBs(); ++x) {
      if (krnl->constSizes[x]) {
        O << "dcl_cb cb" << x + CB_BASE_OFFSET;
        O << "[" << (((krnl->constSizes[x] + 15) & ~15) >> 4) << "]\n";
        ++constNum;
        mMFI->setUsesMem(AMDILDevice::CONSTANT_ID);
      }
    }
    // TODO: If we run out of constant resources, we need to push some of the
    // constant pointers to the software emulated section.
    if (constNum > mSTM->device()->getMaxNumCBs()) {
      assert(0 && "Max constant buffer limit passed!");
      mMFI->addErrorMsg(amd::CompilerErrorMessage[
                          INSUFFICIENT_CONSTANT_RESOURCES]);
    }
  }
}
void AMDILKernelManager::emitLiterals(OSTREAM_TYPE &O) {
  char buffer[256];
  std::map<std::pair<uint64_t, uint64_t>, uint32_t>::iterator vlb, vle;
  for (vlb = mMFI->lit_begin(), vle = mMFI->lit_end(); vlb != vle; ++vlb) {
    uint32_t v[2][2];
    uint64_t a = vlb->first.first;
    uint64_t b = vlb->first.second;
    memcpy(v[0], &a, sizeof(uint64_t));
    memcpy(v[1], &b, sizeof(uint64_t));
    O << "dcl_literal l" << vlb->second << ", ";
    sprintf(buffer, "0x%08X, 0x%08X, 0x%08X, 0x%08X; f128:i128 ",
            v[0][0], v[0][1], v[1][0], v[1][1]);
    O << buffer << vlb->first.first << vlb->first.second << "\n";
  }
}
// If the value is not known, then the uav is set, otherwise the mValueIDMap
// is used.
void AMDILKernelManager::setUAVID(const Value *value, uint32_t ID) {
  if (value) {
    mValueIDMap[value] = ID;
  }
}
uint32_t AMDILKernelManager::getUAVID(const Value *value) {
  if (mValueIDMap.find(value) != mValueIDMap.end()) {
    return mValueIDMap[value];
  }

  if (mSTM->device()->getGeneration() <= AMDILDeviceInfo::HD6XXX) {
    return mSTM->device()->getResourceID(AMDILDevice::ARENA_UAV_ID);
  } else {
    return mSTM->device()->getResourceID(AMDILDevice::RAW_UAV_ID);
  }
}
