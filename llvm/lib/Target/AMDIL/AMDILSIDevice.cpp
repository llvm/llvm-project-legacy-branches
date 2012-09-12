//===-- AMDILSIDevice.cpp -------------------------------------------------===//
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

#include "AMDILSIDevice.h"
#include "AMDILSubtarget.h"
#include "AMDILSIIOExpansion.h"
#include "AMDILSIPointerManager.h"
#include "AMDILSIAsmPrinter.h"

using namespace llvm;

AMDILSIDevice::AMDILSIDevice(AMDILSubtarget *ST)
  : AMDILCaymanDevice(ST)
{
  setCaps();
  std::string name = ST->getDeviceName();
  if (name == "tahiti") {
    mDeviceFlag = OCL_DEVICE_TAHITI;
  } else if (name == "pitcairn") {
    mDeviceFlag = OCL_DEVICE_PITCAIRN;
  } else if (name == "dogs") {    // "dogs" for Oland, which is CI series but has a SI core
    mDeviceFlag = OCL_DEVICE_DOGS;
  } else {
    mDeviceFlag = OCL_DEVICE_CAPEVERDE;
  }
}
AMDILSIDevice::~AMDILSIDevice()
{
}
void
AMDILSIDevice::setCaps()
{
  mHWBits.set(AMDILDeviceInfo::PrivateUAV);
  mHWBits.reset(AMDILDeviceInfo::ArenaUAV);
  mSWBits.reset(AMDILDeviceInfo::ArenaSegment);
  mHWBits.reset(AMDILDeviceInfo::ArenaSegment);
  mHWBits.set(AMDILDeviceInfo::ByteStores);
  mHWBits.set(AMDILDeviceInfo::HW64BitDivMod);
  mSWBits.reset(AMDILDeviceInfo::HW64BitDivMod);
  if (!mSTM->isApple()) {
    if (mSTM->isOverride(AMDILDeviceInfo::Images)) {
      mHWBits.set(AMDILDeviceInfo::Images);
    }
  } else {
    mHWBits.set(AMDILDeviceInfo::Images);
  }
  mHWBits.set(AMDILDeviceInfo::CachedMem);
  mHWBits.set(AMDILDeviceInfo::ByteLDSOps);
  mSWBits.reset(AMDILDeviceInfo::ByteLDSOps);
  mHWBits.set(AMDILDeviceInfo::LongOps);
  mSWBits.reset(AMDILDeviceInfo::LongOps);
  mHWBits.set(AMDILDeviceInfo::TmrReg);
  mHWBits.set(AMDILDeviceInfo::PPAMode);
  mHWBits.set(AMDILDeviceInfo::ConstantMem);
  mHWBits.set(AMDILDeviceInfo::PrivateMem);
  mHWBits.set(AMDILDeviceInfo::LocalMem);
  mHWBits.set(AMDILDeviceInfo::RegionMem);
}
uint32_t
AMDILSIDevice::getGeneration() const
{
  return AMDILDeviceInfo::HD7XXX;
}
uint32_t
AMDILSIDevice::getMaxNumUAVs() const
{
  return 1024;
}
uint32_t
AMDILSIDevice::getResourceID(uint32_t id) const {
  switch(id) {
  default:
    assert(0 && "ID type passed in is unknown!");
    break;
  case ARENA_UAV_ID:
    assert(0 && "Arena UAV is not supported on SI device.");
  case GLOBAL_ID:
  case CONSTANT_ID:
  case RAW_UAV_ID:
    return 9;
  case LDS_ID:
    if (usesHardware(AMDILDeviceInfo::LocalMem)) {
      return DEFAULT_LDS_ID;
    } else {
      return getResourceID(GLOBAL_ID);
    }
  case GDS_ID:
    if (usesHardware(AMDILDeviceInfo::RegionMem)) {
      return DEFAULT_GDS_ID;
    } else {
      return getResourceID(GLOBAL_ID);
    }
  case SCRATCH_ID:
    if (usesHardware(AMDILDeviceInfo::PrivateMem)) {
      return 8;
    } else {
      return getResourceID(GLOBAL_ID);
    }
  };
  return 0;
}
FunctionPass*
AMDILSIDevice::getIOExpansion() const
{
  return new AMDILSIIOExpansion();
}
AsmPrinter*
AMDILSIDevice::getAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS) const
{
  return new AMDILSIAsmPrinter(ASM_PRINTER_ARGUMENTS);
}
FunctionPass*
AMDILSIDevice::getPointerManager(
  TargetMachine& TM, CodeGenOpt::Level OptLevel) const
{
  return new AMDILSIPointerManager();
}
AMDILSIDevice32::AMDILSIDevice32(AMDILSubtarget *ST)
  : AMDILSIDevice(ST)
{
}
AMDILSIDevice32::~AMDILSIDevice32()
{
}
std::string
AMDILSIDevice32::getDataLayout() const
{
  return std::string("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16"
                     "-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:32:32"
                     "-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64"
                     "-v96:128:128-v128:128:128-v192:256:256-v256:256:256"
                     "-v512:512:512-v1024:1024:1024-v2048:2048:2048-a0:0:64"
                     "-n8:16:32:64");
}
#if 0
AMDILSIDevice64on32::AMDILSIDevice32(AMDILSubtarget *ST)
  : AMDILSIDevice(ST)
{
}

AMDILSIDevice64on32::~AMDILSIDevice64on32()
{
}
std::string
AMDILSIDevice64on32::getDataLayout() const
{
  return std::string("e-p:64:32:32-i1:8:8-i8:8:8-i16:16:16"
                     "-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:32:32"
                     "-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64"
                     "-v96:128:128-v128:128:128-v192:256:256-v256:256:256"
                     "-v512:512:512-v1024:1024:1024-v2048:2048:2048-a0:0:64"
                     "-n8:16:32:64");
}
#endif

AMDILSIDevice64::AMDILSIDevice64(AMDILSubtarget *ST)
  : AMDILSIDevice(ST)
{
}
AMDILSIDevice64::~AMDILSIDevice64()
{
}
std::string
AMDILSIDevice64::getDataLayout() const
{
  return std::string("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16"
                     "-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:32:32"
                     "-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64"
                     "-v96:128:128-v128:128:128-v192:256:256-v256:256:256"
                     "-v512:512:512-v1024:1024:1024-v2048:2048:2048"
                     "-n8:16:32:64");
}
