//===-- AMDILSubtarget.cpp ------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the AMD IL specific subclass of TargetSubtarget.
//
//===----------------------------------------------------------------------===//

#include "AMDILSubtarget.h"
#include "AMDIL.h"
#include "AMDILDevices.h"
#include "AMDILKernelManager.h"
#include "AMDILUtilityFunctions.h"
#include "AMDILGenSubtarget.inc"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/SubtargetFeature.h"

using namespace llvm;

#define GET_SUBTARGETINFO_ENUM
#define GET_SUBTARGETINFO_CTOR
#define GET_SUBTARGETINFO_MC_DESC
#define GET_SUBTARGETINFO_TARGET_DESC
#include "AMDILGenSubtarget.inc"

AMDILSubtarget::AMDILSubtarget(llvm::StringRef TT,
                               llvm::StringRef GPU,
                               llvm::StringRef FS) : AMDILGenSubtargetInfo( TT,
                                                                            GPU,
                                                                            FS )
{
  memset(CapsOverride, 0, sizeof(*CapsOverride)
         * AMDILDeviceInfo::MaxNumberCapabilities);
  mIs64bit = false;
  mFlatAddress = false;
  mMetadata30 = false;
  SmallVector<StringRef, DEFAULT_VEC_SLOTS> Features;
  SplitString(FS, Features, ",");
  mDefaultSize[0] = 64;
  mDefaultSize[1] = 1;
  mDefaultSize[2] = 1;
  std::string newFeatures = "";
#if defined(_DEBUG) || defined(DEBUG)
  bool useTest = false;
#endif
  for (size_t x = 0; x < Features.size(); ++x) {
    if (Features[x].startswith("+mwgs")) {
      SmallVector<StringRef, DEFAULT_VEC_SLOTS> sizes;
      SplitString(Features[x], sizes, "-");
      size_t mDim = ::atoi(sizes[1].data());
      if (mDim > 3) {
        mDim = 3;
      }
      for (size_t y = 0; y < mDim; ++y) {
        mDefaultSize[y] = ::atoi(sizes[y+2].data());
      }
#if defined(_DEBUG) || defined(DEBUG)
    } else if (!Features[x].compare("test")) {
      useTest = true;
#endif
    } else if (Features[x].startswith("+cal")) {
    } else {
      if (newFeatures.length() > 0) newFeatures += ',';
      newFeatures += Features[x];
    }
  }
  for (int x = 0; x < 3; ++x) {
    if (!mDefaultSize[x]) {
      mDefaultSize[x] = 1;
    }
  }
#if defined(_DEBUG) || defined(DEBUG)
  if (useTest) {
    GPU = "kauai";
  }
#endif
  ParseSubtargetFeatures(GPU, newFeatures);
#if defined(_DEBUG) || defined(DEBUG)
  if (useTest) {
    GPU = "test";
  }
#endif
  mDevName = GPU;
  mDevice = getDeviceFromName(mDevName, this, mIs64bit);
}
AMDILSubtarget::~AMDILSubtarget()
{
  delete mDevice;
}
bool
AMDILSubtarget::isOverride(AMDILDeviceInfo::Caps caps) const
{
  assert(caps < AMDILDeviceInfo::MaxNumberCapabilities &&
         "Caps index is out of bounds!");
  return CapsOverride[caps];
}
bool
AMDILSubtarget::isApple() const {
  return false;
}
bool
AMDILSubtarget::overridesFlatAS() const
{
  return mFlatAddress;
}
bool
AMDILSubtarget::is64bit() const
{
  return mIs64bit;
}
bool
AMDILSubtarget::isTargetELF() const
{
  return false;
}
bool
AMDILSubtarget::supportMetadata30() const
{
  return mMetadata30;
}
size_t
AMDILSubtarget::getDefaultSize(uint32_t dim) const
{
  if (dim > 3) {
    return 1;
  } else {
    return mDefaultSize[dim];
  }
}
AMDILKernelManager*
AMDILSubtarget::getKernelManager() const
{
  return mKM;
}
void
AMDILSubtarget::setKernelManager(AMDILKernelManager *km) const
{
  mKM = km;
}
std::string
AMDILSubtarget::getDataLayout() const
{
  if (!mDevice) {
    if (is64bit()) {
      return std::string(
               "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16"
               "-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:32:32"
               "-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64"
               "-v96:128:128-v128:128:128-v192:256:256-v256:256:256"
               "-v512:512:512-v1024:1024:1024-v2048:2048:2048-a0:0:64");
    } else {
      return std::string(
               "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16"
               "-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:32:32"
               "-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64"
               "-v96:128:128-v128:128:128-v192:256:256-v256:256:256"
               "-v512:512:512-v1024:1024:1024-v2048:2048:2048-a0:0:64");
    }
  }
  return mDevice->getDataLayout();
}
std::string
AMDILSubtarget::getDeviceName() const
{
  return mDevName;
}
const AMDILDevice *
AMDILSubtarget::device() const
{
  return mDevice;
}
