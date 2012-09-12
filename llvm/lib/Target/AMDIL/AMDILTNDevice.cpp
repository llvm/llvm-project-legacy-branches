//===-- AMDILTNDevice.cpp -------------------------------------------------===//
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

#include "AMDILSubtarget.h"
#include "AMDILTNDevice.h"
#include "AMDILNIDevice.h"
using namespace llvm;

AMDILTrinityDevice::AMDILTrinityDevice(AMDILSubtarget *ST)
  : AMDILCaymanDevice(ST)
{
  std::string name = ST->getDeviceName();
  if(name == "trinity") {
    mDeviceFlag = OCL_DEVICE_TRINITY;
  }
  setCaps();
}
AMDILTrinityDevice::~AMDILTrinityDevice()
{
}
void
AMDILTrinityDevice::setCaps()
{
  // Trinity inherits everything from Cayman. If there are any
  // differences (like disabling FP64, do it here).
}
