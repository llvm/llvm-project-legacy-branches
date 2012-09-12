//===-- AMDILTNDevice.h ---------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Interface for the subtarget data classes.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDILTNDEVICE_H_
#define _AMDILTNDEVICE_H_
#include "AMDILNIDevice.h"
#include "AMDILSubtarget.h"
namespace llvm {
class AMDILSubtarget;
//===---------------------------------------------------------------------===//
// NI generation of devices and their respective sub classes
//===---------------------------------------------------------------------===//

// Trinity devices (devastator and scrapper) are APUs based on Cayman.
// Like Cayman, they have a 4 wide ALU. They do support FP64, but this
// maybe not be advertised at the OpenCL API layer depending on
// performance.
class AMDILTrinityDevice : public AMDILCaymanDevice {
public:
  AMDILTrinityDevice(AMDILSubtarget*);
  virtual ~AMDILTrinityDevice();
private:
  virtual void setCaps();
};   // AMDILTrinityDevice
} // namespace llvm
#endif // _AMDILTNDEVICE_H_
