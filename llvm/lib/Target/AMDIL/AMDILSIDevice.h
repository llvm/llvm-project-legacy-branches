//===-- AMDILSIDevice.h ---------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file will define the interface that the SI generation needs to
// implement in order to correctly answer queries on the capabilities of the specific
// hardware.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDILSIDEVICE_H_
#define _AMDILSIDEVICE_H_
#include "AMDILNIDevice.h"
namespace llvm {
class AMDILSubtarget;

// Device that matches the SI family. The SI family is a scalar architecture
// with a vector unit to do math. The SI device has 8/16 bit native load/store
// instructions, 1024 UAVs and no arena. It
//
class AMDILSIDevice : public AMDILCaymanDevice {
protected:
  AMDILSIDevice(AMDILSubtarget*);
  FunctionPass* getIOExpansion() const;
  AsmPrinter*
  getAsmPrinter(AMDIL_ASM_PRINTER_ARGUMENTS) const;
  FunctionPass*
  getPointerManager(TargetMachine&, CodeGenOpt::Level) const;

public:
  virtual ~AMDILSIDevice();
  virtual uint32_t getGeneration() const;
  virtual uint32_t getMaxNumUAVs() const;
  virtual uint32_t getResourceID(uint32_t) const;
  virtual std::string getDataLayout() const = 0;
protected:
  virtual void setCaps();
};   // AMDILSIDevice
     // 32bit SI device
class AMDILSIDevice32 : public AMDILSIDevice {
public:
  AMDILSIDevice32(AMDILSubtarget*);
  virtual ~AMDILSIDevice32();
  virtual std::string getDataLayout() const;
};   // AMDILSIDevice32
#if 0
// 64bit ptr 32bit SI device
class AMDILSIDevice64on32 : public AMDILSIDevice {
public:
  AMDILSIDevice64on32(AMDILSubtarget*);
  virtual ~AMDILSIDevice64on32();
  virtual std::string getDataLayout() const;
};   // AMDILSIDevice64on32
#endif
// 64bit SI device
class AMDILSIDevice64 : public AMDILSIDevice {
public:
  AMDILSIDevice64(AMDILSubtarget*);
  virtual ~AMDILSIDevice64();
  virtual std::string getDataLayout() const;
};   // AMDILSIDevice64
static const unsigned int MAX_LDS_SIZE_1000 = AMDILDevice::MAX_LDS_SIZE_800;
} // namespace llvm

#endif // _AMDILSIDEVICE_H_
