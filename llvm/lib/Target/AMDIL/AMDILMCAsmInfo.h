//===-- AMDILMCAsmInfo.h --------------------------------------------------===//
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

#ifndef AMDILMCASMINFO_H_
#define AMDILMCASMINFO_H_

#include "llvm/MC/MCAsmInfo.h"
#include "AMDILLLVMPC.h"
namespace llvm {
class Triple;

class AMDILMCAsmInfo : public MCAsmInfo {
public:
  AMDILMCAsmInfo(const Triple &Triple);
  const char*
  getDataASDirective(unsigned int Size, unsigned int AS) const;
};
} // namespace llvm
#endif // AMDILMCASMINFO_H_
