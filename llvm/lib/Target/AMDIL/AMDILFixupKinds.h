//===-- AMDILFixupKinds.h -------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The LLVM Compiler Infrastructure This file is distributed under the
// University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_AMDIL_AMDILFIXUPKINDS_H
#define LLVM_AMDIL_AMDILFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace AMDIL {
enum Fixups {
  reloc_pcrel_4byte = FirstTargetFixupKind,  // 32-bit pcrel, e.g. a branch.
  reloc_riprel_4byte,                        // 32-bit rip-relative
};
}
}

#endif

