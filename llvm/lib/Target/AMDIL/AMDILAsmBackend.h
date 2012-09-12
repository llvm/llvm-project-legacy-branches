//===-- AMDILAsmBackend.h -------------------------------------------------===//
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

#ifndef _AMDIL_ASM_BACKEND_H_
#define _AMDIL_ASM_BACKEND_H_
#include "AMDIL.h"
#include "llvm/MC/MCAsmBackend.h"
#define ASM_BACKEND_CLASS MCAsmBackend

using namespace llvm;
namespace llvm {
class AMDILAsmBackend : public ASM_BACKEND_CLASS {
public:
  AMDILAsmBackend(const ASM_BACKEND_CLASS &T);
  virtual MCObjectWriter *createObjectWriter(raw_ostream &OS) const;
  virtual bool doesSectionRequireSymbols(const MCSection &Section) const;
  virtual bool isSectionAtomizable(const MCSection &Section) const;
  virtual bool isVirtualSection(const MCSection &Section) const;
  virtual void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                          uint64_t Value) const;
  virtual bool
  mayNeedRelaxation(const MCInst &Inst
                    ) const;
  virtual bool
  fixupNeedsRelaxation(const MCFixup &Fixup,
                       uint64_t Value,
                       const MCInstFragment *DF,
                       const MCAsmLayout &Layout) const;
  virtual void relaxInstruction(const MCInst &Inst, MCInst &Res) const;
  virtual bool writeNopData(uint64_t Count, MCObjectWriter *OW) const;
  unsigned getNumFixupKinds() const;
};   // class AMDILAsmBackend;
} // llvm namespace

#endif // _AMDIL_ASM_BACKEND_H_
