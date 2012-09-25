//===-- AMDILMCAsmInfo.cpp ------------------------------------------------===//
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

#include "AMDILMCAsmInfo.h"
#include "AMDILLLVMPC.h"
#ifndef NULL
#define NULL 0
#endif

#include "llvm/Config/config.h"

using namespace llvm;
AMDILMCAsmInfo::AMDILMCAsmInfo(const Triple &Triple) : MCAsmInfo()
{
  //===------------------------------------------------------------------===//
  HasSubsectionsViaSymbols = true;
  HasMachoZeroFillDirective = false;
  HasMachoTBSSDirective = false;
  HasStaticCtorDtorReferenceInStaticMode = false;
  LinkerRequiresNonEmptyDwarfLines = true;
  MaxInstLength = 16;
  PCSymbol = "$";
  SeparatorString = "\n";
  CommentColumn = 40;
  CommentString = ";";
  LabelSuffix = ":";
  GlobalPrefix = "@";
  PrivateGlobalPrefix = ";.";
  LinkerPrivateGlobalPrefix = "!";
  InlineAsmStart = ";#ASMSTART";
  InlineAsmEnd = ";#ASMEND";
  AssemblerDialect = 0;
  AllowQuotesInName = false;
  AllowNameToStartWithDigit = false;
  AllowPeriodsInName = false;

  //===--- Data Emission Directives -------------------------------------===//
  ZeroDirective = ".zero";
  AsciiDirective = ".ascii\t";
  AscizDirective = ".asciz\t";
  Data8bitsDirective = ".byte\t";
  Data16bitsDirective = ".short\t";
  Data32bitsDirective = ".long\t";
  Data64bitsDirective = ".quad\t";
  GPRel32Directive = NULL;
  SunStyleELFSectionSwitchSyntax = true;
  UsesELFSectionDirectiveForBSS = true;
  HasMicrosoftFastStdCallMangling = false;

  //===--- Alignment Information ----------------------------------------===//
  AlignDirective = ";.align\t";
  AlignmentIsInBytes = true;
  TextAlignFillValue = 0;

  //===--- Global Variable Emission Directives --------------------------===//
  GlobalDirective = ".global";
  ExternDirective = ".extern";
  HasSetDirective = false;
  // TODO: This makes the symbol definition have the math instead
  // of the symbol use. This could be disabled and handled as it
  // would simplify the patching code in AMDILMDParser.cpp.
  HasAggressiveSymbolFolding = true;
  LCOMMDirectiveType = LCOMM::None;
  COMMDirectiveAlignmentIsInBytes = false;
  // TODO: This generates .type @__OpenCL_<name>_kernel,@function
  // and .size @__OpenCL_<name>_kernel, ;.<tmp>-@__OpenCL_<name>_kernel,
  // which is not handled in AMDILMDParser.cpp.
  HasDotTypeDotSizeDirective = false;
  HasSingleParameterDotFile = true;
  HasNoDeadStrip = true;
  HasSymbolResolver = false;
  WeakRefDirective = ".weakref\t";
  WeakDefDirective = ".weakdef\t";
  LinkOnceDirective = NULL;
  HiddenVisibilityAttr = MCSA_Hidden;
  HiddenDeclarationVisibilityAttr = MCSA_Hidden;
  ProtectedVisibilityAttr = MCSA_Protected;

  //===--- Dwarf Emission Directives -----------------------------------===//
  HasLEB128 = true;
  SupportsDebugInformation = true;
  ExceptionsType = ExceptionHandling::None;
  DwarfUsesInlineInfoSection = false;
  DwarfSectionOffsetDirective = ".offset";

  //===--- CBE Asm Translation Table -----------------------------------===//
}
const char*
AMDILMCAsmInfo::getDataASDirective(unsigned int Size, unsigned int AS) const
{
  switch (AS) {
  default:
    return NULL;
  case 0:
    return NULL;
  };
  return NULL;
}
