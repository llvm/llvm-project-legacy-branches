//===-- AMDILSIPointerManager.cpp -----------------------------------------===//
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

#include "AMDILPointerManagerImpl.h"
#include "AMDILSIPointerManager.h"
#include "AMDILCompilerErrors.h"
#include "AMDILDeviceInfo.h"
#include "AMDILKernelManager.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILModuleInfo.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalValue.h"
#include "llvm/Metadata.h"
#include "llvm/Module.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/ValueMap.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/FormattedStream.h"
#include <iostream>
#include <set>
#include <map>
#include <list>
#include <queue>
#include <iomanip>
using namespace llvm;

// A byte pointer is a pointer that along the pointer path has a
// byte store assigned to it.
void
AMDILSIPointerManagerImpl::annotateBytePtrs()
{
  PtrSet::iterator siBegin, siEnd;
  std::vector<MachineInstr*>::iterator miBegin, miEnd;
  AMDILMachineFunctionInfo *mMFI = NULL;
  for (siBegin = bytePtrs.begin(), siEnd = bytePtrs.end();
       siBegin != siEnd; ++siBegin) {
    const PointerType *PT = dyn_cast<PointerType>(siBegin->second->getType());
    if (!PT) {
      continue;
    }
    if (conflictPtrs.count(*siBegin)) {
      continue;
    }
    assert(!rawPtrs.count(*siBegin) && "Found a byte pointer "
           "that also exists as a raw pointer!");
    for (miBegin = PtrToInstMap[siBegin->second].begin(),
         miEnd = PtrToInstMap[siBegin->second].end();
         miBegin != miEnd; ++miBegin) {
      MachineInstr *MI = (*miBegin);
      if (DEBUGME) {
        dbgs() << "Annotating pointer as arena. Inst: ";
        (*miBegin)->dump();
      }
      AMDILAS::InstrResEnc curRes;
      getAsmPrinterFlags(*miBegin, curRes);

      if (STM->device()->usesHardware(AMDILDeviceInfo::ConstantMem)
          && PT->getAddressSpace() == AMDILAS::CONSTANT_ADDRESS) {
        // If hardware constant mem is enabled, then we need to
        // get the constant pointer CB number and use that to specify
        // the resource ID.
        MachineFunction *MF = (*miBegin)->getParent()->getParent();
        AMDILModuleInfo *mAMI = &(MF->getMMI().getObjFileInfo<AMDILModuleInfo>());
        const StringRef funcName = (*miBegin)->getParent()->getParent()
                                   ->getFunction()->getName();
        if (mAMI->isKernel(funcName)) {
          const AMDILKernel *krnl = mAMI->getKernel(funcName);
          curRes.bits.ResourceID = mAMI->getConstPtrCB(krnl,
                                                       siBegin->second->getName());
          curRes.bits.HardwareInst = 1;
        } else {
          curRes.bits.ResourceID = STM->device()
                                   ->getResourceID(AMDILDevice::CONSTANT_ID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::LocalMem)
                 && PT->getAddressSpace() == AMDILAS::LOCAL_ADDRESS) {
        // If hardware local mem is enabled, get the local mem ID from
        // the device to use as the ResourceID
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::LDS_ID);
        if (isAtomicInst(*miBegin)) {
          assert(curRes.bits.ResourceID && "Atomic resource ID "
                 "cannot be non-zero!");
          MI->getOperand(MI->getNumOperands()-1)
          .setImm(curRes.bits.ResourceID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::RegionMem)
                 && PT->getAddressSpace() == AMDILAS::REGION_ADDRESS) {
        // If hardware region mem is enabled, get the gds mem ID from
        // the device to use as the ResourceID
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::GDS_ID);
        if (isAtomicInst(*miBegin)) {
          assert(curRes.bits.ResourceID && "Atomic resource ID "
                 "cannot be non-zero!");
          (*miBegin)->getOperand((*miBegin)->getNumOperands()-1)
          .setImm(curRes.bits.ResourceID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::PrivateMem)
                 && PT->getAddressSpace() == AMDILAS::PRIVATE_ADDRESS) {
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::SCRATCH_ID);
      } else {
        if (DEBUGME) {
          dbgs() << __LINE__ << ": Setting byte store bit on instruction: ";
          (*miBegin)->dump();
        }
        curRes.bits.ByteStore = 1;
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::GLOBAL_ID);
        if (MI->getOperand(MI->getNumOperands() - 1).isImm()) {
          MI->getOperand(MI->getNumOperands() - 1).setImm(
            curRes.bits.ResourceID);
        }
        if (DEBUGME) {
          dbgs() << "Annotating pointer as default. Inst: ";
          (*miBegin)->dump();
        }
      }
      setAsmPrinterFlags(*miBegin, curRes);
      KM->setUAVID(siBegin->second, curRes.bits.ResourceID);
      if (!mMFI) {
        mMFI = (*miBegin)->getParent()->getParent()
               ->getInfo<AMDILMachineFunctionInfo>();
      }
      mMFI->uav_insert(curRes.bits.ResourceID);
    }
  }
}
// A raw pointer is any pointer that does not have byte store in its path.
// This function is unique to SI devices as arena is not part of it.
void
AMDILSIPointerManagerImpl::annotateRawPtrs()
{
  PtrSet::iterator siBegin, siEnd;
  std::vector<MachineInstr*>::iterator miBegin, miEnd;
  AMDILMachineFunctionInfo *mMFI = NULL;
  // Now all of the raw pointers will go their own uav ID
  unsigned id = STM->device()
                ->getResourceID(AMDILDevice::GLOBAL_ID);
  for (siBegin = rawPtrs.begin(), siEnd = rawPtrs.end();
       siBegin != siEnd; ++siBegin) {
    const PointerType *PT = dyn_cast<PointerType>(siBegin->second->getType());
    if (!PT) {
      continue;
    }
    if (PT->getAddressSpace() == AMDILAS::GLOBAL_ADDRESS) {
      // If we have a conflict, we don't change it from default.
      if (conflictPtrs.count(*siBegin)) {
        continue;
      }
      ++id;
    }
    assert(!bytePtrs.count(*siBegin) && "Found a raw pointer "
           " that also exists as a byte pointers!");
    for (miBegin = PtrToInstMap[siBegin->second].begin(),
         miEnd = PtrToInstMap[siBegin->second].end();
         miBegin != miEnd; ++miBegin) {
      if (DEBUGME) {
        dbgs() << "Annotating pointer as raw. Inst: ";
        (*miBegin)->dump();
      }
      AMDILAS::InstrResEnc curRes;
      getAsmPrinterFlags(*miBegin, curRes);
      if (!curRes.bits.ConflictPtr) {
        assert(!curRes.bits.ByteStore
               && "Found a instruction that is marked as "
               "raw but has a byte store bit set!");
      } else if (curRes.bits.ConflictPtr) {
        if (curRes.bits.ByteStore) {
          curRes.bits.ByteStore = 0;
        }
      }
      if (STM->device()->usesHardware(AMDILDeviceInfo::ConstantMem)
          && PT->getAddressSpace() == AMDILAS::CONSTANT_ADDRESS) {
        // If hardware constant mem is enabled, then we need to
        // get the constant pointer CB number and use that to specify
        // the resource ID.
        MachineFunction *MF = (*miBegin)->getParent()->getParent();
        AMDILModuleInfo *mAMI = &(MF->getMMI().getObjFileInfo<AMDILModuleInfo>());
        const StringRef funcName = (*miBegin)->getParent()->getParent()
                                   ->getFunction()->getName();
        if (mAMI->isKernel(funcName)) {
          const AMDILKernel *krnl = mAMI->getKernel(funcName);
          curRes.bits.ResourceID = mAMI->getConstPtrCB(krnl,
                                                       siBegin->second->getName());
          curRes.bits.HardwareInst = 1;
        } else {
          curRes.bits.ResourceID = STM->device()
                                   ->getResourceID(AMDILDevice::CONSTANT_ID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::LocalMem)
                 && PT->getAddressSpace() == AMDILAS::LOCAL_ADDRESS) {
        // If hardware local mem is enabled, get the local mem ID from
        // the device to use as the ResourceID
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::LDS_ID);
        if (isAtomicInst(*miBegin)) {
          assert(curRes.bits.ResourceID && "Atomic resource ID "
                 "cannot be non-zero!");
          (*miBegin)->getOperand((*miBegin)->getNumOperands()-1)
          .setImm(curRes.bits.ResourceID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::RegionMem)
                 && PT->getAddressSpace() == AMDILAS::REGION_ADDRESS) {
        // If hardware region mem is enabled, get the gds mem ID from
        // the device to use as the ResourceID
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::GDS_ID);
        if (isAtomicInst(*miBegin)) {
          assert(curRes.bits.ResourceID && "Atomic resource ID "
                 "cannot be non-zero!");
          (*miBegin)->getOperand((*miBegin)->getNumOperands()-1)
          .setImm(curRes.bits.ResourceID);
        }
      } else if (STM->device()->usesHardware(AMDILDeviceInfo::PrivateMem)
                 && PT->getAddressSpace() == AMDILAS::PRIVATE_ADDRESS) {
        curRes.bits.ResourceID = STM->device()
                                 ->getResourceID(AMDILDevice::SCRATCH_ID);
      } else {
        const Argument *curArg = dyn_cast<Argument>(siBegin->second);
        if (!curRes.bits.ConflictPtr || !curArg
            || curArg->hasNoAliasAttr()
            || STM->device()->isSupported(AMDILDeviceInfo::NoAlias)) {
          curRes.bits.ResourceID = id;
        } else {
          curRes.bits.ResourceID = STM->device()
                                   ->getResourceID(AMDILDevice::GLOBAL_ID);
        }
        if (isAtomicInst(*miBegin)) {
          (*miBegin)->getOperand((*miBegin)->getNumOperands()-1)
          .setImm(curRes.bits.ResourceID);
        }
        if (!mMFI) {
          mMFI = (*miBegin)->getParent()->getParent()
                 ->getInfo<AMDILMachineFunctionInfo>();
        }
        KM->setUAVID(siBegin->second, curRes.bits.ResourceID);
        mMFI->uav_insert(curRes.bits.ResourceID);
      }
      if (DEBUGME) {
        dbgs() << "Setting pointer to resource ID "
               << curRes.bits.ResourceID << ": ";
        siBegin->second->dump();
      }
      setAsmPrinterFlags(*miBegin, curRes);
    }
  }
}
// This function annotates the cacheable pointers with the
// CacheableRead bit.
void
AMDILSIPointerManagerImpl::annotateCacheablePtrs()
{
  PtrSet::iterator siBegin, siEnd;
  std::vector<MachineInstr*>::iterator miBegin, miEnd;
  // First we can check the cacheable pointers
  for (siBegin = cacheablePtrs.begin(), siEnd = cacheablePtrs.end();
       siBegin != siEnd; ++siBegin) {
    assert(!bytePtrs.count(*siBegin) && "Found a cacheable pointer "
           "that also exists as a byte pointer!");
    // If we have any kind of conflict, don't add it as cacheable.
    if (conflictPtrs.count(*siBegin)) {
      continue;
    }
    for (miBegin = PtrToInstMap[siBegin->second].begin(),
         miEnd = PtrToInstMap[siBegin->second].end();
         miBegin != miEnd; ++miBegin) {
      if (DEBUGME) {
        dbgs() << "Annotating pointer as cacheable. Inst: ";
        (*miBegin)->dump();
      }
      AMDILAS::InstrResEnc curRes;
      getAsmPrinterFlags(*miBegin, curRes);
      assert(!curRes.bits.ByteStore && "No cacheable pointers should have the "
             "byte Store flag set!");
      curRes.bits.CacheableRead = 1;
      setAsmPrinterFlags(*miBegin, curRes);
    }
  }
}
void
AMDILSIPointerManagerImpl::annotateCacheableInstrs()
{
  CacheableInstrSet::iterator miBegin, miEnd;

  for (miBegin = cacheableSet.begin(),
       miEnd = cacheableSet.end();
       miBegin != miEnd; ++miBegin) {
    if (DEBUGME) {
      dbgs() << "Annotating instr as cacheable. Inst: ";
      (*miBegin)->dump();
    }
    AMDILAS::InstrResEnc curRes;
    getAsmPrinterFlags(*miBegin, curRes);
    assert(!curRes.bits.ByteStore && "No cacheable pointers should have the "
           "byte Store flag set!");
    curRes.bits.CacheableRead = 1;
    setAsmPrinterFlags(*miBegin, curRes);
  }
}
namespace llvm
{
extern void initializeAMDILSIPointerManagerPass(llvm::PassRegistry&);
}

char AMDILSIPointerManager::ID = 0;
INITIALIZE_PASS(AMDILSIPointerManager, "si-pointer-manager",
                "AMDIL SI Pointer Manager", false, false);

AMDILSIPointerManager::AMDILSIPointerManager()
  : MachineFunctionPass(ID)
{
  initializeAMDILSIPointerManagerPass(*PassRegistry::getPassRegistry());
}
void
AMDILSIPointerManager::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.setPreservesAll();
  AU.addRequiredID(MachineDominatorsID);
  MachineFunctionPass::getAnalysisUsage(AU);
}
const char*
AMDILSIPointerManager::getPassName() const
{
  return "AMD IL SI Pointer Manager Pass";
}
bool
AMDILSIPointerManager::runOnMachineFunction(MachineFunction &MF)
{
  if (DEBUGME) {
    dbgs() << getPassName() << "\n";
    dbgs() << MF.getFunction()->getName() << "\n";
    MF.dump();
  }

  const TargetMachine& TM = MF.getTarget();
  AMDILSIPointerManagerImpl impl(MF, TM);
  bool changed = impl.perform();

  return changed;
}
