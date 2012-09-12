//===-- AMDILPointerManagerImpl.h -----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The AMDIL Pointer Manager is a class that does all the checking for
// different pointer characteristics. Pointers have attributes that need to be attached
// to them in order to correctly codegen them efficiently. This class will
// analyze the pointers of a function and then traverse the uses of the pointers and
// determine if a pointer can be cached, should belong in the arena, and what UAV it
// should belong to. There are seperate classes for each unique generation of
// devices. This pass only works in SSA form.
//
//===----------------------------------------------------------------------===//

#ifndef _AMDIL_POINTER_MANAGER_IMPL_H_
#define _AMDIL_POINTER_MANAGER_IMPL_H_

#include "AMDIL.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetMachine.h"
#include <set>
#include <map>
#include <list>
#include <queue>

namespace llvm {
class Value;
class MachineBasicBlock;
class AMDILModuleInfo;

// The default pointer manager implementation.
// This pointer manager implementation just assigns the default ID's to
// each load/store instruction and does nothing else. This is
// the pointer manager for the 7XX series of cards.
class AMDILPointerManagerImpl {
protected:
  // Helper function that allocates the default resource ID for the
  // respective I/O types.
  void allocateDefaultID(AMDILAS::InstrResEnc &curRes,
                         MachineInstr *MI,
                         bool addID);
  void clearTempMIFlags(MachineFunction &MF);

  // The first thing we should do is to allocate the default
  // ID for each load/store/atomic instruction so that
  // it is correctly allocated. Everything else after this
  // is just an optimization to more efficiently allocate
  // resource ID's.
  void allocateDefaultIDs();

public:
  AMDILPointerManagerImpl(MachineFunction& mf, const TargetMachine& tm);
  ~AMDILPointerManagerImpl() {
  }

  // the main driver function
  virtual bool perform();

protected:
  MachineFunction& MF;
  const TargetMachine &TM;
  AMDILMachineFunctionInfo *mMFI;
  const AMDILTargetMachine *ATM;
  const AMDILSubtarget *STM;
  AMDILKernelManager *KM;
  AMDILModuleInfo* mAMI;
};

// The EG pointer manager implementation.
// This pointer manager implementation allocates and trackes
// cached memory, arena resources, raw resources and
// whether multi-uav is utilized or not.
class AMDILEGPointerManagerImpl : public AMDILPointerManagerImpl {
protected:
  // A pair that maps a name of string to a value.
  typedef std::pair<StringRef, const Value*> StrValPair;

  // Typedefing the multiple different set types to that it is
  // easier to read what each set is supposed to handle. This
  // also allows it easier to track which set goes to which
  // argument in a function call.
  typedef std::set<StrValPair> PtrSet;

  typedef std::set<const Value*> ValueSet;

  // A Byte set is the set of all base pointers that must
  // be allocated to the arena path.
  typedef PtrSet ByteSet;

  // A Raw set is the set of all base pointers that can be
  // allocated to the raw path.
  typedef PtrSet RawSet;

  // A cacheable set is the set of all base pointers that
  // are deamed cacheable based on annotations or
  // compiler options.
  typedef PtrSet CacheableSet;

  // A conflict set is a set of all base pointers whose
  // use/def chains conflict with another base pointer.
  typedef PtrSet ConflictSet;

  // An image set is a set of all read/write only image pointers.
  typedef PtrSet ImageSet;

  // An append set is a set of atomic counter base pointers
  typedef std::vector<StrValPair> AppendSet;

  // An append set is a set of atomic counter base pointers
  typedef std::vector<StrValPair> SemaSet;

  // A ConstantSet is a set of constant pool instructions
  typedef std::set<MachineInstr*> CPoolSet;

  // A CacheableInstSet set is a set of instructions that are cachable
  // even if the pointer is not generally cacheable.
  typedef std::set<MachineInstr*> CacheableInstrSet;

  // A pair that maps a virtual register to the equivalent base
  // pointer value that it was derived from.
  typedef std::pair<unsigned, StrValPair> RegValPair;

  // A map that maps between the base pointe rvalue and an array
  // of instructions that are part of the pointer chain. A pointer
  // chain is a recursive def/use chain of all instructions that don't
  // store data to memory unless the pointer is the data being stored.
  typedef std::map<const Value*, std::vector<MachineInstr*> > PtrIMap;

  // A map that holds a set of all base pointers that are used in a machine
  // instruction. This helps to detect when conflict pointers are found
  // such as when pointer subtraction occurs.
  typedef std::map<MachineInstr*, PtrSet> InstPMap;

  // A map that holds the frame index to RegValPair so that writes of
  // pointers to the stack can be tracked.
  typedef std::map<unsigned, RegValPair > FIPMap;

  // A small vector impl that holds all of the register to base pointer
  // mappings for a given function.
  typedef std::map<unsigned, RegValPair> RVPVec;

  typedef SmallSet<const Value*, 1> SmallValSet;
  // map a Value to the list of local arrays that it accesses
  typedef std::map<unsigned, SmallValSet> Reg2ValSet;

  // list of local array sets
  typedef std::vector<SmallValSet> SmallValSets;

  // map from a local array to the index of the local array set
  // that it belongs
  typedef std::map<const Value*, unsigned> Val2SetIDMap;

  typedef std::map<unsigned, std::vector<MachineInstr*> > ValSetId2InstsMap;

  // list of <local array, addri inst> pairs that makes it easy to find
  // the only addri that corresponds to a local array
  typedef std::pair<const GlobalValue*, MachineInstr*> GVInstPair;
  typedef std::vector<GVInstPair> GVInstPairVec;

  // Information related to the cacheability of instructions in a basic block.
  // This is used during the parse phase of the pointer algorithm to track
  // the reachability of stores within a basic block.
  class BlockCacheableInfo {
  public:
    BlockCacheableInfo() :
      mStoreReachesTop(false),
      mStoreReachesExit(false),
      mCacheableSet()
    {
    };

    bool storeReachesTop() const {
      return mStoreReachesTop;
    }
    bool storeReachesExit() const {
      return mStoreReachesExit;
    }
    CacheableInstrSet::const_iterator
    cacheableBegin() const {
      return mCacheableSet.begin();
    }
    CacheableInstrSet::const_iterator
    cacheableEnd()   const {
      return mCacheableSet.end();
    }

    // mark the block as having a global store that reaches it. This
    // will also set the store reaches exit flag, and clear the list
    // of loads (since they are now reachable by a store.)
    bool setReachesTop() {
      bool changedExit = !mStoreReachesExit;

      if (!mStoreReachesTop)
        mCacheableSet.clear();

      mStoreReachesTop = true;
      mStoreReachesExit = true;
      return changedExit;
    }
    // Mark the block as having a store that reaches the exit of the
    // block.
    void setReachesExit() {
      mStoreReachesExit = true;
    }
    // If the top or the exit of the block are not marked as reachable
    // by a store, add the load to the list of cacheable loads.
    void addPossiblyCacheableInst(MachineInstr *load) {
      // By definition, if store reaches top, then store reaches exit.
      // So, we only test for exit here.
      // If we have a volatile load we cannot cache it.
      if (mStoreReachesExit || isVolatileInst(load)) {
        return;
      }

      mCacheableSet.insert(load);
    }
  private:
    bool mStoreReachesTop;     // Does a global store reach the top of this block?
    bool mStoreReachesExit;    // Does a global store reach the exit of this block?
    CacheableInstrSet mCacheableSet;     // The set of loads in the block not
    // reachable by a global store.
  };

  // Map from MachineBasicBlock to it's cacheable load info.
  typedef std::map<MachineBasicBlock*, BlockCacheableInfo> MBBCacheableMap;

protected:
  // A set of all pointers are tracked in this map and
  // if multiple pointers are detected, they go to the same
  // set.
  PtrIMap PtrToInstMap;

  // All of the instructions that are loads, stores or pointer
  // conflicts are tracked in the map with a set of all values
  // that reference the instruction stored.
  InstPMap InstToPtrMap;

  // In order to track across stack entries, we need a map between a
  // frame index and a pointer. That way when we load from a frame
  // index, we know what pointer was stored to the frame index.
  FIPMap FIToPtrMap;

  // Set of all the pointers that are byte pointers. Byte pointers
  // are required to have their instructions go to the arena.
  ByteSet bytePtrs;

  // Set of all the pointers that are cacheable. All of the cache pointers
  // are required to go to a raw uav and cannot go to arena.
  CacheableSet cacheablePtrs;

  // Set of all the pointers that go into a raw buffer. A pointer can
  // exist in either rawPtrs or bytePtrs but not both.
  RawSet rawPtrs;

  // Set of all the pointers that end up having a conflicting instruction
  // somewhere in the pointer path.
  ConflictSet conflictPtrs;

  // Set of all the pointers that are local pointers.
  ValueSet localPtrs;

  // Set of all pointers that are images
  ImageSet images;

  // Set of all pointers that are counters
  AppendSet counters;

  // Set of all pointers that are semaphores
  SemaSet semaphores;

  // Set of all pointers that load from a constant pool
  CPoolSet cpool;

  // Mapping from BB to infomation about the cacheability of the
  // global load instructions in it.
  MBBCacheableMap bbCacheable;

  // A set of load instructions that are cacheable
  // even if all the load instructions of the ptr are not.
  CacheableInstrSet cacheableSet;

  // The lookup table holds all of the registers that
  // are used as we assign pointers values to them.
  // If two pointers collide on the lookup table, then
  // we assign them to the same UAV. If one of the
  // pointers is byte addressable, then we assign
  // them to arena, otherwise we assign them to raw.
  RVPVec lookupTable;

  // A map that maps a register to a set of local pointers
  Reg2ValSet localPtrMap;

  // a list of local pointer sets
  SmallValSets localPtrSets;

  // map a local pointer to the index of the local pointer set that it belongs
  Val2SetIDMap localPtr2SetIdMap;

  // Maps a local pointer set to the set of load/store instructions that
  // access local pointers in the set
  ValSetId2InstsMap localSetId2InstMap;

  // Maps a local pointer to the only Addri instruction that cor
  GVInstPairVec localNAddriVec;

  uint32_t numWriteImages;

  bool doPerPointerLDS;

protected:
  StrValPair createStrValPair(const Value* ptr)
  {
    return std::make_pair(ptr ? ptr->getName() : StringRef(""), ptr);
  }
  // initialize localPtrSets and localPtr2SetIdMap
  void initializeLocalPtrSets();

  std::string findSamplerName(MachineInstr* MI, unsigned &val);
  std::string findSamplerNameFromReg(unsigned reg, unsigned &val);

  // Given a load or store instruction, if it's a local load or store,
  // and if its pointer oper derives from multiple local pointers, then
  // add the local pointers to the conflict local pointer set.
  void detectConflictLocalPtrs(MachineInstr *MI,
                               unsigned reg, const AMDILSubtarget *STM);

  // Function that parses the arguments and updates the lookupTable with the
  // pointer -> register mapping. This function also checks for cacheable
  // pointers and updates the CacheableSet with the arguments that
  // can be cached based on the readonlypointer annotation. The final
  // purpose of this function is to update the imageSet and counterSet
  // with all pointers that are either images or atomic counters.
  uint32_t parseArguments();

  void parseLocalArrays();

  // The call stack is interesting in that even in SSA form, it assigns
  // registers to the same value's over and over again. So we need to
  // ignore the values that are assigned and just deal with the input
  // and return registers.
  void parseCall(MachineBasicBlock::iterator &mBegin,
                 MachineBasicBlock::iterator mEnd);

  // Detect if the current instruction conflicts with another instruction
  // and add the instruction to the correct location accordingly.
  void detectConflictInst(MachineInstr *MI,
                          AMDILAS::InstrResEnc &curRes,
                          bool isLoadStore,
                          unsigned reg,
                          unsigned dstReg);

  // In this case we want to handle a load instruction.
  void parseLoadInst(MachineInstr *MI);

  // In this case we want to handle a store instruction.
  void parseStoreInst(MachineInstr *MI);

  // In this case we want to handle an atomic instruction.
  void parseAtomicInst(MachineInstr *MI);

  // In this case we want to handle a counter instruction.
  void parseAppendInst(MachineInstr *MI);

  /// In this case we want to handle a semaphore instruction.
  void parseSemaInst(MachineInstr *MI);

  // In this case we want to handle an Image instruction.
  void parseImageInst(MachineInstr *MI);

  // if addri's address is a local array, map addri's dest reg to
  // the local array
  void parseAddriInst(MachineInstr *MI);

  // This case handles the rest of the instructions
  void parseInstruction(MachineInstr *MI);

  // This function parses the basic block and based on the instruction type,
  // calls the function to finish parsing the instruction.
  void parseBasicBlock(MachineBasicBlock *MB);

  // Follows the Reverse Post Order Traversal of the basic blocks to
  // determine which order to parse basic blocks in.
  void parseFunction();

  // Helper function that dumps to dbgs() information about
  // a pointer set.
  void dumpPointers(AppendSet &Ptrs, const char *str);

  // Helper function that dumps to dbgs() information about
  // a pointer set.
  void dumpPointers(PtrSet &Ptrs, const char *str);

  // Function that detects all the conflicting pointers and adds
  // the pointers that are detected to the conflict set, otherwise
  // they are added to the raw or byte set based on their usage.
  void detectConflictingPointers();

  // Function that detects aliased constant pool operations.
  void detectAliasedCPoolOps();

  // Function that detects fully cacheable pointers. Fully cacheable pointers
  // are pointers that have no writes to them and no-alias is specified.
  void detectFullyCacheablePointers();

  // Are any of the pointers in PtrSet also in the BytePtrs or the CachePtrs?
  bool ptrSetIntersectsByteOrCache(PtrSet &cacheSet);

  // Function that detects which instructions are cacheable even if
  // all instructions of the pointer are not cacheable. The resulting
  // set of instructions will not contain Ptrs that are in the cacheable
  // ptr set (under the assumption they will get marked cacheable already)
  // or pointers in the byte set, since they are not cacheable.
  void detectCacheableInstrs();

  // This function annotates the cacheable pointers with the
  // CacheableRead bit. The cacheable read bit is set
  // when the number of write images is not equal to the max
  // or if the default RAW_UAV_ID is equal to 11. The first
  // condition means that there is a raw uav between 0 and 7
  // that is available for cacheable reads and the second
  // condition means that UAV 11 is available for cacheable
  // reads.
  virtual void annotateCacheablePtrs();

  // A byte pointer is a pointer that along the pointer path has a
  // byte store assigned to it.
  virtual void annotateBytePtrs();

  // A semaphore pointer is a opaque object that has semaphore instructions
  // in its path.
  virtual void annotateSemaPtrs();

  /// An append pointer is a opaque object that has append instructions
  // in its path.
  virtual void annotateAppendPtrs();

  // A raw pointer is any pointer that does not have byte store in its path.
  virtual void annotateRawPtrs();

  virtual void annotateCacheableInstrs();

  virtual void annotateLocalPtrs();

  // Annotate the instructions along various pointer paths. The paths that
  // are handled are the raw, byte, cacheable and local pointer paths.
  virtual void annotatePtrPath();

  // Allocate MultiUAV pointer ID's for the raw/conflict pointers.
  void allocateMultiUAVPointers();

  void replaceAddri();

public:
  AMDILEGPointerManagerImpl(MachineFunction& mf, const TargetMachine& tm);
  ~AMDILEGPointerManagerImpl() {
  }

  // the main driver function
  virtual bool perform();
};   // class AMDILEGPointerManagerImpl

class AMDILSIPointerManagerImpl : public AMDILEGPointerManagerImpl {
public:
  AMDILSIPointerManagerImpl(MachineFunction &mf, const TargetMachine &tm)
    : AMDILEGPointerManagerImpl(mf, tm) {
  }
  virtual ~AMDILSIPointerManagerImpl() {
  }
protected:
  virtual void annotateBytePtrs();
  virtual void annotateRawPtrs();
  virtual void annotateCacheablePtrs();
  virtual void annotateCacheableInstrs();
};
} // end llvm namespace
#endif // _AMDIL_POINTER_MANAGER_IMPL_H_
