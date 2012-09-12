//===-- AMDILCFGStructurizer.cpp ------------------------------------------===//
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

#define DEBUG_TYPE "structcfg"
#if !defined(NDEBUG)
#define DEBUGME (DebugFlag && isCurrentDebugType(DEBUG_TYPE))
#else
#define DEBUGME 0
#endif

#include "llvm/Support/Debug.h"
#include "AMDILCompilerErrors.h"
#include "AMDILMachineFunctionInfo.h"
#include "AMDILTargetMachine.h"
#include "AMDILUtilityFunctions.h"
#include "llvm/InitializePasses.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"

#define FirstNonDebugInstr(A) A->begin()
using namespace llvm;

// bixia TODO: move this out to analysis lib. Make this work for both target
// AMDIL and CBackend.
// TODO: move-begin.

//===----------------------------------------------------------------------===//
//
// Statistics for CFGStructurizer.
//
//===----------------------------------------------------------------------===//

STATISTIC(numSerialPatternMatch,    "CFGStructurizer number of serial pattern "
          "matched");
STATISTIC(numIfPatternMatch,        "CFGStructurizer number of if pattern "
          "matched");
STATISTIC(numLoopbreakPatternMatch, "CFGStructurizer number of loop-break "
          "pattern matched");
STATISTIC(numLoopcontPatternMatch,  "CFGStructurizer number of loop-continue "
          "pattern matched");
STATISTIC(numLoopPatternMatch,      "CFGStructurizer number of loop pattern "
          "matched");
STATISTIC(numClonedBlock,           "CFGStructurizer cloned blocks");
STATISTIC(numClonedInstr,           "CFGStructurizer cloned instructions");

//===----------------------------------------------------------------------===//
//
// Miscellaneous utility for CFGStructurizer.
//
//===----------------------------------------------------------------------===//
namespace llvmCFGStruct
{
#define SHOWNEWINSTR(i) \
  if (DEBUGME) errs() << "New instr: " << *i << "\n"

#define SHOWNEWBLK(b, msg) \
  if (DEBUGME) { \
    errs() << msg << "BB" << b->getNumber() << "size " << b->size(); \
    errs() << "\n"; \
  }

#define SHOWBLK_DETAIL(b, msg) \
  if (DEBUGME) { \
    if (b) { \
      errs() << msg << "BB" << b->getNumber() << "size " << b->size(); \
      b->print(errs()); \
      errs() << "\n"; \
    } \
  }

#define INVALIDSCCNUM -1
#define INVALIDREGNUM 0

template<class LoopinfoT>
void PrintLoopinfo(const LoopinfoT &LoopInfo, llvm::raw_ostream &OS) {
  for (typename LoopinfoT::iterator iter = LoopInfo.begin(),
       iterEnd = LoopInfo.end();
       iter != iterEnd; ++iter) {
    (*iter)->print(OS, 0);
  }
}
template<class NodeT>
void ReverseVector(SmallVector<NodeT *, DEFAULT_VEC_SLOTS> &Src) {
  size_t sz = Src.size();
  for (size_t i = 0; i < sz/2; ++i) {
    NodeT *t = Src[i];
    Src[i] = Src[sz - i - 1];
    Src[sz - i - 1] = t;
  }
}
} //end namespace llvmCFGStruct

static MachineInstr *getLastBreakInstr(MachineBasicBlock *blk) {
  for (MachineBasicBlock::reverse_iterator iter = blk->rbegin();
       (iter != blk->rend());
       ++iter) {
    MachineInstr *instr = &(*iter);
    if ((instr->getOpcode() == AMDIL::BREAK_LOGICALNZi32r) ||
        (instr->getOpcode() == AMDIL::BREAK_LOGICALZi32r)) {
      return instr;
    }
  }
  return NULL;
}
//===----------------------------------------------------------------------===//
//
// MachinePostDominatorTree
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/DominatorInternals.h"

namespace llvm {
extern void initializeMachinePostDominatorTreePass(PassRegistry&);

/// PostDominatorTree Class - Concrete subclass of DominatorTree that is used
/// to compute the a post-dominator tree.
///
struct MachinePostDominatorTree : public MachineFunctionPass {
  static char ID; // Pass identification, replacement for typeid
  DominatorTreeBase<MachineBasicBlock> *DT;
  MachinePostDominatorTree() : MachineFunctionPass(ID)
  {
    initializeMachinePostDominatorTreePass(*PassRegistry::getPassRegistry());
    DT = new DominatorTreeBase<MachineBasicBlock>(true); //true indicate
    // postdominator
  }
  ~MachinePostDominatorTree();

  virtual bool runOnMachineFunction(MachineFunction &MF);

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  inline const std::vector<MachineBasicBlock *> &getRoots() const {
    return DT->getRoots();
  }
  inline MachineDomTreeNode *getRootNode() const {
    return DT->getRootNode();
  }
  inline MachineDomTreeNode *operator[](MachineBasicBlock *BB) const {
    return DT->getNode(BB);
  }
  inline MachineDomTreeNode *getNode(MachineBasicBlock *BB) const {
    return DT->getNode(BB);
  }
  inline bool dominates(MachineDomTreeNode *A, MachineDomTreeNode *B) const {
    return DT->dominates(A, B);
  }
  inline bool dominates(MachineBasicBlock *A, MachineBasicBlock *B) const {
    return DT->dominates(A, B);
  }
  inline bool
  properlyDominates(const MachineDomTreeNode *A, MachineDomTreeNode *B) const {
    return DT->properlyDominates(A, B);
  }
  inline bool
  properlyDominates(MachineBasicBlock *A, MachineBasicBlock *B) const {
    return DT->properlyDominates(A, B);
  }
  inline MachineBasicBlock *
  findNearestCommonDominator(MachineBasicBlock *A, MachineBasicBlock *B) {
    return DT->findNearestCommonDominator(A, B);
  }
  virtual void print(llvm::raw_ostream &OS, const Module *M = 0) const {
    DT->print(OS);
  }
};
} //end of namespace llvm

char MachinePostDominatorTree::ID = 0;
INITIALIZE_PASS(MachinePostDominatorTree, "machinepostdomtree",
                "MachinePostDominator Tree Construction",
                true, true);

FunctionPass *llvm::createMachinePostDominatorTreePass() {
  return new MachinePostDominatorTree();
}
//const PassInfo *const llvm::MachinePostDominatorsID
//= &machinePostDominatorTreePass;

bool MachinePostDominatorTree::runOnMachineFunction(MachineFunction &F) {
  DT->recalculate(F);
  //DEBUG(DT->dump());
  return false;
}
MachinePostDominatorTree::~MachinePostDominatorTree() {
  delete DT;
}
//===----------------------------------------------------------------------===//
//
// supporting data structure for CFGStructurizer
//
//===----------------------------------------------------------------------===//

namespace llvmCFGStruct
{
template<class PassT>
struct CFGStructTraits {
};

template <class InstrT>
class BlockInformation {
public:
  bool isRetired;
  int sccNum;
  //SmallVector<InstrT*, DEFAULT_VEC_SLOTS> succInstr;
  //Instructions defining the corresponding successor.
  BlockInformation() : isRetired(false), sccNum(INVALIDSCCNUM) {
  }
};

template <class BlockT, class InstrT, class RegiT>
class LandInformation {
public:
  BlockT *landBlk;
  std::set<RegiT> breakInitRegs;  //Registers that need to "reg = 0", before
                                  //WHILELOOP(thisloop) init before entering
                                  //thisloop.
  std::set<RegiT> contInitRegs;   //Registers that need to "reg = 0", after
                                  //WHILELOOP(thisloop) init after entering
                                  //thisloop.
  std::set<RegiT> endbranchInitRegs; //Init after entering this loop, at loop
                                     //land block, branch cond on this reg.
  std::set<RegiT> breakOnRegs;       //registers that need to "if (reg) break
                                     //endif" after ENDLOOP(thisloop) break
                                     //outerLoopOf(thisLoop).
  std::set<RegiT> contOnRegs;       //registers that need to "if (reg) continue
                                    //endif" after ENDLOOP(thisloop) continue on
                                    //outerLoopOf(thisLoop).
  LandInformation() : landBlk(NULL) {
  }
};
} //end of namespace llvmCFGStruct

//===----------------------------------------------------------------------===//
//
// TrivialRegAlloc
//
//===----------------------------------------------------------------------===//

namespace llvmCFGStruct
{
// Stores the list of defs and uses of a virtual register
class DefUseList {
enum {
  FLAG_DEF = 0,
  FLAG_USE = 1
};

public:
  // struct that represents a single def or use
  struct DefOrUseT {
    unsigned _slotIndex;
    unsigned _flag; // flag whether this is a def or use
    bool isDef() const {
      return _flag == FLAG_DEF;
    }
    bool isUse() const {
      return _flag == FLAG_USE;
    }
    DefOrUseT(unsigned slotIndex, unsigned flag)
      : _slotIndex(slotIndex), _flag(flag) {
    }
  };

private:
  typedef SmallVector<DefOrUseT, 2> DefUseVecT;

public:
  typedef DefUseVecT::iterator iterator;
  typedef DefUseVecT::const_iterator const_iterator;

  DefUseVecT _defUses;

  DefUseList() : _defUses() {
  }
  void addDef(unsigned slotIndex) {
    _defUses.push_back(DefOrUseT(slotIndex, FLAG_DEF));
  }
  void addUse(unsigned slotIndex) {
    _defUses.push_back(DefOrUseT(slotIndex, FLAG_USE));
  }
  void clear() {
    _defUses.clear();
  }
  iterator begin() {
    return _defUses.begin();
  }
  const_iterator begin() const {
    return _defUses.begin();
  }
  iterator end() {
    return _defUses.end();
  }
  const_iterator end() const {
    return _defUses.end();
  }
  bool isSorted() const;
  void dump() const;
};

bool DefUseList::isSorted() const
{
  const_iterator it = begin();
  const_iterator e = end();
  assert(it != e && "no def/use");
  const_iterator pre = it;
  for (++it; it != e; ++it) {
    const DefOrUseT& defOrUse = *it;
    if ((*pre)._slotIndex > (*it)._slotIndex) {
      return false;
    }
    pre = it;
  }
  return true;
}
void DefUseList::dump() const
{
  for (const_iterator it = begin(), e = end(); it != e; ++it) {
    const DefOrUseT& defOrUse = *it;
    const char* str = defOrUse.isDef() ? "def" : "use";
    errs() << "    " << defOrUse._slotIndex << " " << str << "\n";
  }
}
// a live interval
class LiveInterval {
enum {
  UndefinedSlotIndex = -1
};
unsigned _vreg;
int _startSlotIndex;
int _endSlotIndex;

public:
  LiveInterval(unsigned vreg)
    : _vreg(vreg),
      _startSlotIndex(UndefinedSlotIndex),
      _endSlotIndex(UndefinedSlotIndex)
  {
  }
  bool hasStart() const {
    return _startSlotIndex != UndefinedSlotIndex;
  }
  bool hasEnd() const {
    return _endSlotIndex != UndefinedSlotIndex;
  }
  void setStart(int slotIndex) {
    _startSlotIndex = slotIndex;
  }
  void setEnd(int slotIndex) {
    _endSlotIndex = slotIndex;
  }
  unsigned vreg() const {
    return _vreg;
  }
  unsigned start() const {
    return _startSlotIndex;
  }
  unsigned end() const {
    return _endSlotIndex;
  }
};

// a list of live intervals
class LiveIntervals {
typedef SmallVector<LiveInterval, 16> IntervalVecType;

public:
  typedef IntervalVecType::iterator iterator;
  typedef IntervalVecType::const_iterator const_iterator;

private:
  IntervalVecType _intervals;
  bool _sorted; // whether the intervals are sorted by start position

private:
  iterator findIntervalImpl(unsigned vreg);

public:
  LiveIntervals(bool sorted) : _intervals(), _sorted(sorted) {
  }
  LiveInterval* findInterval(unsigned vreg) {
    iterator it = findIntervalImpl(vreg);
    if (it == _intervals.end()) {
      return NULL;
    }
    return &*it;
  }
  LiveInterval& createInterval(unsigned vreg) {
    _intervals.push_back(LiveInterval(vreg));
    return _intervals.back();
  }
  void appendInterval(LiveInterval& interval) {
    if (_sorted) {
      assert((_intervals.size() == 0
              || interval.start() >= _intervals.back().start())
             && "unsorted append into sorted LiveIntervals");
    }
    _intervals.push_back(interval);
  }
  void insertInterval(LiveInterval& interval) {
    if (!_sorted) {
      _intervals.push_back(interval);
      return;
    }
    insertIntervalSorted(interval);
  }
  void removeInterval(unsigned vreg);
  iterator removeInterval(iterator it) {
    return _intervals.erase(it);
  }
  void clear() {
    _intervals.clear();
  }
  iterator begin() {
    return _intervals.begin();
  }
  iterator end() {
    return _intervals.end();
  }
  bool isSortedByStart() const;
  void dump() const;

private:
  void insertIntervalSorted(LiveInterval& interval);
};

LiveIntervals::iterator LiveIntervals::findIntervalImpl(unsigned vreg)
{
  iterator it = _intervals.begin();
  iterator end = _intervals.end();
  for (; it != end; ++it) {
    if ((*it).vreg() == vreg) {
      break;
    }
  }
  return it;
}
void LiveIntervals::insertIntervalSorted(LiveInterval& interval)
{
  iterator it = _intervals.begin();
  iterator end = _intervals.end();
  for (; it != end; ++it) {
    if (interval.start() >= (*it).start()) {
      break;
    }
  }
  _intervals.insert(it, interval);
}
void LiveIntervals::removeInterval(unsigned vreg)
{
  iterator it = findIntervalImpl(vreg);
  assert(it != _intervals.end() && "interval not found");
  _intervals.erase(it);
}
bool LiveIntervals::isSortedByStart() const
{
  const_iterator it = _intervals.begin();
  const_iterator end = _intervals.end();
  if (it == end) {
    return true;
  }
  const_iterator pre = it;
  ++it;
  for (; it != end; ++it) {
    if ((*pre).start() > (*it).start()) {
      return false;
    }
    pre = it;
  }
  return true;
}
void LiveIntervals::dump() const
{
  errs() << "Intervals:\n";
  const_iterator it = _intervals.begin();
  const_iterator end = _intervals.end();
  for (; it != end; ++it) {
    const LiveInterval& interval = *it;
    errs() << "  vreg " << TargetRegisterInfo::virtReg2Index(interval.vreg())
           << " start " << interval.start()
           << " end " << interval.end() << "\n";
  }
}
// Trivial linear scan register allocator to allocate physical registers
// for registers requested during CFGStructurizer pass.
// Since register allocator has already been run before this pass, we have
// to define our own register allocator to do very simple register allocation
// for registers requested during this pass.
class TrivialRegAlloc
{
typedef SmallVector<LiveIntervals, 2>   IntervalsVecT;
typedef std::map<unsigned, unsigned>    RegMapT;
typedef std::set<unsigned>              RegSetT;
typedef std::map<unsigned, DefUseList*> VRegDefUseMapT;

private:
  // data structures passed in to this class

  MachineFunction& _func;
  const TargetRegisterClass& _regClass;
  // virtual registers that need physical registers to be allocated
  RegSetT& _vregs;

  // data structures created within this class

  VRegDefUseMapT _vregDefUseMap; // map vreg -> its def/use list
  BitVector _regInUse; // flags which registers are currently in use
  // set of physical registers that can be alloc'ed
  std::vector<unsigned> _regSet;
  RegMapT _regMap; // virtual to physical register map
  LiveIntervals _intervals; // list of all live intervals
  // transient list of currently active live intervals
  LiveIntervals _activeIntervals;
  // transient current interval for which we are trying to allocate a register
  LiveInterval* _currInterval;

private:
  void initRegSet();
  void computeIntervals();
  unsigned getPhysicalRegister();
  void allocateRegisterFor(LiveInterval& interval);
  void releaseRegisterFor(const LiveInterval& interval);
  void handleActiveIntervals(unsigned pos);
  void allocateRegisters();
  void rewrite();

public:
  TrivialRegAlloc(MachineFunction& func,
                  const TargetRegisterClass& regClass,
                  RegSetT& vregs);
  ~TrivialRegAlloc();
  void run(); // main driver of the algorithm
};

TrivialRegAlloc::TrivialRegAlloc(MachineFunction& func,
                                 const TargetRegisterClass& regClass,
                                 RegSetT& vregs)
  : _func(func), _regClass(regClass), _vregs(vregs),
    _vregDefUseMap(), _regInUse(), _regSet(), _regMap(),
    _intervals(true), _activeIntervals(false), _currInterval(NULL)
{
  assert(_regClass.getID() == AMDIL::GPRI32RegClassID && "unimplemented");
}
TrivialRegAlloc::~TrivialRegAlloc()
{
  for (VRegDefUseMapT::iterator I = _vregDefUseMap.begin(),
       E = _vregDefUseMap.end();
       I != E; ++I) {
    delete I->second;
  }
}
// find all physical registers that are still available after the global
// register allocator
static void findAvailPhysRegs(MachineFunction& func,
                              const class TargetRegisterClass& regClass,
                              std::vector<unsigned>& regSet)
{
  ArrayRef<uint16_t> AllocOrder = regClass.getRawAllocationOrder(func);
  for (const uint16_t *begin = AllocOrder.begin(), *end = AllocOrder.end();
       begin != end;
       ++begin) {
    uint16_t tempReg = *begin;
    if (func.getRegInfo().isPhysRegOrOverlapUsed(tempReg)) {
      continue;
    }

    if (tempReg) {
      regSet.push_back(tempReg);
    }
  }
}
// initialize the register set with remaining physical registers that are still
// available and the set of physical registers reserved for CFGStructurizer
void TrivialRegAlloc::initRegSet()
{
  findAvailPhysRegs(_func, _regClass, _regSet);
  for (unsigned i = AMDIL::CFG1; i <= AMDIL::CFG10; ++i) {
    _regSet.push_back(i);
  }
  _regInUse.resize(_regSet.size(), 0);
  if (DEBUGME) {
    errs() << "available physical registers:\n   ";
    for (std::vector<unsigned>::iterator it = _regSet.begin(),
         e = _regSet.end(); it != e; ++it) {
      errs() << " " << *it;
    }
    errs() << "\n";
  }
}
// compute live intervals for the virtual registers created during
// CFGStructurizer pass
void TrivialRegAlloc::computeIntervals() {
  MachineBasicBlock* entryBlk
    = GraphTraits<MachineFunction*>::nodes_begin(&_func);
  unsigned slotIndex = 0;
  // there is only one block now in the function
  for (MachineBasicBlock::iterator iter = entryBlk->begin(),
       iterEnd = entryBlk->end();
       iter != iterEnd;
       ++iter) {
    MachineInstr* inst = iter;
    for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
      MachineOperand& oper = inst->getOperand(i);
      if (!oper.isReg() || !oper.getReg()) {
        continue;
      }
      unsigned vreg = oper.getReg();
      // if not a virtual register that needs reg alloc, skip
      if (!_vregs.count(vreg)) {
        continue;
      }
      // add to vreg's def/use list
      DefUseList*& defUses = _vregDefUseMap[vreg];
      LiveInterval* interval = _intervals.findInterval(vreg);
      if (oper.isDef()) {
        if (defUses == NULL) {
          defUses = new DefUseList();
        }
        defUses->addDef(slotIndex);
        if (interval == NULL) {
          interval = &_intervals.createInterval(vreg);
        }
        if (!interval->hasStart()) {
          interval->setStart(slotIndex);
          if (DEBUGME)
            errs() << "interval for vreg "
                   << TargetRegisterInfo::virtReg2Index(vreg)
                   << " start at " << slotIndex << "\n";
        }
        else {
          assert(slotIndex > interval->start() && "sanity");
        }
      }
      else {
        assert(defUses && "use before def");
        defUses->addUse(slotIndex);
        assert(interval && "use before def");
        assert((!interval->hasEnd() || slotIndex > interval->end())
               && "sanity");
        interval->setEnd(slotIndex);
        if (DEBUGME)
          errs() << "interval for vreg "
                 << TargetRegisterInfo::virtReg2Index(vreg)
                 << " end at " << slotIndex << "\n";
      }
    }
    ++slotIndex;
  }
  if (DEBUGME) {
    _intervals.dump();
    errs() << "def/use map: \n";
    for (VRegDefUseMapT::const_iterator it = _vregDefUseMap.begin(),
         e = _vregDefUseMap.end(); it != e; ++it) {
      DefUseList* defUses = it->second;
      errs() << "  vreg "
             << TargetRegisterInfo::virtReg2Index(it->first) << "\n";
      defUses->dump();
    }
  }
  assert(_intervals.isSortedByStart() && "_intervals not sorted");
#ifndef NDEBUG
  for (VRegDefUseMapT::iterator I = _vregDefUseMap.begin(),
       E = _vregDefUseMap.end();
       I != E; ++I) {
    assert(I->second->isSorted() && "def/uses not sorted");
  }
#endif
}
// pick a physical register that is not in use
unsigned TrivialRegAlloc::getPhysicalRegister()
{
  for (unsigned i = 0; i < _regInUse.size(); ++i) {
    if (!_regInUse[i]) {
      _regInUse[i] = 1;
      return _regSet[i];
    }
  }
  // No physical register available. Has to spill.
  // TODO: add spiller
  abort();
  return 0;
}
// allocate a physical register for the live interval
void TrivialRegAlloc::allocateRegisterFor(LiveInterval& interval)
{
  _currInterval = &interval;
  unsigned vreg = interval.vreg();
  unsigned physicalReg = getPhysicalRegister();
  _regMap[vreg] = physicalReg;
  if (DEBUGME)
    errs() << "allocated reg " << physicalReg << " to vreg "
           << TargetRegisterInfo::virtReg2Index(vreg) << "\n";
// _func->getRegInfo().setPhysRegUsed(tempReg);
}
// release physical register allocated for the interval
void TrivialRegAlloc::releaseRegisterFor(const LiveInterval& interval)
{
  unsigned physicalReg = _regMap[interval.vreg()];
  unsigned i = 0;
  for (; i < _regSet.size(); ++i) {
    if (_regSet[i] == physicalReg) {
      break;
    }
  }
  assert(i < _regSet.size() && "invalid physical regsiter");
  _regInUse[i] = 0;
}
// remove out of active intervals list if an interval becomes inactive
void TrivialRegAlloc::handleActiveIntervals(unsigned pos)
{
  for (LiveIntervals::iterator it = _activeIntervals.begin();
       it != _activeIntervals.end(); ) {
    LiveInterval& interval = *it;
    if (pos > interval.end()) {
      releaseRegisterFor(interval);
      it = _activeIntervals.removeInterval(it);
    }
    else {
      ++it;
    }
  }
}
// allocate physical registers for each live interval in the interval list
void TrivialRegAlloc::allocateRegisters()
{
  // intervals that just become active
  for (LiveIntervals::iterator it = _intervals.begin(),
       end = _intervals.end();
       it != end;
       ++it) {
    LiveInterval& interval = *it;
    // remove intervals that become inactive out of active list
    handleActiveIntervals(interval.start());
    // interval becomes active
    _activeIntervals.appendInterval(interval);
    // allocate registers for interval that just becomes active
    allocateRegisterFor(interval);
  }
}
// rewrite the machine instructions to use the physical registers allocated
void TrivialRegAlloc::rewrite() {
  MachineBasicBlock* entryBlk
    = GraphTraits<MachineFunction*>::nodes_begin(&_func);
  // there is only one block now in the function
  for (MachineBasicBlock::iterator iter = entryBlk->begin(),
       iterEnd = entryBlk->end();
       iter != iterEnd;
       ++iter) {
    MachineInstr* inst = iter;
    for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
      MachineOperand& oper = inst->getOperand(i);
      if (!oper.isReg() || !oper.getReg()) {
        continue;
      }
      unsigned vreg = oper.getReg();
      // if not a virtual register that needs reg alloc, skip
      if (!_vregs.count(vreg)) {
        continue;
      }
      assert(_regMap.find(vreg) != _regMap.end() && "register not allocated");
      unsigned physicalReg = _regMap[vreg];
      oper.setReg(physicalReg);
    }
  }
}
// the main driver of this register allocator
void TrivialRegAlloc::run()
{
  initRegSet();
  computeIntervals();
  allocateRegisters();
  rewrite();
}
//===----------------------------------------------------------------------===//
//
// CFGStructurizer
//
//===----------------------------------------------------------------------===//

// bixia TODO: port it to BasicBlock, not just MachineBasicBlock.
template<class PassT>
class CFGStructurizer
{
public:
  typedef enum {
    Not_SinglePath = 0,
    SinglePath_InPath = 1,
    SinglePath_NotInPath = 2
  } PathToKind;

public:
  typedef typename PassT::InstructionType InstrT;
  typedef typename PassT::FunctionType FuncT;
  typedef typename PassT::DominatortreeType DomTreeT;
  typedef typename PassT::PostDominatortreeType PostDomTreeT;
  typedef typename PassT::DomTreeNodeType DomTreeNodeT;
  typedef typename PassT::LoopinfoType LoopInfoT;

  typedef GraphTraits<FuncT *>                    FuncGTraits;
  //typedef FuncGTraits::nodes_iterator BlockIterator;
  typedef typename FuncT::iterator BlockIterator;

  typedef typename FuncGTraits::NodeType BlockT;
  typedef GraphTraits<BlockT *>                   BlockGTraits;
  typedef GraphTraits<Inverse<BlockT *> >         InvBlockGTraits;
  //typedef BlockGTraits::succ_iterator InstructionIterator;
  typedef typename BlockT::iterator InstrIterator;

  typedef CFGStructTraits<PassT>                  CFGTraits;
  typedef BlockInformation<InstrT>                BlockInfo;
  typedef std::map<BlockT *, BlockInfo *>         BlockInfoMap;

  typedef int RegiT;
  typedef typename PassT::LoopType LoopT;
  typedef LandInformation<BlockT, InstrT, RegiT>  LoopLandInfo;
  typedef std::map<LoopT *, LoopLandInfo *> LoopLandInfoMap;
  //landing info for loop break
  typedef SmallVector<BlockT *, 32>               BlockTSmallerVector;

public:
  CFGStructurizer();
  ~CFGStructurizer();

  /// Perform the CFG structurization
  bool run(FuncT &Func, PassT &Pass);

  /// Perform the CFG preparation
  bool prepare(FuncT &Func, PassT &Pass);

private:
  void   orderBlocks();
  void   printOrderedBlocks(llvm::raw_ostream &OS);

  void processAddedToTraversalBlocks();

  int patternMatch(BlockT *CurBlock);
  int patternMatchGroup(BlockT *CurBlock);

  int serialPatternMatch(BlockT *CurBlock);
  int ifPatternMatch(BlockT *CurBlock);
  int switchPatternMatch(BlockT *CurBlock);
  int loopendPatternMatch(BlockT *CurBlock);
  int loopPatternMatch(BlockT *CurBlock);

  int loopbreakPatternMatch(LoopT *LoopRep, BlockT *LoopHeader);
  int loopcontPatternMatch(LoopT *LoopRep, BlockT *LoopHeader);
  //int loopWithoutBreak(BlockT *);

  inline int getRegister(const class TargetRegisterClass *RegClass);
  void handleLoopbreak(BlockT *ExitingBlock, LoopT *ExitingLoop,
                       BlockT *ExitBlock, LoopT *exitLoop, BlockT *landBlock);
  void handleLoopcontBlock(BlockT *ContingBlock, LoopT *contingLoop,
                           BlockT *ContBlock, LoopT *contLoop);
  bool isSameloopDetachedContbreak(BlockT *Src1Block, BlockT *Src2Block);
  int handleJumpintoIf(BlockT *HeadBlock, BlockT *TrueBlock,
                       BlockT *FalseBlock);
  int handleJumpintoIfImp(BlockT *HeadBlock, BlockT *TrueBlock,
                          BlockT *FalseBlock);
  int improveSimpleJumpintoIf(BlockT *HeadBlock, BlockT *TrueBlock,
                              BlockT *FalseBlock, BlockT **LandBlockPtr);
  void showImproveSimpleJumpintoIf(BlockT *HeadBlock, BlockT *TrueBlock,
                                   BlockT *FalseBlock, BlockT *LandBlock,
                                   bool Detail = false);
  PathToKind singlePathTo(BlockT *SrcBlock, BlockT *DstBlock,
                          bool AllowSideEntry = true);
  BlockT *singlePathEnd(BlockT *srcBlock, BlockT *DstBlock,
                        bool AllowSideEntry = true);
  int cloneOnSideEntryTo(BlockT *PreBlock, BlockT *SrcBlock, BlockT *DstBlock);
  void mergeSerialBlock(BlockT *DstBlock, BlockT *srcBlock);

  void mergeIfthenelseBlock(InstrT *BranchInstr, BlockT *CurBlock,
                            BlockT *TrueBlock, BlockT *FalseBlock,
                            BlockT *LandBlock);
  void mergeLooplandBlock(BlockT *DstBlock, LoopLandInfo *LoopLand);
  void mergeLoopbreakBlock(BlockT *ExitingBlock, BlockT *ExitBlock,
                           BlockT *ExitLandBlock, RegiT SetReg);
  void settleLoopcontBlock(BlockT *ContingBlock, BlockT *ContBlock,
                           RegiT SetReg);
  BlockT *relocateLoopcontBlock(LoopT *ParentLoopRep, LoopT *LoopRep,
                                std::set<BlockT*> &ExitBlockSet,
                                BlockT *ExitLandBlk);
  BlockT *addLoopEndbranchBlock(LoopT *LoopRep,
                                BlockTSmallerVector &ExitingBlocks,
                                BlockTSmallerVector &ExitBlocks);
  BlockT *normalizeInfiniteLoopExit(LoopT *LoopRep);
  void removeUnconditionalBranch(BlockT *SrcBlock);
  void removeRedundantConditionalBranch(BlockT *SrcBlock);
  void addDummyExitBlock(SmallVector<BlockT *, DEFAULT_VEC_SLOTS> &RetBlocks);

  void removeSuccessor(BlockT *SrcBlock);
  BlockT *cloneBlockForPredecessor(BlockT *CurBlock, BlockT *PredBlock);
  BlockT *exitingBlock2ExitBlock (LoopT *LoopRep, BlockT *exitingBlock);

  void migrateInstruction(BlockT *SrcBlock, BlockT *DstBlock,
                          InstrIterator InsertPos);

  void recordSccnum(BlockT *SrcBlock, int SCCNum);
  int getSCCNum(BlockT *srcBlk);

  void retireBlock(BlockT *DstBlock, BlockT *SrcBlock);
  bool isRetiredBlock(BlockT *SrcBlock);
  bool isActiveLoophead(BlockT *CurBlock);
  bool needMigrateBlock(BlockT *Block);

  void addToTraversalBlock(BlockT *srcBlock);

  BlockT *recordLoopLandBlock(LoopT *LoopRep, BlockT *LandBlock,
                              BlockTSmallerVector &exitBlocks,
                              std::set<BlockT*> &ExitBlockSet);
  void setLoopLandBlock(LoopT *LoopRep, BlockT *Block = NULL);
  BlockT *getLoopLandBlock(LoopT *LoopRep);
  LoopLandInfo *getLoopLandInfo(LoopT *LoopRep);

  void addLoopBreakOnReg(LoopT *LoopRep, RegiT RegNum);
  void addLoopContOnReg(LoopT *LoopRep, RegiT RegNum);
  void addLoopBreakInitReg(LoopT *LoopRep, RegiT RegNum);
  void addLoopContInitReg(LoopT *LoopRep, RegiT RegNum);
  void addLoopEndbranchInitReg(LoopT *LoopRep, RegiT RegNum);

  bool hasBackEdge(BlockT *curBlock);
  unsigned getLoopDepth  (LoopT *LoopRep);
  int countActiveBlock(
    typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator IterStart,
    typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator IterEnd);
  BlockT *findNearestCommonPostDom(std::set<BlockT *>&);
  BlockT *findNearestCommonPostDom(BlockT *Block1, BlockT *Block2);

private:
  DomTreeT *domTree;
  PostDomTreeT *postDomTree;
  LoopInfoT *loopInfo;
  PassT *passRep;
  FuncT *funcRep;

  BlockInfoMap blockInfoMap;
  LoopLandInfoMap loopLandInfoMap;
  SmallVector<BlockT *, DEFAULT_VEC_SLOTS> orderedBlks;
  SmallVector<BlockT *, DEFAULT_VEC_SLOTS> addedToTraversalBlks;
  std::set<unsigned> vregs; // new virtual registers created
};  //template class CFGStructurizer

template<class PassT> CFGStructurizer<PassT>::CFGStructurizer()
  : domTree(NULL), postDomTree(NULL), loopInfo(NULL) {
}
template<class PassT> CFGStructurizer<PassT>::~CFGStructurizer() {
  for (typename BlockInfoMap::iterator I = blockInfoMap.begin(),
       E = blockInfoMap.end(); I != E; ++I) {
    delete I->second;
  }
}
template<class PassT>
bool CFGStructurizer<PassT>::prepare(FuncT &func, PassT &pass) {
  passRep = &pass;
  funcRep = &func;

  bool changed = false;
  //func.RenumberBlocks();

  //to do, if not reducible flow graph, make it so ???

  if (DEBUGME) {
    errs() << "AMDILCFGStructurizer::prepare\n";
    //func.viewCFG();
    //func.viewCFGOnly();
    //func.dump();
  }

  //FIXME: gcc complains on this.
  //domTree = &pass.getAnalysis<DomTreeT>();
  //domTree = CFGTraits::getDominatorTree(pass);
  //if (DEBUGME) {
  //    domTree->print(errs());
  //}

  //FIXME: gcc complains on this.
  //domTree = &pass.getAnalysis<DomTreeT>();
  //postDomTree = CFGTraits::getPostDominatorTree(pass);
  //if (DEBUGME) {
  //   postDomTree->print(errs());
  //}

  //FIXME: gcc complains on this.
  //loopInfo = &pass.getAnalysis<LoopInfoT>();
  loopInfo = CFGTraits::getLoopInfo(pass);
  if (DEBUGME) {
    errs() << "LoopInfo:\n";
    PrintLoopinfo(*loopInfo, errs());
  }

  orderBlocks();
  if (DEBUGME) {
    errs() << "Ordered blocks:\n";
    printOrderedBlocks(errs());
  }

  SmallVector<BlockT *, DEFAULT_VEC_SLOTS> retBlks;

  for (typename LoopInfoT::iterator iter = loopInfo->begin(),
       iterEnd = loopInfo->end();
       iter != iterEnd; ++iter) {
    LoopT* loopRep = (*iter);
    BlockTSmallerVector exitingBlks;
    loopRep->getExitingBlocks(exitingBlks);

    if (exitingBlks.size() == 0) {
      BlockT* dummyExitBlk = normalizeInfiniteLoopExit(loopRep);
      if (dummyExitBlk != NULL)
        retBlks.push_back(dummyExitBlk);
    }
  }

  // Remove unconditional branch instr.
  // Add dummy exit block iff there are multiple returns.

  for (typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
       iterBlk = orderedBlks.begin(), iterEndBlk = orderedBlks.end();
       iterBlk != iterEndBlk;
       ++iterBlk) {
    BlockT *curBlk = *iterBlk;
    removeUnconditionalBranch(curBlk);
    removeRedundantConditionalBranch(curBlk);
    if (CFGTraits::isReturnBlock(curBlk)) {
      retBlks.push_back(curBlk);
    }
    assert(curBlk->succ_size() <= 2);
    //assert(curBlk->size() > 0);
    //removeEmptyBlock(curBlk) ??
  } //for

  if (retBlks.size() >= 2) {
    addDummyExitBlock(retBlks);
    changed = true;
  }

  return changed;
} //CFGStructurizer::prepare
template<class PassT>
bool CFGStructurizer<PassT>::run(FuncT &func, PassT &pass) {
  passRep = &pass;
  funcRep = &func;

  //func.RenumberBlocks();

  //Assume reducible CFG...
  if (DEBUGME) {
    errs() << "AMDILCFGStructurizer::run\n";
    errs() << func.getFunction()->getName().str() << "\n";
    //func.viewCFG();
    //func.viewCFGOnly();
    func.dump();
  }

#if 1
  //FIXME: gcc complains on this.
  //domTree = &pass.getAnalysis<DomTreeT>();
  domTree = CFGTraits::getDominatorTree(pass);
  if (DEBUGME) {
    domTree->print(errs(), (const llvm::Module*)0);
  }
#endif

  //FIXME: gcc complains on this.
  //domTree = &pass.getAnalysis<DomTreeT>();
  postDomTree = CFGTraits::getPostDominatorTree(pass);
  if (DEBUGME) {
    postDomTree->print(errs());
  }

  //FIXME: gcc complains on this.
  //loopInfo = &pass.getAnalysis<LoopInfoT>();
  loopInfo = CFGTraits::getLoopInfo(pass);
  if (DEBUGME) {
    errs() << "LoopInfo:\n";
    PrintLoopinfo(*loopInfo, errs());
  }

  orderBlocks();
//#define STRESSTEST
#ifdef STRESSTEST
  //Use the worse block ordering to test the algorithm.
  ReverseVector(orderedBlks);
#endif

  if (DEBUGME) {
    errs() << "Ordered blocks:\n";
    printOrderedBlocks(errs());
  }
  int numIter = 0;
  bool finish = false;
  BlockT *curBlk;
  bool makeProgress = false;
  int numRemainedBlk = countActiveBlock(orderedBlks.begin(),
                                        orderedBlks.end());

  do {
    ++numIter;
    if (DEBUGME) {
      errs() << "numIter = " << numIter
             << ", numRemaintedBlk = " << numRemainedBlk << "\n";
    }

    typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
    iterBlk = orderedBlks.begin();
    typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
    iterBlkEnd = orderedBlks.end();

    typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
    sccBeginIter = iterBlk;
    BlockT *sccBeginBlk = NULL;
    int sccNumBlk = 0;  // The number of active blocks, init to a
                        // maximum possible number.
    int sccNumIter;     // Number of iteration in this SCC.

    while (iterBlk != iterBlkEnd) {
      curBlk = *iterBlk;

      if (sccBeginBlk == NULL) {
        sccBeginIter = iterBlk;
        sccBeginBlk = curBlk;
        sccNumIter = 0;
        sccNumBlk = numRemainedBlk; // Init to maximum possible number.
        if (DEBUGME) {
          errs() << "start processing SCC" << getSCCNum(sccBeginBlk);
          errs() << "\n";
        }
        processAddedToTraversalBlocks();
      }

      if (!isRetiredBlock(curBlk)) {
        patternMatch(curBlk);
      }

      ++iterBlk;

      bool contNextScc = true;
      if (iterBlk == iterBlkEnd
          || getSCCNum(sccBeginBlk) != getSCCNum(*iterBlk)) {
        // Just finish one scc.
        ++sccNumIter;
        int sccRemainedNumBlk = countActiveBlock(sccBeginIter, iterBlk);
        if (sccRemainedNumBlk >= 1 && sccRemainedNumBlk >= sccNumBlk) {
          if (DEBUGME) {
            errs() << "Can't reduce processing SCC " << getSCCNum(curBlk)
                   << ", sccNumIter = " << sccNumIter
                   << ", remain # of blocks " << sccRemainedNumBlk;
            errs() << "doesn't make any progress\n";
          }
          contNextScc = true;
        } else if (sccRemainedNumBlk >= 1 && sccRemainedNumBlk < sccNumBlk) {
          sccNumBlk = sccRemainedNumBlk;
          iterBlk = sccBeginIter;
          contNextScc = false;
          if (DEBUGME) {
            errs() << "repeat processing SCC" << getSCCNum(curBlk)
                   << "sccNumIter = " << sccNumIter << "\n";
            //func.viewCFG();
            //func.viewCFGOnly();
          }
        } else {
          if (DEBUGME) {
            errs() << "finish processing SCC" << getSCCNum(curBlk)
                   << ", remain # of blocks " << sccRemainedNumBlk << "\n";
          }
          // Finish the current scc.
          contNextScc = true;
        }
      } else {
        // Continue on next component in the current scc.
        contNextScc = false;
      }

      if (contNextScc) {
        sccBeginBlk = NULL;
      }
    } //while, "one iteration" over the function.

    BlockT *entryBlk = FuncGTraits::nodes_begin(&func);
    if (entryBlk->succ_size() == 0) {
      finish = true;
      if (DEBUGME) {
        errs() << "Reduce to one block\n";
      }
    } else {
      int newnumRemainedBlk
        = countActiveBlock(orderedBlks.begin(), orderedBlks.end());
      // consider cloned blocks ??
      if (newnumRemainedBlk == 1 || newnumRemainedBlk < numRemainedBlk) {
        makeProgress = true;
        numRemainedBlk = newnumRemainedBlk;
      } else {
        makeProgress = false;
        if (DEBUGME) {
          errs() << "No progress\n";
        }
      }
    }
  } while (!finish && makeProgress);

  // Misc wrap up to maintain the consistency of the Function representation.
  CFGTraits::wrapup(FuncGTraits::nodes_begin(&func));

  // allocate physical registers for virtual registers created during this pass
  TrivialRegAlloc regAlloc(func, AMDIL::GPRI32RegClass, vregs);
  regAlloc.run();

  // Detach retired Block, release memory.
  for (typename BlockInfoMap::iterator iterMap = blockInfoMap.begin(),
       iterEndMap = blockInfoMap.end(); iterMap != iterEndMap; ++iterMap) {
    if ((*iterMap).second && (*iterMap).second->isRetired) {
      assert(((*iterMap).first)->getNumber() != -1);
      if (DEBUGME) {
        errs() << "Erase BB" << ((*iterMap).first)->getNumber() << "\n";
      }
      (*iterMap).first->eraseFromParent();  //Remove from the parent Function.
    }
    delete (*iterMap).second;
  }
  blockInfoMap.clear();

  // clear loopLandInfoMap
  for (typename LoopLandInfoMap::iterator iterMap = loopLandInfoMap.begin(),
       iterEndMap = loopLandInfoMap.end(); iterMap != iterEndMap; ++iterMap) {
    delete (*iterMap).second;
  }
  loopLandInfoMap.clear();

  if (DEBUGME) {
    //func.viewCFG();
    //func.dump();
  }

  if (!finish) {
    MachineFunction *MF = &func;
    AMDILMachineFunctionInfo *mMFI =
      MF->getInfo<AMDILMachineFunctionInfo>();
    mMFI->addErrorMsg(amd::CompilerErrorMessage[IRREDUCIBLE_CF]);
  }

  return true;
} //CFGStructurizer::run
/// Print the ordered Blocks.
///
template<class PassT>
void CFGStructurizer<PassT>::printOrderedBlocks(llvm::raw_ostream &os) {
  size_t i = 0;
  for (typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
       iterBlk = orderedBlks.begin(), iterBlkEnd = orderedBlks.end();
       iterBlk != iterBlkEnd;
       ++iterBlk, ++i) {
    os << "BB" << (*iterBlk)->getNumber();
    os << "(" << getSCCNum(*iterBlk) << "," << (*iterBlk)->size() << ")";
    if (i != 0 && i % 10 == 0) {
      os << "\n";
    } else {
      os << " ";
    }
  }
} //printOrderedBlocks
/// Compute the reversed DFS post order of Blocks
///
template<class PassT> void CFGStructurizer<PassT>::orderBlocks() {
  int sccNum = 0;
  BlockT *bb;
  for (scc_iterator<FuncT *> sccIter = scc_begin(funcRep),
       sccEnd = scc_end(funcRep); sccIter != sccEnd; ++sccIter, ++sccNum) {
    std::vector<BlockT *> &sccNext = *sccIter;
    for (typename std::vector<BlockT *>::const_iterator
         blockIter = sccNext.begin(), blockEnd = sccNext.end();
         blockIter != blockEnd; ++blockIter) {
      bb = *blockIter;
      orderedBlks.push_back(bb);
      recordSccnum(bb, sccNum);
    }
  }

  //walk through all the block in func to check for unreachable
  for (BlockIterator blockIter1 = FuncGTraits::nodes_begin(funcRep),
       blockEnd1 = FuncGTraits::nodes_end(funcRep);
       blockIter1 != blockEnd1; ++blockIter1) {
    BlockT *bb = &(*blockIter1);
    sccNum = getSCCNum(bb);
    if (sccNum == INVALIDSCCNUM) {
      errs() << "unreachable block BB" << bb->getNumber() << "\n";
    }
  } //end of for
} //orderBlocks
/// Compute the reversed DFS post order of Blocks
///
template<class PassT> void CFGStructurizer<PassT>::
processAddedToTraversalBlocks() {
  typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
  iterBlk = addedToTraversalBlks.begin();
  typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::const_iterator
  iterBlkEnd = addedToTraversalBlks.end();
  BlockT *curBlk;

  while (iterBlk != iterBlkEnd) {
    curBlk = *iterBlk;

    if (!isRetiredBlock(curBlk)) {
      patternMatch(curBlk);
    }

    ++iterBlk;
  }
} //CFGStructurizer<PassT>::processAddedToTraversalBlocks
template<class PassT> int CFGStructurizer<PassT>::patternMatch(BlockT *curBlk)
{
  int numMatch = 0;
  int curMatch;

  if (DEBUGME) {
    errs() << "Begin patternMatch BB" << curBlk->getNumber() << "\n";
  }

  while ((curMatch = patternMatchGroup(curBlk)) > 0) {
    numMatch += curMatch;
  }

  if (DEBUGME) {
    errs() << "End patternMatch BB" << curBlk->getNumber()
           << ", numMatch = " << numMatch << "\n";
  }

  return numMatch;
} //patternMatch
template<class PassT>
int CFGStructurizer<PassT>::patternMatchGroup(BlockT *curBlk) {
  int numMatch = 0;
  numMatch += serialPatternMatch(curBlk);
  numMatch += ifPatternMatch(curBlk);
  //numMatch += switchPatternMatch(curBlk);
  numMatch += loopendPatternMatch(curBlk);
  numMatch += loopPatternMatch(curBlk);
  return numMatch;
} //patternMatchGroup
template<class PassT>
int CFGStructurizer<PassT>::serialPatternMatch(BlockT *curBlk) {
  if (curBlk->succ_size() != 1) {
    return 0;
  }

  BlockT *childBlk = *curBlk->succ_begin();
  if (childBlk->pred_size() != 1 || isActiveLoophead(childBlk)) {
    return 0;
  }

  mergeSerialBlock(curBlk, childBlk);
  ++numSerialPatternMatch;
  return 1;
} //serialPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::ifPatternMatch(BlockT *curBlk) {
  //two edges
  if (curBlk->succ_size() != 2) {
    return 0;
  }

  if (hasBackEdge(curBlk)) {
    return 0;
  }

  InstrT *branchInstr = CFGTraits::getNormalBlockBranchInstr(curBlk);
  if (branchInstr == NULL) {
    return 0;
  }

  assert(CFGTraits::isCondBranch(branchInstr));

  BlockT *trueBlk = CFGTraits::getTrueBranch(branchInstr);
  BlockT *falseBlk = CFGTraits::getFalseBranch(curBlk, branchInstr);
  BlockT *landBlk;
  int cloned = 0;

  // TODO: Simplify
  if (trueBlk->succ_size() == 1 && falseBlk->succ_size() == 1
      && *trueBlk->succ_begin() == *falseBlk->succ_begin()) {
    landBlk = *trueBlk->succ_begin();
  } else if (trueBlk->succ_size() == 0 && falseBlk->succ_size() == 0) {
    landBlk = NULL;
  } else if (trueBlk->succ_size() == 1 && *trueBlk->succ_begin() == falseBlk) {
    landBlk = falseBlk;
    falseBlk = NULL;
  } else if (falseBlk->succ_size() == 1
             && *falseBlk->succ_begin() == trueBlk) {
    landBlk = trueBlk;
    trueBlk = NULL;
  } else if (falseBlk->succ_size() == 1
             && isSameloopDetachedContbreak(trueBlk, falseBlk)) {
    landBlk = *falseBlk->succ_begin();
  } else if (trueBlk->succ_size() == 1
             && isSameloopDetachedContbreak(falseBlk, trueBlk)) {
    landBlk = *trueBlk->succ_begin();
  } else {
    return handleJumpintoIf(curBlk, trueBlk, falseBlk);
  }

  // improveSimpleJumpinfoIf can handle the case where landBlk == NULL but the
  // new BB created for landBlk==NULL may introduce new challenge to the
  // reduction process.
  if (landBlk != NULL &&
      ((trueBlk && trueBlk->pred_size() > 1)
       || (falseBlk && falseBlk->pred_size() > 1))) {
    cloned += improveSimpleJumpintoIf(curBlk, trueBlk, falseBlk, &landBlk);
  }

  if (trueBlk && trueBlk->pred_size() > 1) {
    trueBlk = cloneBlockForPredecessor(trueBlk, curBlk);
    ++cloned;
  }

  if (falseBlk && falseBlk->pred_size() > 1) {
    falseBlk = cloneBlockForPredecessor(falseBlk, curBlk);
    ++cloned;
  }

  mergeIfthenelseBlock(branchInstr, curBlk, trueBlk, falseBlk, landBlk);

  ++numIfPatternMatch;

  numClonedBlock += cloned;

  return 1 + cloned;
} //ifPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::switchPatternMatch(BlockT *curBlk) {
  return 0;
} //switchPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::loopendPatternMatch(BlockT *curBlk) {
  LoopT *loopRep = loopInfo->getLoopFor(curBlk);
  typename std::vector<LoopT *> nestedLoops;
  while (loopRep) {
    nestedLoops.push_back(loopRep);
    loopRep = loopRep->getParentLoop();
  }

  if (nestedLoops.size() == 0) {
    return 0;
  }

  // Process nested loop outside->inside, so "continue" to a outside loop won't
  // be mistaken as "break" of the current loop.
  int num = 0;
  for (typename std::vector<LoopT *>::reverse_iterator
       iter = nestedLoops.rbegin(), iterEnd = nestedLoops.rend();
       iter != iterEnd; ++iter) {
    loopRep = *iter;

    if (getLoopLandBlock(loopRep) != NULL) {
      continue;
    }

    BlockT *loopHeader = loopRep->getHeader();

    int numBreak = loopbreakPatternMatch(loopRep, loopHeader);

    if (numBreak == -1) {
      break;
    }

    int numCont = loopcontPatternMatch(loopRep, loopHeader);
    num += numBreak + numCont;
  }

  return num;
} //loopendPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::loopPatternMatch(BlockT *curBlk) {
  if (curBlk->succ_size() != 0) {
    return 0;
  }

  int numLoop = 0;
  LoopT *loopRep = loopInfo->getLoopFor(curBlk);
  while (loopRep && loopRep->getHeader() == curBlk) {
    LoopLandInfo *loopLand = getLoopLandInfo(loopRep);
    if (loopLand) {
      BlockT *landBlk = loopLand->landBlk;
      assert(landBlk);
      if (!isRetiredBlock(landBlk)) {
        mergeLooplandBlock(curBlk, loopLand);
        ++numLoop;
      }
    }
    loopRep = loopRep->getParentLoop();
  }

  numLoopPatternMatch += numLoop;

  return numLoop;
} //loopPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::loopbreakPatternMatch(LoopT *loopRep,
                                                  BlockT *loopHeader) {
  BlockTSmallerVector exitingBlks;
  loopRep->getExitingBlocks(exitingBlks);

  if (DEBUGME) {
    errs() << "Loop has " << exitingBlks.size() << " exiting blocks\n";
  }

  if (exitingBlks.size() == 0) {
    setLoopLandBlock(loopRep);
    return 0;
  }

  // Compute the corresponding exitBlks and exit block set.
  BlockTSmallerVector exitBlks;
  std::set<BlockT *> exitBlkSet;
  for (typename BlockTSmallerVector::const_iterator iter = exitingBlks.begin(),
       iterEnd = exitingBlks.end(); iter != iterEnd; ++iter) {
    BlockT *exitingBlk = *iter;
    BlockT *exitBlk = exitingBlock2ExitBlock(loopRep, exitingBlk);
    exitBlks.push_back(exitBlk);
    exitBlkSet.insert(exitBlk);  //non-duplicate insert
  }

  assert(exitBlkSet.size() > 0);
  assert(exitBlks.size() == exitingBlks.size());

  if (DEBUGME) {
    errs() << "Loop has " << exitBlkSet.size() << " exit blocks\n";
  }

  // Find exitLandBlk.
  BlockT *exitLandBlk = NULL;
  int numCloned = 0;
  int numSerial = 0;

  if (exitBlkSet.size() == 1)
  {
    exitLandBlk = *exitBlkSet.begin();
  } else {
    exitLandBlk = findNearestCommonPostDom(exitBlkSet);

    if (exitLandBlk == NULL) {
      return -1;
    }

    bool allInPath = true;
    bool allNotInPath = true;
    for (typename std::set<BlockT*>::const_iterator
         iter = exitBlkSet.begin(),
         iterEnd = exitBlkSet.end();
         iter != iterEnd; ++iter) {
      BlockT *exitBlk = *iter;

      PathToKind pathKind = singlePathTo(exitBlk, exitLandBlk, true);
      if (DEBUGME) {
        errs() << "BB" << exitBlk->getNumber()
               << " to BB" << exitLandBlk->getNumber() << " PathToKind="
               << pathKind << "\n";
      }

      allInPath = allInPath && (pathKind == SinglePath_InPath);
      allNotInPath = allNotInPath && (pathKind == SinglePath_NotInPath);

      if (!allInPath && !allNotInPath) {
        if (DEBUGME) {
          errs() << "singlePath check fail\n";
        }
        return -1;
      }
    } // check all exit blocks

    if (allNotInPath) {
      // TODO: Simplify, maybe separate function?
      //funcRep->viewCFG();
      LoopT *parentLoopRep = loopRep->getParentLoop();
      BlockT *parentLoopHeader = NULL;
      if (parentLoopRep)
        parentLoopHeader = parentLoopRep->getHeader();

      if (exitLandBlk == parentLoopHeader &&
          (exitLandBlk = relocateLoopcontBlock(parentLoopRep,
                                               loopRep,
                                               exitBlkSet,
                                               exitLandBlk)) != NULL) {
        if (DEBUGME) {
          errs() << "relocateLoopcontBlock success\n";
        }
      } else if ((exitLandBlk = addLoopEndbranchBlock(loopRep,
                                                      exitingBlks,
                                                      exitBlks)) != NULL) {
        if (DEBUGME) {
          errs() << "insertEndbranchBlock success\n";
        }
      } else {
        if (DEBUGME) {
          errs() << "loop exit fail\n";
        }
        return -1;
      }
    } else if ((exitLandBlk = addLoopEndbranchBlock(loopRep,
                                                    exitingBlks,
                                                    exitBlks)) != NULL) {
      //current addLoopEndbranchBlock always does something and return non-NULL
      if (DEBUGME) {
        errs() << "insertEndbranchBlock success\n";
      }
    }

    // Handle side entry to exit path.
    exitBlks.clear();
    exitBlkSet.clear();
    for (typename BlockTSmallerVector::iterator iterExiting =
           exitingBlks.begin(),
         iterExitingEnd = exitingBlks.end();
         iterExiting != iterExitingEnd; ++iterExiting) {
      BlockT *exitingBlk = *iterExiting;
      BlockT *exitBlk = exitingBlock2ExitBlock(loopRep, exitingBlk);
      BlockT *newExitBlk = exitBlk;

      if (exitBlk != exitLandBlk && exitBlk->pred_size() > 1) {
        newExitBlk = cloneBlockForPredecessor(exitBlk, exitingBlk);
        ++numCloned;
      }

      numCloned += cloneOnSideEntryTo(exitingBlk, newExitBlk, exitLandBlk);

      exitBlks.push_back(newExitBlk);
      exitBlkSet.insert(newExitBlk);
    }

    for (typename BlockTSmallerVector::iterator iterExit = exitBlks.begin(),
         iterExitEnd = exitBlks.end();
         iterExit != iterExitEnd; ++iterExit) {
      BlockT *exitBlk = *iterExit;
      numSerial += serialPatternMatch(exitBlk);
    }

    for (typename BlockTSmallerVector::iterator iterExit = exitBlks.begin(),
         iterExitEnd = exitBlks.end();
         iterExit != iterExitEnd; ++iterExit) {
      BlockT *exitBlk = *iterExit;
      if (exitBlk->pred_size() > 1) {
        if (exitBlk != exitLandBlk) {
          return -1;
        }
      } else {
        if (exitBlk != exitLandBlk &&
            (exitBlk->succ_size() != 1 ||
             *exitBlk->succ_begin() != exitLandBlk)) {
          return -1;
        }
      }
    }
  } // else

  // LoopT *exitLandLoop = loopInfo->getLoopFor(exitLandBlk);
  exitLandBlk = recordLoopLandBlock(loopRep, exitLandBlk, exitBlks, exitBlkSet);

  // Fold break into the breaking block. Leverage across level breaks.
  assert(exitingBlks.size() == exitBlks.size());
  for (typename BlockTSmallerVector::const_iterator iterExit = exitBlks.begin(),
       iterExiting = exitingBlks.begin(), iterExitEnd = exitBlks.end();
       iterExit != iterExitEnd; ++iterExit, ++iterExiting) {
    BlockT *exitBlk = *iterExit;
    BlockT *exitingBlk = *iterExiting;
    assert(exitBlk->pred_size() == 1 || exitBlk == exitLandBlk);
    LoopT *exitingLoop = loopInfo->getLoopFor(exitingBlk);
    handleLoopbreak(exitingBlk, exitingLoop, exitBlk, loopRep, exitLandBlk);
  }

  int numBreak = static_cast<int>(exitingBlks.size());
  numLoopbreakPatternMatch += numBreak;
  numClonedBlock += numCloned;
  return numBreak + numSerial + numCloned;
} //loopbreakPatternMatch
template<class PassT>
int CFGStructurizer<PassT>::loopcontPatternMatch(LoopT *loopRep,
                                                 BlockT *loopHeader) {
  int numCont = 0;
  SmallVector<BlockT *, DEFAULT_VEC_SLOTS> contBlk;
  for (typename InvBlockGTraits::ChildIteratorType iter =
         InvBlockGTraits::child_begin(loopHeader),
       iterEnd = InvBlockGTraits::child_end(loopHeader);
       iter != iterEnd; ++iter) {
    BlockT *curBlk = *iter;
    if (loopRep->contains(curBlk)) {
      handleLoopcontBlock(curBlk, loopInfo->getLoopFor(curBlk),
                          loopHeader, loopRep);
      contBlk.push_back(curBlk);
      ++numCont;
    }
  }

  for (typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::iterator
       iter = contBlk.begin(), iterEnd = contBlk.end();
       iter != iterEnd; ++iter) {
    (*iter)->removeSuccessor(loopHeader);
  }

  numLoopcontPatternMatch += numCont;

  return numCont;
} //loopcontPatternMatch
template<class PassT>
bool CFGStructurizer<PassT>::isSameloopDetachedContbreak(BlockT *src1Blk,
                                                         BlockT *src2Blk) {
  // return true iff src1Blk->succ_size() == 0 && src1Blk and src2Blk are in the
  // same loop with LoopLandInfo without explicitly keeping track of
  // loopContBlks and loopBreakBlks, this is a method to get the information.
  //
  if (src1Blk->succ_size() == 0) {
    LoopT *loopRep = loopInfo->getLoopFor(src1Blk);
    if (loopRep != NULL && loopRep == loopInfo->getLoopFor(src2Blk)) {
      LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];
      if (theEntry != NULL) {
        if (DEBUGME) {
          errs() << "isLoopContBreakBlock yes src1 = BB"
                 << src1Blk->getNumber()
                 << " src2 = BB" << src2Blk->getNumber() << "\n";
        }
        return true;
      }
    }
  }
  return false;
}  //isSameloopDetachedContbreak
template<class PassT>
int CFGStructurizer<PassT>::handleJumpintoIf(BlockT *headBlk,
                                             BlockT *trueBlk,
                                             BlockT *falseBlk) {
  int num = handleJumpintoIfImp(headBlk, trueBlk, falseBlk);
  if (num == 0) {
    if (DEBUGME) {
      errs() << "handleJumpintoIf swap trueBlk and FalseBlk" << "\n";
    }
    num = handleJumpintoIfImp(headBlk, falseBlk, trueBlk);

    if (num == 0) {
      if (DEBUGME) {
        errs() << "handleJumpintoIf check NULL as common postdom:";
      }
      BlockT* trueEnd;
      BlockT* falseEnd;
      if ((trueEnd = singlePathEnd(trueBlk, NULL)) &&
          (falseEnd = singlePathEnd(falseBlk, NULL)) &&
          loopInfo->getLoopFor(trueEnd) == loopInfo->getLoopFor(falseEnd)) {
        if (DEBUGME) {
          errs() << " working\n";
        }
        num += cloneOnSideEntryTo(headBlk, trueBlk, NULL);
        num += cloneOnSideEntryTo(headBlk, falseBlk, NULL);

        numClonedBlock += num;
        num += serialPatternMatch(*headBlk->succ_begin());
        num += serialPatternMatch(*(headBlk->succ_begin()+1));
        num += ifPatternMatch(headBlk);
        assert(num > 0); //
      } else {
        if (DEBUGME) {
          errs() << " not working\n";
        }
      }
    } //check NULL
  }
  return num;
}
template<class PassT>
int CFGStructurizer<PassT>::handleJumpintoIfImp(BlockT *headBlk,
                                                BlockT *trueBlk,
                                                BlockT *falseBlk) {
  int num = 0;
  BlockT *downBlk;

  //trueBlk could be the common post dominator
  downBlk = trueBlk;

  if (DEBUGME) {
    errs() << "handleJumpintoIfImp head = BB" << headBlk->getNumber()
           << " true = BB" << trueBlk->getNumber()
           << ", numSucc=" << trueBlk->succ_size()
           << " false = BB" << falseBlk->getNumber() << "\n";
  }

  while (downBlk) {
    if (DEBUGME) {
      errs() << "check down = BB" << downBlk->getNumber();
    }

    if ( //postDomTree->dominates(downBlk, falseBlk) &&
      singlePathTo(falseBlk, downBlk) == SinglePath_InPath) {
      if (DEBUGME) {
        errs() << " working\n";
      }

      num += cloneOnSideEntryTo(headBlk, trueBlk, downBlk);
      num += cloneOnSideEntryTo(headBlk, falseBlk, downBlk);

      numClonedBlock += num;
      num += serialPatternMatch(*headBlk->succ_begin());
      num += serialPatternMatch(*(headBlk->succ_begin()+1));
      num += ifPatternMatch(headBlk);
      assert(num > 0); //

      break;
    }
    if (DEBUGME) {
      errs() << " not working\n";
    }
    downBlk = (downBlk->succ_size() == 1) ? (*downBlk->succ_begin()) : NULL;
  } // walk down the postDomTree

  return num;
} //handleJumpintoIf
template<class PassT>
void CFGStructurizer<PassT>::showImproveSimpleJumpintoIf(BlockT *headBlk,
                                                         BlockT *trueBlk,
                                                         BlockT *falseBlk,
                                                         BlockT *landBlk,
                                                         bool detail) {
  errs() << "head = BB" << headBlk->getNumber()
         << " size = " << headBlk->size();
  if (detail) {
    errs() << "\n";
    headBlk->print(errs());
    errs() << "\n";
  }

  if (trueBlk) {
    errs() << ", true = BB" << trueBlk->getNumber() << " size = "
           << trueBlk->size() << " numPred = " << trueBlk->pred_size();
    if (detail) {
      errs() << "\n";
      trueBlk->print(errs());
      errs() << "\n";
    }
  }
  if (falseBlk) {
    errs() << ", false = BB" << falseBlk->getNumber() << " size = "
           << falseBlk->size() << " numPred = " << falseBlk->pred_size();
    if (detail) {
      errs() << "\n";
      falseBlk->print(errs());
      errs() << "\n";
    }
  }
  if (landBlk) {
    errs() << ", land = BB" << landBlk->getNumber() << " size = "
           << landBlk->size() << " numPred = " << landBlk->pred_size();
    if (detail) {
      errs() << "\n";
      landBlk->print(errs());
      errs() << "\n";
    }
  }

  errs() << "\n";
} //showImproveSimpleJumpintoIf
template<class PassT>
int CFGStructurizer<PassT>::improveSimpleJumpintoIf(BlockT *headBlk,
                                                    BlockT *trueBlk,
                                                    BlockT *falseBlk,
                                                    BlockT **plandBlk) {
  bool migrateTrue = false;
  bool migrateFalse = false;

  BlockT *landBlk = *plandBlk;

  assert((trueBlk == NULL || trueBlk->succ_size() <= 1)
         && (falseBlk == NULL || falseBlk->succ_size() <= 1));

  if (trueBlk == falseBlk) {
    return 0;
  }

#if 0
  if (DEBUGME) {
    errs() << "improveSimpleJumpintoIf: ";
    showImproveSimpleJumpintoIf(headBlk, trueBlk, falseBlk, landBlk, 0);
  }
#endif

  // unsigned landPredSize = landBlk ? landBlk->pred_size() : 0;
  // May consider the # landBlk->pred_size() as it represents the number of
  // assignment initReg = .. needed to insert.
  migrateTrue = needMigrateBlock(trueBlk);
  migrateFalse = needMigrateBlock(falseBlk);

  if (!migrateTrue && !migrateFalse) {
    return 0;
  }

  // If we need to migrate either trueBlk and falseBlk, migrate the rest that
  // have more than one predecessors.  without doing this, its predecessor
  // rather than headBlk will have undefined value in initReg.
  if (!migrateTrue && trueBlk && trueBlk->pred_size() > 1) {
    migrateTrue = true;
  }
  if (!migrateFalse && falseBlk && falseBlk->pred_size() > 1) {
    migrateFalse = true;
  }

  if (DEBUGME) {
    errs() << "before improveSimpleJumpintoIf: ";
    showImproveSimpleJumpintoIf(headBlk, trueBlk, falseBlk, landBlk, 0);
    //showImproveSimpleJumpintoIf(headBlk, trueBlk, falseBlk, landBlk, 1);
  }

  // org: headBlk => if () {trueBlk} else {falseBlk} => landBlk
  //
  // new: headBlk => if () {initReg = 1; org trueBlk branch} else
  //      {initReg = 0; org falseBlk branch }
  //      => landBlk => if (initReg) {org trueBlk} else {org falseBlk}
  //      => org landBlk
  //      if landBlk->pred_size() > 2, put the about if-else inside
  //      if (initReg !=2) {...}
  //
  // add initReg = initVal to headBlk
  unsigned initReg = getRegister(&AMDIL::GPRI32RegClass);
  if (!migrateTrue || !migrateFalse) {
    int initVal = migrateTrue ? 0 : 1;
    CFGTraits::insertAssignInstrBefore(headBlk, passRep, initReg, initVal);
  }

  int numNewBlk = 0;

  if (landBlk == NULL) {
    landBlk = funcRep->CreateMachineBasicBlock();
    funcRep->push_back(landBlk);  //insert to function

    if (trueBlk) {
      trueBlk->addSuccessor(landBlk);
    } else {
      headBlk->addSuccessor(landBlk);
    }

    if (falseBlk) {
      falseBlk->addSuccessor(landBlk);
    } else {
      headBlk->addSuccessor(landBlk);
    }

    numNewBlk++;
  }

  bool landBlkHasOtherPred = (landBlk->pred_size() > 2);

  //insert AMDIL::ENDIF to avoid special case "input landBlk == NULL"
  typename BlockT::iterator insertPos =
    CFGTraits::getInstrPos
      (landBlk, CFGTraits::insertInstrBefore(landBlk, AMDIL::ENDIF, passRep));

  if (landBlkHasOtherPred) {
    unsigned immReg = getRegister(&AMDIL::GPRI32RegClass);
    CFGTraits::insertAssignInstrBefore(insertPos, passRep, immReg, 2);
    unsigned cmpResReg = getRegister(&AMDIL::GPRI32RegClass);
    CFGTraits::insertCompareInstrBefore(landBlk, insertPos, passRep, cmpResReg,
                                        initReg, immReg);
    CFGTraits::insertCondBranchBefore(landBlk, insertPos,
                                      AMDIL::IF_LOGICALZi32r, passRep,
                                      cmpResReg, DebugLoc());
  }

  CFGTraits::insertCondBranchBefore(landBlk, insertPos, AMDIL::IF_LOGICALNZi32r,
                                    passRep, initReg, DebugLoc());

  if (migrateTrue) {
    migrateInstruction(trueBlk, landBlk, insertPos);
    // need to uncondionally insert the assignment to ensure a path from its
    // predecessor rather than headBlk has valid value in initReg if
    // (initVal != 1).
    CFGTraits::insertAssignInstrBefore(trueBlk, passRep, initReg, 1);
  }
  CFGTraits::insertInstrBefore(insertPos, AMDIL::ELSE, passRep);

  if (migrateFalse) {
    migrateInstruction(falseBlk, landBlk, insertPos);
    // need to uncondionally insert the assignment to ensure a path from its
    // predecessor rather than headBlk has valid value in initReg if
    // (initVal != 0)
    CFGTraits::insertAssignInstrBefore(falseBlk, passRep, initReg, 0);
  }
  //CFGTraits::insertInstrBefore(insertPos, AMDIL::ENDIF, passRep);

  if (landBlkHasOtherPred) {
    // add endif
    CFGTraits::insertInstrBefore(insertPos, AMDIL::ENDIF, passRep);

    // put initReg = 2 to other predecessors of landBlk
    for (typename BlockT::pred_iterator predIter = landBlk->pred_begin(),
         predIterEnd = landBlk->pred_end(); predIter != predIterEnd;
         ++predIter) {
      BlockT *curBlk = *predIter;
      if (curBlk != trueBlk && curBlk != falseBlk) {
        CFGTraits::insertAssignInstrBefore(curBlk, passRep, initReg, 2);
      }
    } //for
  }
  if (DEBUGME) {
    errs() << "result from improveSimpleJumpintoIf: ";
    showImproveSimpleJumpintoIf(headBlk, trueBlk, falseBlk, landBlk, 0);
    //showImproveSimpleJumpintoIf(headBlk, trueBlk, falseBlk, landBlk, 1);
  }

  // update landBlk
  *plandBlk = landBlk;

  return numNewBlk;
} //improveSimpleJumpintoIf
// Since we are after the register allocator, we don't want to use virtual
// registers as it is possible that we can get a virtual register that is
// passed the 65K limit of IL text format. So instead we serach through the
// register class for an unused physical register and mark it as used. If we
// cannot find a register, then we do some funky math on the virtual registers
// so that we don't clobber the physicals and make sure we don't go over the
// 65k limit.
template<class PassT>
inline int CFGStructurizer<PassT>::getRegister(
  const class TargetRegisterClass *RegClass)
{
  unsigned reg = funcRep->getRegInfo().createVirtualRegister(RegClass);
  vregs.insert(reg);
  if (DEBUGME)
    errs() << "created virtual register "
           << TargetRegisterInfo::virtReg2Index(reg) << "\n";
  return reg;
}
template<class PassT>
void CFGStructurizer<PassT>::handleLoopbreak(BlockT *exitingBlk,
                                             LoopT *exitingLoop,
                                             BlockT *exitBlk,
                                             LoopT *exitLoop,
                                             BlockT *landBlk) {
  if (DEBUGME) {
    errs() << "Trying to break loop-depth = " << getLoopDepth(exitLoop)
           << " from loop-depth = " << getLoopDepth(exitingLoop) << "\n";
  }

  RegiT initReg = INVALIDREGNUM;
  if (exitingLoop != exitLoop) {
    initReg = getRegister(&AMDIL::GPRI32RegClass);
    assert(initReg != INVALIDREGNUM);
    addLoopBreakInitReg(exitLoop, initReg);
    while (exitingLoop != exitLoop && exitingLoop) {
      addLoopBreakOnReg(exitingLoop, initReg);
      exitingLoop = exitingLoop->getParentLoop();
    }
    assert(exitingLoop == exitLoop);
  }

  mergeLoopbreakBlock(exitingBlk, exitBlk, landBlk, initReg);
} //handleLoopbreak
template<class PassT>
void CFGStructurizer<PassT>::handleLoopcontBlock(BlockT *contingBlk,
                                                 LoopT *contingLoop,
                                                 BlockT *contBlk,
                                                 LoopT *contLoop) {
  if (DEBUGME) {
    errs() << "loopcontPattern cont = BB" << contingBlk->getNumber()
           << " header = BB" << contBlk->getNumber() << "\n";

    errs() << "Trying to continue loop-depth = "
           << getLoopDepth(contLoop)
           << " from loop-depth = " << getLoopDepth(contingLoop) << "\n";
  }

  RegiT initReg = INVALIDREGNUM;
  if (contingLoop != contLoop) {
    initReg = getRegister(&AMDIL::GPRI32RegClass);
    assert(initReg != INVALIDREGNUM);
    addLoopContInitReg(contLoop, initReg);
    while (contingLoop && contingLoop->getParentLoop() != contLoop) {
      addLoopBreakOnReg(contingLoop, initReg);  //not addLoopContOnReg
      contingLoop = contingLoop->getParentLoop();
    }
    assert(contingLoop && contingLoop->getParentLoop() == contLoop);
    addLoopContOnReg(contingLoop, initReg);
  }

  settleLoopcontBlock(contingBlk, contBlk, initReg);
  //contingBlk->removeSuccessor(loopHeader);
} //handleLoopcontBlock
template<class PassT>
void CFGStructurizer<PassT>::mergeSerialBlock(BlockT *dstBlk, BlockT *srcBlk) {
  if (DEBUGME) {
    errs() << "serialPattern BB" << dstBlk->getNumber()
           << " <= BB" << srcBlk->getNumber() << "\n";
  }
  //removeUnconditionalBranch(dstBlk);
  dstBlk->splice(dstBlk->end(), srcBlk, FirstNonDebugInstr(srcBlk), srcBlk->end());

  dstBlk->removeSuccessor(srcBlk);
  CFGTraits::cloneSuccessorList(dstBlk, srcBlk);

  removeSuccessor(srcBlk);
  retireBlock(dstBlk, srcBlk);
} //mergeSerialBlock
template<class PassT>
void CFGStructurizer<PassT>::mergeIfthenelseBlock(InstrT *branchInstr,
                                                  BlockT *curBlk,
                                                  BlockT *trueBlk,
                                                  BlockT *falseBlk,
                                                  BlockT *landBlk) {
  if (DEBUGME) {
    errs() << "ifPattern BB" << curBlk->getNumber();
    errs() << "{  ";
    if (trueBlk) {
      errs() << "BB" << trueBlk->getNumber();
    }
    errs() << "  } else ";
    errs() << "{  ";
    if (falseBlk) {
      errs() << "BB" << falseBlk->getNumber();
    }
    errs() << "  }\n ";
    errs() << "landBlock: ";
    if (landBlk == NULL) {
      errs() << "NULL";
    } else {
      errs() << "BB" << landBlk->getNumber();
    }
    errs() << "\n";
  }

  int oldOpcode = branchInstr->getOpcode();
  DebugLoc branchDL = branchInstr->getDebugLoc();
  bool badDebugLoc = branchDL == DebugLoc() ? true : false;
  if (badDebugLoc) {
    InstrT * brkInstr = getLastBreakInstr(curBlk);
    if (brkInstr) {
      branchDL = brkInstr->getDebugLoc();
    }
  }

//    transform to
//    if cond
//       trueBlk
//    else
//       falseBlk
//    endif
//    landBlk

  typename BlockT::iterator branchInstrPos =
    CFGTraits::getInstrPos(curBlk, branchInstr);
  CFGTraits::insertCondBranchBefore(branchInstrPos,
                                    CFGTraits::getBranchNzeroOpcode(oldOpcode),
                                    passRep,
                                    branchDL);

  if (trueBlk) {
    curBlk->splice(branchInstrPos, trueBlk, FirstNonDebugInstr(
                     trueBlk), trueBlk->end());
    curBlk->removeSuccessor(trueBlk);
    if (landBlk && trueBlk->succ_size()!=0) {
      trueBlk->removeSuccessor(landBlk);
    }
    retireBlock(curBlk, trueBlk);
  }
  if (badDebugLoc) {
    CFGTraits::insertInstrBefore(branchInstrPos, AMDIL::ELSE, passRep, branchDL);
  } else {
    CFGTraits::insertInstrBefore(branchInstrPos, AMDIL::ELSE, passRep);
  }

  if (falseBlk) {
    curBlk->splice(branchInstrPos, falseBlk, FirstNonDebugInstr(falseBlk),
                   falseBlk->end());
    curBlk->removeSuccessor(falseBlk);
    if (landBlk && falseBlk->succ_size() != 0) {
      falseBlk->removeSuccessor(landBlk);
    }
    retireBlock(curBlk, falseBlk);
  }
  CFGTraits::insertInstrBefore(branchInstrPos, AMDIL::ENDIF, passRep);

  //curBlk->remove(branchInstrPos);
  branchInstr->eraseFromParent();

  if (landBlk && trueBlk && falseBlk) {
    curBlk->addSuccessor(landBlk);
  }
} //mergeIfthenelseBlock
template<class PassT>
void CFGStructurizer<PassT>::mergeLooplandBlock(BlockT *dstBlk,
                                                LoopLandInfo *loopLand) {
  BlockT *landBlk = loopLand->landBlk;

  if (DEBUGME) {
    errs() << "loopPattern header = BB" << dstBlk->getNumber()
           << " land = BB" << landBlk->getNumber() << "\n";
  }

  // Loop contInitRegs are init at the beginning of the loop.
  for (typename std::set<RegiT>::const_iterator iter =
         loopLand->contInitRegs.begin(),
       iterEnd = loopLand->contInitRegs.end(); iter != iterEnd; ++iter) {
    CFGTraits::insertAssignInstrBefore(dstBlk, passRep, *iter, 0);
  }

  // Loop endbranchInitRegs are init after entering the loop.
  for (typename std::set<RegiT>::const_iterator iter =
         loopLand->endbranchInitRegs.begin(),
       iterEnd = loopLand->endbranchInitRegs.end(); iter != iterEnd; ++iter) {
    CFGTraits::insertAssignInstrBefore(dstBlk, passRep, *iter, 0);
  }

  /* we last inserterd the DebugLoc in the
   * BREAK_LOGICALZi32r or AMDIL::BREAK_LOGICALNZ statement in the current dstBlk.
   * search for the DebugLoc in the that statement.
   * if not found, we have to insert the empty/default DebugLoc */
  InstrT *loopBreakInstr = CFGTraits::getLoopBreakInstr(dstBlk);
  DebugLoc DLBreak =
    (loopBreakInstr) ? loopBreakInstr->getDebugLoc() : DebugLoc();

  // fogbugz #7310: work-around discussed with Uri regarding do-while loops:
  // in case the the WHILELOOP line number is greater than do.body line numbers,
  // take the do.body line number instead.
  MachineBasicBlock::iterator iter = dstBlk->begin();
  MachineInstr *instrDoBody = &(*iter);
  DebugLoc DLBreakDoBody =
    (instrDoBody) ? instrDoBody->getDebugLoc() : DebugLoc();
  DebugLoc DLBreakMin =
    (DLBreak.getLine() < DLBreakDoBody.getLine()) ? DLBreak : DLBreakDoBody;

  CFGTraits::insertInstrBefore(dstBlk, AMDIL::WHILELOOP, passRep, DLBreakMin);
  // Loop breakInitRegs are init before entering the loop.
  for (typename std::set<RegiT>::const_iterator iter =
         loopLand->breakInitRegs.begin(),
       iterEnd = loopLand->breakInitRegs.end(); iter != iterEnd; ++iter)
  {
    CFGTraits::insertAssignInstrBefore(dstBlk, passRep, *iter, 0);
  }

  /* we last inserterd the DebugLoc in the continue statement in the current dstBlk
   * search for the DebugLoc in the continue statement.
   * if not found, we have to insert the empty/default DebugLoc */
  InstrT *continueInstr = CFGTraits::getContinueInstr(dstBlk);
  DebugLoc DLContinue =
    (continueInstr) ? continueInstr->getDebugLoc() : DebugLoc();

  CFGTraits::insertInstrEnd(dstBlk, AMDIL::ENDLOOP, passRep, DLContinue);
  // Loop breakOnRegs are check after the ENDLOOP: break the loop outside this
  // loop.
  for (typename std::set<RegiT>::const_iterator iter =
         loopLand->breakOnRegs.begin(),
       iterEnd = loopLand->breakOnRegs.end(); iter != iterEnd; ++iter) {
    CFGTraits::insertCondBranchEnd(dstBlk, AMDIL::BREAK_LOGICALNZi32r, passRep,
                                   *iter);
  }

  // Loop contOnRegs are check after the ENDLOOP: cont the loop outside this
  // loop.
  for (std::set<RegiT>::const_iterator iter = loopLand->contOnRegs.begin(),
       iterEnd = loopLand->contOnRegs.end(); iter != iterEnd; ++iter) {
    CFGTraits::insertCondBranchEnd(dstBlk, AMDIL::CONTINUE_LOGICALNZi32r,
                                   passRep, *iter);
  }

  dstBlk->splice(dstBlk->end(), landBlk, landBlk->begin(), landBlk->end());

  for (typename BlockT::succ_iterator iter = landBlk->succ_begin(),
       iterEnd = landBlk->succ_end(); iter != iterEnd; ++iter) {
    dstBlk->addSuccessor(*iter);  // *iter's predecessor is also taken care of.
  }

  removeSuccessor(landBlk);
  retireBlock(dstBlk, landBlk);
} //mergeLooplandBlock
template<class PassT>
void CFGStructurizer<PassT>::mergeLoopbreakBlock(BlockT *exitingBlk,
                                                 BlockT *exitBlk,
                                                 BlockT *exitLandBlk,
                                                 RegiT setReg) {
  if (DEBUGME) {
    errs() << "loopbreakPattern exiting = BB" << exitingBlk->getNumber()
           << " exit = BB" << exitBlk->getNumber()
           << " land = BB" << exitLandBlk->getNumber() << "\n";
  }

  InstrT *branchInstr = CFGTraits::getLoopendBlockBranchInstr(exitingBlk);
  assert(branchInstr && CFGTraits::isCondBranch(branchInstr));

  DebugLoc DL = branchInstr->getDebugLoc();

  BlockT *trueBranch = CFGTraits::getTrueBranch(branchInstr);
  int oldOpcode = branchInstr->getOpcode();

  //    transform exitingBlk to
  //    if ( ) {
  //       exitBlk (if exitBlk != exitLandBlk)
  //       setReg = 1
  //       break
  //    }endif
  //    successor = {orgSuccessor(exitingBlk) - exitBlk}

  typename BlockT::iterator branchInstrPos =
    CFGTraits::getInstrPos(exitingBlk, branchInstr);

  if (exitBlk == exitLandBlk && setReg == INVALIDREGNUM) {
    //break_logical
    int newOpcode =
      (trueBranch == exitBlk) ? CFGTraits::getBreakNzeroOpcode(oldOpcode)
      : CFGTraits::getBreakZeroOpcode(oldOpcode);
    CFGTraits::insertCondBranchBefore(branchInstrPos, newOpcode, passRep, DL);
  } else {
    int newOpcode =
      (trueBranch == exitBlk) ? CFGTraits::getBranchNzeroOpcode(oldOpcode)
      : CFGTraits::getBranchZeroOpcode(oldOpcode);
    CFGTraits::insertCondBranchBefore(branchInstrPos, newOpcode, passRep, DL);
    if (exitBlk != exitLandBlk) {
      //splice is insert-before ...
      exitingBlk->splice(branchInstrPos, exitBlk, exitBlk->begin(),
                         exitBlk->end());
    }
    if (setReg != INVALIDREGNUM) {
      CFGTraits::insertAssignInstrBefore(branchInstrPos, passRep, setReg, 1);
    }
    CFGTraits::insertInstrBefore(branchInstrPos, AMDIL::BREAK, passRep);
    CFGTraits::insertInstrBefore(branchInstrPos, AMDIL::ENDIF, passRep);
  } //if_logical

  //now branchInst can be erase safely
  //exitingBlk->eraseFromParent(branchInstr);
  branchInstr->eraseFromParent();

  //now take care of successors, retire blocks
  exitingBlk->removeSuccessor(exitBlk);
  if (exitBlk != exitLandBlk) {
    //splice is insert-before ...
    exitBlk->removeSuccessor(exitLandBlk);
    retireBlock(exitingBlk, exitBlk);
  }
} //mergeLoopbreakBlock
template<class PassT>
void CFGStructurizer<PassT>::settleLoopcontBlock(BlockT *contingBlk,
                                                 BlockT *contBlk,
                                                 RegiT setReg) {
  if (DEBUGME) {
    errs() << "settleLoopcontBlock conting = BB"
           << contingBlk->getNumber()
           << ", cont = BB" << contBlk->getNumber() << "\n";
  }

  InstrT *branchInstr = CFGTraits::getLoopendBlockBranchInstr(contingBlk);
  if (branchInstr) {
    assert(CFGTraits::isCondBranch(branchInstr));
    typename BlockT::iterator branchInstrPos =
      CFGTraits::getInstrPos(contingBlk, branchInstr);
    BlockT *trueBranch = CFGTraits::getTrueBranch(branchInstr);
    int oldOpcode = branchInstr->getOpcode();
    DebugLoc DL = branchInstr->getDebugLoc();

    //    transform contingBlk to
    //     if () {
    //          move instr after branchInstr
    //          continue
    //        or
    //          setReg = 1
    //          break
    //     }endif
    //     successor = {orgSuccessor(contingBlk) - loopHeader}

    bool useContinueLogical =
      (setReg == INVALIDREGNUM && (&*contingBlk->rbegin()) == branchInstr);

    if (useContinueLogical == false)
    {
      int branchOpcode =
        trueBranch == contBlk ? CFGTraits::getBranchNzeroOpcode(oldOpcode)
        : CFGTraits::getBranchZeroOpcode(oldOpcode);

      CFGTraits::insertCondBranchBefore(branchInstrPos, branchOpcode, passRep,
                                        DL);

      if (setReg != INVALIDREGNUM) {
        CFGTraits::insertAssignInstrBefore(branchInstrPos, passRep, setReg, 1);
        // insertEnd to ensure phi-moves, if exist, go before the continue-instr.
        CFGTraits::insertInstrEnd(contingBlk, AMDIL::BREAK, passRep, DL);
      } else {
        // insertEnd to ensure phi-moves, if exist, go before the continue-instr.
        CFGTraits::insertInstrEnd(contingBlk, AMDIL::CONTINUE, passRep, DL);
      }

      CFGTraits::insertInstrEnd(contingBlk, AMDIL::ENDIF, passRep, DL);
    } else {
      int branchOpcode =
        trueBranch == contBlk ? CFGTraits::getContinueNzeroOpcode(oldOpcode)
        : CFGTraits::getContinueZeroOpcode(oldOpcode);

      CFGTraits::insertCondBranchBefore(branchInstrPos, branchOpcode,
                                        passRep, DL);
    }

    //contingBlk->eraseFromParent(branchInstr);
    branchInstr->eraseFromParent();
  } else {
    /* if we've arrived here then we've already erased the branch instruction
         * travel back up the basic block to see the last reference of our debug location
         * we've just inserted that reference here so it should be representative */
    if (setReg != INVALIDREGNUM) {
      CFGTraits::insertAssignInstrBefore(contingBlk, passRep, setReg, 1);
      // insertEnd to ensure phi-moves, if exist, go before the continue-instr.
      CFGTraits::insertInstrEnd(contingBlk, AMDIL::BREAK, passRep,
                                CFGTraits::getLastDebugLocInBB(contingBlk));
    } else {
      // insertEnd to ensure phi-moves, if exist, go before the continue-instr.
      CFGTraits::insertInstrEnd(contingBlk, AMDIL::CONTINUE, passRep,
                                CFGTraits::getLastDebugLocInBB(contingBlk));
    }
  } //else
} //settleLoopcontBlock
// BBs in exitBlkSet are determined as in break-path for loopRep,
// before we can put code for BBs as inside loop-body for loopRep
// check whether those BBs are determined as cont-BB for parentLoopRep
// earlier.
// If so, generate a new BB newBlk
//    (1) set newBlk common successor of BBs in exitBlkSet
//    (2) change the continue-instr in BBs in exitBlkSet to break-instr
//    (3) generate continue-instr in newBlk
//
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::relocateLoopcontBlock(LoopT *parentLoopRep,
                                              LoopT *loopRep,
                                              std::set<BlockT *> &exitBlkSet,
                                              BlockT *exitLandBlk) {
  std::set<BlockT *> endBlkSet;

//  BlockT *parentLoopHead = parentLoopRep->getHeader();

  for (typename std::set<BlockT *>::const_iterator iter = exitBlkSet.begin(),
       iterEnd = exitBlkSet.end();
       iter != iterEnd; ++iter) {
    BlockT *exitBlk = *iter;
    BlockT *endBlk = singlePathEnd(exitBlk, exitLandBlk);

    if (endBlk == NULL || CFGTraits::getContinueInstr(endBlk) == NULL)
      return NULL;

    endBlkSet.insert(endBlk);
  }

  BlockT *newBlk = funcRep->CreateMachineBasicBlock();
  funcRep->push_back(newBlk);  //insert to function
  CFGTraits::insertInstrEnd(newBlk, AMDIL::CONTINUE, passRep);
  SHOWNEWBLK(newBlk, "New continue block: ");

  for (typename std::set<BlockT*>::const_iterator iter = endBlkSet.begin(),
       iterEnd = endBlkSet.end();
       iter != iterEnd; ++iter) {
    BlockT *endBlk = *iter;
    InstrT *contInstr = CFGTraits::getContinueInstr(endBlk);
    if (contInstr) {
      contInstr->eraseFromParent();
    }
    endBlk->addSuccessor(newBlk);
    if (DEBUGME) {
      errs() << "Add new continue Block to BB"
             << endBlk->getNumber() << " successors\n";
    }
  }

  return newBlk;
} //relocateLoopcontBlock
// LoopEndbranchBlock is a BB created by the CFGStructurizer to use as
// LoopLandBlock. This BB branch on the loop endBranchInit register to the
// pathes corresponding to the loop exiting branches.

template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::addLoopEndbranchBlock(LoopT *loopRep,
                                              BlockTSmallerVector &exitingBlks,
                                              BlockTSmallerVector &exitBlks) {
  const TargetInstrInfo *tii = passRep->getTargetInstrInfo();

  RegiT endBranchReg = getRegister(&AMDIL::GPRI32RegClass);
  assert(endBranchReg != INVALIDREGNUM);

  // reg = 0 before entering the loop
  addLoopEndbranchInitReg(loopRep, endBranchReg);

  uint32_t numBlks = static_cast<uint32_t>(exitingBlks.size());
  assert(numBlks >=2 && numBlks == exitBlks.size());

  BlockT *preExitingBlk = exitingBlks[0];
  BlockT *preExitBlk = exitBlks[0];
  BlockT *preBranchBlk = funcRep->CreateMachineBasicBlock();
  funcRep->push_back(preBranchBlk);  //insert to function
  SHOWNEWBLK(preBranchBlk, "New loopEndbranch block: ");

  BlockT *newLandBlk = preBranchBlk;

  CFGTraits::replaceInstrUseOfBlockWith(preExitingBlk, preExitBlk,
                                        newLandBlk);
  preExitingBlk->removeSuccessor(preExitBlk);
  preExitingBlk->addSuccessor(newLandBlk);

  //it is redundant to add reg = 0 to exitingBlks[0]

  // For 1..n th exiting path (the last iteration handles two pathes) create the
  // branch to the previous path and the current path.
  for (uint32_t i = 1; i < numBlks; ++i) {
    BlockT *curExitingBlk = exitingBlks[i];
    BlockT *curExitBlk = exitBlks[i];
    BlockT *curBranchBlk;

    if (i == numBlks - 1) {
      curBranchBlk = curExitBlk;
    } else {
      curBranchBlk = funcRep->CreateMachineBasicBlock();
      addToTraversalBlock(curBranchBlk);
      funcRep->push_back(curBranchBlk);  //insert to function
      SHOWNEWBLK(curBranchBlk, "New loopEndbranch block: ");
    }

    // Add reg = i to exitingBlks[i].
    CFGTraits::insertAssignInstrBefore(curExitingBlk, passRep, endBranchReg, i);

    // Remove the edge (exitingBlks[i] exitBlks[i]) add new edge
    // (exitingBlks[i], newLandBlk).
    CFGTraits::replaceInstrUseOfBlockWith(curExitingBlk, curExitBlk,
                                          newLandBlk);
    curExitingBlk->removeSuccessor(curExitBlk);
    curExitingBlk->addSuccessor(newLandBlk);

    // add to preBranchBlk the branch instruction:
    // if (endBranchReg == preVal)
    //    preExitBlk
    // else
    //    curBranchBlk
    //
    // preValReg = i - 1

    DebugLoc DL;
    RegiT preValReg = getRegister(&AMDIL::GPRI32RegClass);
    MachineInstr* preValInst
      = BuildMI(preBranchBlk, DL, tii->get(AMDIL::LOADCONSTi32), preValReg)
        .addImm(i - 1); //preVal
    SHOWNEWINSTR(preValInst);

    // condResReg = (endBranchReg == preValReg)
    RegiT condResReg = getRegister(&AMDIL::GPRI32RegClass);
    MachineInstr* cmpInst
      = BuildMI(preBranchBlk, DL, tii->get(AMDIL::EQi32rr), condResReg)
        .addReg(endBranchReg).addReg(preValReg);
    SHOWNEWINSTR(cmpInst);

    MachineInstr* condBranchInst
      = BuildMI(preBranchBlk, DL, tii->get(AMDIL::BRANCHi32br))
        .addMBB(preExitBlk).addReg(condResReg);
    SHOWNEWINSTR(condBranchInst);

    preBranchBlk->addSuccessor(preExitBlk);
    preBranchBlk->addSuccessor(curBranchBlk);

    // Update preExitingBlk, preExitBlk, preBranchBlk.
    preExitingBlk = curExitingBlk;
    preExitBlk = curExitBlk;
    preBranchBlk = curBranchBlk;
  }  //end for 1 .. n blocks

  return newLandBlk;
} //addLoopEndbranchBlock
template<class PassT>
typename CFGStructurizer<PassT>::PathToKind
CFGStructurizer<PassT>::singlePathTo(BlockT *srcBlk, BlockT *dstBlk,
                                     bool allowSideEntry) {
  assert(dstBlk);

  if (srcBlk == dstBlk) {
    return SinglePath_InPath;
  }

  while (srcBlk && srcBlk->succ_size() == 1) {
    srcBlk = *srcBlk->succ_begin();
    if (srcBlk == dstBlk) {
      return SinglePath_InPath;
    }

    if (!allowSideEntry && srcBlk->pred_size() > 1) {
      return Not_SinglePath;
    }
  }

  if (srcBlk && srcBlk->succ_size()==0) {
    return SinglePath_NotInPath;
  }

  return Not_SinglePath;
} //singlePathTo
// If there is a single path from srcBlk to dstBlk, return the last block before
// dstBlk If there is a single path from srcBlk->end without dstBlk, return the
// last block in the path Otherwise, return NULL
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::singlePathEnd(BlockT *srcBlk, BlockT *dstBlk,
                                      bool allowSideEntry) {
  //assert(dstBlk);

  if (srcBlk == dstBlk) {
    return srcBlk;
  }

  if (srcBlk->succ_size() == 0) {
    return srcBlk;
  }

  while (srcBlk && srcBlk->succ_size() == 1) {
    BlockT *preBlk = srcBlk;

    srcBlk = *srcBlk->succ_begin();
    if (srcBlk == NULL) {
      return preBlk;
    }

    if (!allowSideEntry && srcBlk->pred_size() > 1) {
      return NULL;
    }
  }

  if (srcBlk && srcBlk->succ_size()==0) {
    return srcBlk;
  }

  return NULL;
} //singlePathEnd
template<class PassT>
int CFGStructurizer<PassT>::cloneOnSideEntryTo(BlockT *preBlk, BlockT *srcBlk,
                                               BlockT *dstBlk) {
  int cloned = 0;
  assert(preBlk->isSuccessor(srcBlk));
  while (srcBlk && srcBlk != dstBlk) {
    assert(srcBlk->succ_size() == 1 ||
           (srcBlk->succ_size() == 0 && dstBlk == NULL));
    if (srcBlk->pred_size() > 1) {
      srcBlk = cloneBlockForPredecessor(srcBlk, preBlk);
      ++cloned;
    }

    preBlk = srcBlk;
    if (srcBlk->succ_size() == 1)
      srcBlk = *srcBlk->succ_begin();
    else
      srcBlk = NULL;
  }

  return cloned;
} //cloneOnSideEntryTo
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::cloneBlockForPredecessor(BlockT *curBlk,
                                                 BlockT *predBlk) {
  assert(predBlk->isSuccessor(curBlk) &&
         "succBlk is not a prececessor of curBlk");

  BlockT *cloneBlk = CFGTraits::clone(curBlk);  //clone instructions
  CFGTraits::replaceInstrUseOfBlockWith(predBlk, curBlk, cloneBlk);
  //srcBlk, oldBlk, newBlk

  predBlk->removeSuccessor(curBlk);
  predBlk->addSuccessor(cloneBlk);

  // add all successor to cloneBlk
  CFGTraits::cloneSuccessorList(cloneBlk, curBlk);

  numClonedInstr += curBlk->size();

  if (DEBUGME) {
    errs() << "Cloned block: " << "BB"
           << curBlk->getNumber() << "size " << curBlk->size() << "\n";
  }

  SHOWNEWBLK(cloneBlk, "result of Cloned block: ");

  return cloneBlk;
} //cloneBlockForPredecessor
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::exitingBlock2ExitBlock(LoopT *loopRep,
                                               BlockT *exitingBlk) {
  BlockT *exitBlk = NULL;

  for (typename BlockT::succ_iterator iterSucc = exitingBlk->succ_begin(),
       iterSuccEnd = exitingBlk->succ_end();
       iterSucc != iterSuccEnd; ++iterSucc) {
    BlockT *curBlk = *iterSucc;
    if (!loopRep->contains(curBlk)) {
      assert(exitBlk == NULL);
      exitBlk = curBlk;
    }
  }

  assert(exitBlk != NULL);

  return exitBlk;
} //exitingBlock2ExitBlock
template<class PassT>
void CFGStructurizer<PassT>::migrateInstruction(BlockT *srcBlk,
                                                BlockT *dstBlk,
                                                InstrIterator insertPos) {
  InstrIterator spliceEnd;
  //look for the input branchinstr, not the AMDIL branchinstr
  InstrT *branchInstr = CFGTraits::getNormalBlockBranchInstr(srcBlk);
  if (branchInstr == NULL) {
    if (DEBUGME) {
      errs() << "migrateInstruction don't see branch instr\n";
    }
    spliceEnd = srcBlk->end();
  } else {
    if (DEBUGME) {
      errs() << "migrateInstruction see branch instr\n";
      branchInstr->dump();
    }
    spliceEnd = CFGTraits::getInstrPos(srcBlk, branchInstr);
  }
  if (DEBUGME) {
    errs() << "migrateInstruction before splice dstSize = " << dstBlk->size()
           << "srcSize = " << srcBlk->size() << "\n";
  }

  //splice insert before insertPos
  dstBlk->splice(insertPos, srcBlk, srcBlk->begin(), spliceEnd);

  if (DEBUGME) {
    errs() << "migrateInstruction after splice dstSize = " << dstBlk->size()
           << "srcSize = " << srcBlk->size() << "\n";
  }
} //migrateInstruction
// normalizeInfiniteLoopExit change
//   B1:
//        uncond_br LoopHeader
//
// to
//   B1:
//        cond_br 1 LoopHeader dummyExit
// and return the newly added dummy exit block
//
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::normalizeInfiniteLoopExit(LoopT* LoopRep) {
  BlockT *loopHeader;
  BlockT *loopLatch;
  loopHeader = LoopRep->getHeader();
  loopLatch = LoopRep->getLoopLatch();
  BlockT *dummyExitBlk = NULL;
  if (loopHeader!=NULL && loopLatch!=NULL) {
    InstrT *branchInstr = CFGTraits::getLoopendBlockBranchInstr(loopLatch);
    if (branchInstr!=NULL && CFGTraits::isUncondBranch(branchInstr)) {
      dummyExitBlk = funcRep->CreateMachineBasicBlock();
      funcRep->push_back(dummyExitBlk);  //insert to function
      SHOWNEWBLK(dummyExitBlk, "DummyExitBlock to normalize infiniteLoop: ");

      if (DEBUGME) errs() << "Old branch instr: " << *branchInstr << "\n";

      typename BlockT::iterator insertPos =
        CFGTraits::getInstrPos(loopLatch, branchInstr);
      unsigned immReg = getRegister(&AMDIL::GPRI32RegClass);
      CFGTraits::insertAssignInstrBefore(insertPos, passRep, immReg, 1);
      InstrT *newInstr =
        CFGTraits::insertInstrBefore(insertPos, AMDIL::BRANCHi32br,
                                     passRep);
      MachineInstrBuilder(newInstr).addMBB(loopHeader).addReg(immReg, false);
      SHOWNEWINSTR(newInstr);
      branchInstr->eraseFromParent();
      loopLatch->addSuccessor(dummyExitBlk);
    }
  }

  return dummyExitBlk;
} //normalizeInfiniteLoopExit
template<class PassT>
void CFGStructurizer<PassT>::removeUnconditionalBranch(BlockT *srcBlk) {
  InstrT *branchInstr;

  // I saw two unconditional branch in one basic block in example
  // test_fc_do_while_or.c need to fix the upstream on this to remove the loop.
  while ((branchInstr = CFGTraits::getLoopendBlockBranchInstr(srcBlk))
         && CFGTraits::isUncondBranch(branchInstr)) {
    if (DEBUGME) {
      errs() << "Removing unconditional branch instruction";
      branchInstr->dump();
    }
    branchInstr->eraseFromParent();
  }
} //removeUnconditionalBranch
template<class PassT>
void CFGStructurizer<PassT>::removeRedundantConditionalBranch(BlockT *srcBlk) {
  if (srcBlk->succ_size() == 2) {
    BlockT *blk1 = *srcBlk->succ_begin();
    BlockT *blk2 = *(srcBlk->succ_begin()+1);

    if (blk1 == blk2) {
      InstrT *branchInstr = CFGTraits::getNormalBlockBranchInstr(srcBlk);
      assert(branchInstr && CFGTraits::isCondBranch(branchInstr));
      if (DEBUGME) {
        errs() << "Removing unneeded conditional branch instruction";
        branchInstr->dump();
      }
      branchInstr->eraseFromParent();
      SHOWNEWBLK(blk1, "Removing redundant successor");
      srcBlk->removeSuccessor(blk1);
    }
  }
} //removeRedundantConditionalBranch
template<class PassT>
void CFGStructurizer<PassT>::addDummyExitBlock(SmallVector<BlockT*,
                                                           DEFAULT_VEC_SLOTS> &
                                               retBlks) {
  BlockT *dummyExitBlk = funcRep->CreateMachineBasicBlock();
  funcRep->push_back(dummyExitBlk);  //insert to function
  CFGTraits::insertInstrEnd(dummyExitBlk, AMDIL::RETURN, passRep);

  for (typename SmallVector<BlockT *, DEFAULT_VEC_SLOTS>::iterator iter =
         retBlks.begin(),
       iterEnd = retBlks.end(); iter != iterEnd; ++iter) {
    BlockT *curBlk = *iter;
    InstrT *curInstr = CFGTraits::getReturnInstr(curBlk);
    if (curInstr) {
      curInstr->eraseFromParent();
    }
#if 0
    if (curBlk->size()==0 && curBlk->pred_size() == 1) {
      if (DEBUGME) {
        errs() << "Replace empty block BB" <<  curBlk->getNumber()
               << " with dummyExitBlock\n";
      }
      BlockT *predb = *curBlk->pred_begin();
      predb->removeSuccessor(curBlk);
      curBlk = predb;
    } //handle empty curBlk
#endif
    curBlk->addSuccessor(dummyExitBlk);
    if (DEBUGME) {
      errs() << "Add dummyExitBlock to BB" << curBlk->getNumber()
             << " successors\n";
    }
  } //for

  SHOWNEWBLK(dummyExitBlk, "DummyExitBlock: ");
} //addDummyExitBlock
template<class PassT>
void CFGStructurizer<PassT>::removeSuccessor(BlockT *srcBlk) {
  while (srcBlk->succ_size()) {
    srcBlk->removeSuccessor(*srcBlk->succ_begin());
  }
}
template<class PassT>
void CFGStructurizer<PassT>::recordSccnum(BlockT *srcBlk, int sccNum) {
  BlockInfo *&srcBlkInfo = blockInfoMap[srcBlk];

  if (srcBlkInfo == NULL) {
    srcBlkInfo = new BlockInfo();
  }

  srcBlkInfo->sccNum = sccNum;
}
template<class PassT>
int CFGStructurizer<PassT>::getSCCNum(BlockT *srcBlk) {
  BlockInfo *srcBlkInfo = blockInfoMap[srcBlk];
  return srcBlkInfo ? srcBlkInfo->sccNum : INVALIDSCCNUM;
}
template<class PassT>
void CFGStructurizer<PassT>::addToTraversalBlock(BlockT *srcBlk) {
  if (DEBUGME) {
    errs() << "AddToTraversal BB" << srcBlk->getNumber() << "\n";
  }

  addedToTraversalBlks.push_back(srcBlk);
}
template<class PassT>
void CFGStructurizer<PassT>::retireBlock(BlockT *dstBlk, BlockT *srcBlk) {
  if (DEBUGME) {
    errs() << "Retiring BB" << srcBlk->getNumber() << "\n";
  }

  BlockInfo *&srcBlkInfo = blockInfoMap[srcBlk];

  if (srcBlkInfo == NULL) {
    srcBlkInfo = new BlockInfo();
  }

  srcBlkInfo->isRetired = true;
  //int i = srcBlk->succ_size();
  //int j = srcBlk->pred_size();
  assert(srcBlk->succ_size() == 0 && srcBlk->pred_size() == 0
         && "can't retire block yet");
}
template<class PassT>
bool CFGStructurizer<PassT>::isRetiredBlock(BlockT *srcBlk) {
  BlockInfo *srcBlkInfo = blockInfoMap[srcBlk];
  return (srcBlkInfo && srcBlkInfo->isRetired);
}
template<class PassT>
bool CFGStructurizer<PassT>::isActiveLoophead(BlockT *curBlk) {
  LoopT *loopRep = loopInfo->getLoopFor(curBlk);
  while (loopRep && loopRep->getHeader() == curBlk) {
    LoopLandInfo *loopLand = getLoopLandInfo(loopRep);

    if(loopLand == NULL)
      return true;

    BlockT *landBlk = loopLand->landBlk;
    assert(landBlk);
    if (!isRetiredBlock(landBlk)) {
      return true;
    }

    loopRep = loopRep->getParentLoop();
  }

  return false;
} //isActiveLoophead
template<class PassT>
bool CFGStructurizer<PassT>::needMigrateBlock(BlockT *blk) {
  const unsigned blockSizeThreshold = 30;
  const unsigned cloneInstrThreshold = 100;

  bool multiplePreds = blk && (blk->pred_size() > 1);

  if(!multiplePreds)
    return false;

  unsigned blkSize = blk->size();
  return ((blkSize > blockSizeThreshold)
          && (blkSize * (blk->pred_size() - 1) > cloneInstrThreshold));
} //needMigrateBlock
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::recordLoopLandBlock(LoopT *loopRep, BlockT *landBlk,
                                            BlockTSmallerVector &exitBlks,
                                            std::set<BlockT *> &exitBlkSet) {
  SmallVector<BlockT *, DEFAULT_VEC_SLOTS> inpathBlks;  //in exit path blocks

  for (typename BlockT::pred_iterator predIter = landBlk->pred_begin(),
       predIterEnd = landBlk->pred_end();
       predIter != predIterEnd; ++predIter) {
    BlockT *curBlk = *predIter;
    if (curBlk == landBlk) continue;  // in case landBlk is a single-block loop
    if (loopRep->contains(curBlk) || exitBlkSet.count(curBlk)) {
      inpathBlks.push_back(curBlk);
    }
  } //for

  //if landBlk has predecessors that are not in the given loop,
  //create a new block
  BlockT *newLandBlk = landBlk;
  if (inpathBlks.size() != landBlk->pred_size()) {
    newLandBlk = funcRep->CreateMachineBasicBlock();
    funcRep->push_back(newLandBlk);  //insert to function
    newLandBlk->addSuccessor(landBlk);
    for (typename SmallVector<BlockT*, DEFAULT_VEC_SLOTS>::iterator iter =
           inpathBlks.begin(),
         iterEnd = inpathBlks.end(); iter != iterEnd; ++iter) {
      BlockT *curBlk = *iter;
      CFGTraits::replaceInstrUseOfBlockWith(curBlk, landBlk, newLandBlk);
      //srcBlk, oldBlk, newBlk
      curBlk->removeSuccessor(landBlk);
      curBlk->addSuccessor(newLandBlk);
    }
    for (size_t i = 0, tot = exitBlks.size(); i < tot; ++i) {
      if (exitBlks[i] == landBlk) {
        exitBlks[i] = newLandBlk;
      }
    }
    SHOWNEWBLK(newLandBlk, "NewLandingBlock: ");
  }

  setLoopLandBlock(loopRep, newLandBlk);

  return newLandBlk;
} // recordLoopbreakLand
template<class PassT>
void CFGStructurizer<PassT>::setLoopLandBlock(LoopT *loopRep, BlockT *blk) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }
  assert(theEntry->landBlk == NULL);

  if (blk == NULL) {
    blk = funcRep->CreateMachineBasicBlock();
    funcRep->push_back(blk);  //insert to function
    SHOWNEWBLK(blk, "DummyLandingBlock for loop without break: ");
  }

  theEntry->landBlk = blk;

  if (DEBUGME) {
    errs() << "setLoopLandBlock loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  landing-block = BB" << blk->getNumber() << "\n";
  }
} // setLoopLandBlock
template<class PassT>
void CFGStructurizer<PassT>::addLoopBreakOnReg(LoopT *loopRep, RegiT regNum) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }

  theEntry->breakOnRegs.insert(regNum);

  if (DEBUGME) {
    errs() << "addLoopBreakOnReg loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  regNum = " << regNum << "\n";
  }
} // addLoopBreakOnReg
template<class PassT>
void CFGStructurizer<PassT>::addLoopContOnReg(LoopT *loopRep, RegiT regNum) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }
  theEntry->contOnRegs.insert(regNum);

  if (DEBUGME) {
    errs() << "addLoopContOnReg loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  regNum = " << regNum << "\n";
  }
} // addLoopContOnReg
template<class PassT>
void CFGStructurizer<PassT>::addLoopBreakInitReg(LoopT *loopRep,
                                                 RegiT regNum) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }
  theEntry->breakInitRegs.insert(regNum);

  if (DEBUGME) {
    errs() << "addLoopBreakInitReg loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  regNum = " << regNum << "\n";
  }
} // addLoopBreakInitReg
template<class PassT>
void CFGStructurizer<PassT>::addLoopContInitReg(LoopT *loopRep, RegiT regNum) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }
  theEntry->contInitRegs.insert(regNum);

  if (DEBUGME) {
    errs() << "addLoopContInitReg loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  regNum = " << regNum << "\n";
  }
} // addLoopContInitReg
template<class PassT>
void CFGStructurizer<PassT>::addLoopEndbranchInitReg(LoopT *loopRep,
                                                     RegiT regNum) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  if (theEntry == NULL) {
    theEntry = new LoopLandInfo();
  }
  theEntry->endbranchInitRegs.insert(regNum);

  if (DEBUGME)
  {
    errs() << "addLoopEndbranchInitReg loop-header = BB"
           << loopRep->getHeader()->getNumber()
           << "  regNum = " << regNum << "\n";
  }
} // addLoopEndbranchInitReg
template<class PassT>
typename CFGStructurizer<PassT>::LoopLandInfo *
CFGStructurizer<PassT>::getLoopLandInfo(LoopT *loopRep) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  return theEntry;
} // getLoopLandInfo
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::getLoopLandBlock(LoopT *loopRep) {
  LoopLandInfo *&theEntry = loopLandInfoMap[loopRep];

  return theEntry ? theEntry->landBlk : NULL;
} // getLoopLandBlock
template<class PassT>
bool CFGStructurizer<PassT>::hasBackEdge(BlockT *curBlk) {
  LoopT *loopRep = loopInfo->getLoopFor(curBlk);
  if (loopRep == NULL)
    return false;

  BlockT *loopHeader = loopRep->getHeader();

  return curBlk->isSuccessor(loopHeader);
} //hasBackEdge
template<class PassT>
unsigned CFGStructurizer<PassT>::getLoopDepth(LoopT *loopRep) {
  return loopRep ? loopRep->getLoopDepth() : 0;
} //getLoopDepth
template<class PassT>
int CFGStructurizer<PassT>::countActiveBlock
  (typename SmallVector<BlockT*, DEFAULT_VEC_SLOTS>::const_iterator iterStart,
  typename SmallVector<BlockT*, DEFAULT_VEC_SLOTS>::const_iterator iterEnd) {
  int count = 0;
  while (iterStart != iterEnd) {
    if (!isRetiredBlock(*iterStart)) {
      ++count;
    }
    ++iterStart;
  }

  return count;
} //countActiveBlock
// This is work around solution for findNearestCommonDominator not avaiable to
// post dom a proper fix should go to Dominators.h.

template<class PassT>
typename CFGStructurizer<PassT>::BlockT*
CFGStructurizer<PassT>::findNearestCommonPostDom(BlockT *blk1, BlockT *blk2) {
  if (postDomTree->dominates(blk1, blk2)) {
    return blk1;
  }
  if (postDomTree->dominates(blk2, blk1)) {
    return blk2;
  }

  DomTreeNodeT *node1 = postDomTree->getNode(blk1);
  DomTreeNodeT *node2 = postDomTree->getNode(blk2);

  // Handle newly cloned node.
  if (node1 == NULL && blk1->succ_size() == 1) {
    return findNearestCommonPostDom(*blk1->succ_begin(), blk2);
  }
  if (node2 == NULL && blk2->succ_size() == 1) {
    return findNearestCommonPostDom(blk1, *blk2->succ_begin());
  }

  if (node1 == NULL || node2 == NULL) {
    return NULL;
  }

  node1 = node1->getIDom();
  while (node1) {
    if (postDomTree->dominates(node1, node2)) {
      return node1->getBlock();
    }
    node1 = node1->getIDom();
  }

  return NULL;
}
template<class PassT>
typename CFGStructurizer<PassT>::BlockT *
CFGStructurizer<PassT>::findNearestCommonPostDom
  (typename std::set<BlockT *> &blks) {
  BlockT *commonDom;
  typename std::set<BlockT *>::const_iterator iter = blks.begin();
  typename std::set<BlockT *>::const_iterator iterEnd = blks.end();
  for (commonDom = *iter; iter != iterEnd && commonDom != NULL; ++iter) {
    BlockT *curBlk = *iter;
    if (curBlk != commonDom) {
      commonDom = findNearestCommonPostDom(curBlk, commonDom);
    }
  }

  if (DEBUGME) {
    errs() << "Common post dominator for exit blocks is ";
    if (commonDom) {
      errs() << "BB" << commonDom->getNumber() << "\n";
    } else {
      errs() << "NULL\n";
    }
  }

  return commonDom;
} //findNearestCommonPostDom
} //end namespace llvm

//todo: move-end

//===----------------------------------------------------------------------===//
//
// CFGStructurizer for AMDIL
//
//===----------------------------------------------------------------------===//

using namespace llvmCFGStruct;

namespace llvm
{
class AMDILCFGStructurizer : public MachineFunctionPass
{
public:
  typedef MachineInstr InstructionType;
  typedef MachineFunction FunctionType;
  typedef MachineBasicBlock BlockType;
  typedef MachineLoopInfo LoopinfoType;
  typedef MachineDominatorTree DominatortreeType;
  typedef MachinePostDominatorTree PostDominatortreeType;
  typedef MachineDomTreeNode DomTreeNodeType;
  typedef MachineLoop LoopType;
//private:
  const TargetInstrInfo *TII;

//public:
//  static char ID;

public:
  AMDILCFGStructurizer(char &pid);
  const TargetInstrInfo *getTargetInstrInfo() const;
  // this is abstract base class
  virtual bool runOnMachineFunction(MachineFunction &F) = 0;

private:
};   //end of class AMDILCFGStructurizer

//char AMDILCFGStructurizer::ID = 0;
} //end of namespace llvm
AMDILCFGStructurizer::AMDILCFGStructurizer(char &pid)
  : MachineFunctionPass(pid), TII(NULL) {
}
const TargetInstrInfo *AMDILCFGStructurizer::getTargetInstrInfo() const {
  return TII;
}
//===----------------------------------------------------------------------===//
//
// CFGPrepare
//
//===----------------------------------------------------------------------===//

namespace llvm
{
extern void initializeAMDILCFGPreparePass(llvm::PassRegistry&);
}

using namespace llvmCFGStruct;

namespace llvm
{
class AMDILCFGPrepare : public AMDILCFGStructurizer
{
public:
  static char ID;

public:
  AMDILCFGPrepare();

  virtual const char *getPassName() const;
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;

  bool runOnMachineFunction(MachineFunction &F);

private:
};   //end of class AMDILCFGPrepare

char AMDILCFGPrepare::ID = 0;
} //end of namespace llvm

AMDILCFGPrepare::AMDILCFGPrepare()
  : AMDILCFGStructurizer(ID)
{
  initializeAMDILCFGPreparePass(*PassRegistry::getPassRegistry());
}
const char *AMDILCFGPrepare::getPassName() const {
  return "AMD IL Control Flow Graph Preparation Pass";
}
void AMDILCFGPrepare::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addPreserved<MachineFunctionAnalysis>();
  AU.addRequired<MachineFunctionAnalysis>();
  AU.addRequired<MachineDominatorTree>();
  AU.addRequired<MachinePostDominatorTree>();
  AU.addRequired<MachineLoopInfo>();
}
//===----------------------------------------------------------------------===//
//
// CFGPerform
//
//===----------------------------------------------------------------------===//

namespace llvm
{
extern void initializeAMDILCFGPerformPass(llvm::PassRegistry&);
}

using namespace llvmCFGStruct;

namespace llvm
{
class AMDILCFGPerform : public AMDILCFGStructurizer
{
public:
  static char ID;

public:
  AMDILCFGPerform();
  virtual const char *getPassName() const;
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;
  bool runOnMachineFunction(MachineFunction &F);

private:
};   //end of class AMDILCFGPerform

char AMDILCFGPerform::ID = 0;
} //end of namespace llvm

AMDILCFGPerform::AMDILCFGPerform()
  : AMDILCFGStructurizer(ID)
{
  initializeAMDILCFGPerformPass(*PassRegistry::getPassRegistry());
}
const char *AMDILCFGPerform::getPassName() const {
  return "AMD IL Control Flow Graph structurizer Pass";
}
void AMDILCFGPerform::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addPreserved<MachineFunctionAnalysis>();
  AU.addRequired<MachineFunctionAnalysis>();
  AU.addRequired<MachineDominatorTree>();
  AU.addRequired<MachinePostDominatorTree>();
  AU.addRequired<MachineLoopInfo>();
}
//===----------------------------------------------------------------------===//
//
// CFGStructTraits<AMDILCFGStructurizer>
//
//===----------------------------------------------------------------------===//

namespace llvmCFGStruct
{
// this class is tailor to the AMDIL backend
template<>
struct CFGStructTraits<AMDILCFGStructurizer>
{
  typedef int RegiT;

  static int getBreakNzeroOpcode(int oldOpcode) {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::BREAK_LOGICALNZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::BREAK_LOGICALNZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::BREAK_LOGICALNZi64r;
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi32br: return AMDIL::BREAK_LOGICALNZi32r;
    case AMDIL::BRANCHi16bi:
    case AMDIL::BRANCHi16br: return AMDIL::BREAK_LOGICALNZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::BREAK_LOGICALNZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
  static int getBreakZeroOpcode(int oldOpcode) {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::BREAK_LOGICALZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::BREAK_LOGICALZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::BREAK_LOGICALZi64r;
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi32br: return AMDIL::BREAK_LOGICALZi32r;
    case AMDIL::BRANCHi16bi:
    case AMDIL::BRANCHi16br: return AMDIL::BREAK_LOGICALZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::BREAK_LOGICALZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
  static int getBranchNzeroOpcode(int oldOpcode) {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::IF_LOGICALNZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::IF_LOGICALNZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::IF_LOGICALNZi64r;
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi32br: return AMDIL::IF_LOGICALNZi32r;
    case AMDIL::BRANCHi16bi:
    case AMDIL::BRANCHi16br: return AMDIL::IF_LOGICALNZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::IF_LOGICALNZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
  static int getBranchZeroOpcode(int oldOpcode) {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::IF_LOGICALZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::IF_LOGICALZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::IF_LOGICALZi64r;
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi32br: return AMDIL::IF_LOGICALZi32r;
    case AMDIL::BRANCHi16br:
    case AMDIL::BRANCHi16bi: return AMDIL::IF_LOGICALZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::IF_LOGICALZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
  static int getContinueNzeroOpcode(int oldOpcode)
  {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::CONTINUE_LOGICALNZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::CONTINUE_LOGICALNZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::CONTINUE_LOGICALNZi64r;
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi32br: return AMDIL::CONTINUE_LOGICALNZi32r;
    case AMDIL::BRANCHi16bi:
    case AMDIL::BRANCHi16br: return AMDIL::CONTINUE_LOGICALNZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::CONTINUE_LOGICALNZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
  static int getContinueZeroOpcode(int oldOpcode) {
    switch(oldOpcode) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf64br: return AMDIL::CONTINUE_LOGICALZf64r;
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHf32br: return AMDIL::CONTINUE_LOGICALZf32r;
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi64br: return AMDIL::CONTINUE_LOGICALZi64r;
    case AMDIL::BRANCHi32br:
    case AMDIL::BRANCHi32bi: return AMDIL::CONTINUE_LOGICALZi32r;
    case AMDIL::BRANCHi16br:
    case AMDIL::BRANCHi16bi: return AMDIL::CONTINUE_LOGICALZi16r;
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHi8br:  return AMDIL::CONTINUE_LOGICALZi8r;
    default:
      assert(0 && "internal error");
    };
    return -1;
  }
// the explicitly represented branch target is the true branch target
#define getExplicitBranch getTrueBranch
#define setExplicitBranch setTrueBranch

  static MachineBasicBlock *getTrueBranch(MachineInstr *instr) {
    return instr->getOperand(0).getMBB();
  }
  static void setTrueBranch(MachineInstr *instr, MachineBasicBlock *blk) {
    instr->getOperand(0).setMBB(blk);
  }
  static MachineBasicBlock *
  getFalseBranch(MachineBasicBlock *blk, MachineInstr *instr) {
    assert(blk->succ_size() == 2);
    MachineBasicBlock *trueBranch = getTrueBranch(instr);
    MachineBasicBlock::succ_iterator iter = blk->succ_begin();
    MachineBasicBlock::succ_iterator iterNext = iter;
    ++iterNext;

    return (*iter == trueBranch) ? *iterNext : *iter;
  }
  static bool isCondBranch(MachineInstr *instr) {
    switch (instr->getOpcode()) {
    case AMDIL::BRANCHf64bi:
    case AMDIL::BRANCHf32bi:
    case AMDIL::BRANCHi64bi:
    case AMDIL::BRANCHi32bi:
    case AMDIL::BRANCHi16bi:
    case AMDIL::BRANCHi8bi:
    case AMDIL::BRANCHf64br:
    case AMDIL::BRANCHf32br:
    case AMDIL::BRANCHi64br:
    case AMDIL::BRANCHi32br:
    case AMDIL::BRANCHi16br:
    case AMDIL::BRANCHi8br:
      break;
    default:
      return false;
    }
    return true;
  }
  static bool isUncondBranch(MachineInstr *instr) {
    switch (instr->getOpcode()) {
    case AMDIL::BRANCHb:
      break;
    default:
      return false;
    }
    return true;
  }
  static DebugLoc getLastDebugLocInBB(MachineBasicBlock *blk) {
    //get DebugLoc from the first MachineBasicBlock instruction with debug info
    DebugLoc DL;
    for (MachineBasicBlock::iterator iter = blk->begin();
         iter != blk->end();
         ++iter) {
      MachineInstr *instr = &(*iter);
      if (instr->getDebugLoc().isUnknown() == false) {
        DL = instr->getDebugLoc();
      }
    }
    return DL;
  }
  static MachineInstr *getNormalBlockBranchInstr(MachineBasicBlock *blk) {
    MachineBasicBlock::reverse_iterator iter = blk->rbegin();
    MachineInstr *instr = &*iter;
    if (instr && (isCondBranch(instr) || isUncondBranch(instr))) {
      return instr;
    }
    return NULL;
  }
  // The correct naming for this is getPossibleLoopendBlockBranchInstr.
  //
  // BB with backward-edge could have move instructions after the branch
  // instruction.  Such move instruction "belong to" the loop backward-edge.
  //
  static MachineInstr *getLoopendBlockBranchInstr(MachineBasicBlock *blk) {
    for (MachineBasicBlock::reverse_iterator iter = blk->rbegin(),
         iterEnd = blk->rend(); iter != iterEnd; ++iter) {
      // FIXME: Simplify
      MachineInstr *instr = &*iter;
      if (instr) {
        if (isCondBranch(instr) || isUncondBranch(instr)) {
          return instr;
        } else if (instr->getOpcode() == TargetOpcode::COPY) {
          break;
        }
      }
    }
    return NULL;
  }
  static MachineInstr *getReturnInstr(MachineBasicBlock *blk) {
    MachineBasicBlock::reverse_iterator iter = blk->rbegin();
    if (iter != blk->rend()) {
      MachineInstr *instr = &(*iter);
      if (instr->getOpcode() == AMDIL::RETURN) {
        return instr;
      }
    }
    return NULL;
  }
  static MachineInstr *getContinueInstr(MachineBasicBlock *blk) {
    MachineBasicBlock::reverse_iterator iter = blk->rbegin();
    if (iter != blk->rend()) {
      MachineInstr *instr = &(*iter);
      if (instr->getOpcode() == AMDIL::CONTINUE) {
        return instr;
      }
    }
    return NULL;
  }
  static MachineInstr *getLoopBreakInstr(MachineBasicBlock *blk) {
    for (MachineBasicBlock::iterator iter = blk->begin();
         (iter != blk->end());
         ++iter) {
      MachineInstr *instr = &(*iter);
      if ((instr->getOpcode() == AMDIL::BREAK_LOGICALNZi32r) ||
          (instr->getOpcode() == AMDIL::BREAK_LOGICALZi32r)) {
        return instr;
      }
    }
    return NULL;
  }
  static bool isReturnBlock(MachineBasicBlock *blk) {
    MachineInstr *instr = getReturnInstr(blk);
    bool isReturn = (blk->succ_size() == 0);
    if (instr) {
      assert(isReturn);
    } else if (isReturn) {
      if (DEBUGME) {
        errs() << "BB" << blk->getNumber()
               <<" is return block without RETURN instr\n";
      }
    }

    return isReturn;
  }
  static MachineBasicBlock::iterator
  getInstrPos(MachineBasicBlock *blk, MachineInstr *instr) {
    assert(instr->getParent() == blk && "instruction doesn't belong to block");
    MachineBasicBlock::iterator iter = blk->begin();
    MachineBasicBlock::iterator iterEnd = blk->end();
    while (&(*iter) != instr && iter != iterEnd) {
      ++iter;
    }

    assert(iter != iterEnd);
    return iter;
  } //getInstrPos
  static MachineInstr *insertInstrBefore(MachineBasicBlock *blk, int newOpcode,
                                         AMDILCFGStructurizer *passRep) {
    return insertInstrBefore(blk,newOpcode,passRep,DebugLoc());
  } //insertInstrBefore
  static MachineInstr *insertInstrBefore(MachineBasicBlock *blk,
                                         int newOpcode,
                                         AMDILCFGStructurizer *passRep,
                                         DebugLoc DL) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(newOpcode), DL);

    MachineBasicBlock::iterator res;
    if (blk->begin() != blk->end()) {
      blk->insert(blk->begin(), newInstr);
    } else {
      blk->push_back(newInstr);
    }

    SHOWNEWINSTR(newInstr);

    return newInstr;
  } //insertInstrBefore
  static void insertInstrEnd(MachineBasicBlock *blk, int newOpcode,
                             AMDILCFGStructurizer *passRep) {
    insertInstrEnd(blk,newOpcode,passRep,DebugLoc());
  } //insertInstrEnd
  static void insertInstrEnd(MachineBasicBlock *blk, int newOpcode,
                             AMDILCFGStructurizer *passRep, DebugLoc DL) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineInstr *newInstr = blk->getParent()
                             ->CreateMachineInstr(tii->get(newOpcode), DL);

    blk->push_back(newInstr);
    //assume the instruction doesn't take any reg operand ...

    SHOWNEWINSTR(newInstr);
  } //insertInstrEnd
  static MachineInstr *insertInstrBefore(MachineBasicBlock::iterator instrPos,
                                         int newOpcode,
                                         AMDILCFGStructurizer *passRep,
                                         DebugLoc DL) {
    MachineInstr *oldInstr = &(*instrPos);
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineBasicBlock *blk = oldInstr->getParent();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(newOpcode), DL);

    blk->insert(instrPos, newInstr);
    //assume the instruction doesn't take any reg operand ...

    SHOWNEWINSTR(newInstr);
    return newInstr;
  } //insertInstrBefore
  static MachineInstr *insertInstrBefore(MachineBasicBlock::iterator instrPos,
                                         int newOpcode,
                                         AMDILCFGStructurizer *passRep) {
    return insertInstrBefore(instrPos, newOpcode, passRep, DebugLoc());
  } //insertInstrBefore
  static void insertCondBranchBefore(MachineBasicBlock::iterator instrPos,
                                     int newOpcode,
                                     AMDILCFGStructurizer *passRep,
                                     DebugLoc DL) {
    MachineInstr *oldInstr = &(*instrPos);
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineBasicBlock *blk = oldInstr->getParent();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(newOpcode),
                                           DL);

    blk->insert(instrPos, newInstr);
    if (oldInstr->getOperand(1).isReg()) {
      MachineInstrBuilder(newInstr).addReg(
        oldInstr->getOperand(1).getReg(), false);
    } else if (oldInstr->getOperand(1).isImm()) {
      MachineInstrBuilder(newInstr).addImm(
        oldInstr->getOperand(1).getImm());
    } else if (oldInstr->getOperand(1).isFPImm()) {
      MachineInstrBuilder(newInstr).addFPImm(
        oldInstr->getOperand(1).getFPImm());
    }

    SHOWNEWINSTR(newInstr);
    //erase later oldInstr->eraseFromParent();
  } //insertCondBranchBefore
  static void insertCondBranchBefore(MachineBasicBlock *blk,
                                     MachineBasicBlock::iterator insertPos,
                                     int newOpcode,
                                     AMDILCFGStructurizer *passRep,
                                     RegiT regNum,
                                     DebugLoc DL) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();

    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(newOpcode), DL);

    //insert before
    blk->insert(insertPos, newInstr);
    MachineInstrBuilder(newInstr).addReg(regNum, false);

    SHOWNEWINSTR(newInstr);
  } //insertCondBranchBefore
  static void insertCondBranchEnd(MachineBasicBlock *blk,
                                  int newOpcode,
                                  AMDILCFGStructurizer *passRep,
                                  RegiT regNum) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(newOpcode), DebugLoc());

    blk->push_back(newInstr);
    MachineInstrBuilder(newInstr).addReg(regNum, false);

    SHOWNEWINSTR(newInstr);
  } //insertCondBranchEnd
  static void insertAssignInstrBefore(MachineBasicBlock::iterator instrPos,
                                      AMDILCFGStructurizer *passRep,
                                      RegiT regNum, int regVal) {
    MachineInstr *oldInstr = &(*instrPos);
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineBasicBlock *blk = oldInstr->getParent();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(AMDIL::LOADCONSTi32),
                                           DebugLoc());
    MachineInstrBuilder(newInstr).addReg(regNum, RegState::Define); //set target
    MachineInstrBuilder(newInstr).addImm(regVal); //set src value

    blk->insert(instrPos, newInstr);

    SHOWNEWINSTR(newInstr);
  } //insertAssignInstrBefore
  static void insertAssignInstrBefore(MachineBasicBlock *blk,
                                      AMDILCFGStructurizer *passRep,
                                      RegiT regNum, int regVal) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();

    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(AMDIL::LOADCONSTi32),
                                           DebugLoc());
    MachineInstrBuilder(newInstr).addReg(regNum, RegState::Define); //set target
    MachineInstrBuilder(newInstr).addImm(regVal); //set src value

    if (blk->begin() != blk->end()) {
      blk->insert(blk->begin(), newInstr);
    } else {
      blk->push_back(newInstr);
    }

    SHOWNEWINSTR(newInstr);
  } //insertInstrBefore
  static void insertCompareInstrBefore(MachineBasicBlock *blk,
                                       MachineBasicBlock::iterator instrPos,
                                       AMDILCFGStructurizer *passRep,
                                       RegiT dstReg, RegiT src1Reg,
                                       RegiT src2Reg) {
    const TargetInstrInfo *tii = passRep->getTargetInstrInfo();
    MachineInstr *newInstr =
      blk->getParent()->CreateMachineInstr(tii->get(AMDIL::EQi32rr), DebugLoc());

    MachineInstrBuilder(newInstr).addReg(dstReg, RegState::Define); //set target
    MachineInstrBuilder(newInstr).addReg(src1Reg); //set src value
    MachineInstrBuilder(newInstr).addReg(src2Reg); //set src value

    blk->insert(instrPos, newInstr);
    SHOWNEWINSTR(newInstr);
  } //insertCompareInstrBefore
  static void cloneSuccessorList(MachineBasicBlock *dstBlk,
                                 MachineBasicBlock *srcBlk) {
    for (MachineBasicBlock::succ_iterator iter = srcBlk->succ_begin(),
         iterEnd = srcBlk->succ_end(); iter != iterEnd; ++iter) {
      dstBlk->addSuccessor(*iter);  // *iter's predecessor is also taken care of
    }
  } //cloneSuccessorList
  static MachineBasicBlock *clone(MachineBasicBlock *srcBlk) {
    MachineFunction *func = srcBlk->getParent();
    MachineBasicBlock *newBlk = func->CreateMachineBasicBlock();
    func->push_back(newBlk);  //insert to function
    //newBlk->setNumber(srcBlk->getNumber());
    for (MachineBasicBlock::const_instr_iterator iter = srcBlk->instr_begin(),
         iterEnd = srcBlk->instr_end();
         iter != iterEnd; ++iter) {
      MachineInstr *instr = func->CloneMachineInstr(iter);
      // This is a workaround for LLVM bugzilla 8420 because CloneMachineInstr
      // does not clone the AsmPrinterFlags.
      instr->setAsmPrinterFlag(
        (llvm::MachineInstr::CommentFlag)iter->getAsmPrinterFlags());
      newBlk->push_back(instr);
    }
    return newBlk;
  }
  //MachineBasicBlock::ReplaceUsesOfBlockWith doesn't serve the purpose because
  //the AMDIL instruction is not recognized as terminator fix this and retire
  //this routine
  static void replaceInstrUseOfBlockWith(MachineBasicBlock *srcBlk,
                                         MachineBasicBlock *oldBlk,
                                         MachineBasicBlock *newBlk) {
    MachineInstr *branchInstr = getLoopendBlockBranchInstr(srcBlk);
    if (branchInstr && isCondBranch(branchInstr) &&
        getExplicitBranch(branchInstr) == oldBlk) {
      setExplicitBranch(branchInstr, newBlk);
    }
  }
  static void wrapup(MachineBasicBlock *entryBlk) {
    assert((!entryBlk->getParent()->getJumpTableInfo()
            || entryBlk->getParent()->getJumpTableInfo()->isEmpty())
           && "found a jump table");

    //collect continue right before endloop
    SmallVector<MachineInstr *, DEFAULT_VEC_SLOTS> contInstr;
    MachineBasicBlock::iterator pre = entryBlk->begin();
    MachineBasicBlock::iterator iterEnd = entryBlk->end();
    MachineBasicBlock::iterator iter = pre;
    while (iter != iterEnd) {
      if (pre->getOpcode() == AMDIL::CONTINUE
          && iter->getOpcode() == AMDIL::ENDLOOP) {
        contInstr.push_back(pre);
      }
      pre = iter;
      ++iter;
    }  //end while

    //delete continue right before endloop
    for (unsigned i = 0; i < contInstr.size(); ++i) {
      contInstr[i]->eraseFromParent();
    }

    // TODO to fix up jump table so later phase won't be confused.  if
    // (jumpTableInfo->isEmpty() == false) { need to clean the jump table, but
    // there isn't such an interface yet.  alternatively, replace all the other
    // blocks in the jump table with the entryBlk //}
  } //wrapup
  static MachineDominatorTree *getDominatorTree(AMDILCFGStructurizer &pass) {
    return &pass.getAnalysis<MachineDominatorTree>();
  }
  static MachinePostDominatorTree*
  getPostDominatorTree(AMDILCFGStructurizer &pass) {
    return &pass.getAnalysis<MachinePostDominatorTree>();
  }
  static MachineLoopInfo *getLoopInfo(AMDILCFGStructurizer &pass) {
    return &pass.getAnalysis<MachineLoopInfo>();
  }
}; // template class CFGStructTraits
} //end of namespace llvm

using namespace llvm;

INITIALIZE_PASS_BEGIN(AMDILCFGPrepare, "amdcfgprepare",
                      "AMD IL Control Flow Graph Preparation Pass",
                      false, false);
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree);
INITIALIZE_PASS_DEPENDENCY(MachinePostDominatorTree);
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfo);
INITIALIZE_PASS_END(AMDILCFGPrepare, "amdcfgprepare",
                    "AMD IL Control Flow Graph Preparation Pass",
                    false, false);

INITIALIZE_PASS_BEGIN(AMDILCFGPerform, "amdcfgperform",
                      "AMD IL Control Flow Graph structurizer Pass",
                      false, false);
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree);
INITIALIZE_PASS_DEPENDENCY(MachinePostDominatorTree);
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfo);
INITIALIZE_PASS_END(AMDILCFGPerform, "amdcfgperform",
                    "AMD IL Control Flow Graph structurizer Pass",
                    false, false);

// createAMDILCFGPreparationPass- Returns a pass
FunctionPass *llvm::createAMDILCFGPreparationPass() {
  return new AMDILCFGPrepare();
}
bool AMDILCFGPrepare::runOnMachineFunction(MachineFunction &func) {
  TII = func.getTarget().getInstrInfo();
  return llvmCFGStruct::CFGStructurizer<AMDILCFGStructurizer>().prepare(func,
                                                                        *this);
}
// createAMDILCFGStructurizerPass- Returns a pass
FunctionPass *llvm::createAMDILCFGStructurizerPass() {
  return new AMDILCFGPerform();
}
bool AMDILCFGPerform::runOnMachineFunction(MachineFunction &func) {
  TII = func.getTarget().getInstrInfo();
  return llvmCFGStruct::CFGStructurizer<AMDILCFGStructurizer>().run(func,
                                                                    *this);
}
//end of file newline goes below

