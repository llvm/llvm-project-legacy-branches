//===-- JIT.h - Class definition for the JIT --------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the top-level JIT data structure.
//
//===----------------------------------------------------------------------===//

#ifndef JIT_H
#define JIT_H

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/PassManager.h"
#include "llvm/Support/ValueHandle.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MutexGuard.h"

namespace llvm {

class Function;
struct JITEvent_EmittedFunctionDetails;
class MachineCodeEmitter;
class MachineCodeInfo;
class TargetJITInfo;
class TargetMachine;

class JITState {
private:

  // JIT adaptive compilation. Different pass managers with different levels of
  // optimizations
  // Pass manager for passes to compile functions at CodeGen::OptLevel == NONE
  bool PMOptNoneInited;
  FunctionPassManager PMOptNone;

  // Pass manager for passes to compile functions at CodeGen::OptLevel == LESS
  bool PMOptLessInited;
  FunctionPassManager PMOptLess;

  // Pass manager for passes to compile functions at CodeGen::OptLevel == DEFAULT
  bool PMOptDftInited;
  FunctionPassManager PMOptDft;

  // Pass manager for passes to compile functions at CodeGen::OptLevel == AGGRESSIVE
  bool PMOptAggrInited;
  FunctionPassManager PMOptAggr;

  Module *M;               // Module used to create the PM

  /// PendingFunctions - Functions which have not been code generated yet, but
  /// were called from a function being code generated.
  std::vector<AssertingVH<Function> > PendingFunctions;

  /// FunctionToCallCountMap - Function and the number of times it gets called from the time
  /// it is compiled
  std::map<Function*, int*> FunctionToCallCountMap;

  /// FunctionToCompLevelMap - Function and the current compilation level map
  std::map<Function*, CodeGenOpt::Level> FunctionToCompLevelMap;

  /// FunctionToJITTimeMap - Function and whether it has been JITted before
  std::map<Function*, bool> FunctionToJITTimeMap;

public:

  /// Recompilation threshold for the adaptive compilation framework
  enum JITReCompCount {
    CompilationNone = 100,         // Compile at -O0
    CompilationLess = 1000,        // Compile at -O1
    CompilationDefault = 10000,    // Compile at -O2
    CompilationAggressive = 100000 // Compile at -O3
  };

  /// isPMInited - Return whether the pass manager with the given optimization level
  /// is initialized
  bool isPMInited(CodeGenOpt::Level OptLevel) {
      switch (OptLevel) {
        case CodeGenOpt::None :
             return PMOptNoneInited;
        case CodeGenOpt::Less:
             return PMOptLessInited;
        case CodeGenOpt::Default:
             return PMOptDftInited;
        case CodeGenOpt::Aggressive:
             return PMOptNoneInited;
        default:
            break;
     }
     return false;
   }

  /// setPMInited - Mark the pass manager with the given optimization level
  /// as initialized
  void setPMInited(CodeGenOpt::Level OptLevel) {
      switch (OptLevel) {
        case CodeGenOpt::None :
             PMOptNoneInited = true;
             break;
        case CodeGenOpt::Less:
             PMOptLessInited = true;
             break;
        case CodeGenOpt::Default:
             PMOptDftInited = true;
             break;
        case CodeGenOpt::Aggressive:
             PMOptAggrInited = true;
             break;
        default:
            break;
     }
   }

  /// getSingleInitJIT - Return the optimization level associated with the one and
  /// only one initialized pass manager. Panic if multiple pass managers are
  /// initialized
  CodeGenOpt::Level getSingleInitJIT() {
      int InitedPMCount = 0;
      CodeGenOpt::Level InitedOpt;

      if (PMOptNoneInited) {
         InitedPMCount ++;
         InitedOpt = CodeGenOpt::None;
      }
      if (PMOptLessInited) {
         InitedPMCount ++;
         InitedOpt = CodeGenOpt::Less;
      }
      if (PMOptDftInited) {
         InitedPMCount ++;
         InitedOpt = CodeGenOpt::Default;
      }
      if (PMOptAggrInited) {
         InitedPMCount ++;
         InitedOpt = CodeGenOpt::Aggressive;
      }
      assert(InitedPMCount == 1 && "Invalid number of Pass Managers Initialized");
      return InitedOpt;
   }

  /// getNextCompThrhold - Get the compilation threshold based on the
  /// compilation level given
  int getCompThreshold(CodeGenOpt::Level OptLevel) const {
      switch (OptLevel) {
        case CodeGenOpt::None :
            return CompilationNone;
        case CodeGenOpt::Less:
            return CompilationLess;
        case CodeGenOpt::Default:
            return CompilationDefault;
        case  CodeGenOpt::Aggressive:
            return CompilationAggressive;
        default:
            break;
     }
     return CompilationNone;
  }

  explicit JITState(Module *M)
           : PMOptNoneInited(false), PMOptNone(M), PMOptLessInited(false), PMOptLess(M),
             PMOptDftInited(false), PMOptDft(M), PMOptAggrInited(false), PMOptAggr(M),
             M(M) {}

  /// PopulateFunctionToCallCountMap - Find all the functions in the modules
  /// and allocate an invocation counter to it
  void PopulateFunctionToCallCountMap(bool AdaptiveCompDebug);

  /// isFirstTimeJITed - Return true when this function has not been JITted
  bool isFirstTimeJITed(const MutexGuard &L, Function *F) {
      bool firstTime = FunctionToJITTimeMap.find(F)->second;
      FunctionToJITTimeMap.find(F)->second = false;
      return firstTime;
  }

  /// hasReachedAggrComp - Return true if the last compilation for the function
  /// was an aggressive compilation
  bool hasReachedAggrComp(const MutexGuard &L, Function *F) {
      return FunctionToCompLevelMap.find(F)->second == CodeGenOpt::Aggressive;
  }

  /// getNextCompLevel - Return the next compilation for the function
  CodeGenOpt::Level getNextCompLevel(const MutexGuard &L, Function *F) {
   // Determine the next compilation level based on the current
   // invocation counter
   CodeGenOpt::Level nextCompLevel = CodeGenOpt::None;
   CodeGenOpt::Level CurrentLevel = FunctionToCompLevelMap.find(F)->second;
   switch (CurrentLevel) {
      case CodeGenOpt::None :
        nextCompLevel = CodeGenOpt::Less;
        break;
      case CodeGenOpt::Less :
        nextCompLevel = CodeGenOpt::Default;
        break;
      case CodeGenOpt::Default :
        nextCompLevel = CodeGenOpt::Aggressive;
        break;
      case CodeGenOpt::Aggressive :
        nextCompLevel = CodeGenOpt::Aggressive;
        break;
      default :
        assert(false && "Invalid compilation level given");
   }
   return nextCompLevel;
  }

  // setNextCompLevel - Set the next compilation level for the function
  void setNextCompLevel(const MutexGuard &L, Function *F,
                        CodeGenOpt::Level nextCompLevel) {
      FunctionToCompLevelMap.find(F)->second = nextCompLevel;
  }

  /// getCounterForFunction - Return the invocation counter address
  /// for the given function
  void *getCounterForFunction(const MutexGuard &L, Function *F) {
       return FunctionToCallCountMap.find(F)->second;
  }

  /// setCounterForFunction - Set the invocation count for the given function
  void setCounterForFunction(const MutexGuard &L, Function *F, int newCount) {
     *((int *)FunctionToCallCountMap.find(F)->second) = newCount;
  }

  /// getPM - Return pass manager according to the level of the optimizations
  /// specified. If no optimization level is specified, the pass manager with
  /// CodeGenOpt::Default is returned
  FunctionPassManager &getPM(const MutexGuard &L,
                             CodeGenOpt::Level OptLevel = CodeGenOpt::Default) {
    switch(OptLevel) {
       case CodeGenOpt::None :
          return PMOptNone;
       case CodeGenOpt::Less :
          return PMOptLess;
       case CodeGenOpt::Default :
          return PMOptDft;
       case CodeGenOpt::Aggressive :
          return PMOptAggr;
       default :
          assert(false && "Invalid optimization level given");
    }
  }

  Module *getModule() const { return M; }
  std::vector<AssertingVH<Function> > &getPendingFunctions(const MutexGuard &L){
    return PendingFunctions;
  }
};


class JIT : public ExecutionEngine {
  /// types
  typedef ValueMap<const BasicBlock *, void *>
      BasicBlockAddressMapTy;
  /// data
  TargetMachine &TM;       // The current target we are compiling to
  TargetJITInfo &TJI;      // The JITInfo for the target we are compiling to
  JITCodeEmitter *JCE;     // JCE object
  std::vector<JITEventListener*> EventListeners;

  /// AllocateGVsWithCode - Some applications require that global variables and
  /// code be allocated into the same region of memory, in which case this flag
  /// should be set to true.  Doing so breaks freeMachineCodeForFunction.
  bool AllocateGVsWithCode;

  /// True while the JIT is generating code.  Used to assert against recursive
  /// entry.
  bool isAlreadyCodeGenerating;

  JITState *jitstate;

  /// BasicBlockAddressMap - A mapping between LLVM basic blocks and their
  /// actualized version, only filled for basic blocks that have their address
  /// taken.
  BasicBlockAddressMapTy BasicBlockAddressMap;


  JIT(Module *M, TargetMachine &tm, TargetJITInfo &tji,
      JITMemoryManager *JMM, CodeGenOpt::Level OptLevel,
      bool AllocateGVsWithCode);
public:
  ~JIT();

  static void Register() {
    JITCtor = createJIT;
  }

  /// getJITInfo - Return the target JIT information structure.
  ///
  TargetJITInfo &getJITInfo() const { return TJI; }

  /// create - Create an return a new JIT compiler if there is one available
  /// for the current target.  Otherwise, return null.
  ///
  static ExecutionEngine *create(Module *M,
                                 std::string *Err,
                                 JITMemoryManager *JMM,
                                 CodeGenOpt::Level OptLevel =
                                   CodeGenOpt::Default,
                                 bool GVsWithCode = true,
                                 CodeModel::Model CMM = CodeModel::Default) {
    return ExecutionEngine::createJIT(M, Err, JMM, OptLevel, GVsWithCode,
                                      CMM);
  }

  virtual void addModule(Module *M);

  /// removeModule - Remove a Module from the list of modules.  Returns true if
  /// M is found.
  virtual bool removeModule(Module *M);

  /// runFunction - Start execution with the specified function and arguments.
  ///
  virtual GenericValue runFunction(Function *F,
                                   const std::vector<GenericValue> &ArgValues);

  /// EnableAdaptiveCompilation - When adaptive compilation is on. The JIT will choose
  /// to compile a function with regard to the number of times the function is called
  /// previously. i.e. The JIT will first compile a function at the lowest level of
  /// optimization, but will choose to recompile it at a higher optimization if it is
  /// called frequently
  virtual void EnableAdaptiveCompilation(bool Enabled = true) {
     // Call the Execution Engine function first
     ExecutionEngine::EnableAdaptiveCompilation(Enabled);

     MutexGuard locked(lock);
     // Then the adaptive compilation specific data structure needs
     // to be initialized
     InitializePM(locked, CodeGenOpt::None);
     InitializePM(locked, CodeGenOpt::Less);
     InitializePM(locked, CodeGenOpt::Default);
     InitializePM(locked, CodeGenOpt::Aggressive);

     jitstate->PopulateFunctionToCallCountMap(isAdaptiveCompDebug());
  }

  /// InitializePM - Initialize a pass manager with the given
  /// optimization level
  void InitializePM(MutexGuard &locked, CodeGenOpt::Level OptLevel) {

     if (!jitstate->isPMInited(OptLevel)) {
        // get the specified pass manager
        FunctionPassManager &PM = jitstate->getPM(locked, OptLevel);
        PM.add(new TargetData(*TM.getTargetData()));

        // Turn the machine code intermediate representation into bytes in memory that
        // may be executed.
        if (TM.addPassesToEmitMachineCode(PM, *JCE, OptLevel)) {
           report_fatal_error("Target does not support machine code emission!");
        }
        // Initialize passes.
        PM.doInitialization();
        jitstate->setPMInited(OptLevel);
     }
  }

  /// InitializeAllAvailPMs - Initialize all the pass managers
  /// in the jitstate
  void InitializeAllAvailPMs(MutexGuard &locked) {
     // Initialize all the pass managers available in the JITState
     InitializePM(locked, CodeGenOpt::None);
     InitializePM(locked, CodeGenOpt::Less);
     InitializePM(locked, CodeGenOpt::Default);
     InitializePM(locked, CodeGenOpt::Aggressive);
  }

  /// getPointerToNamedFunction - This method returns the address of the
  /// specified function by using the dlsym function call.  As such it is only
  /// useful for resolving library symbols, not code generated symbols.
  ///
  /// If AbortOnFailure is false and no function with the given name is
  /// found, this function silently returns a null pointer. Otherwise,
  /// it prints a message to stderr and aborts.
  ///
  void *getPointerToNamedFunction(const std::string &Name,
                                  bool AbortOnFailure = true);

  // CompilationCallback - Invoked the first time that a call site is found,
  // which causes lazy compilation of the target function.
  //
  static void CompilationCallback();

  /// getPointerToFunction - This returns the address of the specified function,
  /// compiling it if necessary.
  ///
  void *getPointerToFunction(Function *F);

  /// addPointerToBasicBlock - Adds address of the specific basic block.
  void addPointerToBasicBlock(const BasicBlock *BB, void *Addr);

  /// clearPointerToBasicBlock - Removes address of specific basic block.
  void clearPointerToBasicBlock(const BasicBlock *BB);

  /// getPointerToBasicBlock - This returns the address of the specified basic
  /// block, assuming function is compiled.
  void *getPointerToBasicBlock(BasicBlock *BB);

  /// getOrEmitGlobalVariable - Return the address of the specified global
  /// variable, possibly emitting it to memory if needed.  This is used by the
  /// Emitter.
  void *getOrEmitGlobalVariable(const GlobalVariable *GV);

  /// getPointerToFunctionOrStub - If the specified function has been
  /// code-gen'd, return a pointer to the function.  If not, compile it, or use
  /// a stub to implement lazy compilation if available.
  ///
  void *getPointerToFunctionOrStub(Function *F);

  /// recompileAndRelinkFunction - This method is used to force a function
  /// which has already been compiled, to be compiled again, possibly
  /// after it has been modified. Then the entry to the old copy is overwritten
  /// with a branch to the new copy. If there was no old copy, this acts
  /// just like JIT::getPointerToFunction().
  ///
  void *recompileAndRelinkFunction(Function *F);

  /// freeMachineCodeForFunction - deallocate memory used to code-generate this
  /// Function.
  ///
  void freeMachineCodeForFunction(Function *F);

  /// addPendingFunction - while jitting non-lazily, a called but non-codegen'd
  /// function was encountered.  Add it to a pending list to be processed after
  /// the current function.
  ///
  void addPendingFunction(Function *F);

  /// getCodeEmitter - Return the code emitter this JIT is emitting into.
  ///
  JITCodeEmitter *getCodeEmitter() const { return JCE; }

  static ExecutionEngine *createJIT(Module *M,
                                    std::string *ErrorStr,
                                    JITMemoryManager *JMM,
                                    CodeGenOpt::Level OptLevel,
                                    bool GVsWithCode,
                                    TargetMachine *TM);

  // Run the JIT on F and return information about the generated code
  void runJITOnFunction(Function *F, MachineCodeInfo *MCI = 0);

  virtual void RegisterJITEventListener(JITEventListener *L);
  virtual void UnregisterJITEventListener(JITEventListener *L);
  /// These functions correspond to the methods on JITEventListener.  They
  /// iterate over the registered listeners and call the corresponding method on
  /// each.
  void NotifyFunctionEmitted(
      const Function &F, void *Code, size_t Size,
      const JITEvent_EmittedFunctionDetails &Details);
  void NotifyFreeingMachineCode(void *OldPtr);

  BasicBlockAddressMapTy &
  getBasicBlockAddressMap(const MutexGuard &) {
    return BasicBlockAddressMap;
  }


private:
  static JITCodeEmitter *createEmitter(JIT &J, JITMemoryManager *JMM,
                                       TargetMachine &tm);
  void runJITOnFunctionUnlocked(Function *F, const MutexGuard &locked);
  void updateFunctionStub(Function *F);
  void jitTheFunction(Function *F, const MutexGuard &locked);

protected:

  /// getMemoryforGV - Allocate memory for a global variable.
  virtual char* getMemoryForGV(const GlobalVariable* GV);

};

} // End llvm namespace

#endif
