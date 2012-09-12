//===-- AMDILFormats.td ---------------------------------------------------===//
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

include "AMDILTokenDesc.td"

//===--------------------------------------------------------------------===//
// The parent IL instruction class that inherits the Instruction class. This
// class sets the corresponding namespace, the out and input dag lists the
// pattern to match to and the string to print out for the assembly printer.
//===--------------------------------------------------------------------===//
class ILFormat<ILOpCode op, dag outs, dag ins, string asmstr, list<dag> pattern>
: Instruction {
     let Namespace = "AMDIL";
     dag OutOperandList = outs;
     dag InOperandList = ins;
     ILOpCode operation = op;
     let Pattern = pattern;
     let AsmString = !strconcat(asmstr, "\n");
     bits<2> extLoadType = 0;
     bit load = 0;
     bit store = 0;
     bit truncate = 0;
     bit atomic = 0;
     bit addr64 = 0;
     bit global = 0;
     bit private = 0;
     bit constant = 0;
     bit cpool = 0;
     bit region = 0;
     bit local = 0;
     bit gds = 0;
     bit lds = 0;
     bit cbmem = 0;
     bit scratch = 0;
     bit rawuav = 0;
     bit arenauav = 0;
     bit image = 0;
     bits<2> info = 0;
     bit txld = 0;
     bit sema = 0;
     bit append = 0;
     bit swsextload = 0;
     bit loadconst = 0;
     bit ieee = 0;
     bit zeroop = 0;
     bit flat = 0;
     bits<6> swizzle = 0;
     bit gws = 0;
     bit packed = 0;
     bit sub32bit = 0;
     bits<2> packType = 0;
     bit vector = 0;
     // This has to be kept in sync with (1ULL << AMDIL.h::AMDID) enumeration.
     let TSFlags{2-1} = extLoadType;
     let TSFlags{3} = load;
     let TSFlags{4} = store;
     let TSFlags{5} = truncate;
     let TSFlags{6} = atomic;
     let TSFlags{7} = addr64;
     let TSFlags{8} = global;
     let TSFlags{9} = private;
     let TSFlags{10} = constant;
     let TSFlags{11} = cpool;
     let TSFlags{12} = region;
     let TSFlags{13} = local;
     let TSFlags{14} = gds;
     let TSFlags{15} = lds;
     let TSFlags{16} = cbmem;
     let TSFlags{17} = scratch;
     let TSFlags{18} = rawuav;
     let TSFlags{19} = arenauav;
     let TSFlags{20} = image;
     let TSFlags{22-21} = info;
     let TSFlags{23} = txld;
     let TSFlags{24} = sema;
     let TSFlags{25} = append;
     let TSFlags{26} = swsextload;
     let TSFlags{27} = loadconst;
     let TSFlags{28} = ieee;
     let TSFlags{29} = zeroop;
     let TSFlags{30} = flat;
     let TSFlags{36-31} = swizzle;
     let TSFlags{37} = gws;
     let TSFlags{38} = packed;
     let TSFlags{39} = sub32bit;
     let TSFlags{41-40} = packType;
     let TSFlags{42} = vector;
}

//===--------------------------------------------------------------------===//
// The macro class that is an extension of ILFormat but is tailored for
// macros only where all the register types are the same
//===--------------------------------------------------------------------===//
class UnaryMacro<RegisterClass Dst, RegisterClass Src0, SDNode OpNode>
: ILFormat<IL_OP_MACRO, (outs Dst:$dst),
     (ins Src0:$src0),
     "($dst),($src0)",
     [(set Dst:$dst, (OpNode Src0:$src0))]>;

//===--------------------------------------------------------------------===//
// The macro class is an extension of ILFormat but is tailored for
// macros only where all the register types are the same
//===--------------------------------------------------------------------===//
class BinaryMacro<RegisterClass Dst,
      RegisterClass Src0,
      RegisterClass Src1,
      SDNode OpNode>
      : ILFormat<IL_OP_MACRO, (outs Dst:$dst),
      (ins Src0: $src0, Src1:$src1),
      "($dst),($src0, $src1)",
      [(set Dst:$dst, (OpNode Src0:$src0, Src1:$src1))]>;

//===--------------------------------------------------------------------===//
// Classes for dealing with atomic instructions w/ 32bit pointers
//===--------------------------------------------------------------------===//
class Append<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM3232:$id),
      !strconcat(op.Text, !strconcat(idType," $dst")),
      [(set GPRI32:$dst, (intr ADDR:$id))]>;


class UniAtom<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr")),
      [(set GPRI32:$dst, (intr ADDR:$ptr, timm:$id))]>;


class UniAtomNoRet<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM3232:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr")),
      [(intr ADDR:$ptr, timm:$id)]>;

class BinAtom<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, GPRI32:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src")),
      [(set GPRI32:$dst, (intr ADDR:$ptr, GPRI32:$src, timm:$id))]>;


class BinAtomNoRet<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM3232:$ptr, GPRI32:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src")),
      [(intr ADDR:$ptr, GPRI32:$src, timm:$id)]>;

class TriAtom<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src, $src1")),
      [(set GPRI32:$dst, (intr ADDR:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id))]>;

class CmpXChg<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM3232:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src1, $src")),
      [(set GPRI32:$dst, (intr ADDR:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id))]>;

class TriAtomNoRet<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM3232:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src, $src1")),
      [(intr ADDR:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id)]>;

class CmpXChgNoRet<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM3232:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src1, $src")),
      [(intr ADDR:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id)]>;


class UniAtomI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM3232:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, ${ptr}0")),
      [(set GPRI64:$dst, (intr ADDR:$ptr, timm:$id))]>;


class UniAtomNoRetI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM3232:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," ${ptr}0")),
      [(intr ADDR:$ptr, timm:$id)]>;

class BinAtomI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM3232:$ptr, GPRI64:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, ${ptr}0, $src")),
      [(set GPRI64:$dst, (i64 (intr ADDR:$ptr, GPRI64:$src, timm:$id)))]>;


class BinAtomNoRetI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM3232:$ptr, GPRI64:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," ${ptr}0, $src")),
      [(intr ADDR:$ptr, GPRI64:$src, timm:$id)]>;

class TriAtomI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM3232:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, ${ptr}0, $src, $src1")),
      [(set GPRI64:$dst, (intr ADDR:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id))]>;

class CmpXChgI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM3232:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, ${ptr}0, $src1, $src")),
      [(set GPRI64:$dst, (intr ADDR:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id))]>;

class TriAtomNoRetI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM3232:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," ${ptr}0, $src, $src1")),
      [(intr ADDR:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id)]>;

class CmpXChgNoRetI64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM3232:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," ${ptr}0, $src1, $src")),
      [(intr ADDR:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id)]>;


//===--------------------------------------------------------------------===//
// Classes for dealing with atomic instructions w/ 64bit pointers
//===--------------------------------------------------------------------===//
class Append64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM6464:$id),
      !strconcat(op.Text, !strconcat(idType," $dst")),
      [(set GPRI32:$dst, (intr ADDR64:$id))]>;


class UniAtom64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM6464:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr")),
      [(set GPRI32:$dst, (intr ADDR64:$ptr, timm:$id))]>;


class UniAtomNoRet64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM6464:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr")),
      [(intr ADDR64:$ptr, timm:$id)]>;

class BinAtom64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM6464:$ptr, GPRI32:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src")),
      [(set GPRI32:$dst, (intr ADDR64:$ptr, GPRI32:$src, timm:$id))]>;


class BinAtomNoRet64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM6464:$ptr, GPRI32:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src")),
      [(intr ADDR64:$ptr, GPRI32:$src, timm:$id)]>;

class TriAtom64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM6464:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src, $src1")),
      [(set GPRI32:$dst, (intr ADDR64:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id))]>;

class CmpXChg64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI32:$dst),
      (ins MEM6464:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src1, $src")),
      [(set GPRI32:$dst, (intr ADDR64:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id))]>;

class TriAtomNoRet64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM6464:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src, $src1")),
      [(intr ADDR64:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id)]>;

class CmpXChgNoRet64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM6464:$ptr, GPRI32:$src, GPRI32:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src1, $src")),
      [(intr ADDR64:$ptr, GPRI32:$src, GPRI32:$src1, timm:$id)]>;


class UniAtom64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM6464:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr")),
      [(set GPRI64:$dst, (intr ADDR64:$ptr, timm:$id))]>;


class UniAtomNoRet64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM6464:$ptr, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr")),
      [(intr ADDR64:$ptr, timm:$id)]>;

class BinAtom64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM6464:$ptr, GPRI64:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src")),
      [(set GPRI64:$dst, (intr ADDR64:$ptr, GPRI64:$src, timm:$id))]>;


class BinAtomNoRet64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs), (ins MEM6464:$ptr, GPRI64:$src, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src")),
      [(intr ADDR64:$ptr, GPRI64:$src, timm:$id)]>;

class TriAtom64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM6464:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src, $src1")),
      [(set GPRI64:$dst, (intr ADDR64:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id))]>;

class CmpXChg64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs GPRI64:$dst),
      (ins MEM6464:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $dst, $ptr, $src1, $src")),
      [(set GPRI64:$dst, (intr ADDR64:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id))]>;

class TriAtomNoRet64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM6464:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src, $src1")),
      [(intr ADDR64:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id)]>;

class CmpXChgNoRet64I64<ILOpCode op, string idType, SDNode intr>
      : ILFormat<op, (outs),
      (ins MEM6464:$ptr, GPRI64:$src, GPRI64:$src1, i32imm:$id),
      !strconcat(op.Text, !strconcat(idType," $ptr, $src1, $src")),
      [(intr ADDR64:$ptr, GPRI64:$src, GPRI64:$src1, timm:$id)]>;

// atomic global load
class AtomGLoadClass<ILOpCode op, Operand memType, RegisterClass valRegType,
                     ComplexPattern addrType>
  : ILFormat<op, (outs valRegType:$dst),
             (ins memType:$ptr, GPRI32:$order, i32imm:$id),
             !strconcat(op.Text, "_id($id)_uncached $dst, $ptr"),
             [(set valRegType:$dst,
              (atom_g_load addrType:$ptr, GPRI32:$order, timm:$id))]>;

// atomic global store
class AtomGStoreClass<ILOpCode op, Operand memType, RegisterClass valRegType,
                      ComplexPattern addrType>
  : ILFormat<op, (outs),
             (ins memType:$ptr, valRegType:$src, GPRI32:$order, i32imm:$id),
             !strconcat(op.Text, "_id($id)_uncached $ptr, $src"),
             [(atom_g_store addrType:$ptr, valRegType:$src,
                            GPRI32:$order, timm:$id)]>;
//===--------------------------------------------------------------------===//
// Intrinsic classes
// Generic versions of the above classes but for Target specific intrinsics
// instead of SDNode patterns.
//===--------------------------------------------------------------------===//
let TargetPrefix = "AMDIL", isTarget = 1 in {
     class VoidIntLong :
          Intrinsic<[llvm_i64_ty], [], []>;
     class VoidIntInt :
          Intrinsic<[llvm_i32_ty], [], []>;
     class VoidIntBool :
          Intrinsic<[llvm_i32_ty], [], []>;
     class UnaryIntInt :
          Intrinsic<[llvm_anyint_ty], [LLVMMatchType<0>], []>;
     class UnaryIntFloat :
          Intrinsic<[llvm_anyfloat_ty], [LLVMMatchType<0>], []>;
     class ConvertIntFTOI :
          Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], []>;
     class ConvertIntITOF :
          Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], []>;
     class UnaryIntNoRetInt :
          Intrinsic<[], [llvm_anyint_ty], []>;
     class UnaryIntNoRetFloat :
          Intrinsic<[], [llvm_anyfloat_ty], []>;
     class BinaryIntInt :
          Intrinsic<[llvm_anyint_ty], [LLVMMatchType<0>, LLVMMatchType<0>], []>;
     class BinaryIntFloat :
          Intrinsic<[llvm_anyfloat_ty], [LLVMMatchType<0>, LLVMMatchType<0>], []>;
     class BinaryIntNoRetInt :
          Intrinsic<[], [llvm_anyint_ty, LLVMMatchType<0>], []>;
     class BinaryIntNoRetFloat :
          Intrinsic<[], [llvm_anyfloat_ty, LLVMMatchType<0>], []>;
     class TernaryIntInt :
          Intrinsic<[llvm_anyint_ty], [LLVMMatchType<0>,
          LLVMMatchType<0>, LLVMMatchType<0>], []>;
     class TernaryIntFloat :
          Intrinsic<[llvm_anyfloat_ty], [LLVMMatchType<0>,
          LLVMMatchType<0>, LLVMMatchType<0>], []>;
     class QuaternaryIntInt :
          Intrinsic<[llvm_anyint_ty], [LLVMMatchType<0>,
          LLVMMatchType<0>, LLVMMatchType<0>, LLVMMatchType<0>], []>;
     class UnaryAtomicInt :
          Intrinsic<[llvm_i32_ty], [llvm_ptr_ty, llvm_i32_ty], [IntrReadWriteArgMem]>;
     class BinaryAtomicInt :
          Intrinsic<[llvm_i32_ty], [llvm_ptr_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadWriteArgMem]>;
     class TernaryAtomicInt :
          Intrinsic<[llvm_i32_ty], [llvm_ptr_ty, llvm_i32_ty, llvm_i32_ty, llvm_i32_ty]>;
     class VoidAtomicIntNoRet :
          Intrinsic<[], [llvm_ptr_ty], [IntrReadWriteArgMem]>;
     class UnaryAtomicIntNoRet :
          Intrinsic<[], [llvm_ptr_ty, llvm_i32_ty], [IntrReadWriteArgMem]>;
     class BinaryAtomicIntNoRet :
          Intrinsic<[], [llvm_ptr_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadWriteArgMem]>;
     class TernaryAtomicIntNoRet :
          Intrinsic<[], [llvm_ptr_ty, llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadWriteArgMem]>;
     
class UnaryAtomicLong :
  Intrinsic<[llvm_i64_ty], [llvm_ptr_ty, llvm_i64_ty], [IntrReadWriteArgMem]>;
class BinaryAtomicLong :
  Intrinsic<[llvm_i64_ty], [llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty], [IntrReadWriteArgMem]>;
class TernaryAtomicLong :
  Intrinsic<[llvm_i64_ty], [llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty, llvm_i64_ty]>;
class VoidAtomicLongNoRet :
  Intrinsic<[], [llvm_ptr_ty], [IntrReadWriteArgMem]>;
class UnaryAtomicLongNoRet :
  Intrinsic<[], [llvm_ptr_ty, llvm_i64_ty], [IntrReadWriteArgMem]>;
class BinaryAtomicLongNoRet :
  Intrinsic<[], [llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty], [IntrReadWriteArgMem]>;
class TernaryAtomicLongNoRet :
  Intrinsic<[], [llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty, llvm_i64_ty], [IntrReadWriteArgMem]>;
}
