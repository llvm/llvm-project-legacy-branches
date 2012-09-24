//===-- R600Intrinsics.td - R600 Instrinsic defs -------*- tablegen -*-----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// R600 Intrinsic Definitions
//
//===----------------------------------------------------------------------===//

let TargetPrefix = "R600", isTarget = 1 in {
  def int_R600_load_input : Intrinsic<[llvm_float_ty], [llvm_i32_ty], [IntrNoMem]>;
  def int_R600_load_input_perspective :
    Intrinsic<[llvm_float_ty], [llvm_i32_ty], [IntrReadMem]>;
  def int_R600_load_input_constant :
    Intrinsic<[llvm_float_ty], [llvm_i32_ty], [IntrReadMem]>;
  def int_R600_load_input_linear :
    Intrinsic<[llvm_float_ty], [llvm_i32_ty], [IntrReadMem]>;
  def int_R600_load_input_position :
    Intrinsic<[llvm_float_ty], [llvm_i32_ty], [IntrReadMem]>;
  def int_R600_load_input_face :
    Intrinsic<[llvm_i1_ty], [llvm_i32_ty], [IntrReadMem]>;
}
