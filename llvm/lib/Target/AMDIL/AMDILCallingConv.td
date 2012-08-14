//===-- AMDILCallingConv.td -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the AMDIL architectures.
//
//===----------------------------------------------------------------------===//

def RetCC_AMDIL32 : CallingConv<[
 // Since IL has no return values, all values can be emulated on the stack
 // The stack can then be mapped to a number of sequential virtual registers
 // in IL

 CCIfType<[i1, i8, i16, i32, f32], CCAssignToReg<
 [ include "AMDILRegisterUsesScalar.td" ]> >,

 CCIfType<[v2i32, v2f32, v2i8, v2i16, f64, i64], CCAssignToReg<
 [ include "AMDILRegisterUsesV2.td" ]> >,

 CCIfType<[v4i32, v4f32, v4i8, v4i16, v2f64, v2i64], CCAssignToReg<
 [ include "AMDILRegisterUsesV4.td" ]> >,

 CCAssignToStack<16, 16>
 ]>;

// AMDIL 32-bit C Calling convention.
def CC_AMDIL32 : CallingConv<[
 // CCIfByVal<CCPassByVal<4, 4>>,
 // Since IL has parameter values, all values can be emulated on the stack
 // The stack can then be mapped to a number of sequential virtual registers
 // in IL

 CCIfType<[i1, i8, i16, i32, f32], CCAssignToReg<
 [ include "AMDILRegisterUsesScalar.td" ]> >,

 CCIfType<[v2i32, v2f32, v2i8, v2i16, f64, i64], CCAssignToReg<
 [ include "AMDILRegisterUsesV2.td" ]> >,

 CCIfType<[v4i32, v4f32, v4i8, v4i16, v2f64, v2i64], CCAssignToReg<
 [ include "AMDILRegisterUsesV4.td" ]> >,

 CCAssignToStack<16, 16>
 ]>;


