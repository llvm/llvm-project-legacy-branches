//===-- llvm/VectorLLVM/VectorInstrinsics.h - LLVM Vector Intrinsic Function Handling ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a set of enums which allow processing of
// intrinsic functions for Vector-LLVM.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VECTOR_INTRINSICS_H
#define LLVM_VECTOR_INTRINSICS_H

namespace llvm {

/// Intrinsic Namespace - This namespace contains an enum with a value for
/// every vector intrinsic/builtin function known by LLVM.
///
namespace VectorIntrinsic {
  enum ID {
    not_intrinsic = 0,   // Must be zero


  };

} // End VectorIntrinsic namespace

} // End llvm namespace

#endif
