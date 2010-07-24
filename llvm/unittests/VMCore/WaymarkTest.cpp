//===- llvm/unittest/VMCore/ConstantsTest.cpp - Constants unit tests ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// we perform white-box tests
//
#define private public
#include "llvm/Use.h"
#undef private
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "gtest/gtest.h"

namespace llvm {
namespace {

TEST(WaymarkTest, NativeArray) {
  static char tail[22] = "s02s33s30y2y0s1x0syxS";
	Constant *A = ConstantArray::get (getGlobalContext(), tail, false);
  /*ASSERT_NE*/EXPECT_NE(A, (Constant*)NULL);
  EXPECT_EQ(21U, A->getNumOperands());
	const Use *U = &A->getOperandUse(0);
	const Use *Ue = &A->getOperandUse(20);
  for (; U != Ue; ++U)
  {
    EXPECT_EQ(Ue + 1, U->getImpliedUser());
  }
}

}  // end anonymous namespace
}  // end namespace llvm
