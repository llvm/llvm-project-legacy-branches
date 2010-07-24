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
	const Constant *A = ConstantArray::get (getGlobalContext(), tail, false);
  ASSERT_NE(A, (const Constant*)NULL);
  EXPECT_EQ(1U + 20, A->getNumOperands());
	const Use *U = &A->getOperandUse(0);
	const Use *Ue = &A->getOperandUse(20);
  for (; U != Ue; ++U)
  {
    EXPECT_EQ(Ue + 1, U->getImpliedUser());
  }
}

TEST(WaymarkTest, TwoBit) {
  Use* many = new Use[8212];
  ASSERT_TRUE(many);
	Use::initTags<2>(many, many + 8212, 0);
  for (const Use *U = many, *Ue = many + 8212 - 1; U != Ue; ++U)
  {
    EXPECT_EQ(Ue + 1, Use::getImpliedUser<2>(U));
  }
}

char m3(const Use& U)
{
  static char ms[9] = "0123sxyS";
  return ms[U.Prev.getInt()];
}

TEST(WaymarkTest, ThreeBit) {
  Use* many = new Use[8212];
  ASSERT_TRUE(many);
	Use::initTags<3>(many, many + 8212, 0);
  for (const Use *U = many, *Ue = many + 8212 - 1; U != Ue; ++U)
  {
    EXPECT_EQ(Ue + 1, Use::getImpliedUser<3>(U));
  }

  std::string segment8000("32sx330312sx330232sx330212sx330132sx330112sx330032");
  std::string segment5000("s032001s031322s031303s031230s031211s031132s031113s");
  std::string segment3000("311y32233y32221y32203y32131y32113y32101y32023y3201");
  std::string segment2000("3030sx3301x33000sx3232x32310sx3223x32220sx3220x321");
  std::string segment100 ("02sx13x122sx111sx100s030s020s010s000s330s320s310s3");
  std::string segment50  ("00y31y22y13y10y01sx31sx21sx11s02s33s30y2y0s1x0syxS");

	std::string result(50, ' ');
	std::transform(many + 8212 - 8000, many + 8212 - 7950, result.begin(), m3);
  EXPECT_EQ('2', result[49]);
  EXPECT_EQ(segment8000, result);
}

}  // end anonymous namespace
}  // end namespace llvm
