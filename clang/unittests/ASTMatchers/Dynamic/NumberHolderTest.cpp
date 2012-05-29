//===- unittest/ASTMatchers/Dynamic/NumberHolderTest.cpp - NumberHolder unit tests ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------------------===//

#include "clang/ASTMatchers/Dynamic/NumberHolder.h"

#include <limits>

#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {
namespace {

TEST(NumberHolderTest, CanCovertBetweenTypes) {
  // Every type can represent 123.
  const int32_t Expected = 123;
  NumberHolder Holder(Expected);

  EXPECT_TRUE(Holder.is<int8_t>());
  EXPECT_EQ(Expected, Holder.get<int8_t>());

  EXPECT_TRUE(Holder.is<int16_t>());
  EXPECT_EQ(Expected, Holder.get<int16_t>());

  EXPECT_TRUE(Holder.is<int32_t>());
  EXPECT_EQ(Expected, Holder.get<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(Expected, Holder.get<int64_t>());

  EXPECT_TRUE(Holder.is<uint8_t>());
  EXPECT_EQ(Expected, Holder.get<uint8_t>());

  EXPECT_TRUE(Holder.is<uint16_t>());
  EXPECT_EQ(Expected, Holder.get<uint16_t>());

  EXPECT_TRUE(Holder.is<uint32_t>());
  EXPECT_EQ(static_cast<uint32_t>(Expected), Holder.get<uint32_t>());

  EXPECT_TRUE(Holder.is<uint64_t>());
  EXPECT_EQ(static_cast<uint64_t>(Expected), Holder.get<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

TEST(NumberHolderTest, NegativeValues) {
  // Only signed types can represent -123.
  const int32_t Expected = -123;
  NumberHolder Holder(Expected);

  EXPECT_TRUE(Holder.is<int8_t>());
  EXPECT_EQ(Expected, Holder.get<int8_t>());

  EXPECT_TRUE(Holder.is<int16_t>());
  EXPECT_EQ(Expected, Holder.get<int16_t>());

  EXPECT_TRUE(Holder.is<int32_t>());
  EXPECT_EQ(Expected, Holder.get<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(Expected, Holder.get<int64_t>());

  EXPECT_FALSE(Holder.is<uint8_t>());
  EXPECT_FALSE(Holder.is<uint16_t>());
  EXPECT_FALSE(Holder.is<uint32_t>());
  EXPECT_FALSE(Holder.is<unsigned long>());
  EXPECT_FALSE(Holder.is<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

TEST(NumberHolderTest, SignedOverflow) {
  // Overflow int16_t. Value is still a valid uint16_t.
  const uint64_t Expected = std::numeric_limits<int16_t>::max() + 1LL;
  NumberHolder Holder(Expected);

  EXPECT_FALSE(Holder.is<int8_t>());
  EXPECT_FALSE(Holder.is<int16_t>());

  EXPECT_TRUE(Holder.is<int32_t>());
  EXPECT_EQ(static_cast<int32_t>(Expected), Holder.get<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(static_cast<int64_t>(Expected), Holder.get<int64_t>());

  EXPECT_FALSE(Holder.is<uint8_t>());

  EXPECT_TRUE(Holder.is<uint16_t>());
  EXPECT_EQ(Expected, Holder.get<uint16_t>());

  EXPECT_TRUE(Holder.is<uint32_t>());
  EXPECT_EQ(Expected, Holder.get<uint32_t>());

  EXPECT_TRUE(Holder.is<uint64_t>());
  EXPECT_EQ(Expected, Holder.get<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

TEST(NumberHolderTest, UnsignedOverflow) {
  // Overflow everything up to uint32_t.
  const uint64_t Expected = std::numeric_limits<uint32_t>::max() + 1LL;
  NumberHolder Holder(Expected);

  EXPECT_FALSE(Holder.is<int8_t>());
  EXPECT_FALSE(Holder.is<int16_t>());
  EXPECT_FALSE(Holder.is<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(static_cast<int64_t>(Expected), Holder.get<int64_t>());

  EXPECT_FALSE(Holder.is<uint8_t>());
  EXPECT_FALSE(Holder.is<uint16_t>());
  EXPECT_FALSE(Holder.is<uint32_t>());

  EXPECT_TRUE(Holder.is<uint64_t>());
  EXPECT_EQ(Expected, Holder.get<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

TEST(NumberHolderTest, DoubleOverflow) {
  // Overflow a double using a very large integer.
  const int64_t Expected = std::numeric_limits<int64_t>::max();
  NumberHolder Holder(Expected);

  EXPECT_FALSE(Holder.is<int8_t>());
  EXPECT_FALSE(Holder.is<int16_t>());
  EXPECT_FALSE(Holder.is<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(Expected, Holder.get<int64_t>());

  EXPECT_FALSE(Holder.is<uint8_t>());
  EXPECT_FALSE(Holder.is<uint16_t>());
  EXPECT_FALSE(Holder.is<uint32_t>());

  EXPECT_TRUE(Holder.is<uint64_t>());
  EXPECT_EQ(static_cast<uint64_t>(Expected), Holder.get<uint64_t>());

  EXPECT_FALSE(Holder.is<float>());
  EXPECT_FALSE(Holder.is<double>());
}

TEST(NumberHolderTest, FloatingPoint) {
  // No integer should accept a floating point.
  const double Expected = 1.5;
  NumberHolder Holder(Expected);

  EXPECT_FALSE(Holder.is<int8_t>());
  EXPECT_FALSE(Holder.is<int16_t>());
  EXPECT_FALSE(Holder.is<int32_t>());
  EXPECT_FALSE(Holder.is<int64_t>());
  EXPECT_FALSE(Holder.is<uint8_t>());
  EXPECT_FALSE(Holder.is<uint16_t>());
  EXPECT_FALSE(Holder.is<uint32_t>());
  EXPECT_FALSE(Holder.is<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

TEST(NumberHolderTest, NotReallyFloatingPoint) {
  // If the double is not really a floating point, then we accept it as ints.
  const double Expected = 1234e5;
  NumberHolder Holder(Expected);

  EXPECT_FALSE(Holder.is<int8_t>());
  EXPECT_FALSE(Holder.is<int16_t>());

  EXPECT_TRUE(Holder.is<int32_t>());
  EXPECT_EQ(Expected, Holder.get<int32_t>());

  EXPECT_TRUE(Holder.is<int64_t>());
  EXPECT_EQ(Expected, Holder.get<int64_t>());

  EXPECT_FALSE(Holder.is<uint8_t>());
  EXPECT_FALSE(Holder.is<uint16_t>());

  EXPECT_TRUE(Holder.is<uint32_t>());
  EXPECT_EQ(Expected, Holder.get<uint32_t>());

  EXPECT_TRUE(Holder.is<uint64_t>());
  EXPECT_EQ(Expected, Holder.get<uint64_t>());

  EXPECT_TRUE(Holder.is<float>());
  EXPECT_EQ(Expected, Holder.get<float>());

  EXPECT_TRUE(Holder.is<double>());
  EXPECT_EQ(Expected, Holder.get<double>());
}

}  // end anonymous namespace
}  // end namespace dynamic
}  // end namespace ast_matchers
}  // end namespace clang
