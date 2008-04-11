//===-- Use.cpp - Implement the Use class -------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the algorithm for finding the User of a Use.
//
//===----------------------------------------------------------------------===//

#include "llvm/Value.h"

namespace llvm {

//===----------------------------------------------------------------------===//
//                         Use getImpliedUser Implementation
//===----------------------------------------------------------------------===//

enum ValuePtrTag { zeroDigitTag = 0, oneDigitTag = 1, stopTag = 0x2, fullStopTag = 0x3 };

const Use *Use::getImpliedUser() const {
  bool StopEncountered = false;
  ptrdiff_t Offset = 1;
  const Use *Current = this;

  while (true) {
    unsigned Tag = unsigned(Current->Val) & 0x3;
    switch (Tag)
      {
      case zeroDigitTag:
      case oneDigitTag:
        if (StopEncountered)
          Offset = (Offset << 1) + Tag;
        break;
      case stopTag:
        if (StopEncountered)
          return Current + Offset;
        StopEncountered = true;
        Current += 2;
        continue;
      case fullStopTag:
        return Current + 1;
      }

    ++Current;
  }
}

//===----------------------------------------------------------------------===//
//                         Use initTags Implementation
//===----------------------------------------------------------------------===//

void Use::initTags(Use *Start, Use *Stop, ptrdiff_t Done) {
  ptrdiff_t Count = 0;
  while (Start != Stop) 
  {
    --Stop;
    if (!Count) {
      Stop->Val = reinterpret_cast<Value*>(Done == 0 ? fullStopTag : stopTag);
      ++Done;
      Count = Done;
    } else {
      Stop->Val = reinterpret_cast<Value*>(Count & 1);
      Count >>= 1;
      ++Done;
    }
  }
}

//===----------------------------------------------------------------------===//
//                         Use zap Implementation
//===----------------------------------------------------------------------===//

void Use::zap(Use *Start, const Use *Stop) {
  while (Start != Stop) {
    (Start++)->set(0);
  }
}

} // End llvm namespace
