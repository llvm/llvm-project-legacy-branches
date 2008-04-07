//===-- Use.cpp - Implement the Use class -------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the algoritm for finding the User of a Use.
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
  ptrdiff_t Offset = 0;
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
        break;
      case fullStopTag:
        return Current + 1;
      }

    ++Current;
  }
}

void Use::initTags(Use* start, Use* stop, ptrdiff_t done) {
    ptrdiff_t Count = 0;
    while (start != stop) 
    {
        --stop;
        if (!Count) {
            stop->Val = reinterpret_cast<Value*>(done == 0 ? fullStopTag : stopTag);
            ++done;
            Count = done;
        } else {
            stop->Val = reinterpret_cast<Value*>(Count & 1);
            Count >>= 1;
            ++done;
        }
    }
}


}
