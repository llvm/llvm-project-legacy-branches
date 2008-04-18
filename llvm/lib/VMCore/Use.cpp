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

#include "llvm/User.h"

namespace llvm {

//===----------------------------------------------------------------------===//
//                         Use getImpliedUser Implementation
//===----------------------------------------------------------------------===//

const Use *Use::getImpliedUser() const {
  bool StopEncountered = false;
  ptrdiff_t Offset = 1;
  const Use *Current = this;

  while (true) {
    unsigned Tag = extractTag<PrevPtrTag, fullStopTag>(Current->Prev);
    switch (Tag) {
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

Use *Use::initTags(Use * const Start, Use *Stop, ptrdiff_t Done) {
  ptrdiff_t Count = 0;
  while (Start != Stop) {
    --Stop;
    Stop->Val = 0;
    if (!Count) {
      Stop->Prev = reinterpret_cast<Use**>(Done == 0 ? fullStopTag : stopTag);
      ++Done;
      Count = Done;
    } else {
      Stop->Prev = reinterpret_cast<Use**>(Count & 1);
      Count >>= 1;
      ++Done;
    }
  }

  return Start;
}

//===----------------------------------------------------------------------===//
//                         Use zap Implementation
//===----------------------------------------------------------------------===//

void Use::zap(Use *Start, const Use *Stop, bool del) {
  Use *Iter = Start;
  while (Iter != Stop) {
    (Iter++)->set(0);
  }
  if (del)
    ::operator delete(Start);
}

//===----------------------------------------------------------------------===//
//                         AugmentedUse layout struct
//===----------------------------------------------------------------------===//

struct AugmentedUse : Use {
  User *ref;
  AugmentedUse(); // not implemented
};


//===----------------------------------------------------------------------===//
//                         Use getUser Implementation
//===----------------------------------------------------------------------===//

User *Use::getUser() const {
  const Use *End = getImpliedUser();
  User *She = static_cast<const AugmentedUse*>(End - 1)->ref;
  She = extractTag<Tag, tagOne>(She)
      ? llvm::stripTag<tagOne>(She)
      : reinterpret_cast<User*>(const_cast<Use*>(End));

  return She;
}

//===----------------------------------------------------------------------===//
//                         User allocHungoffUses Implementation
//===----------------------------------------------------------------------===//

Use *User::allocHungoffUses(unsigned N) const {
  Use *Begin = static_cast<Use*>(::operator new(sizeof(Use) * N + sizeof(AugmentedUse) - sizeof(Use)));
  Use *End = Begin + N;
  static_cast<AugmentedUse&>(End[-1]).ref = addTag(this, tagOne);
  return Use::initTags(Begin, End);
}

} // End llvm namespace
