//===-- Use.cpp - Implement the Use class ---------------------------------===//
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
//                         Use swap Implementation
//===----------------------------------------------------------------------===//

void Use::swap(Use &RHS) {
  Value *V1(Val);
  Value *V2(RHS.Val);
  if (V1 != V2) {
    if (V1) {
      removeFromList();
    }

    if (V2) {
      RHS.removeFromList();
      Val = V2;
      V2->addUse(*this);
    } else {
      Val = 0;
    }

    if (V1) {
      RHS.Val = V1;
      V1->addUse(RHS);
    } else {
      RHS.Val = 0;
    }
  }
}

//===----------------------------------------------------------------------===//
//                         Use getImpliedUser Implementations
//===----------------------------------------------------------------------===//

template <>
const Use *Use::getImpliedUser<3>(const Use *Current) {
  while (true) {
    unsigned Tag = (Current++)->Prev.getInt();
    ptrdiff_t Offset;
    if (Tag & 4)
    switch (Tag) {
      case stop64Tag: {
        PrevPtrTag nextTag = (Current++)->Prev.getInt();
        switch (nextTag) {
          case 0: Offset = 4; goto digits;
          case xStop64Tag: Offset = 1; goto digits;
          case yStop64Tag: return Current + 2;
          default: Offset = nextTag; goto digits;
        }
      }
      case xStop64Tag:
        if (Current->Prev.getInt() == fullStop64Tag)
          return Current + 1;
        Offset = 1; goto digits;
      case yStop64Tag:
        if (Current->Prev.getInt() == xStop64Tag)
          return Current + 2;
        Offset = 2;
        while (true) {
          digits:
          unsigned Tag = Current->Prev.getInt();
          switch (Tag) {
            case zero64Tag:
            case one64Tag:
            case two64Tag:
            case three64Tag:
              ++Current;
              Offset = (Offset << 2) + Tag;
              continue;
            case xStop64Tag:
              return Current + (Offset << 2) + one64Tag + 1;
            default:
              return Current + Offset;
          }
        }

      case fullStop64Tag:
        return Current;
    }
  }
}

template <>
const Use *Use::getImpliedUser<2>(const Use *Current) {
  while (true) {
    unsigned Tag = (Current++)->Prev.getInt();
    switch (Tag) {
      case zeroDigitTag:
      case oneDigitTag:
        continue;

      case stopTag: {
        ++Current;
        ptrdiff_t Offset = 1;
        while (true) {
          unsigned Tag = Current->Prev.getInt();
          switch (Tag) {
            case zeroDigitTag:
            case oneDigitTag:
              ++Current;
              Offset = (Offset << 1) + Tag;
              continue;
            default:
              return Current + Offset;
          }
        }
      }

      case fullStopTag:
        return Current;
    }
  }
}

//===----------------------------------------------------------------------===//
//                         Use initTags Implementations
//===----------------------------------------------------------------------===//

template <>
Use *Use::initTags<2>(Use * const Start, Use *Stop, ptrdiff_t Done) {
  while (Done < 20) {
    if (Start == Stop--)
      return Start;
    static const PrevPtrTag tags[20] = { fullStopTag, oneDigitTag, stopTag,
                                         oneDigitTag, oneDigitTag, stopTag,
                                         zeroDigitTag, oneDigitTag, oneDigitTag,
                                         stopTag, zeroDigitTag, oneDigitTag,
                                         zeroDigitTag, oneDigitTag, stopTag,
                                         oneDigitTag, oneDigitTag, oneDigitTag,
                                         oneDigitTag, stopTag
                                       };
    Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(tags[Done++]));
    Stop->Val = 0;
  }

  ptrdiff_t Count = Done;
  while (Start != Stop) {
    --Stop;
    Stop->Val = 0;
    if (!Count) {
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(stopTag));
      ++Done;
      Count = Done;
    } else {
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(Count & 1));
      Count >>= 1;
      ++Done;
    }
  }

  return Start;
}

template <>
Use *Use::initTags<3>(Use * const Start, Use *Stop, ptrdiff_t Done) {
  while (Done < 21) {
    if (Start == Stop--)
      return Start;
    static const PrevPtrTag tags[21] = { fullStop64Tag, xStop64Tag, yStop64Tag,
                                         stop64Tag, zero64Tag, xStop64Tag,
                                         one64Tag, stop64Tag, zero64Tag,
                                         yStop64Tag, two64Tag, yStop64Tag,
                                         zero64Tag, three64Tag, stop64Tag,
                                         three64Tag, three64Tag, stop64Tag,
                                         two64Tag, zero64Tag, stop64Tag
                                       };
    Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(tags[Done++]));
    Stop->Val = 0;
  }

  ptrdiff_t Count = Done;
  while (Start != Stop) {
    --Stop;
    Stop->Val = 0;
    switch (Count) {
    case 0:
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(stop64Tag));
      Count = ++Done;
      continue;
    case 1:
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(xStop64Tag));
      Count = (Done & 3) == 1 ? Done >> 2 : 0;
      ++Done;
      continue;
    case 2:
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(yStop64Tag));
      Count = ++Done;
      continue;
    case 4:
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(zero64Tag));
      ++Done;
      Count = 0;
      continue;
    default:
      Stop->Prev.setFromOpaqueValue(reinterpret_cast<Use**>(Count & 3));
      Count >>= 2;
      ++Done;
    }
  }

  return Start;
}

//===----------------------------------------------------------------------===//
//                         Use zap Implementation
//===----------------------------------------------------------------------===//

void Use::zap(Use *Start, const Use *Stop, bool del) {
  if (del) {
    while (Start != Stop) {
      (--Stop)->~Use();
    }
    ::operator delete(Start);
    return;
  }

  while (Start != Stop) {
    (Start++)->set(0);
  }
}

//===----------------------------------------------------------------------===//
//                         AugmentedUse layout struct
//===----------------------------------------------------------------------===//

struct AugmentedUse : public Use {
  PointerIntPair<User*, 1, Tag> ref;
  AugmentedUse(); // not implemented
};


//===----------------------------------------------------------------------===//
//                         Use getUser Implementation
//===----------------------------------------------------------------------===//

User *Use::getUser() const {
  const Use *End = getImpliedUser<availableTagBits>(this);
  const PointerIntPair<User*, 1, Tag>& ref(
                                static_cast<const AugmentedUse*>(End - 1)->ref);
  User *She = ref.getPointer();
  return ref.getInt()
    ? She
    : (User*)End;
}

//===----------------------------------------------------------------------===//
//                         User allocHungoffUses Implementation
//===----------------------------------------------------------------------===//

Use *User::allocHungoffUses(unsigned N) const {
  Use *Begin = static_cast<Use*>(::operator new(sizeof(Use) * N
                                                + sizeof(AugmentedUse)
                                                - sizeof(Use)));
  Use *End = Begin + N;
  PointerIntPair<User*, 1, Tag>& ref(static_cast<AugmentedUse&>(End[-1]).ref);
  ref.setPointer(const_cast<User*>(this));
  ref.setInt(tagOne);
  return Use::initTags<Use::availableTagBits>(Begin, End);
}

//===----------------------------------------------------------------------===//
//                         User operator new Implementations
//===----------------------------------------------------------------------===//

void *User::operator new(size_t s, unsigned Us) {
  void *Storage = ::operator new(s + sizeof(Use) * Us);
  Use *Start = static_cast<Use*>(Storage);
  Use *End = Start + Us;
  User *Obj = reinterpret_cast<User*>(End);
  Obj->OperandList = Start;
  Obj->NumOperands = Us;
  Use::initTags<Use::availableTagBits>(Start, End);
  return Obj;
}

/// Prefixed allocation - just before the first Use, allocate a NULL pointer.
/// The destructor can detect its presence and readjust the OperandList
/// for deletion.
///
void *User::operator new(size_t s, unsigned Us, bool Prefix) {
  // Currently prefixed allocation only admissible for
  // unconditional branch instructions.
  if (!Prefix)
    return operator new(s, Us);

  assert(Us == 1 && "Other than one Use allocated?");
  typedef PointerIntPair<void*, 2, Use::PrevPtrTag> TaggedPrefix;
  void *Raw = ::operator new(s + sizeof(TaggedPrefix) + sizeof(Use) * Us);
  TaggedPrefix *Pre = static_cast<TaggedPrefix*>(Raw);
  Pre->setFromOpaqueValue(0);
  void *Storage = Pre + 1; // skip over prefix
  Use *Start = static_cast<Use*>(Storage);
  Use *End = Start + Us;
  User *Obj = reinterpret_cast<User*>(End);
  Obj->OperandList = Start;
  Obj->NumOperands = Us;
  Use::initTags<Use::availableTagBits>(Start, End);
  return Obj;
}

//===----------------------------------------------------------------------===//
//                         User operator delete Implementation
//===----------------------------------------------------------------------===//

void User::operator delete(void *Usr) {
  User *Start = static_cast<User*>(Usr);
  Use *Storage = static_cast<Use*>(Usr) - Start->NumOperands;
  //
  // Look for a variadic User.
  if (Storage == Start->OperandList) {
    ::operator delete(Storage);
    return;
  }
  //
  // Check for the flag whether the destructor has detected a prefixed
  // allocation, in which case we remove the flag and delete starting
  // at OperandList.
  if (reinterpret_cast<intptr_t>(Start->OperandList) & 1) {
    ::operator delete(reinterpret_cast<char*>(Start->OperandList) - 1);
    return;
  }
  //
  // In all other cases just delete the nullary User (covers hung-off
  // uses also).
  ::operator delete(Usr);
}

} // End llvm namespace
