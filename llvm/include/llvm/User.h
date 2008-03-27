//===-- llvm/User.h - User class definition ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class defines the interface that one who 'use's a Value must implement.
// Each instance of the Value class keeps track of what User's have handles
// to it.
//
//  * Instructions are the largest class of User's.
//  * Constants may be users of other constants (think arrays and stuff)
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_USER_H
#define LLVM_USER_H

#include "llvm/Value.h"

namespace llvm {

/*==============================================================================


   -----------------------------------------------------------------
   --- Interaction and relationship between User and Use objects ---
   -----------------------------------------------------------------


A subclass of User can choose between incorporating its Use objects
or refer to them out-of-line by means of a pointer. A mixed variant
(some Uses inline others hung off) is impractical and breaks the invariant
that the Use objects belonging to the same User form a contiguous array.

We have 2 different layouts in the User (sub)classes:

Layout a)
The Use object(s) are inside (resp. at fixed offset) of the User
object and there are a fixed number of them.

Layout b)
The Use object(s) are referenced by a pointer to an
array from the User object and there may be a variable
number of them.

Initially each layout will posses a direct pointer to the
start of the array of Uses. Though not mandatory for layout a),
we stick to this redundancy for the sake of simplicity.
The User object will also store the number of Use objects it
has. (Theoretically this information can also be calculated
given the scheme presented below.)

Special forms of allocation operators (operator new)
will enforce the following memory layouts:


#  Layout a) will be modelled by prepending the User object
#  by the Use[] array.
#      
#      ...---.---.---.---.-------...
#        | V | V | V | V | User
#      '''---'---'---'---'-------'''


#  Layout b) will be modelled by pointing at the Use[] array.
#      
#      .-------...
#      | User
#      '-------'''
#          |
#          v
#          .---.---.---.---...
#          | V | V | V | V |
#          '---'---'---'---'''


Since the Use objects will be deprived of the direct pointer to
their User objects, there must be a fast and exact method to
recover it. This is accomplished by the following scheme:

A bit-encoding in the 2 LSBits of the Use::Val will allow to find the
start of the User object:

00 --> binary digit 0
01 --> binary digit 1
10 --> stop and calc (s)
11 --> full stop (S)

Given a Use*, all we have to do is walk till we get a
stop and we either have a User immediately behind or
we have to walk to the next stop picking up digits
and calculate the offset:

.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.----------------
| s | 1 | 0 | 1 | 1 | s | 0 | 1 | 1 | 0 | s | 0 | 0 | 0 | 1 | S | User (or User*)
'---'---'---'---'---'---'---'---'---'---'---'---'---'---'---'---'----------------
|+16                |+11                |+6                 |+1
|                   |                   |                   |__>
|                   |                   |______________________>
|                   |__________________________________________>
|______________________________________________________________>


Only the significant number of bits need to be stored between the
stops, so that the worst case is 21 memory accesses when there are
1000 Use objects.

The following literate Haskell fragment demonstrates the concept:

> 
> digits :: Int -> [Char] -> [Char]
> digits 0 acc = '0' : acc
> digits 1 acc = '1' : acc
> digits n acc = digits (n `div` 2) $ digits (n `mod` 2) acc
> 
> dist :: Int -> [Char] -> [Char]
> dist 0 [] = ['S']
> dist 0 acc = acc
> dist 1 acc = let r = dist 0 acc in 's' : digits (length r) r
> dist n acc = dist (n - 1) $ dist 1 acc
> 
> takeLast n ss = reverse $ take n $ reverse ss
> 
> test = takeLast 40 $ dist 20 []
> 

Printing <test> gives: "1s100000s11010s10100s1111s1010s110s11s1S"

The reverse algorithm computes the
length of the string just by examining
a certain prefix:

> pref :: [Char] -> Int
> pref "S" = 1
> pref ('s':rest) = decode 1 0 rest
> pref (_:rest) = 1 + pref rest
> 
> decode walk acc ('0':rest) = decode (walk + 1) (acc * 2) rest
> decode walk acc ('1':rest) = decode (walk + 1) (acc * 2 + 1) rest
> decode walk acc _ = walk + acc
> 

Now, as expected, printing <pref test> gives 40.


To maintain the invariant that the 2 LSBits of each Value* in Use
never change after being set up, setters of Use::Val must re-tag the
new Value* on every modification. Accordingly getters must strip the
tag bits.

For layout b) instead of the User we will find a pointer (with LSBit set).
Following this pointer brings us to the User. A portable trick will ensure
that the first bytes of User (if interpreted as a pointer) will never have
the LSBit set.

==============================================================================*/

class User : public Value {
  User(const User &);             // Do not implement
  void *operator new(size_t);     // Do not implement
protected:
  /// OperandList - This is a pointer to the array of Users for this operand.
  /// For nodes of fixed arity (e.g. a binary operator) this array will live
  /// embedded into the derived class.  For nodes of variable arity
  /// (e.g. ConstantArrays, CallInst, PHINodes, ReturnInst etc), this memory 
  /// will be dynamically allocated and should be destroyed by the classes 
  /// virtual dtor.
  Use *OperandList;

  /// NumOperands - The number of values used by this User.
  ///
  unsigned NumOperands;

  void *operator new(size_t s, unsigned) {
    return ::operator new(s);
  }
  User(const Type *Ty, unsigned vty, Use *OpList, unsigned NumOps)
    : Value(Ty, vty), OperandList(OpList), NumOperands(NumOps) {}

public:
  Value *getOperand(unsigned i) const {
    assert(i < NumOperands && "getOperand() out of range!");
    return OperandList[i];
  }
  void setOperand(unsigned i, Value *Val) {
    assert(i < NumOperands && "setOperand() out of range!");
    OperandList[i] = Val;
  }
  unsigned getNumOperands() const { return NumOperands; }

  // ---------------------------------------------------------------------------
  // Operand Iterator interface...
  //
  typedef Use*       op_iterator;
  typedef const Use* const_op_iterator;

  inline op_iterator       op_begin()       { return OperandList; }
  inline const_op_iterator op_begin() const { return OperandList; }
  inline op_iterator       op_end()         { return OperandList+NumOperands; }
  inline const_op_iterator op_end()   const { return OperandList+NumOperands; }

  // dropAllReferences() - This function is in charge of "letting go" of all
  // objects that this User refers to.  This allows one to
  // 'delete' a whole class at a time, even though there may be circular
  // references... first all references are dropped, and all use counts go to
  // zero.  Then everything is delete'd for real.  Note that no operations are
  // valid on an object that has "dropped all references", except operator
  // delete.
  //
  void dropAllReferences() {
    Use *OL = OperandList;
    for (unsigned i = 0, e = NumOperands; i != e; ++i)
      OL[i].set(0);
  }

  /// replaceUsesOfWith - Replaces all references to the "From" definition with
  /// references to the "To" definition.
  ///
  void replaceUsesOfWith(Value *From, Value *To);

  // Methods for support type inquiry through isa, cast, and dyn_cast:
  static inline bool classof(const User *) { return true; }
  static inline bool classof(const Value *V) {
    return isa<Instruction>(V) || isa<Constant>(V);
  }
};

template<> struct simplify_type<User::op_iterator> {
  typedef Value* SimpleType;

  static SimpleType getSimplifiedValue(const User::op_iterator &Val) {
    return static_cast<SimpleType>(Val->get());
  }
};

template<> struct simplify_type<const User::op_iterator>
  : public simplify_type<User::op_iterator> {};

template<> struct simplify_type<User::const_op_iterator> {
  typedef Value* SimpleType;

  static SimpleType getSimplifiedValue(const User::const_op_iterator &Val) {
    return static_cast<SimpleType>(Val->get());
  }
};

template<> struct simplify_type<const User::const_op_iterator>
  : public simplify_type<User::const_op_iterator> {};


// value_use_iterator::getOperandNo - Requires the definition of the User class.
template<typename UserTy>
unsigned value_use_iterator<UserTy>::getOperandNo() const {
  return U - U->getUser()->op_begin();
}

} // End llvm namespace

#endif
