//===- LowerFixedVector.cpp -  Implementation of LowerFixedVector Transform ---------===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file was developed by Brad Jones and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
// This file implements lowering FixedVector datatypes into more primitive
// FixedVector datatypes, and finally to scalar operations.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar.h"
#include "llvm/Argument.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/ADT/StringExtras.h"
#include <algorithm>
#include <map>
#include <iostream>

using namespace llvm;

namespace {

/// This pass converts fixed-length vector operators to an equivalent
/// operations on smaller fixed-length vector data, to possibly scalar
/// operations.  Currently it supports lowering to scalar operations.
///
/// @brief Transforms fixed-length vector instructions to simpler instructions.
///
class LowerFixedVector : public FunctionPass, public InstVisitor<LowerFixedVector> {
public:
   /// @brief Lowers fixed-length vector operations to scalar operations. 
   /// @param F The fuction to process
   virtual bool runOnFunction(Function &F);

   /// @brief Lowers fixed-length vector load instructions.
   /// @param LI the load instruction to convert
   void visitLoadInst(LoadInst& LI);

   /// @brief Lowers fixed-length vector store instructions.
   /// @param SI the store instruction to convert
   void visitStoreInst(StoreInst& SI);

   /// @brief Lowers fixed-length vector binary operations.
   /// @param BO the binary operator to convert
   void visitBinaryOperator(BinaryOperator& BO);

   /// @brief Lowers fixed-length vector select instructions.
   /// @param SELI the select operator to convert
   void visitSelectInst(SelectInst& SELI);

   /// This function asserts if the instruction is a FixedVectorType but
   /// is handled by another function.
   /// 
   /// @brief Asserts if FixedVectorType instruction is not handled elsewhere.
   /// @param I the unhandled instruction
   void visitInstruction(Instruction &I)
   {
      if(isa<FixedVectorType>(I.getType())) {
         std::cerr << "Unhandled Instruction with FixedVector ReturnType: " << 
                      I << '\n';
      }
   }
private:
   /// @brief Retrieves lowered values for a fixed-length vector value.
   /// @param val the fixed-length vector value
   /// @return the lowered values
   std::vector<Value*>& getValues(Value* val);

   /// @brief Sets lowered values for a fixed-length vector value.
   /// @param val the fixed-length vector value
   /// @param values the corresponding lowered values
   void setValues(Value* val,const std::vector<Value*>& values);

   // Data Members
   /// @brief whether we changed the function or not   
   bool Changed;

   /// @brief a map from old fixed-length vector values to new smaller fixed-length vector values
   std::map<Value*,std::vector<Value*> > fixedVectorToScalarMap;

   /// Instructions in the source program to get rid of
   /// after we do a pass (the old fixed-length vector instructions)
   std::vector<Instruction*> instrsToRemove;
}; 

RegisterOpt<LowerFixedVector> 
X("lower-fixed-vector", 
  "lowers fixed-length vector operations to operations on smaller fixed-length vector datatypes");

} // end namespace   

FunctionPass *llvm::createLowerFixedVectorPass() { return new LowerFixedVector(); }


// This function sets lowered values for a corresponding
// fixed-length vector value.  Note, in the case of a forward reference
// getValues(Value*) will have already been called for 
// the fixed-length vector parameter.  This function will then replace 
// all references in the in the function of the "dummy" 
// value the previous getValues(Value*) call 
// returned with actual references.
void LowerFixedVector::setValues(Value* value,const std::vector<Value*>& values)
{
   std::map<Value*,std::vector<Value*> >::iterator it = 
         fixedVectorToScalarMap.lower_bound(value);
   if (it == fixedVectorToScalarMap.end() || it->first != value) {
       // there was not a forward reference to this element
       fixedVectorToScalarMap.insert(it,std::make_pair(value,values));
   }
   else {
      // replace forward declarations with actual definitions
      assert(it->second.size() == values.size() && 
             "Error forward refences and actual definition differ in size");
      for (unsigned i = 0, e = values.size(); i != e; ++i) {
           // replace and get rid of old forward references
           it->second[i]->replaceAllUsesWith(values[i]);
           delete it->second[i];
           it->second[i] = values[i];
      }
   }
}

// This function will examine the fixed-length vector value parameter
// and if it is a fixed-length vector constant or a forward reference
// properly create the lowered values needed.  Otherwise
// it will simply retreive values from a  
// setValues(Value*,const std::vector<Value*>&) 
// call.  Failing both of these cases, it will abort
// the program.
std::vector<Value*>& LowerFixedVector::getValues(Value* value)
{
   assert(isa<FixedVectorType>(value->getType()) &&
          "Value must be FixedVectorType");

   // reject further processing if this one has
   // already been handled
   std::map<Value*,std::vector<Value*> >::iterator it = 
      fixedVectorToScalarMap.lower_bound(value);
   if (it != fixedVectorToScalarMap.end() && it->first == value) {
       return it->second;
   }

   if (ConstantVector* CP = dyn_cast<ConstantVector>(value)) {
       // non-zero constant case
       std::vector<Value*> results;
       results.reserve(CP->getNumOperands());
       for (unsigned i = 0, e = CP->getNumOperands(); i != e; ++i) {
          results.push_back(CP->getOperand(i));
       }
       return fixedVectorToScalarMap.insert(it,
                                       std::make_pair(value,results))->second;
   }
   else if (ConstantAggregateZero* CAZ =
            dyn_cast<ConstantAggregateZero>(value)) {
       // zero constant 
       const FixedVectorType* PKT = cast<FixedVectorType>(CAZ->getType());
       std::vector<Value*> results;
       results.reserve(PKT->getNumElements());
   
       Constant* C = Constant::getNullValue(PKT->getElementType());
       for (unsigned i = 0, e = PKT->getNumElements(); i != e; ++i) {
            results.push_back(C);
       }
       return fixedVectorToScalarMap.insert(it,
                                       std::make_pair(value,results))->second;
   }
   else if (isa<Instruction>(value)) {
       // foward reference
       const FixedVectorType* PKT = cast<FixedVectorType>(value->getType());
       std::vector<Value*> results;
       results.reserve(PKT->getNumElements());
   
      for (unsigned i = 0, e = PKT->getNumElements(); i != e; ++i) {
           results.push_back(new Argument(PKT->getElementType()));
      }
      return fixedVectorToScalarMap.insert(it,
                                      std::make_pair(value,results))->second;
   }
   else {
       // we don't know what it is, and we are trying to retrieve
       // a value for it
       assert(false && "Unhandled FixedVectorType value");
       abort();
   }
}

void LowerFixedVector::visitLoadInst(LoadInst& LI)
{
   // Make sure what we are dealing with is a fixed-length vector type
   if (const FixedVectorType* PKT = dyn_cast<FixedVectorType>(LI.getType())) {
       // Initialization, Idx is needed for getelementptr needed later
       std::vector<Value*> Idx(2);
       Idx[0] = ConstantUInt::get(Type::UIntTy,0);

       ArrayType* AT = ArrayType::get(PKT->getContainedType(0),
                                      PKT->getNumElements());
       PointerType* APT = PointerType::get(AT);

       // Cast the fixed-length vector type to an array
       Value* array = new CastInst(LI.getPointerOperand(),
                                   APT,
                                   LI.getName() + ".a",
                                   &LI);

       // Convert this load into num elements number of loads
       std::vector<Value*> values;
       values.reserve(PKT->getNumElements());

       for (unsigned i = 0, e = PKT->getNumElements(); i != e; ++i) {
            // Calculate the second index we will need
            Idx[1] = ConstantUInt::get(Type::UIntTy,i);

            // Get the pointer
            Value* val = new GetElementPtrInst(array, 
                                               Idx,
                                               LI.getName() + 
                                               ".ge." + utostr(i),
                                               &LI);

            // generate the new load and save the result in fixedVectorToScalar map
            values.push_back(new LoadInst(val, 
                             LI.getName()+"."+utostr(i),
                             LI.isVolatile(),
                             &LI));
       }
               
       setValues(&LI,values);
       Changed = true;
       instrsToRemove.push_back(&LI);
   }
}

void LowerFixedVector::visitBinaryOperator(BinaryOperator& BO)
{
   // Make sure both operands are FixedVectorTypes
   if (isa<FixedVectorType>(BO.getOperand(0)->getType())) {
       std::vector<Value*>& op0Vals = getValues(BO.getOperand(0));
       std::vector<Value*>& op1Vals = getValues(BO.getOperand(1));
       std::vector<Value*> result;
       assert((op0Vals.size() == op1Vals.size()) &&
              "The two fixed-length vector operand to scalar maps must be equal in size.");

       result.reserve(op0Vals.size());
   
       // generate the new binary op and save the result
       for (unsigned i = 0; i != op0Vals.size(); ++i) {
            result.push_back(BinaryOperator::create(BO.getOpcode(), 
                                                    op0Vals[i], 
                                                    op1Vals[i],
                                                    BO.getName() + 
                                                    "." + utostr(i),
                                                    &BO));
       }

       setValues(&BO,result);
       Changed = true;
       instrsToRemove.push_back(&BO);
   }
}

void LowerFixedVector::visitStoreInst(StoreInst& SI)
{
   if (const FixedVectorType* PKT = 
       dyn_cast<FixedVectorType>(SI.getOperand(0)->getType())) {
       // We will need this for getelementptr
       std::vector<Value*> Idx(2);
       Idx[0] = ConstantUInt::get(Type::UIntTy,0);
         
       ArrayType* AT = ArrayType::get(PKT->getContainedType(0),
                                      PKT->getNumElements());
       PointerType* APT = PointerType::get(AT);

       // cast the fixed-length vector to an array type
       Value* array = new CastInst(SI.getPointerOperand(),
                                   APT,
                                   "store.ge.a.",
                                   &SI);
       std::vector<Value*>& values = getValues(SI.getOperand(0));
      
       assert((values.size() == PKT->getNumElements()) &&
              "Scalar must have the same number of elements as FixedVector Type");

       for (unsigned i = 0, e = PKT->getNumElements(); i != e; ++i) {
            // Generate the indices for getelementptr
            Idx[1] = ConstantUInt::get(Type::UIntTy,i);
            Value* val = new GetElementPtrInst(array, 
                                               Idx,
                                               "store.ge." +
                                               utostr(i) + ".",
                                               &SI);
            new StoreInst(values[i], val, SI.isVolatile(),&SI);
       }
                 
       Changed = true;
       instrsToRemove.push_back(&SI);
   }
}

void LowerFixedVector::visitSelectInst(SelectInst& SELI)
{
   // Make sure both operands are FixedVectorTypes
   if (isa<FixedVectorType>(SELI.getType())) {
       std::vector<Value*>& op0Vals = getValues(SELI.getTrueValue());
       std::vector<Value*>& op1Vals = getValues(SELI.getFalseValue());
       std::vector<Value*> result;

      assert((op0Vals.size() == op1Vals.size()) &&
             "The two fixed-length vector operand to scalar maps must be equal in size.");

      for (unsigned i = 0; i != op0Vals.size(); ++i) {
           result.push_back(new SelectInst(SELI.getCondition(),
                                           op0Vals[i], 
                                           op1Vals[i],
                                           SELI.getName()+ "." + utostr(i),
                                           &SELI));
      }
   
      setValues(&SELI,result);
      Changed = true;
      instrsToRemove.push_back(&SELI);
   }
}

bool LowerFixedVector::runOnFunction(Function& F)
{
   // initialize
   Changed = false; 
  
   // Does three passes:
   // Pass 1) Converts FixedVector Operations to 
   //         new FixedVector Operations on smaller
   //         datatypes
   visit(F);
  
   // Pass 2) Drop all references
   std::for_each(instrsToRemove.begin(),
                 instrsToRemove.end(),
                 std::mem_fun(&Instruction::dropAllReferences));

   // Pass 3) Delete the Instructions to remove aka fixed-length vector instructions
   for (std::vector<Instruction*>::iterator i = instrsToRemove.begin(), 
                                            e = instrsToRemove.end(); 
        i != e; ++i) {
        (*i)->getParent()->getInstList().erase(*i);   
   }

   // clean-up
   fixedVectorToScalarMap.clear();
   instrsToRemove.clear();

   return Changed;
}

