//===-- macrodb.h ---------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

/*! \file macrodata.hpp
 *  \brief   Declarations of CMacroData internal class.
 *           place to include 2 .hpp files generated with macrotool utility (see).
 *
 *  \author Alexander Lyashevsky (Alexander.Lyashevsky@amd.com)
 *  \date   March 2009
 */
#ifndef MACRODB_HPP_
#define MACRODB_HPP_

namespace amd {
namespace macrodata {
struct SMacroEntry {
  const char* Name;
  const char* Body;
  int Inputs;
  int Outputs;
};
}

class CMacroData {
public:
  CMacroData();
  ~CMacroData();
public:
  int MacroDBFindMacro( const char * _pcMacroNm );
  const char *MacroDBGetMacro( int _iMacroId );
  const char ** MacroDBGetMacroList( int *_MacroListCounter, int _iMacroId );
  int MacroDBFindNumInputs ( int _iMacroId );
  int MacroDBFindNumOutputs ( int _iMacroId );
protected:
  int mInit;
  int mMacroDBCounter;
  int *mRefNbr;
  int *mRefIndex;
  char **mMacroRef;

  int InitMacroDB( void );
  int ResolveReferences( void );
  int NumberOfReferences( int Ord );
  int InsertReferences( int Ord, int StartPos );
  int SearchForPattern(char *_SearchBuf, const char *_Pattern, int _PatLen);
  int ExtractString(int *_Pos0,
                    int *_Pos1,
                    char * _Name,
                    char *_pBuf,
                    const char*_Delim0,
                    const char*_Delim1);
};

// real macros
#include "macrodb_gen.h"
}

#endif /*MACRODB_HPP_*/
