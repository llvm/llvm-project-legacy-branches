//===-- macrodata.cpp -----------------------------------------------------===//
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
 *  \brief  Macrodata compile-time/run-time implementation.
 *
 *  \author Alexander Lyashevsky (Alexander.Lyashevsky@amd.com)
 *  \date   March 2009
 */
#include <stdio.h>
#include <string.h>
#include "macrodata.h"
#include "macrodb.h"

namespace amd {
static const char *csMacroCallPattern = "mcall(";

static CMacroData sMacroDataDBObject;

CMacroData :: CMacroData()
{
  mInit = 0;
  mMacroDBCounter = 0;
  mRefNbr = 0;
  mRefIndex = 0;
  mMacroRef = 0;

  InitMacroDB();
  ResolveReferences();
}
CMacroData :: ~CMacroData()
{
  if ( mRefNbr )
  {
    delete [] mRefNbr;
    mRefNbr = 0;
  }
  if ( mRefIndex )
  {
    delete [] mRefIndex;
    mRefIndex = 0;
  }
  if ( mMacroRef )
  {
    delete [] mMacroRef;
    mMacroRef = 0;
  }
}
int CMacroData ::SearchForPattern(char *_SearchBuf,
                                  const char *_Pattern,
                                  int _PatLen)
{
  int r = -1;
  int BufLen = (int)strlen(_SearchBuf);
  for(int i = 0; i < BufLen - _PatLen; i++)
  {
    if (!memcmp(&_SearchBuf[i],_Pattern,_PatLen))
    {
      r = i;
      break;
    }
  }
  return(r);
}
int CMacroData ::ExtractString(int *_Pos0,
                               int *_Pos1,
                               char * _Name,
                               char *_pBuf,
                               const char*_Delim0,
                               const char*_Delim1)
{
  int r = 0;
//int len = (int)strlen(_pBuf);
  int len0 = (int)strlen(_Delim0);
  int len1 = (int)strlen(_Delim1);
  *_Pos0 = SearchForPattern(_pBuf, _Delim0,len0 );
  *_Pos1 = SearchForPattern(&_pBuf[(*_Pos0)+len0], _Delim1, len1);
  if ( *_Pos0 != -1 && *_Pos1 != -1 )
  {
    int nameLen = *_Pos1;
    *_Pos1 += (*_Pos0)+len0;
    memcpy(_Name, &_pBuf[(*_Pos0)+len0], nameLen);
    _Name[nameLen] = 0;
    r = 1;
  }
  return(r);
}
int CMacroData :: InitMacroDB( void )
{
  int r = 1;
// count macros
  for(mMacroDBCounter = 0;
      amd::sMacroDB[mMacroDBCounter].Name[0] != 0;
      mMacroDBCounter++) ;
  mInit = 1;
  return (r);
}
int CMacroData :: NumberOfReferences( int Ord )
{
  int r = 0;
  char *pMacro;
  int patLen = (int)strlen(csMacroCallPattern);
  int pos = 0;
  pMacro = (char*)sMacroDB[Ord].Body;
  while( 1 )
  {
    pos = SearchForPattern(&pMacro[pos], csMacroCallPattern, patLen);
    if ( pos != -1)
    {
      r++;
      pos += patLen;
    }
    else
    {
      break;
    }
  }

  return(r);
}
int CMacroData :: InsertReferences( int Ord, int StartPos )
{
  int r = 0;
  char *pMacro;
  int patLen = (int)strlen(csMacroCallPattern);
  int pos = 0;
  pMacro = (char*)sMacroDB[Ord].Body;
  r = 0;
  while( 1 )
  {
    pos = SearchForPattern(&pMacro[pos], csMacroCallPattern, patLen);
    if ( pos != -1)
    {
      char Nmbr[64];
      int pos0,pos1;
      if (ExtractString(&pos0,&pos1,Nmbr, &pMacro[pos], csMacroCallPattern,
                        ")") != -1)
      {
        int newOrd;
        sscanf(Nmbr,"%d",&newOrd);
        mMacroRef[StartPos + r] = (char*)sMacroDB[newOrd].Body;
        r++;
      }
      pos += patLen;
    }
    else
    {
      break;
    }
  }

// last is itself
  mMacroRef[StartPos + mRefNbr[Ord] - 1] = (char*)sMacroDB[Ord].Body;
  return(r);
}
int CMacroData :: ResolveReferences( void )
{
  int r = 1;
  int totalRef;
  int startPos;
  if ( mRefNbr )
  {
    delete [] mRefNbr;
  }
  mRefNbr = new int [mMacroDBCounter];
  for(int i = 0; i < mMacroDBCounter; i++)
  {
// plus itself
    mRefNbr[i] = NumberOfReferences(i) + 1;
  }
// count total ref and set starting ref position per macro
  totalRef = 0;
  for(int i = 0; i < mMacroDBCounter; i++)
  {
    totalRef += mRefNbr[i];
  }

  if ( mRefIndex )
  {
    delete [] mRefIndex;
  }
  mRefIndex = new int [mMacroDBCounter];

  if ( mMacroRef )
  {
    delete [] mMacroRef;
  }

  mMacroRef = new char*[totalRef];

  startPos = 0;
  for( int i = 0; i < mMacroDBCounter; i++)
  {
    InsertReferences( i, startPos );
    mRefIndex[i] = startPos;
    startPos += mRefNbr[i];
  }

  return (r);
}
int CMacroData :: MacroDBFindMacro( const char * _pcMacroNm )
{
  int r = -1;
  if ( mInit )
  {
    for ( int i = 0; i < mMacroDBCounter; i++)
    {
      if ( !strcmp(_pcMacroNm,sMacroDB[i].Name))
      {
        r = i;
        break;
      }
    }
  }
  return(r);
}
const char *CMacroData :: MacroDBGetMacro( int _iMacroId )
{
  const char *r = 0;
  if ( mInit && _iMacroId >= 0 && _iMacroId < mMacroDBCounter)
  {
    r = sMacroDB[_iMacroId].Body;
  }

  return r;
}
const char ** CMacroData :: MacroDBGetMacroList( int *_MacroListCounter,
                                                 int _iMacroId )
{
  const char **r = 0;
  if ( mInit && _MacroListCounter && _iMacroId >= 0 && _iMacroId <
       mMacroDBCounter)
  {
    int refPos = mRefIndex[_iMacroId];
    r = (const char **)&mMacroRef[refPos];
    *_MacroListCounter = mRefNbr[_iMacroId];
  }
  return(r);
}
int CMacroData :: MacroDBFindNumInputs( int _iMacroId )
{
  int r = 0;
  if ( mInit && _iMacroId >=0 && _iMacroId < mMacroDBCounter)
  {
    r = sMacroDB[_iMacroId].Inputs;
  }
  return r;
}
int CMacroData :: MacroDBFindNumOutputs( int _iMacroId )
{
  int r = 0;
  if ( mInit && _iMacroId >=0 && _iMacroId < mMacroDBCounter)
  {
    r = sMacroDB[_iMacroId].Outputs;
  }
  return r;
}
// public:

int MacroDBFindMacro( const char * _pcMacroNm )
{
  return(sMacroDataDBObject.MacroDBFindMacro(_pcMacroNm));
}
const char ** MacroDBGetMacro( int *_MacroListCounter, int _iMacroId )
{
  return(sMacroDataDBObject.MacroDBGetMacroList(_MacroListCounter, _iMacroId));
}
int MacroDBNumInputs(int _iMacroId)
{
  return(sMacroDataDBObject.MacroDBFindNumInputs(_iMacroId));
}
int MacroDBNumOutputs(int _iMacroId)
{
  return (sMacroDataDBObject.MacroDBFindNumOutputs(_iMacroId));
}
} // namespace amd
