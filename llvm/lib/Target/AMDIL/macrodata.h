//===-- macrodata.h -------------------------------------------------------===//
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
 *  \brief  Macrodata compile-time/run-time interface.
 *
 *  \author Alexander Lyashevsky (Alexander.Lyashevsky@amd.com)
 *  \date   March 2009
 */

#ifndef MACRODATA_HPP_
#define MACRODATA_HPP_

namespace amd {
/*! \brief Returns macro ordinal.
*
*  \details
*/
int MacroDBFindMacro( const char * _pcMacroNm );
/*! \brief Returns list of macro and number of the elements in the list.
*
*  \details
*  Examples of invocation:
int ordinal = amd::MacroDBFindMacro( "asinpi_float" );
const char **MacroPtrs;
const char *MacroPtr;
int MacrosCnt;
    MacroPtrs  =  amd::MacroDBGetMacro(&MacrosCnt,ordinal);
        for( int i = 0; i < MacrosCnt; i++)
    {
           MacroPtr = MacroPtrs[i];
        }
*/
const char ** MacroDBGetMacro( int *_MacroListCounter, int _iMacroId );

/*! \brief returns the number of inputs for the specific macro
*/
int MacroDBNumInputs(int macronum);

/*! \brief returns the number of outputs for the specific macro
*/
int MacroDBNumOutputs(int macronum);
} // namespace amd

#endif /*MACRODATA_HPP_*/
