//===-- RegularExpression.cpp -----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "lldb/Core/RegularExpression.h"
#include <string.h>

using namespace lldb_private;

//----------------------------------------------------------------------
// Default constructor
//----------------------------------------------------------------------
RegularExpression::RegularExpression() :
	m_re(),
	m_regex(llvm::StringRef())
{
}

//----------------------------------------------------------------------
// Constructor that compiles "re" using "flags" and stores the
// resulting compiled regular expression into this object.
//----------------------------------------------------------------------
RegularExpression::RegularExpression(const char* re, int flags) :
	m_re(),
    m_regex(llvm::StringRef())
{
    Compile(re);
}

//----------------------------------------------------------------------
// Constructor that compiles "re" using "flags" and stores the
// resulting compiled regular expression into this object.
//----------------------------------------------------------------------
RegularExpression::RegularExpression(const char* re) :
    m_re(),
    m_regex(llvm::StringRef())
{
    Compile(re);
}

RegularExpression::RegularExpression(const RegularExpression &rhs) :
    m_regex(llvm::StringRef())
 {
     Compile(rhs.GetText(), rhs.GetCompileFlags());
 }


const RegularExpression &
RegularExpression::operator= (const RegularExpression &rhs)
{
    if (&rhs != this)
    {
        Compile (rhs.GetText(), rhs.GetCompileFlags());
    }
    return *this;
}
//----------------------------------------------------------------------
// Destructor
//
// Any previosuly compiled regular expression contained in this
// object will be freed.
//----------------------------------------------------------------------
RegularExpression::~RegularExpression()
{

}

//----------------------------------------------------------------------
// Compile a regular expression using the supplied regular
// expression text and flags. The compied regular expression lives
// in this object so that it can be readily used for regular
// expression matches. Execute() can be called after the regular
// expression is compiled. Any previosuly compiled regular
// expression contained in this object will be freed.
//
// RETURNS
//  True of the refular expression compiles successfully, false
//  otherwise.
//----------------------------------------------------------------------
bool
RegularExpression::Compile(const char* re)
{
    return Compile (re, m_compile_flags);
}

bool
RegularExpression::Compile(const char* re, int flags)
{
    Free();
    m_compile_flags = flags;
    m_re = re;
    m_regex = llvm::Regex(llvm::StringRef(re));
 
    return IsValid();
}

//----------------------------------------------------------------------
// Execute a regular expression match using the compiled regular
// expression that is already in this object against the match
// string "s". If any parens are used for regular expression
// matches "match_count" should indicate the number of regmatch_t
// values that are present in "match_ptr". The regular expression
// will be executed using the "execute_flags".
//----------------------------------------------------------------------
bool
RegularExpression::Execute(const char* s, size_t num_matches, int execute_flags) const
{
    return m_regex.match(llvm::StringRef(s), &m_matches);
}

bool
RegularExpression::GetMatchAtIndex (const char* s, uint32_t idx, std::string& match_str) const
{
    if (idx < m_matches.size())
    {
        match_str = m_matches[idx];
        return true;
    }
    return false;
}


//----------------------------------------------------------------------
// Returns true if the regular expression compiled and is ready
// for execution.
//----------------------------------------------------------------------
bool
RegularExpression::IsValid () const
{
    std::string err;
    return m_regex.isValid(err);
}

//----------------------------------------------------------------------
// Returns the text that was used to compile the current regular
// expression.
//----------------------------------------------------------------------
const char*
RegularExpression::GetText () const
{
    if (m_re.empty())
        return NULL;
    return m_re.c_str();
}

//----------------------------------------------------------------------
// Free any contained compiled regular expressions.
//----------------------------------------------------------------------
void
RegularExpression::Free()
{
    m_re.clear();
    m_regex = llvm::Regex(llvm::StringRef());
    m_matches.clear();
}

std::string
RegularExpression::GetErrorAsCString () const
{
    std::string err;
    m_regex.isValid(err);
    return err;
}

bool
RegularExpression::operator < (const RegularExpression& rhs) const
{
    return (m_re < rhs.m_re);
}

