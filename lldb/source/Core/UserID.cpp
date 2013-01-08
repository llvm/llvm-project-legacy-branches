//===-- UserID.cpp ----------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "lldb/Core/UserID.h"
#include "lldb/Core/Stream.h"

#ifndef _WIN32
#include <inttypes.h>
#endif

using namespace lldb;
using namespace lldb_private;

UserID::~UserID ()
{
}

Stream&
lldb_private::operator << (Stream& strm, const UserID& uid)
{
#ifdef _WIN32
    strm.Printf("{0xllu}", uid.GetID());
#else
    strm.Printf("{0x%8.8" PRIx64 "}", uid.GetID());
#endif
    return strm;
}
