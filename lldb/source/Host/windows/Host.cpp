//===-- source/Host/linux/Host.cpp ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// C Includes
#include <stdio.h>

// C++ Includes
// Other libraries and framework includes
// Project includes
#include "lldb/Core/Error.h"
#include "lldb/Target/Process.h"

#include "lldb/Host/Host.h"
#include "lldb/Core/DataBufferHeap.h"
#include "lldb/Core/DataExtractor.h"

using namespace lldb;
using namespace lldb_private;

bool
Host::GetOSVersion(uint32_t &major, 
                   uint32_t &minor, 
                   uint32_t &update)
{
    OSVERSIONINFOEX info;
    
    ZeroMemory(&info, sizeof(OSVERSIONINFOEX));
    info.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    
    if (GetVersionEx((LPOSVERSIONINFO) &info) == 0) {
        return false;
    }

    major = (uint32_t) info.dwMajorVersion;
    minor = (uint32_t) info.dwMinorVersion;
    update = (uint32_t) info.wServicePackMajor;

    return true;
}

Error
Host::LaunchProcess (ProcessLaunchInfo &launch_info)
{
    Error error;
    assert(!"Not implemented yet!!!");
    return error;
}

lldb::DataBufferSP
Host::GetAuxvData(lldb_private::Process *process)
{
    return 0;
}
