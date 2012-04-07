//===-- PlatformMacOSX.cpp --------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PlatformMacOSX.h"

// C Includes
#include <sys/stat.h>
#include <sys/sysctl.h>

// C++ Includes
// Other libraries and framework includes
// Project includes
#include "lldb/Core/Error.h"
#include "lldb/Breakpoint/BreakpointLocation.h"
#include "lldb/Core/Module.h"
#include "lldb/Core/ModuleList.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/Core/StreamString.h"
#include "lldb/Host/FileSpec.h"
#include "lldb/Host/Host.h"
#include "lldb/Target/Process.h"
#include "lldb/Target/Target.h"

using namespace lldb;
using namespace lldb_private;
    
static uint32_t g_initialize_count = 0;

void
PlatformMacOSX::Initialize ()
{
    if (g_initialize_count++ == 0)
    {
#if defined (__APPLE__)
        PlatformSP default_platform_sp (new PlatformMacOSX(true));
        default_platform_sp->SetSystemArchitecture (Host::GetArchitecture());
        Platform::SetDefaultPlatform (default_platform_sp);
#endif        
        PluginManager::RegisterPlugin (PlatformMacOSX::GetShortPluginNameStatic(false),
                                       PlatformMacOSX::GetDescriptionStatic(false),
                                       PlatformMacOSX::CreateInstance);
    }

}

void
PlatformMacOSX::Terminate ()
{
    if (g_initialize_count > 0)
    {
        if (--g_initialize_count == 0)
        {
            PluginManager::UnregisterPlugin (PlatformMacOSX::CreateInstance);
        }
    }
}

Platform* 
PlatformMacOSX::CreateInstance (bool force, const ArchSpec *arch)
{
    // The only time we create an instance is when we are creating a remote
    // macosx platform
    const bool is_host = false;
    
    bool create = force;
    if (create == false && arch && arch->IsValid())
    {
        const llvm::Triple &triple = arch->GetTriple();
        const llvm::Triple::OSType os = triple.getOS();
        const llvm::Triple::VendorType vendor = triple.getVendor();
        if (os == llvm::Triple::Darwin && vendor == llvm::Triple::Apple)
            create = true;
    }
    if (create)
        return new PlatformMacOSX (is_host);
    return NULL;
}


const char *
PlatformMacOSX::GetPluginNameStatic ()
{
    return "PlatformMacOSX";
}

const char *
PlatformMacOSX::GetShortPluginNameStatic (bool is_host)
{
    if (is_host)
        return Platform::GetHostPlatformName ();
    else
        return "remote-macosx";
}

const char *
PlatformMacOSX::GetDescriptionStatic (bool is_host)
{
    if (is_host)
        return "Local Mac OS X user platform plug-in.";
    else
        return "Remote Mac OS X user platform plug-in.";
}

//------------------------------------------------------------------
/// Default Constructor
//------------------------------------------------------------------
PlatformMacOSX::PlatformMacOSX (bool is_host) :
    PlatformDarwin (is_host)
{
}

//------------------------------------------------------------------
/// Destructor.
///
/// The destructor is virtual since this class is designed to be
/// inherited from by the plug-in instance.
//------------------------------------------------------------------
PlatformMacOSX::~PlatformMacOSX()
{
}

Error
PlatformMacOSX::GetFile (const FileSpec &platform_file, 
                         const UUID *uuid_ptr,
                         FileSpec &local_file)
{
    if (IsRemote())
    {
        if (m_remote_platform_sp)
            return m_remote_platform_sp->GetFile (platform_file, uuid_ptr, local_file);
    }

    // Default to the local case
    local_file = platform_file;
    return Error();
}

bool
PlatformMacOSX::GetSupportedArchitectureAtIndex (uint32_t idx, ArchSpec &arch)
{
#if defined (__arm__)
    return ARMGetSupportedArchitectureAtIndex (idx, arch);
#else
    return x86GetSupportedArchitectureAtIndex (idx, arch);
#endif
}

uint32_t
PlatformMacOSX::RunShellCommand (const std::string &command_line)
{
    if (IsHost())
    {
        return Host::RunProgramAndGetExitCode(FileSpec(command_line.c_str(),false));
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->RunShellCommand(command_line);
    return Platform::RunShellCommand(command_line);
}

uint32_t
PlatformMacOSX::MakeDirectory (const std::string &path,
                               mode_t mode)
{
    if (IsHost())
    {
        return Host::MakeDirectory (path.c_str(), mode);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->MakeDirectory(path, mode);
    return Platform::MakeDirectory(path,mode);
}

uint32_t
PlatformMacOSX::OpenFile (const FileSpec& file_spec,
          uint32_t flags,
          mode_t mode)
{
    if (IsHost())
    {
        return Host::OpenFile(file_spec, flags, mode);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->OpenFile(file_spec, flags, mode);
    return Platform::OpenFile(file_spec, flags, mode);
}

bool
PlatformMacOSX::CloseFile (uint32_t fd)
{
    if (IsHost())
    {
        return Host::CloseFile(fd);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->CloseFile(fd);
    return Platform::CloseFile(fd);
}

uint32_t
PlatformMacOSX::ReadFile (uint32_t fd, uint64_t offset,
                          void *data_ptr, size_t len)
{
    if (IsHost())
    {
        return Host::ReadFile(fd, offset, data_ptr, len);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->ReadFile(fd, offset, data_ptr, len);
    return Platform::ReadFile(fd, offset, data_ptr, len);
}

uint32_t
PlatformMacOSX::WriteFile (uint32_t fd, uint64_t offset,
                           void* data, size_t len)
{
    if (IsHost())
    {
        return Host::WriteFile(fd, offset, data, len);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->WriteFile(fd, offset, data, len);
    return Platform::WriteFile(fd, offset, data, len);
}

lldb_private::Error
PlatformMacOSX::PutFile (const lldb_private::FileSpec& source,
                         const lldb_private::FileSpec& destination,
                         uint32_t uid,
                         uint32_t gid)
{
    return Platform::PutFile(source,destination,uid,gid);
}
