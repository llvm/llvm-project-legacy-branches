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

#include <sstream>

// Other libraries and framework includes
// Project includes
#include "lldb/Core/Error.h"
#include "lldb/Breakpoint/BreakpointLocation.h"
#include "lldb/Core/DataBufferHeap.h"
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

lldb::user_id_t
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
PlatformMacOSX::CloseFile (lldb::user_id_t fd)
{
    if (IsHost())
    {
        return Host::CloseFile(fd);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->CloseFile(fd);
    return Platform::CloseFile(fd);
}

lldb::user_id_t
PlatformMacOSX::GetFileSize (const FileSpec& file_spec)
{
    if (IsHost())
    {
        return Host::GetFileSize(file_spec);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->GetFileSize(file_spec);
    return Platform::GetFileSize(file_spec);
}

uint32_t
PlatformMacOSX::ReadFile (lldb::user_id_t fd, uint64_t offset,
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
PlatformMacOSX::WriteFile (lldb::user_id_t fd, uint64_t offset,
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
#define PUTFILE_CHUNK_SIZE 1024
    if (IsHost())
    {
        if (FileSpec::Equal(source, destination, true))
            return Error();
        // cp src dst
        // chown uid:gid dst
        std::string src_path(512, 0);
        uint32_t len = source.GetPath(&src_path[0], 512);
        if (len >= 512)
        {
            src_path = std::string(len+1,0);
            len = source.GetPath(&src_path[0], len);
        }
        std::string dst_path(512, 0);
        len = destination.GetPath(&dst_path[0], 512);
        if (len >= 512)
        {
            src_path = std::string(len+1,0);
            len = destination.GetPath(&dst_path[0], len);
        }
        std::stringstream cp_command("cp ");
        cp_command << src_path << ' ' << dst_path;
        if (RunShellCommand(cp_command.str()) != 0)
            return Error("unable to perform copy");
        if (uid == UINT32_MAX && gid == UINT32_MAX)
            return Error();
        std::stringstream chown_command("chown ");
        if (uid != UINT32_MAX)
            chown_command << uid;
        if (gid != UINT32_MAX)
            chown_command << ':' << gid;
        chown_command << ' ' << dst_path;
        if (RunShellCommand(chown_command.str()) != 0)
            return Error("unable to perform chown");
        return Error();
    }
    if (IsRemote() && m_remote_platform_sp)
    {
        // open
        // read, write, read, write, ...
        // close
        // chown uid:gid dst
        File source_file(source, File::eOpenOptionRead, File::ePermissionsUserRW);
        if (!source_file.IsValid())
            return Error("unable to open source file");
        lldb::user_id_t dest_file = OpenFile(destination, File::eOpenOptionCanCreate | File::eOpenOptionWrite | File::eOpenOptionTruncate,
            File::ePermissionsUserRWX | File::ePermissionsGroupRX | File::ePermissionsWorldRX);
        if (dest_file == UINT64_MAX)
            return Error("unable to open target file");
        lldb::DataBufferSP buffer_sp(new DataBufferHeap(PUTFILE_CHUNK_SIZE, 0));
        Error err;
        uint64_t offset = 0;
        while (err.Success())
        {
            size_t read_data = PUTFILE_CHUNK_SIZE;
            err = source_file.Read(buffer_sp->GetBytes(), read_data);
            if (read_data)
            {
                WriteFile(dest_file, offset, buffer_sp->GetBytes(), read_data);
                offset += read_data;
            }
            else
                break;
        }
        CloseFile(dest_file);
        if (uid == UINT32_MAX && gid == UINT32_MAX)
            return Error();
        std::string dst_path;
        destination.GetPath(dst_path);
        std::stringstream chown_command("chown ");
        if (uid != UINT32_MAX)
            chown_command << uid;
        if (gid != UINT32_MAX)
            chown_command << ':' << gid;
        chown_command << ' ' << dst_path;
        if (RunShellCommand(chown_command.str()) != 0)
            return Error("unable to perform chown");
        return Error();
    }
    return Platform::PutFile(source,destination,uid,gid);
}

lldb_private::Error
PlatformMacOSX::GetFile (const lldb_private::FileSpec& source /* remote file path */,
                         const lldb_private::FileSpec& destination /* local file path */)
{
#define GETFILE_CHUNK_SIZE 1024
    if (IsHost())
    {
        if (FileSpec::Equal(source, destination, true))
            return Error("local scenario->source and destination are the same file path: no operation performed");
        // cp src dst
        char src_path[PATH_MAX];
        char dst_path[PATH_MAX];
        if (!source.GetPath(src_path, sizeof(src_path)) || !destination.GetPath(dst_path, sizeof(dst_path)))
        {
            return Error("max path length exceeded?");
        }
        std::stringstream cp_command("cp ");
        cp_command << src_path << ' ' << dst_path;
        if (RunShellCommand(cp_command.str()) != 0)
            return Error("unable to perform copy");
        return Error();
    }
    else if (IsRemote() && m_remote_platform_sp)
    {
        // open src and dst
        // read, read, read, ...
        // close src
        // write dst
        // close dst
        user_id_t fd_src = OpenFile(source, File::eOpenOptionRead, File::ePermissionsDefault);
        user_id_t fd_dst = Host::OpenFile(destination,
                                          File::eOpenOptionCanCreate|File::eOpenOptionWrite,
                                          File::ePermissionsDefault);
        if (fd_src == UINT64_MAX)
            return Error("unable to open source file");
        if (fd_dst == UINT64_MAX)
            return Error("unable to open destination file");

        lldb::DataBufferSP buffer_sp(new DataBufferHeap(GETFILE_CHUNK_SIZE, 0));
        uint8_t *cursor = buffer_sp->GetBytes();
        Error err;
        uint64_t offset = 0;
        while (err.Success())
        {
            user_id_t n_read = ReadFile(fd_src, offset, cursor, GETFILE_CHUNK_SIZE);
            if (n_read == UINT32_MAX)
                return Error("error during read operation");
            // Break out of the loop once we reach end of file.
            if (n_read == 0)
                break;

            if (!Host::WriteFile(fd_dst, offset, cursor, n_read))
                return Error("unable to write to destination file");

            offset += n_read;
            cursor += n_read;

        }
        // Ignore the close error of src.
        CloseFile(fd_src);
        // And close the dst file descriptot.
        if (!Host::CloseFile(fd_dst))
            return Error("unable to close destination file");
        return Error();
    }
    return Platform::GetFile(source,destination);
}
