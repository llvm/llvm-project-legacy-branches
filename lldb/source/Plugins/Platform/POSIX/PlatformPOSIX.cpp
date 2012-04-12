//===-- PlatformPOSIX.cpp ---------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PlatformPOSIX.h"

// C Includes
// C++ Includes
// Other libraries and framework includes
// Project includes

#include "lldb/Core/DataBufferHeap.h"
#include "lldb/Core/StreamString.h"
#include "lldb/Host/FileSpec.h"
#include "lldb/Host/Host.h"

using namespace lldb;
using namespace lldb_private;


//------------------------------------------------------------------
/// Default Constructor
//------------------------------------------------------------------
PlatformPOSIX::PlatformPOSIX (bool is_host) :
Platform(is_host),  // This is the local host platform
m_remote_platform_sp ()
{
}

//------------------------------------------------------------------
/// Destructor.
///
/// The destructor is virtual since this class is designed to be
/// inherited from by the plug-in instance.
//------------------------------------------------------------------
PlatformPOSIX::~PlatformPOSIX()
{
}

PlatformPOSIX::POSIXPlatformConnectionOptions::POSIXPlatformConnectionOptions (CommandInterpreter &interpreter) :
Options (interpreter),
m_rsync (false),
m_rsync_opts (),
m_ssh (false),
m_ssh_opts ()
{
}

PlatformPOSIX::POSIXPlatformConnectionOptions::~POSIXPlatformConnectionOptions ()
{
}

Error
PlatformPOSIX::POSIXPlatformConnectionOptions::SetOptionValue (uint32_t option_idx, const char *option_arg)
{
    Error error;
    char short_option = (char) m_getopt_table[option_idx].val;
    
    switch (short_option)
    {
        case 'r':
            m_rsync = true;
            break;
            
        case 'R':
            m_rsync_opts.assign(option_arg);
            break;
            
        case 's':
            m_ssh = true;
            break;
            
        case 'S':
            m_ssh_opts.assign(option_arg);
            break;
            
        default:
            error.SetErrorStringWithFormat ("unrecognized option '%c'", short_option);
            break;
    }
    
    return error;
}

void
PlatformPOSIX::POSIXPlatformConnectionOptions::OptionParsingStarting ()
{
    m_rsync = false;
    m_rsync_opts.clear();
    m_ssh = false;
    m_ssh_opts.clear();
}

const OptionDefinition*
PlatformPOSIX::POSIXPlatformConnectionOptions::GetDefinitions ()
{
    return g_option_table;
}

OptionDefinition
PlatformPOSIX::POSIXPlatformConnectionOptions::g_option_table[] =
{
    {   LLDB_OPT_SET_ALL, false, "rsync"            , 'r', no_argument,       NULL, 0, eArgTypeNone         , "Enable rsync." },
    {   LLDB_OPT_SET_ALL, false, "rsync-opts"       , 'R', required_argument, NULL, 0, eArgTypeCommandName  , "Platform-specific options required for rsync to work." },
    {   LLDB_OPT_SET_ALL, false, "ssh"              , 's', no_argument,       NULL, 0, eArgTypeNone         , "Enable SSH." },
    {   LLDB_OPT_SET_ALL, false, "ssh-opts"         , 'S', required_argument, NULL, 0, eArgTypeCommandName  , "Platform-specific options required for SSH to work." },
    {   0,                false, NULL               ,  0 , 0                , NULL, 0, eArgTypeNone         , NULL }
};

Options *
PlatformPOSIX::GetConnectionOptions (lldb_private::CommandInterpreter& interpreter)
{
    if (m_options.get() == NULL)
        m_options.reset(new POSIXPlatformConnectionOptions(interpreter));
    return m_options.get();
}

uint32_t
PlatformPOSIX::RunShellCommand (const std::string &command_line)
{
    if (IsHost())
    {
        return Host::RunProgramAndGetExitCode(FileSpec(command_line.c_str(),false));
    }
    if (IsRemote())
    {
        if (GetSupportsSSH())
        {
            // run the command over SSH
            StreamString command;
            command.Printf("ssh %s %s %s",
                           GetSSHOpts(),
                           GetHostname(),
                           command_line.c_str());
            return m_remote_platform_sp->RunShellCommand(command.GetData());
        }
        else if (m_remote_platform_sp)
        {
            // plain run the command
            return m_remote_platform_sp->RunShellCommand(command_line);
        }
    }
    return Platform::RunShellCommand(command_line);
}

uint32_t
PlatformPOSIX::MakeDirectory (const std::string &path,
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
PlatformPOSIX::OpenFile (const FileSpec& file_spec,
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
PlatformPOSIX::CloseFile (lldb::user_id_t fd)
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
PlatformPOSIX::ReadFile (lldb::user_id_t fd, uint64_t offset,
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
PlatformPOSIX::WriteFile (lldb::user_id_t fd, uint64_t offset,
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

static uint32_t
chown_file(Platform *platform,
           const char* path,
           uint32_t uid = UINT32_MAX,
           uint32_t gid = UINT32_MAX)
{
    if (!platform || !path || *path == 0)
        return UINT32_MAX;
    
    if (uid == UINT32_MAX && gid == UINT32_MAX)
        return 0;   // pretend I did chown correctly - actually I just didn't care
    
    StreamString command;
    command.PutCString("chown ");
    if (uid != UINT32_MAX)
        command.Printf("%d",uid);
    if (gid != UINT32_MAX)
        command.Printf(":%d",gid);
    command.Printf("%s",path);
    return platform->RunShellCommand(command.GetData());
}

lldb_private::Error
PlatformPOSIX::PutFile (const lldb_private::FileSpec& source,
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
        std::string src_path;
        if (source.GetPath(src_path) == 0)
            return Error("unable to get file path for source");
        std::string dst_path;
        if (destination.GetPath(dst_path) == 0)
            return Error("unable to get file path for destination");
        StreamString command;
        command.Printf("cp %s %s", src_path.c_str(), dst_path.c_str());
        if (RunShellCommand(command.GetData()) != 0)
            return Error("unable to perform copy");
        if (uid == UINT32_MAX && gid == UINT32_MAX)
            return Error();
        if (chown_file(this,dst_path.c_str(),uid,gid) != 0)
            return Error("unable to perform chown");
        return Error();
    }
    if (IsRemote() && m_remote_platform_sp)
    {
        if (GetSupportsRSync())
        {
            std::string src_path;
            if (source.GetPath(src_path) == 0)
                return Error("unable to get file path for source");
            std::string dst_path;
            if (destination.GetPath(dst_path) == 0)
                return Error("unable to get file path for destination");
            StreamString command;
            command.Printf("rsync %s %s %s:%s",
                           GetRSyncOpts(),
                           src_path.c_str(),
                           GetHostname(),
                           dst_path.c_str());
            if (RunShellCommand(command.GetData()) == 0)
            {
                if (chown_file(this,dst_path.c_str(),uid,gid) != 0)
                    return Error("unable to perform chown");
                return Error();
            }
            // if we are still here rsync has failed - let's try the slow way before giving up
        }
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
        if (chown_file(this,dst_path.c_str(),uid,gid) != 0)
            return Error("unable to perform chown");
        return Error();
    }
    return Platform::PutFile(source,destination,uid,gid);
}

lldb::user_id_t
PlatformPOSIX::GetFileSize (const FileSpec& file_spec)
{
    if (IsHost())
    {
        return Host::GetFileSize(file_spec);
    }
    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->GetFileSize(file_spec);
    return Platform::GetFileSize(file_spec);
}

lldb_private::Error
PlatformPOSIX::GetFile (const lldb_private::FileSpec& source /* remote file path */,
                         const lldb_private::FileSpec& destination /* local file path */)
{
#define GETFILE_CHUNK_SIZE 1024
    // Check the args, first. 
    std::string src_path;
    if (source.GetPath(src_path) == 0)
        return Error("unable to get file path for source");
    std::string dst_path;
    if (destination.GetPath(dst_path) == 0)
        return Error("unable to get file path for destination");
    if (IsHost())
    {
        if (FileSpec::Equal(source, destination, true))
            return Error("local scenario->source and destination are the same file path: no operation performed");
        // cp src dst
        StreamString cp_command;
        cp_command.Printf("cp %s %s", src_path.c_str(), dst_path.c_str());
        if (RunShellCommand(cp_command.GetData()) != 0)
            return Error("unable to perform copy");
        return Error();
    }
    else if (IsRemote() && m_remote_platform_sp)
    {
        if (GetSupportsRSync())
        {
            StreamString command;
            command.Printf("rsync %s %s:%s %s",
                           GetRSyncOpts(),
                           GetHostname(),
                           src_path.c_str(),
                           dst_path.c_str());
            printf("Running command: %s\n", command.GetData());
            if (RunShellCommand(command.GetData()) == 0)
                return Error();
            // If we are here, rsync has failed - let's try the slow way before giving up
        }
        // open src and dst
        // read/write, read/write, read/write, ...
        // close src
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
