//===-- PlatformFreeBSD.cpp ---------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PlatformFreeBSD.h"

// C Includes
#include <stdio.h>
#include <sys/utsname.h>

// C++ Includes
// Other libraries and framework includes
// Project includes
#include "lldb/Core/Error.h"
#include "lldb/Core/Debugger.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/Host/Host.h"

using namespace lldb;
using namespace lldb_private;

Platform *
PlatformFreeBSD::CreateInstance (bool force, const lldb_private::ArchSpec *arch)
{
    // The only time we create an instance is when we are creating a remote
    // freebsd platform
    const bool is_host = false;

    bool create = force;
    if (create == false && arch && arch->IsValid())
    {
        const llvm::Triple &triple = arch->GetTriple();
        const llvm::Triple::OSType os = triple.getOS();
        if (os == llvm::Triple::FreeBSD || os == llvm::Triple::KFreeBSD)
            create = true;
    }
    if (create)
        return new PlatformFreeBSD (is_host);
    return NULL;

}

const char *
PlatformFreeBSD::GetPluginNameStatic()
{
    return "plugin.platform.freebsd";
}

const char *
PlatformFreeBSD::GetShortPluginNameStatic (bool is_host)
{
    if (is_host)
        return Platform::GetHostPlatformName ();
    else
        return "remote-freebsd";
}

const char *
PlatformFreeBSD::GetDescriptionStatic (bool is_host)
{
    if (is_host)
        return "Local FreeBSD user platform plug-in.";
    else
        return "Remote FreeBSD user platform plug-in.";
}

static uint32_t g_initialize_count = 0;

void
PlatformFreeBSD::Initialize ()
{
    if (g_initialize_count++ == 0)
    {
#if defined (__FreeBSD__)
    	// Force a host flag to true for the default platform object.
        PlatformSP default_platform_sp (new PlatformFreeBSD(true));
        default_platform_sp->SetSystemArchitecture (Host::GetArchitecture());
        Platform::SetDefaultPlatform (default_platform_sp);
#endif
        PluginManager::RegisterPlugin(PlatformFreeBSD::GetShortPluginNameStatic(false),
                                      PlatformFreeBSD::GetDescriptionStatic(false),
                                      PlatformFreeBSD::CreateInstance);
    }
}

void
PlatformFreeBSD::Terminate ()
{
    if (g_initialize_count > 0 && --g_initialize_count == 0)
    	PluginManager::UnregisterPlugin (PlatformFreeBSD::CreateInstance);
}

//------------------------------------------------------------------
/// Default Constructor
//------------------------------------------------------------------
PlatformFreeBSD::PlatformFreeBSD (bool is_host) :
Platform(is_host)
{
}

//------------------------------------------------------------------
/// Destructor.
///
/// The destructor is virtual since this class is designed to be
/// inherited from by the plug-in instance.
//------------------------------------------------------------------
PlatformFreeBSD::~PlatformFreeBSD()
{
}

//TODO:VK: inherit PlatformPOSIX
lldb_private::Error
PlatformFreeBSD::RunShellCommand (const char *command,
                                  const char *working_dir,
                                  int *status_ptr,
                                  int *signo_ptr,
                                  std::string *command_output,
                                  uint32_t timeout_sec)
{
    if (IsHost())
        return Host::RunShellCommand(command, working_dir, status_ptr, signo_ptr, command_output, timeout_sec);
    else
    {
        if (m_remote_platform_sp)
            return m_remote_platform_sp->RunShellCommand(command, working_dir, status_ptr, signo_ptr, command_output, timeout_sec);
        else
            return Error("unable to run a remote command without a platform");
    }
}


Error
PlatformFreeBSD::ResolveExecutable (const FileSpec &exe_file,
                                    const ArchSpec &exe_arch,
                                    lldb::ModuleSP &exe_module_sp,
                                    const FileSpecList *module_search_paths_ptr)
{
    // Nothing special to do here, just use the actual file and architecture

    if (lldb_private::IsLogVerbose())
    {
        LogSP log(lldb_private::GetLogIfAllCategoriesSet (LIBLLDB_LOG_VERBOSE));
        log->Printf("Resolve executable '%s/%s'", exe_file.GetDirectory().AsCString(), exe_file.GetFilename().AsCString());
        log->Printf("Resolve executable arch '%s'", exe_arch.GetArchitectureName());
        }

    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->ResolveExecutable (exe_file,
                                                             exe_arch,
                                                             exe_module_sp,
                                                             module_search_paths_ptr);
            
    return Platform::ResolveExecutable(exe_file, exe_arch, exe_module_sp, module_search_paths_ptr);
            }

size_t
PlatformFreeBSD::GetSoftwareBreakpointTrapOpcode (Target &target, BreakpointSite *bp_site)
{
    ArchSpec arch = target.GetArchitecture();
    const uint8_t *trap_opcode = NULL;
    size_t trap_opcode_size = 0;

    switch (arch.GetCore())
    {
    default:
        assert(false && "Unhandled architecture in PlatformFreeBSD::GetSoftwareBreakpointTrapOpcode()");
        break;

    case ArchSpec::eCore_x86_32_i386:
    case ArchSpec::eCore_x86_64_x86_64:
        {
            static const uint8_t g_i386_opcode[] = { 0xCC };
            trap_opcode = g_i386_opcode;
            trap_opcode_size = sizeof(g_i386_opcode);
        }
        break;
    }

    if (bp_site->SetTrapOpcode(trap_opcode, trap_opcode_size))
        return trap_opcode_size;

    return 0;
}

bool
PlatformFreeBSD::GetRemoteOSVersion ()
{
    if (m_remote_platform_sp)
        return m_remote_platform_sp->GetOSVersion (m_major_os_version,
                                                   m_minor_os_version,
                                                   m_update_os_version);
    return false;
}

bool
PlatformFreeBSD::GetRemoteOSBuildString (std::string &s)
{
    if (m_remote_platform_sp)
        return m_remote_platform_sp->GetRemoteOSBuildString (s);
    s.clear();
    return false;
}

bool
PlatformFreeBSD::GetRemoteOSKernelDescription (std::string &s)
{
    if (m_remote_platform_sp)
        return m_remote_platform_sp->GetRemoteOSKernelDescription (s);
    s.clear();
    return false;
}

// Remote Platform subclasses need to override this function
ArchSpec
PlatformFreeBSD::GetRemoteSystemArchitecture ()
{
    if (m_remote_platform_sp)
        return m_remote_platform_sp->GetRemoteSystemArchitecture ();
    return ArchSpec();
}


const char *
PlatformFreeBSD::GetHostname ()
{
    if (IsHost())
        return Platform::GetHostname();

    if (m_remote_platform_sp)
        return m_remote_platform_sp->GetHostname ();
    return NULL;
}

bool
PlatformFreeBSD::IsConnected () const
{
    if (IsHost())
        return true;
    else if (m_remote_platform_sp)
        return m_remote_platform_sp->IsConnected();
    return false;
}

Error
PlatformFreeBSD::ConnectRemote (Args& args)
{
    Error error;
    if (IsHost())
    {
        error.SetErrorStringWithFormat ("can't connect to the host platform '%s', always connected", GetShortPluginName());
    }
    else
    {
        if (!m_remote_platform_sp)
            m_remote_platform_sp = Platform::Create ("remote-gdb-server", error);

        if (m_remote_platform_sp)
        {
            if (error.Success())
            {
                if (m_remote_platform_sp)
                {
                    error = m_remote_platform_sp->ConnectRemote (args);
                }
                else
                {
                    error.SetErrorString ("\"platform connect\" takes a single argument: <connect-url>");
                }
            }
        }
        else
            error.SetErrorString ("failed to create a 'remote-gdb-server' platform");

        if (error.Fail())
            m_remote_platform_sp.reset();
    }

    return error;
}

Error
PlatformFreeBSD::DisconnectRemote ()
{
    Error error;

    if (IsHost())
    {
        error.SetErrorStringWithFormat ("can't disconnect from the host platform '%s', always connected", GetShortPluginName());
    }
    else
    {
        if (m_remote_platform_sp)
            error = m_remote_platform_sp->DisconnectRemote ();
        else
            error.SetErrorString ("the platform is not currently connected");
    }
    return error;
}

bool
PlatformFreeBSD::GetProcessInfo (lldb::pid_t pid, ProcessInstanceInfo &process_info)
{
    bool success = false;
    if (IsHost())
    {
        success = Platform::GetProcessInfo (pid, process_info);
    }
    else if (m_remote_platform_sp) 
    {
        success = m_remote_platform_sp->GetProcessInfo (pid, process_info);
    }
    return success;
}



uint32_t
PlatformFreeBSD::FindProcesses (const ProcessInstanceInfoMatch &match_info,
                               ProcessInstanceInfoList &process_infos)
{
    uint32_t match_count = 0;
    if (IsHost())
    {
        // Let the base class figure out the host details
        match_count = Platform::FindProcesses (match_info, process_infos);
    }
    else
    {
        // If we are remote, we can only return results if we are connected
        if (m_remote_platform_sp)
            match_count = m_remote_platform_sp->FindProcesses (match_info, process_infos);
    }
    return match_count;
}

Error
PlatformFreeBSD::LaunchProcess (ProcessLaunchInfo &launch_info)
{
    Error error;
    if (IsHost())
    {
        error = Platform::LaunchProcess (launch_info);
    }
    else
    {
        if (m_remote_platform_sp)
            error = m_remote_platform_sp->LaunchProcess (launch_info);
        else
            error.SetErrorString ("the platform is not currently connected");
    }
    return error;
}

lldb::ProcessSP
PlatformFreeBSD::Attach(ProcessAttachInfo &attach_info,
                        Debugger &debugger,
                        Target *target,
                        Listener &listener,
                        Error &error)
{
    lldb::ProcessSP process_sp;
    if (IsHost())
    {
        if (target == NULL)
        {
            TargetSP new_target_sp;
            FileSpec emptyFileSpec;
            ArchSpec emptyArchSpec;

            error = debugger.GetTargetList().CreateTarget (debugger,
                                                           emptyFileSpec,
                                                           emptyArchSpec,
                                                           false,
                                                           m_remote_platform_sp,
                                                           new_target_sp);
            target = new_target_sp.get();
        }
        else
            error.Clear();

        if (target && error.Success())
        {
            debugger.GetTargetList().SetSelectedTarget(target);
            // The freebsd always currently uses the GDB remote debugger plug-in
            // so even when debugging locally we are debugging remotely!
            // Just like the darwin plugin.
            process_sp = target->CreateProcess (listener, "gdb-remote", NULL);

            if (process_sp)
                error = process_sp->Attach (attach_info);
        }
    }
    else
    {
        if (m_remote_platform_sp)
            process_sp = m_remote_platform_sp->Attach (attach_info, debugger, target, listener, error);
        else
            error.SetErrorString ("the platform is not currently connected");
    }
    return process_sp;
}

const char *
PlatformFreeBSD::GetUserName (uint32_t uid)
{
    // Check the cache in Platform in case we have already looked this uid up
    const char *user_name = Platform::GetUserName(uid);
    if (user_name)
        return user_name;

    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->GetUserName(uid);
    return NULL;
}

const char *
PlatformFreeBSD::GetGroupName (uint32_t gid)
{
    const char *group_name = Platform::GetGroupName(gid);
    if (group_name)
        return group_name;

    if (IsRemote() && m_remote_platform_sp)
        return m_remote_platform_sp->GetGroupName(gid);
    return NULL;
}


// From PlatformMacOSX only
Error
PlatformFreeBSD::GetFile (const FileSpec &platform_file,
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

Error
PlatformFreeBSD::GetSharedModule (const ModuleSpec &module_spec,
                                  ModuleSP &module_sp,
                                  const FileSpecList *module_search_paths_ptr,
                                  ModuleSP *old_module_sp_ptr,
                                  bool *did_create_ptr)
{
    Error error;
    module_sp.reset();

    if (IsRemote())
    {
        // If we have a remote platform always, let it try and locate
        // the shared module first.
        if (m_remote_platform_sp)
        {
            error = m_remote_platform_sp->GetSharedModule (module_spec,
                                                           module_sp,
                                                           module_search_paths_ptr,
                                                           old_module_sp_ptr,
                                                           did_create_ptr);
        }
    }

    if (!module_sp)
    {
        // Fall back to the local platform and find the file locally
        error = Platform::GetSharedModule (module_spec,
                                           module_sp,
                                           module_search_paths_ptr,
                                           old_module_sp_ptr,
                                           did_create_ptr);
    }
    if (module_sp)
        module_sp->SetPlatformFileSpec(module_spec.GetFileSpec());
    return error;
}


bool
PlatformFreeBSD::GetSupportedArchitectureAtIndex (uint32_t idx, ArchSpec &arch)
{
    // From macosx;s plugin code. For FreeBSD we may want to support more archs.
    if (idx == 0)
    {
        arch = Host::GetArchitecture (Host::eSystemDefaultArchitecture);
        return arch.IsValid();
    }
    else if (idx == 1)
    {
        ArchSpec platform_arch (Host::GetArchitecture (Host::eSystemDefaultArchitecture));
        ArchSpec platform_arch64 (Host::GetArchitecture (Host::eSystemDefaultArchitecture64));
        if (platform_arch == platform_arch64)
        {
            // This freebsd platform supports both 32 and 64 bit. Since we already
            // returned the 64 bit arch for idx == 0, return the 32 bit arch
            // for idx == 1
            arch = Host::GetArchitecture (Host::eSystemDefaultArchitecture32);
            return arch.IsValid();
        }
    }
    return false;
}

void
PlatformFreeBSD::GetStatus (Stream &strm)
{
    struct utsname un;

    strm << "      Host: ";

    ::memset(&un, 0, sizeof(utsname));
    if (uname(&un) == -1)
        strm << "FreeBSD" << '\n';

    strm << un.sysname << ' ' << un.release;
    if (un.nodename[0] != '\0')
        strm << " (" << un.nodename << ')';
    strm << '\n';

    // Dump a common information about the platform status.
    Platform::GetStatus(strm);
}
