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
PlatformMacOSX::GetSymbolFile (const FileSpec &platform_file, 
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

lldb_private::Error
PlatformMacOSX::GetFile (const lldb_private::FileSpec &platform_file,
                         const lldb_private::UUID *uuid_ptr,
                         lldb_private::FileSpec &local_file)
{
    if (IsRemote() && m_remote_platform_sp)
    {
        std::string local_os_build;
        Host::GetOSBuildString(local_os_build);
        std::string remote_os_build;
        m_remote_platform_sp->GetOSBuildString(remote_os_build);
        if (local_os_build.compare(remote_os_build) == 0)
        {
            // same OS version: the local file is good enough
            local_file = platform_file;
            return Error();
        }
        else
        {
            // try to find the file in the cache
            std::string cache_path(GetLocalCacheDirectory());
            std::string module_path;
            platform_file.GetPath(module_path);
            cache_path.append(module_path);
            FileSpec module_cache_spec(cache_path.c_str(),false);
            if (module_cache_spec.Exists())
            {
                local_file = module_cache_spec;
                return Error();
            }
            // bring in the remote module file
            FileSpec module_cache_folder = module_cache_spec.CopyByRemovingLastPathComponent();
            StreamString mkdir_folder_cmd;
            // try to make the local directory first
            mkdir_folder_cmd.Printf("mkdir -p %s/%s", module_cache_folder.GetDirectory().AsCString(), module_cache_folder.GetFilename().AsCString());
            Host::RunShellCommand(mkdir_folder_cmd.GetData(),
                                  NULL,
                                  NULL,
                                  NULL,
                                  NULL,
                                  60);
            Error err = GetFile(platform_file, module_cache_spec);
            if (err.Fail())
                return err;
            if (module_cache_spec.Exists())
            {
                local_file = module_cache_spec;
                return Error();
            }
            else
                return Error("unable to obtain valid module file");
        }
    }
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

lldb_private::Error
PlatformMacOSX::GetSharedModule (const lldb_private::ModuleSpec &module_spec,
                                 lldb::ModuleSP &module_sp,
                                 const lldb_private::FileSpecList *module_search_paths_ptr,
                                 lldb::ModuleSP *old_module_sp_ptr,
                                 bool *did_create_ptr)
{
    printf("[%s] Trying to find module %s/%s - platform path %s/%s symbol path %s/%s\n",
           (IsHost() ? "host" : "remote"),
           module_spec.GetFileSpec().GetDirectory().AsCString(),
           module_spec.GetFileSpec().GetFilename().AsCString(),
           module_spec.GetPlatformFileSpec().GetDirectory().AsCString(),
           module_spec.GetPlatformFileSpec().GetFilename().AsCString(),
           module_spec.GetSymbolFileSpec().GetDirectory().AsCString(),
           module_spec.GetSymbolFileSpec().GetFilename().AsCString());
    if (module_spec.GetFileSpec().Exists() && !module_sp)
    {
        module_sp.reset(new Module(module_spec));
        return Error();
    }
    // try to find the module in the cache
    std::string cache_path(GetLocalCacheDirectory());
    std::string module_path;
    module_spec.GetFileSpec().GetPath(module_path);
    cache_path.append(module_path);
    FileSpec module_cache_spec(cache_path.c_str(),false);
    if (module_cache_spec.Exists())
    {
        ModuleSpec local_spec(module_cache_spec, module_spec.GetArchitecture());
        module_sp.reset(new Module(local_spec));
        module_sp->SetPlatformFileSpec(module_spec.GetFileSpec());
        return Error();
    }
    // bring in the remote module file
    FileSpec module_cache_folder = module_cache_spec.CopyByRemovingLastPathComponent();
    StreamString mkdir_folder_cmd;
    // try to make the local directory first
    mkdir_folder_cmd.Printf("mkdir -p %s/%s", module_cache_folder.GetDirectory().AsCString(), module_cache_folder.GetFilename().AsCString());
    Host::RunShellCommand(mkdir_folder_cmd.GetData(),
                          NULL,
                          NULL,
                          NULL,
                          NULL,
                          60);
    Error err = GetFile(module_spec.GetFileSpec(), module_cache_spec);
    if (err.Fail())
        return err;
    if (module_cache_spec.Exists())
    {
        ModuleSpec local_spec(module_cache_spec, module_spec.GetArchitecture());
        module_sp.reset(new Module(local_spec));
        module_sp->SetPlatformFileSpec(module_spec.GetFileSpec());
        return Error();
    }
    else
        return Error("unable to obtain valid module file");
}
