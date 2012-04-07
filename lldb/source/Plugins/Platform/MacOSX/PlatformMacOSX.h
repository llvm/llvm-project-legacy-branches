//===-- PlatformMacOSX.h ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PlatformMacOSX_h_
#define liblldb_PlatformMacOSX_h_

// C Includes
// C++ Includes
// Other libraries and framework includes
// Project includes
#include "PlatformDarwin.h"

class PlatformMacOSX : public PlatformDarwin
{
public:

    //------------------------------------------------------------
    // Class functions
    //------------------------------------------------------------
    static lldb_private::Platform* 
    CreateInstance (bool force, const lldb_private::ArchSpec *arch);

    static void
    Initialize ();

    static void
    Terminate ();
    
    static const char *
    GetPluginNameStatic ();

    static const char *
    GetShortPluginNameStatic(bool is_host);

    static const char *
    GetDescriptionStatic(bool is_host);
    
    //------------------------------------------------------------
    // Class Methods
    //------------------------------------------------------------
    PlatformMacOSX (bool is_host);

    virtual
    ~PlatformMacOSX();

    //------------------------------------------------------------
    // lldb_private::PluginInterface functions
    //------------------------------------------------------------
    virtual const char *
    GetPluginName()
    {
        return GetPluginNameStatic ();
    }
    
    virtual const char *
    GetShortPluginName()
    {
        return GetShortPluginNameStatic (IsHost());
    }
    
    virtual uint32_t
    GetPluginVersion()
    {
        return 1;
    }
    
    virtual const char *
    GetDescription ()
    {
        return GetDescriptionStatic (IsHost());
    }

    virtual lldb_private::Error
    GetFile (const lldb_private::FileSpec &platform_file, 
             const lldb_private::UUID *uuid_ptr,
             lldb_private::FileSpec &local_file);
    
    virtual uint32_t
    RunShellCommand (const std::string &command_line);
    
    virtual uint32_t
    MakeDirectory (const std::string &path,
                   mode_t mode);
    
    virtual lldb_private::Error
    PutFile (const lldb_private::FileSpec& source,
             const lldb_private::FileSpec& destination,
             uint32_t uid = UINT32_MAX,
             uint32_t gid = UINT32_MAX);
    
    virtual uint32_t
    OpenFile (const lldb_private::FileSpec& file_spec,
              uint32_t flags,
              mode_t mode);
    
    virtual bool
    CloseFile (uint32_t fd);
    
    virtual uint32_t
    ReadFile (uint32_t fd, uint64_t offset,
              void *data_ptr, size_t len);
    
    virtual uint32_t
    WriteFile (uint32_t fd, uint64_t offset,
               void* data, size_t len);

    virtual bool
    GetSupportedArchitectureAtIndex (uint32_t idx, 
                                     lldb_private::ArchSpec &arch);

private:
    DISALLOW_COPY_AND_ASSIGN (PlatformMacOSX);

};

#endif  // liblldb_PlatformMacOSX_h_
