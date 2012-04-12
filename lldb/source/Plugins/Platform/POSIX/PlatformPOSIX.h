//===-- PlatformPOSIX.h -----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PlatformPOSIX_h_
#define liblldb_PlatformPOSIX_h_

// C Includes
// C++ Includes

#include <memory>

// Other libraries and framework includes
// Project includes
#include "lldb/Interpreter/Options.h"
#include "lldb/Target/Platform.h"

class PlatformPOSIX : public lldb_private::Platform
{
public:
    PlatformPOSIX (bool is_host);
    
    virtual
    ~PlatformPOSIX();
    
    //------------------------------------------------------------
    // lldb_private::Platform functions
    //------------------------------------------------------------
    virtual lldb_private::Options *
    GetConnectionOptions (lldb_private::CommandInterpreter& interpreter);
    
    virtual lldb_private::Error
    PutFile (const lldb_private::FileSpec& source,
             const lldb_private::FileSpec& destination,
             uint32_t uid = UINT32_MAX,
             uint32_t gid = UINT32_MAX);
    
    virtual lldb::user_id_t
    OpenFile (const lldb_private::FileSpec& file_spec,
              uint32_t flags,
              mode_t mode);
    
    virtual bool
    CloseFile (lldb::user_id_t fd);
    
    virtual uint32_t
    ReadFile (lldb::user_id_t fd, uint64_t offset,
              void *data_ptr, size_t len);
    
    virtual uint32_t
    WriteFile (lldb::user_id_t fd, uint64_t offset,
               void* data, size_t len);
    
    virtual lldb::user_id_t
    GetFileSize (const lldb_private::FileSpec& file_spec);

    virtual lldb_private::Error
    GetFile (const lldb_private::FileSpec& source,
             const lldb_private::FileSpec& destination);
    
    virtual uint32_t
    RunShellCommand (const std::string &command_line);
    
    virtual uint32_t
    MakeDirectory (const std::string &path,
                   mode_t mode);
    
protected:
    std::auto_ptr<lldb_private::Options> m_options;
    
    class POSIXPlatformConnectionOptions : public lldb_private::Options
    {
    public:
        POSIXPlatformConnectionOptions (lldb_private::CommandInterpreter &interpreter);
        
        virtual
        ~POSIXPlatformConnectionOptions ();
        
        virtual lldb_private::Error
        SetOptionValue (uint32_t option_idx, const char *option_arg);
        
        void
        OptionParsingStarting ();
        
        const lldb_private::OptionDefinition*
        GetDefinitions ();
        
        // Options table: Required for subclasses of Options.
        
        static lldb_private::OptionDefinition g_option_table[];
        
        // Instance variables to hold the values for command options.
        
        bool m_rsync;
        std::string m_rsync_opts;
        bool m_ssh;
        std::string m_ssh_opts;
    };
    
    lldb::PlatformSP m_remote_platform_sp; // Allow multiple ways to connect to a remote POSIX-compliant OS
    
private:
    DISALLOW_COPY_AND_ASSIGN (PlatformPOSIX);
    
};

#endif  // liblldb_PlatformPOSIX_h_
