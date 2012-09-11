//===-- Driver.cpp ----------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Driver.h"

#ifdef _WIN32
#include "lldb/lldb-windows.h"
#include "lldb/lldb-private-log.h"
#include "lldb/Core/StreamCallback.h"
#include "lldb/Core/Log.h"
#endif
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>

#include "IOChannel.h"
#include "lldb/API/SBBreakpoint.h"
#include "lldb/API/SBCommandInterpreter.h"
#include "lldb/API/SBCommandReturnObject.h"
#include "lldb/API/SBCommunication.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBEvent.h"
#include "lldb/API/SBHostOS.h"
#include "lldb/API/SBListener.h"
#include "lldb/API/SBStream.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBProcess.h"

using namespace lldb;

static void reset_stdin_termios ();

Driver::Driver () :
    SBBroadcaster ("Driver"),
    m_debugger (NULL),
    m_editline_pty (),
    m_editline_slave_fh (NULL),
    m_editline_reader (),
    m_io_channel_ap (),
    m_option_data (),
    m_waiting_for_command (false)
{
}

Driver::~Driver ()
{
}

const char *
Driver::GetFilename() const
{
    if (m_option_data.m_args.empty())
        return NULL;
    return m_option_data.m_args.front().c_str();
}

const char *
Driver::GetCrashLogFilename() const
{
    if (m_option_data.m_crash_log.empty())
        return NULL;
    return m_option_data.m_crash_log.c_str();
}

lldb::ScriptLanguage
Driver::GetScriptLanguage() const
{
    return m_option_data.m_script_lang;
}

size_t
Driver::GetNumSourceCommandFiles () const
{
    return m_option_data.m_source_command_files.size();
}

const char *
Driver::GetSourceCommandFileAtIndex (uint32_t idx) const
{
    if (idx < m_option_data.m_source_command_files.size())
        return m_option_data.m_source_command_files[idx].c_str();
    return NULL;
}

bool
Driver::GetDebugMode() const
{
    return m_option_data.m_debug_mode;
}

size_t
Driver::GetProcessSTDOUT ()
{
    //  The process has stuff waiting for stdout; get it and write it out to the appropriate place.
    char stdio_buffer[1024];
    size_t len;
    size_t total_bytes = 0;
    while ((len = m_debugger.GetSelectedTarget().GetProcess().GetSTDOUT (stdio_buffer, sizeof (stdio_buffer))) > 0)
    {
        m_io_channel_ap->OutWrite (stdio_buffer, len, ASYNC);
        total_bytes += len;
    }
    return total_bytes;
}

size_t
Driver::GetProcessSTDERR ()
{
    //  The process has stuff waiting for stderr; get it and write it out to the appropriate place.
    char stdio_buffer[1024];
    size_t len;
    size_t total_bytes = 0;
    while ((len = m_debugger.GetSelectedTarget().GetProcess().GetSTDERR (stdio_buffer, sizeof (stdio_buffer))) > 0)
    {
        m_io_channel_ap->ErrWrite (stdio_buffer, len, ASYNC);
        total_bytes += len;
    }
    return total_bytes;
}

void
Driver::UpdateSelectedThread ()
{
    using namespace lldb;
    SBProcess process(m_debugger.GetSelectedTarget().GetProcess());
    if (!process.IsValid())
        return;
    
    SBThread curr_thread (process.GetSelectedThread());
    SBThread thread;
    StopReason curr_thread_stop_reason = eStopReasonInvalid;
    curr_thread_stop_reason = curr_thread.GetStopReason();

    if (!curr_thread.IsValid() ||
        curr_thread_stop_reason == eStopReasonInvalid ||
        curr_thread_stop_reason == eStopReasonNone)
    {
        // Prefer a thread that has just completed its plan over another thread as current thread.
        SBThread plan_thread;
        SBThread other_thread;
       const size_t num_threads = process.GetNumThreads();
        size_t i;
        for (i = 0; i < num_threads; ++i)
        {
            thread = process.GetThreadAtIndex(i);
            StopReason thread_stop_reason = thread.GetStopReason();
            switch (thread_stop_reason)
			{
            default:
            case eStopReasonInvalid:
            case eStopReasonNone:
                break;

            case eStopReasonTrace:
            case eStopReasonBreakpoint:
            case eStopReasonWatchpoint:
            case eStopReasonSignal:
            case eStopReasonException:
                if (!other_thread.IsValid())
                    other_thread = thread;
                break;
            case eStopReasonPlanComplete:
                if (!plan_thread.IsValid())
                    plan_thread = thread;
                break;
            }
        }
        if (plan_thread.IsValid())
            process.SetSelectedThread (plan_thread);
        else if (other_thread.IsValid())
            process.SetSelectedThread (other_thread);
        else
		{
			if (curr_thread.IsValid())
				thread = curr_thread;
			else
				thread = process.GetThreadAtIndex(0);

			if (thread.IsValid())
				process.SetSelectedThread (thread);
        }
    }
}

void LogOutput(const char * msg, void *baton)
{
    puts(msg);
}

void Driver::Initialize()
{
    m_debugger = SBDebugger::Create(false, LogOutput, 0);
    // We want to be able to handle CTRL+D in the terminal to have it terminate
    // certain input
    m_debugger.SetCloseInputOnEOF (false);
    InitializePseudoTerminal();
 

    m_debugger.SetErrorFileHandle (stderr, false);
    m_debugger.SetOutputFileHandle (stdout, false);
    m_debugger.SetInputFileHandle (stdin, true);
 
    m_debugger.SetUseExternalEditor(m_option_data.m_use_external_editor);

    InitializeEditLineIO();	

	
    SBCommandInterpreter sb_interpreter = m_debugger.GetCommandInterpreter();
    m_interpreter = &sb_interpreter;

    SBListener listener(m_debugger.GetListener());
    listener.StartListeningForEventClass(m_debugger, 
                                         SBTarget::GetBroadcasterClassName(), 
                                         SBTarget::eBroadcastBitBreakpointChanged);
    if (!listener.IsValid())
        return;

    listener.StartListeningForEvents (*m_io_channel_ap,
                                        IOChannel::eBroadcastBitHasUserInput |
                                        IOChannel::eBroadcastBitUserInterrupt |
                                        IOChannel::eBroadcastBitThreadShouldExit |
                                        IOChannel::eBroadcastBitThreadDidStart |
                                        IOChannel::eBroadcastBitThreadDidExit);

    if (!m_io_channel_ap->Start ())
        return;
    
	iochannel_thread_exited = false;
	
    listener.StartListeningForEvents (sb_interpreter.GetBroadcaster(),
                                        SBCommandInterpreter::eBroadcastBitQuitCommandReceived |
                                        SBCommandInterpreter::eBroadcastBitAsynchronousOutputData |
                                        SBCommandInterpreter::eBroadcastBitAsynchronousErrorData);

    // Before we handle any options from the command line, we parse the
    // .lldbinit file in the user's home directory.
    SBCommandReturnObject result;
    sb_interpreter.SourceInitFileInHomeDirectory(result);

    if (GetDebugMode())
    {
        result.PutError (m_debugger.GetErrorFileHandle());
        result.PutOutput (m_debugger.GetOutputFileHandle());
    }
	HandleCommandLine(result);

    // Now that all option parsing is done, we try and parse the .lldbinit
    // file in the current working directory
    sb_interpreter.SourceInitFileInCurrentWorkingDirectory (result);
    if (GetDebugMode())	
    {
        result.PutError(m_debugger.GetErrorFileHandle());
        result.PutOutput(m_debugger.GetOutputFileHandle());
    }
	SBEvent event;
    // Make sure the IO channel is started up before we try to tell it we
    // are ready for input
    listener.WaitForEventForBroadcasterWithType (UINT32_MAX, 
                                                    *m_io_channel_ap,
                                                    IOChannel::eBroadcastBitThreadDidStart, 
                                                    event);
    
    // If we were asked to attach, then do that here:
    // I'm going to use the command string rather than directly
    // calling the API's because then I don't have to recode the
    // event handling here.
    if (!m_option_data.m_process_name.empty()
        || m_option_data.m_process_pid != LLDB_INVALID_PROCESS_ID)
    {
        AttachToProcess();
    }
    
    ReadyForCommand ();
}

void
Driver::MainLoop ()
{
    SBEvent event;
    while (!GetIsDone())
    {
        m_listener->WaitForEvent (UINT32_MAX, event);
        if (event.IsValid())
		{
            ProcessEvent(event);
        }
    }
	
	
    DestroyPseudoTerminal();
    CloseIOChannelFile ();
 
    if (!iochannel_thread_exited)
    {
	        event.Clear();
        m_listener->GetNextEventForBroadcasterWithType (*m_io_channel_ap,
                                                        IOChannel::eBroadcastBitThreadDidExit,
                                                        event);
        if (!event.IsValid())
        {
            // Send end EOF to the driver file descriptor
            m_io_channel_ap->Stop();
        }
    }
	SBDebugger::Destroy (m_debugger);
}


void
Driver::ReadyForCommand ()
{
    if (m_waiting_for_command == false)
    {
        m_waiting_for_command = true;
        BroadcastEventByType (Driver::eBroadcastBitReadyForInput, true);
    }
}

// defined in DriverPosix.cpp
void SetupPosixSignals();

int
main (int argc, char const *argv[], const char *envp[])
{
#if 1 // Enable for debug logging
    lldb::StreamSP logStream(new lldb_private::StreamCallback(LogOutput, 0));
    const char* logCategories[] = { 0 };
    lldb::LogSP log = lldb_private::EnableLog(logStream, 0, logCategories, 0);
    log->GetMask().Reset(LIBLLDB_LOG_ALL);
#endif
    SBDebugger::Initialize();
    
    SBHostOS::ThreadCreated ("<lldb.driver.main-thread>");

	SetupPosixSignals();
    // Create a scope for driver so that the driver object will destroy itself
    // before SBDebugger::Terminate() is called.
    {
        Driver driver;

        bool exit = false;
        SBError error = driver.ParseArgs (argc, argv, stdout, exit);
        if (error.Fail())
        {
            const char *error_cstr = error.GetCString ();
            if (error_cstr)
                ::fprintf (stderr, "error: %s\n", error_cstr);
        }
        else if (!exit)
        {
            driver.Initialize();
        }
    }

    SBDebugger::Terminate();
    return 0;
}
