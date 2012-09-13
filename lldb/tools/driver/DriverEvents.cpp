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

// This function handles events that were broadcast by the process.
void
Driver::HandleBreakpointEvent (const SBEvent &event)
{
    const uint32_t event_type = SBBreakpoint::GetBreakpointEventTypeFromEvent (event);
    
    if (event_type & eBreakpointEventTypeAdded
        || event_type & eBreakpointEventTypeRemoved
        || event_type & eBreakpointEventTypeEnabled
        || event_type & eBreakpointEventTypeDisabled
        || event_type & eBreakpointEventTypeCommandChanged
        || event_type & eBreakpointEventTypeConditionChanged
        || event_type & eBreakpointEventTypeIgnoreChanged
        || event_type & eBreakpointEventTypeLocationsResolved)
        return;
        // Don't do anything about these events, since the breakpoint commands already echo these actions.
    
    if (event_type & eBreakpointEventTypeLocationsAdded)
    {
        uint32_t num_new_locations = SBBreakpoint::GetNumBreakpointLocationsFromEvent(event);
        if (num_new_locations > 0)
        {
            SBBreakpoint breakpoint = SBBreakpoint::GetBreakpointFromEvent(event);

            char message[256];
            int message_len = ::snprintf (message, sizeof(message), "%d location%s added to breakpoint %d\n", 
                                          num_new_locations,
                                          num_new_locations == 1 ? " " : "s ",
                                          breakpoint.GetID());
            m_io_channel_ap->OutWrite(message, message_len, ASYNC);
        }
    }
    else if (event_type & eBreakpointEventTypeLocationsRemoved)
    {
       // These locations just get disabled, not sure it is worth spamming folks about this on the command line.
    }
    else if (event_type & eBreakpointEventTypeLocationsResolved)
    {
       // This might be an interesting thing to note, but I'm going to leave it quiet for now, it just looked noisy.
    }
}

// This function handles events that were broadcast by the process.
void
Driver::HandleProcessEvent (const SBEvent &event)
{
    using namespace lldb;
    const uint32_t event_type = event.GetType();

    if (event_type & SBProcess::eBroadcastBitSTDOUT)
    {
        // The process has stdout available, get it and write it out to the
        // appropriate place.
        GetProcessSTDOUT ();
    }
    else if (event_type & SBProcess::eBroadcastBitSTDERR)
    {
        // The process has stderr available, get it and write it out to the
        // appropriate place.
        GetProcessSTDERR ();
    }
    else if (event_type & SBProcess::eBroadcastBitStateChanged)
    {
        // Drain all stout and stderr so we don't see any output come after
        // we print our prompts
        GetProcessSTDOUT ();
        GetProcessSTDERR ();
        // Something changed in the process;  get the event and report the process's current status and location to
        // the user.
        StateType event_state = SBProcess::GetStateFromEvent (event);
        if (event_state == eStateInvalid)
            return;

        SBProcess process (SBProcess::GetProcessFromEvent (event));
        assert (process.IsValid());

        switch (event_state)
        {
        case eStateInvalid:
        case eStateUnloaded:
        case eStateConnected:
        case eStateAttaching:
        case eStateLaunching:
        case eStateStepping:
        case eStateDetached:
            {
                char message[1024];
                int message_len = ::snprintf (message, sizeof(message), "Process %llu %s\n", process.GetProcessID(),
                                              m_debugger.StateAsCString (event_state));
                m_io_channel_ap->OutWrite(message, message_len, ASYNC);
            }
            break;

        case eStateRunning:
            // Don't be chatty when we run...
            break;

        case eStateExited:
            {
                SBCommandReturnObject result;
                m_debugger.GetCommandInterpreter().HandleCommand("process status", result, false);
                m_io_channel_ap->ErrWrite (result.GetError(), result.GetErrorSize(), ASYNC);
                m_io_channel_ap->OutWrite (result.GetOutput(), result.GetOutputSize(), ASYNC);
            }
            break;

        case eStateStopped:
        case eStateCrashed:
        case eStateSuspended:
            // Make sure the program hasn't been auto-restarted:
            if (SBProcess::GetRestartedFromEvent (event))
            {
                // FIXME: Do we want to report this, or would that just be annoyingly chatty?
                char message[1024];
                int message_len = ::snprintf (message, sizeof(message), "Process %llu stopped and was programmatically restarted.\n",
                                              process.GetProcessID());
                m_io_channel_ap->OutWrite(message, message_len, ASYNC);
            }
            else
            {
                if (GetDebugger().GetSelectedTarget() == process.GetTarget())
                {
                    SBCommandReturnObject result;
                    UpdateSelectedThread ();
                    m_debugger.GetCommandInterpreter().HandleCommand("process status", result, false);
                    m_io_channel_ap->ErrWrite (result.GetError(), result.GetErrorSize(), ASYNC);
                    m_io_channel_ap->OutWrite (result.GetOutput(), result.GetOutputSize(), ASYNC);
                }
                else
                {
                    SBStream out_stream;
                    uint32_t target_idx = GetDebugger().GetIndexOfTarget(process.GetTarget());
                    if (target_idx != UINT32_MAX)
                        out_stream.Printf ("Target %d: (", target_idx);
                    else
                        out_stream.Printf ("Target <unknown index>: (");
                    process.GetTarget().GetDescription (out_stream, eDescriptionLevelBrief);
                    out_stream.Printf (") stopped.\n");
                    m_io_channel_ap->OutWrite (out_stream.GetData(), out_stream.GetSize(), ASYNC);
                }
            }
            break;
        }
    }
}

//  This function handles events broadcast by the IOChannel (HasInput, UserInterrupt, or ThreadShouldExit).

bool
Driver::HandleIOEvent (const SBEvent &event)
{
    bool quit = false;

    const uint32_t event_type = event.GetType();

    if (event_type & IOChannel::eBroadcastBitHasUserInput)
    {
        // We got some input (i.e. a command string) from the user; pass it off to the command interpreter for
        // handling.

        const char *command_string = SBEvent::GetCStringFromEvent(event);
        if (command_string == NULL)
            command_string = "";
        SBCommandReturnObject result;
        
        // We don't want the result to bypass the OutWrite function in IOChannel, as this can result in odd
        // output orderings and problems with the prompt.
        m_debugger.GetCommandInterpreter().HandleCommand (command_string, result, true);

        if (result.GetOutputSize() > 0)
            m_io_channel_ap->OutWrite (result.GetOutput(), result.GetOutputSize(), NO_ASYNC);
            
        if (result.GetErrorSize() > 0)
            m_io_channel_ap->OutWrite (result.GetError(), result.GetErrorSize(), NO_ASYNC);

        // We are done getting and running our command, we can now clear the
        // m_waiting_for_command so we can get another one.
        m_waiting_for_command = false;

        // If our editline input reader is active, it means another input reader
        // got pushed onto the input reader and caused us to become deactivated.
        // When the input reader above us gets popped, we will get re-activated
        // and our prompt will refresh in our callback
        if (m_editline_reader.IsActive())
        {
            ReadyForCommand ();
        }
    }
    else if (event_type & IOChannel::eBroadcastBitUserInterrupt)
    {
        // This is here to handle control-c interrupts from the user.  It has not yet really been implemented.
        // TO BE DONE:  PROPERLY HANDLE CONTROL-C FROM USER
        //m_io_channel_ap->CancelInput();
        // Anything else?  Send Interrupt to process?
    }
    else if ((event_type & IOChannel::eBroadcastBitThreadShouldExit) ||
             (event_type & IOChannel::eBroadcastBitThreadDidExit))
    {
        // If the IOChannel thread is trying to go away, then it is definitely
        // time to end the debugging session.
        quit = true;
    }

    return quit;
}

void
Driver::MasterThreadBytesReceived (void *baton, const void *src, size_t src_len)
{
    Driver *driver = (Driver*)baton;
    driver->GetFromMaster ((const char *)src, src_len);
}

void
Driver::GetFromMaster (const char *src, size_t src_len)
{
    // Echo the characters back to the Debugger's stdout, that way if you
    // type characters while a command is running, you'll see what you've typed.
    FILE *out_fh = m_debugger.GetOutputFileHandle();
    if (out_fh)
        ::fwrite (src, 1, src_len, out_fh);
}

size_t
Driver::EditLineInputReaderCallback 
(
    void *baton, 
    SBInputReader *reader, 
    InputReaderAction notification,
    const char *bytes, 
    size_t bytes_len
)
{
    Driver *driver = (Driver *)baton;

    switch (notification)
    {
    case eInputReaderActivate:
        break;

    case eInputReaderReactivate:
        driver->ReadyForCommand();
        break;

    case eInputReaderDeactivate:
        break;
        
    case eInputReaderAsynchronousOutputWritten:
        if (driver->m_io_channel_ap.get() != NULL)
            driver->m_io_channel_ap->RefreshPrompt();
        break;

    case eInputReaderInterrupt:
        if (driver->m_io_channel_ap.get() != NULL)
        {
            SBProcess process = driver->GetDebugger().GetSelectedTarget().GetProcess();
            if (!driver->m_io_channel_ap->EditLineHasCharacters()
                &&  process.IsValid() && process.GetState() == lldb::eStateRunning)
            {
                process.Stop();
            }
            else
            {
                driver->m_io_channel_ap->OutWrite ("^C\n", 3, NO_ASYNC);
                // I wish I could erase the entire input line, but there's no public API for that.
                driver->m_io_channel_ap->EraseCharsBeforeCursor();
                driver->m_io_channel_ap->RefreshPrompt();
            }
        }
        break;
        
    case eInputReaderEndOfFile:
        if (driver->m_io_channel_ap.get() != NULL)
        {
            driver->m_io_channel_ap->OutWrite ("^D\n", 3, NO_ASYNC);
            driver->m_io_channel_ap->RefreshPrompt ();
        }
#ifdef __unix__
        write (driver->m_editline_pty.GetMasterFileDescriptor(), "quit\n", 5);
#endif
        break;

    case eInputReaderGotToken:
#ifdef __unix__
        write (driver->m_editline_pty.GetMasterFileDescriptor(), bytes, bytes_len);
#endif
        break;
        
    case eInputReaderDone:
        break;
    }
    return bytes_len;
}

void
Driver::AttachToProcess()
{
    std::string command_str("process attach ");
    if (m_option_data.m_process_pid != LLDB_INVALID_PROCESS_ID)
    {
        command_str.append("-p ");
        char pid_buffer[32];
        ::snprintf (pid_buffer, sizeof(pid_buffer), "%llu", m_option_data.m_process_pid);
        command_str.append(pid_buffer);
    }
    else 
    {
        command_str.append("-n \"");
        command_str.append(m_option_data.m_process_name);
        command_str.push_back('\"');
        if (m_option_data.m_wait_for)
            command_str.append(" -w");
    }
                
    if (m_debugger.GetOutputFileHandle())
        ::fprintf (m_debugger.GetOutputFileHandle(), 
                    "Attaching to process with:\n    %s\n", 
                    command_str.c_str());
                                               
    // Force the attach to be synchronous:
    bool orig_async = m_debugger.GetAsync();
    m_debugger.SetAsync(true);
    m_debugger.HandleCommand(command_str.c_str());
    m_debugger.SetAsync(orig_async);
}

void
Driver::ProcessEvent(SBEvent& event)
{
    if (!event.GetBroadcaster().IsValid())
        return;

    uint32_t event_type = event.GetType();
    if (event.BroadcasterMatchesRef (*m_io_channel_ap))
    {
        if ((event_type & IOChannel::eBroadcastBitThreadShouldExit) ||
            (event_type & IOChannel::eBroadcastBitThreadDidExit))
        {
            SetIsDone();
            if (event_type & IOChannel::eBroadcastBitThreadDidExit)
                iochannel_thread_exited = true;
        }
        else
        {
            if (HandleIOEvent (event))
                SetIsDone();
        }
    }
    else if (SBProcess::EventIsProcessEvent (event))
    {
        HandleProcessEvent (event);
    }
    else if (SBBreakpoint::EventIsBreakpointEvent (event))
    {
        HandleBreakpointEvent (event);
    }
    else if (event.BroadcasterMatchesRef (m_interpreter->GetBroadcaster()))
    {
        // TODO: deprecate the eBroadcastBitQuitCommandReceived event
        // now that we have SBCommandInterpreter::SetCommandOverrideCallback()
        // that can take over a command
        if (event_type & SBCommandInterpreter::eBroadcastBitQuitCommandReceived)
        {
            SetIsDone();
        }
        else if (event_type & SBCommandInterpreter::eBroadcastBitAsynchronousErrorData)
        {
            const char *data = SBEvent::GetCStringFromEvent (event);
            m_io_channel_ap->ErrWrite (data, strlen(data), ASYNC);
        }
        else if (event_type & SBCommandInterpreter::eBroadcastBitAsynchronousOutputData)
        {
            const char *data = SBEvent::GetCStringFromEvent (event);
            m_io_channel_ap->OutWrite (data, strlen(data), ASYNC);
        }
    }
}
