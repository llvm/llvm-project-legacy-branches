//===-- Driver.cpp ----------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Driver.h"

#ifdef _POSIX_SOURCE
#include <getopt.h>
#include <libgen.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#endif

#ifdef __unix__
static void reset_stdin_termios ();
static bool g_old_stdin_termios_is_valid = false;
static struct termios g_old_stdin_termios;
#endif

// In the Driver::MainLoop, we change the terminal settings.  This function is
// added as an atexit handler to make sure we clean them up.
static void
reset_stdin_termios ()
{
#ifdef _POSIX_SOURCE
    if (g_old_stdin_termios_is_valid)
    {
        g_old_stdin_termios_is_valid = false;
        ::tcsetattr (STDIN_FILENO, TCSANOW, &g_old_stdin_termios);
    }
#endif
}

void
Driver::InitializePseudoTerminal()
{
#ifndef _WIN32
    char error_str[1024];
    if (m_editline_pty.OpenFirstAvailableMaster(O_RDWR|O_NOCTTY, error_str, sizeof(error_str)) == false)
    {
        ::fprintf (stderr, "error: failed to open driver pseudo terminal : %s", error_str);
        exit(1);
    }
    else
    {
        const char *driver_slave_name = m_editline_pty.GetSlaveName (error_str, sizeof(error_str));
        if (driver_slave_name == NULL)
        {
            ::fprintf (stderr, "error: failed to get slave name for driver pseudo terminal : %s", error_str);
            exit(2);
        }
        else
        {
            m_editline_slave_fh = ::fopen (driver_slave_name, "r+");
            if (m_editline_slave_fh == NULL)
            {
                SBError error;
                error.SetErrorToErrno();
                ::fprintf (stderr, "error: failed to get open slave for driver pseudo terminal : %s",
                           error.GetCString());
                exit(3);
            }

            ::setbuf (m_editline_slave_fh, NULL);
        }
    }

    lldb_utility::PseudoTerminal editline_output_pty;
    FILE *editline_output_slave_fh = NULL;
    
    if (editline_output_pty.OpenFirstAvailableMaster (O_RDWR|O_NOCTTY, error_str, sizeof (error_str)) == false)
    {
        ::fprintf (stderr, "error: failed to open output pseudo terminal : %s", error_str);
        exit(1);
    }
    else
    {
        const char *output_slave_name = editline_output_pty.GetSlaveName (error_str, sizeof(error_str));
        if (output_slave_name == NULL)
        {
            ::fprintf (stderr, "error: failed to get slave name for output pseudo terminal : %s", error_str);
            exit(2);
        }
        else
        {
            editline_output_slave_fh = ::fopen (output_slave_name, "r+");
            if (editline_output_slave_fh == NULL)
            {
                SBError error;
                error.SetErrorToErrno();
                ::fprintf (stderr, "error: failed to get open slave for output pseudo terminal : %s",
                           error.GetCString());
                exit(3);
            }
            ::setbuf (editline_output_slave_fh, NULL);
        }
    }
  
     struct termios stdin_termios;

    if (::tcgetattr(STDIN_FILENO, &g_old_stdin_termios) == 0)
    {
        g_old_stdin_termios_is_valid = true;
        atexit (reset_stdin_termios);
    }

    ::setbuf (stdin, NULL);
    ::setbuf (stdout, NULL);
#endif
}

void
Driver::InitializeEditLineIO()
{
#ifndef _WIN32
    // You have to drain anything that comes to the master side of the PTY.  master_out_comm is
    // for that purpose.  The reason you need to do this is a curious reason...  editline will echo
    // characters to the PTY when it gets characters while el_gets is not running, and then when
    // you call el_gets (or el_getc) it will try to reset the terminal back to raw mode which blocks
    // if there are unconsumed characters in the out buffer.
    // However, you don't need to do anything with the characters, since editline will dump these
    // unconsumed characters after printing the prompt again in el_gets.

    SBCommunication master_out_comm("driver.editline");
    master_out_comm.SetCloseOnEOF (false);
    master_out_comm.AdoptFileDesriptor(m_editline_pty.GetMasterFileDescriptor(), false);
    master_out_comm.SetReadThreadBytesReceivedCallback(Driver::MasterThreadBytesReceived, this);

    if (master_out_comm.ReadThreadStart () == false)
    {
        ::fprintf (stderr, "error: failed to start master out read thread");
        exit(5);
    }

    m_io_channel_ap.reset (new IOChannel(m_editline_slave_fh, editline_output_slave_fh, stdout, stderr, this));

    SBCommunication out_comm_2("driver.editline_output");
    out_comm_2.SetCloseOnEOF (false);
    out_comm_2.AdoptFileDesriptor (editline_output_pty.GetMasterFileDescriptor(), false);
    out_comm_2.SetReadThreadBytesReceivedCallback (IOChannel::LibeditOutputBytesReceived, m_io_channel_ap.get());

    if (out_comm_2.ReadThreadStart () == false)
    {
        ::fprintf (stderr, "error: failed to start libedit output read thread");
        exit (5);
    }

    struct winsize window_size;
    if (isatty (STDIN_FILENO)
        && ::ioctl (STDIN_FILENO, TIOCGWINSZ, &window_size) == 0)
    {
        if (window_size.ws_col > 0)
            m_debugger.SetTerminalWidth (window_size.ws_col);
    }

    // Since input can be redirected by the debugger, we must insert our editline
    // input reader in the queue so we know when our reader should be active
    // and so we can receive bytes only when we are supposed to.
    SBError err (m_editline_reader.Initialize (m_debugger, 
                                               Driver::EditLineInputReaderCallback, // callback
                                               this,                              // baton
                                               eInputReaderGranularityByte,       // token_size
                                               NULL,                              // end token - NULL means never done
                                               NULL,                              // prompt - taken care of elsewhere
                                               false));                           // echo input - don't need Debugger 
                                                                                  // to do this, we handle it elsewhere
    
    if (err.Fail())
    {
        ::fprintf (stderr, "error: %s", err.GetCString());
        exit (6);
    }
    
    m_debugger.PushInputReader (m_editline_reader);
#endif
}

void
Driver::DestroyPseudoTerminal()
{
#ifndef _WIN32
    editline_output_pty.CloseMasterFileDescriptor();
    master_out_comm.Disconnect();
    out_comm_2.Disconnect();
    reset_stdin_termios();
    fclose (stdin);
#endif
}

void
Driver::CloseIOChannelFile ()
{
#ifdef __unix__
    // Write an End of File sequence to the file descriptor to ensure any
    // read functions can exit.
    char eof_str[] = "\x04";
    ::write (m_editline_pty.GetMasterFileDescriptor(), eof_str, strlen(eof_str));

    m_editline_pty.CloseMasterFileDescriptor();

    if (m_editline_slave_fh)
    {
        ::fclose (m_editline_slave_fh);
        m_editline_slave_fh = NULL;
    }
#endif
}

#ifdef __unix__
void
sigwinch_handler (int signo)
{
    struct winsize window_size;
    if (isatty (STDIN_FILENO)
        && ::ioctl (STDIN_FILENO, TIOCGWINSZ, &window_size) == 0)
    {
        if ((window_size.ws_col > 0) && g_driver != NULL)
        {
            g_driver->GetDebugger().SetTerminalWidth (window_size.ws_col);
        }
    }
}

void
sigint_handler (int signo)
{
    static bool g_interrupt_sent = false;
    if (g_driver)
    {
        if (!g_interrupt_sent)
        {
            g_interrupt_sent = true;
            g_driver->GetDebugger().DispatchInputInterrupt();
            g_interrupt_sent = false;
            return;
        }
    }
    
    exit (signo);
}
#endif

void
SetupPosixSignals()
{
#ifdef __unix__
    signal (SIGPIPE, SIG_IGN);
    signal (SIGWINCH, sigwinch_handler);
    signal (SIGINT, sigint_handler);
#endif
}
