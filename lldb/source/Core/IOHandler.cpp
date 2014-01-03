//===-- IOHandler.cpp -------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//


#include "lldb/lldb-python.h"

#include <stdio.h>	/* ioctl, TIOCGWINSZ */
#include <sys/ioctl.h>	/* ioctl, TIOCGWINSZ */


#include <string>

#include "lldb/Breakpoint/BreakpointLocation.h"
#include "lldb/Core/IOHandler.h"
#include "lldb/Core/Debugger.h"
#include "lldb/Core/State.h"
#include "lldb/Core/StreamFile.h"
#include "lldb/Host/Editline.h"
#include "lldb/Interpreter/CommandCompletions.h"
#include "lldb/Interpreter/CommandInterpreter.h"
#include "lldb/Symbol/Block.h"
#include "lldb/Symbol/Function.h"
#include "lldb/Target/ThreadPlan.h"


#include <ncurses.h>

using namespace lldb;
using namespace lldb_private;

IOHandler::IOHandler (Debugger &debugger) :
    IOHandler (debugger,
               StreamFileSP(), // Adopt STDIN from top input reader
               StreamFileSP(), // Adopt STDOUT from top input reader
               StreamFileSP()) // Adopt STDERR from top input reader
{
}


IOHandler::IOHandler (Debugger &debugger,
                      const lldb::StreamFileSP &input_sp,
                      const lldb::StreamFileSP &output_sp,
                      const lldb::StreamFileSP &error_sp) :
    m_debugger (debugger),
    m_input_sp (input_sp),
    m_output_sp (output_sp),
    m_error_sp (error_sp),
    m_user_data (NULL),
    m_done (false),
    m_active (false)
{
    // If any files are not specified, then adopt them from the
    // top input reader.
    if (!m_input_sp || !m_output_sp || !m_error_sp)
        debugger.GetIOHandlerFiles (*this);
}

IOHandler::~IOHandler()
{
}


int
IOHandler::GetInputFD()
{
    if (m_input_sp)
        return m_input_sp->GetFile().GetDescriptor();
    return -1;
}

int
IOHandler::GetOutputFD()
{
    if (m_output_sp)
        return m_output_sp->GetFile().GetDescriptor();
    return -1;
}

int
IOHandler::GetErrorFD()
{
    if (m_error_sp)
        return m_error_sp->GetFile().GetDescriptor();
    return -1;
}

FILE *
IOHandler::GetInputFILE()
{
    if (m_input_sp)
        return m_input_sp->GetFile().GetStream();
    return NULL;
}

FILE *
IOHandler::GetOutputFILE()
{
    if (m_output_sp)
        return m_output_sp->GetFile().GetStream();
    return NULL;
}

FILE *
IOHandler::GetErrorFILE()
{
    if (m_error_sp)
        return m_error_sp->GetFile().GetStream();
    return NULL;
}

StreamFileSP &
IOHandler::GetInputStreamFile()
{
    return m_input_sp;
}

StreamFileSP &
IOHandler::GetOutputStreamFile()
{
    return m_output_sp;
}


StreamFileSP &
IOHandler::GetErrorStreamFile()
{
    return m_error_sp;
}


IOHandlerConfirm::IOHandlerConfirm (Debugger &debugger,
                                    const char *prompt,
                                    bool default_response) :
    IOHandlerEditline(debugger,
                      NULL,     // NULL editline_name means no history loaded/saved
                      NULL,
                      false,    // Multi-line
                      *this),
    m_default_response (default_response),
    m_user_response (default_response)
{
    StreamString prompt_stream;
    prompt_stream.PutCString(prompt);
    if (m_default_response)
        prompt_stream.Printf(": [Y/n] ");
    else
        prompt_stream.Printf(": [y/N] ");
    
    SetPrompt (prompt_stream.GetString().c_str());
    
}


IOHandlerConfirm::~IOHandlerConfirm ()
{
}

int
IOHandlerConfirm::IOHandlerComplete (IOHandler &io_handler,
                                     const char *current_line,
                                     const char *cursor,
                                     const char *last_char,
                                     int skip_first_n_matches,
                                     int max_matches,
                                     StringList &matches)
{
    if (current_line == cursor)
    {
        if (m_default_response)
        {
            matches.AppendString("y");
        }
        else
        {
            matches.AppendString("n");
        }
    }
    return matches.GetSize();
}

void
IOHandlerConfirm::IOHandlerInputComplete (IOHandler &io_handler, std::string &line)
{
    if (line == "y" || line == "Y" || line == "yes" || line == "YES" || line == "Yes")
        io_handler.SetIsDone(true);
    if (line == "n" || line == "N" || line == "no" || line == "NO" || line == "No")
        io_handler.SetIsDone(true);
}

int
IOHandlerDelegate::IOHandlerComplete (IOHandler &io_handler,
                                      const char *current_line,
                                      const char *cursor,
                                      const char *last_char,
                                      int skip_first_n_matches,
                                      int max_matches,
                                      StringList &matches)
{
    switch (m_completion)
    {
    case Completion::None:
        break;

    case Completion::LLDBCommand:
        return io_handler.GetDebugger().GetCommandInterpreter().HandleCompletion (current_line,
                                                                                  cursor,
                                                                                  last_char,
                                                                                  skip_first_n_matches,
                                                                                  max_matches,
                                                                                  matches);

    case Completion::Expression:
        {
            bool word_complete = false;
            const char *word_start = cursor;
            if (cursor > current_line)
                --word_start;
            while (word_start > current_line && !isspace(*word_start))
                --word_start;
            CommandCompletions::InvokeCommonCompletionCallbacks (io_handler.GetDebugger().GetCommandInterpreter(),
                                                                 CommandCompletions::eVariablePathCompletion,
                                                                 word_start,
                                                                 skip_first_n_matches,
                                                                 max_matches,
                                                                 NULL,
                                                                 word_complete,
                                                                 matches);
            
            size_t num_matches = matches.GetSize();
            if (num_matches > 0)
            {
                std::string common_prefix;
                matches.LongestCommonPrefix (common_prefix);
                const size_t partial_name_len = strlen(word_start);
                
                // If we matched a unique single command, add a space...
                // Only do this if the completer told us this was a complete word, however...
                if (num_matches == 1 && word_complete)
                {
                    common_prefix.push_back(' ');
                }
                common_prefix.erase (0, partial_name_len);
                matches.InsertStringAtIndex(0, std::move(common_prefix));
            }
            return num_matches;
        }
        break;
    }
    
    
    return 0;
}


IOHandlerEditline::IOHandlerEditline (Debugger &debugger,
                                      const char *editline_name, // Used for saving history files
                                      const char *prompt,
                                      bool multi_line,
                                      IOHandlerDelegate &delegate) :
    IOHandlerEditline(debugger,
                      StreamFileSP(), // Inherit input from top input reader
                      StreamFileSP(), // Inherit output from top input reader
                      StreamFileSP(), // Inherit error from top input reader
                      editline_name,  // Used for saving history files
                      prompt,
                      multi_line,
                      delegate)
{
}

IOHandlerEditline::IOHandlerEditline (Debugger &debugger,
                                      const lldb::StreamFileSP &input_sp,
                                      const lldb::StreamFileSP &output_sp,
                                      const lldb::StreamFileSP &error_sp,
                                      const char *editline_name, // Used for saving history files
                                      const char *prompt,
                                      bool multi_line,
                                      IOHandlerDelegate &delegate) :
    IOHandler (debugger, input_sp, output_sp, error_sp),
    m_editline_ap (),
    m_delegate (delegate),
    m_multi_line (multi_line)
{
    m_editline_ap.reset(new Editline (editline_name,
                                      prompt ? prompt : "",
                                      GetInputFILE (),
                                      GetOutputFILE (),
                                      GetErrorFILE ()));
    m_editline_ap->SetLineCompleteCallback (LineCompletedCallback, this);
    m_editline_ap->SetAutoCompleteCallback (AutoCompleteCallback, this);
}

IOHandlerEditline::~IOHandlerEditline ()
{
    m_editline_ap.reset();
}


bool
IOHandlerEditline::GetLine (std::string &line)
{
    if (m_editline_ap)
    {
        return m_editline_ap->GetLine(line).Success();
    }
    else
    {
        FILE *in = GetInputFILE();
        char buffer[256];
        bool done = false;
        while (!done)
        {
            if (fgets(buffer, sizeof(buffer), in) == NULL)
                done = true;
            else
            {
                const size_t buffer_len = strlen(buffer);
                line.append(buffer, buffer_len);
                assert (buffer[buffer_len] == '\0');
                const char last_char = buffer[buffer_len-1];
                if (last_char == '\r' || last_char == '\n')
                    done = true;
            }
        }
        return !line.empty();
    }
}


LineStatus
IOHandlerEditline::LineCompletedCallback (Editline *editline,
                                          StringList &lines,
                                          uint32_t line_idx,
                                          Error &error,
                                          void *baton)
{
    IOHandlerEditline *editline_reader = (IOHandlerEditline *) baton;
    return editline_reader->m_delegate.IOHandlerLinesUpdated(*editline_reader, lines, line_idx, error);
}

int
IOHandlerEditline::AutoCompleteCallback (const char *current_line,
                                         const char *cursor,
                                         const char *last_char,
                                         int skip_first_n_matches,
                                         int max_matches,
                                         StringList &matches,
                                         void *baton)
{
    IOHandlerEditline *editline_reader = (IOHandlerEditline *) baton;
    if (editline_reader)
        return editline_reader->m_delegate.IOHandlerComplete (*editline_reader,
                                                              current_line,
                                                              cursor,
                                                              last_char,
                                                              skip_first_n_matches,
                                                              max_matches,
                                                              matches);
    return 0;
}

const char *
IOHandlerEditline::GetPrompt ()
{
    if (m_editline_ap)
        return m_editline_ap->GetPrompt ();
    return false;
}

bool
IOHandlerEditline::SetPrompt (const char *p)
{
    if (m_editline_ap)
    {
        m_editline_ap->SetPrompt (p);
        return true;
    }
    return false;
}

bool
IOHandlerEditline::GetLines (StringList &lines)
{
    bool success = false;
    if (m_editline_ap)
    {
        std::string end_token;
        success = m_editline_ap->GetLines(end_token, lines).Success();
    }
    else
    {
        LineStatus lines_status = LineStatus::Success;

        while (lines_status == LineStatus::Success)
        {
            std::string line;
            if (GetLine(line))
            {
                lines.AppendString(line);
                Error error;
                lines_status = m_delegate.IOHandlerLinesUpdated(*this, lines, lines.GetSize() - 1, error);
            }
            else
            {
                lines_status = LineStatus::Done;
            }
        }
        success = lines.GetSize() > 0;
    }
    return success;
}

// Each IOHandler gets to run until it is done. It should read data
// from the "in" and place output into "out" and "err and return
// when done.
void
IOHandlerEditline::Run ()
{
    std::string line;
    while (IsActive())
    {
        if (m_multi_line)
        {
            StringList lines;
            if (GetLines (lines))
            {
                line = lines.CopyList();
                m_delegate.IOHandlerInputComplete(*this, line);
            }
            else
            {
                m_done = true;
            }
        }
        else
        {
            if (GetLine(line))
            {
                m_delegate.IOHandlerInputComplete(*this, line);
            }
            else
            {
                m_done = true;
            }
        }
    }
}

void
IOHandlerEditline::Hide ()
{
    if (m_editline_ap && m_editline_ap->GettingLine())
        m_editline_ap->Hide();
}


void
IOHandlerEditline::Refresh ()
{
    if (m_editline_ap && m_editline_ap->GettingLine())
        m_editline_ap->Refresh();
}

void
IOHandlerEditline::Interrupt ()
{
    if (m_editline_ap)
        m_editline_ap->Interrupt();
}

void
IOHandlerEditline::GotEOF()
{
    if (m_editline_ap)
        m_editline_ap->Interrupt();
}

#include "lldb/Core/ValueObject.h"
#include "lldb/Symbol/VariableList.h"
#include "lldb/Target/Target.h"
#include "lldb/Target/Process.h"
#include "lldb/Target/Thread.h"
#include "lldb/Target/StackFrame.h"

#define KEY_RETURN   10
#define KEY_ESCAPE  27

namespace curses
{
    class Menu;
    class MenuDelegate;
    class Window;
    class WindowDelegate;
    typedef std::shared_ptr<Menu> MenuSP;
    typedef std::shared_ptr<MenuDelegate> MenuDelegateSP;
    typedef std::shared_ptr<Window> WindowSP;
    typedef std::shared_ptr<WindowDelegate> WindowDelegateSP;
    typedef std::vector<MenuSP> Menus;
    typedef std::vector<WindowSP> Windows;
    typedef std::vector<WindowDelegateSP> WindowDelegates;

    enum HandleCharResult
    {
        eKeyNotHandled      = 0,
        eKeyHandled         = 1,
        eQuitApplication    = 2
    };
    
    enum class MenuActionResult
    {
        Success,
        Error,
        Quit    // Exit all menus and quit
    };


    class WindowDelegate
    {
    public:
        virtual
        ~WindowDelegate()
        {
        }
        
        virtual bool
        WindowDelegateDraw (Window &window, bool force)
        {
            return false; // Drawing not handled
        }
        
        virtual HandleCharResult
        WindowDelegateHandleChar (Window &window, int key)
        {
            return eKeyNotHandled;
        }
    };
    

    class Window
    {
    public:

        Window (const char *name) :
            m_name (name),
            m_window (NULL),
            m_parent (NULL),
            m_subwindows (),
            m_delegate_sp (),
            m_curr_active_window_idx (UINT32_MAX),
            m_prev_active_window_idx (UINT32_MAX),
            m_delete (false),
            m_needs_update (true),
            m_can_activate (true)
        {
        }
        
        Window (const char *name, WINDOW *w, bool del = true) :
            m_name (name),
            m_window (w),
            m_parent (NULL),
            m_subwindows (),
            m_delegate_sp (),
            m_curr_active_window_idx (UINT32_MAX),
            m_prev_active_window_idx (UINT32_MAX),
            m_delete (del),
            m_needs_update (true),
            m_can_activate (true)
        {
        }
        
        Window (const char *name, int nlines, int ncols, int begin_y, int begin_x) :
            m_name (name),
            m_window (::newwin (nlines, ncols, begin_y, begin_x)),
            m_parent (NULL),
            m_subwindows (),
            m_delegate_sp (),
            m_curr_active_window_idx (UINT32_MAX),
            m_prev_active_window_idx (UINT32_MAX),
            m_delete (true),
            m_needs_update (true),
            m_can_activate (true)
        {
        }
        
        virtual
        ~Window ()
        {
            RemoveSubWindows ();
            Reset ();
        }
        
        void
        Reset (WINDOW *w = NULL, bool del = true)
        {
            if (m_window == w)
                return;
            
            if (m_window && m_delete)
            {
                ::delwin (m_window);
                m_window = NULL;
                m_delete = false;
            }
            if (w)
            {
                m_window = w;
                m_delete = del;
            }
        }
        
        void    AttributeOn (attr_t attr)   { ::wattron (m_window, attr); }
        void    AttributeOff (attr_t attr)  { ::wattroff (m_window, attr); }
        void    Box (chtype v_char = ACS_VLINE, chtype h_char = ACS_HLINE) { ::box(m_window, v_char, h_char); }
        void    Clear ()    { ::wclear (m_window); }
        void    Erase ()    { ::werase (m_window); }
        int     GetChar ()  { return ::wgetch (m_window); }
        int     GetX ()     { return ::getcurx (m_window); }
        int     GetY ()     { return ::getcury (m_window); }
        int     GetMinX()   { return ::getbegx (m_window); }
        int     GetMinY()   { return ::getbegy (m_window); }
        int     GetMaxX()   { return ::getmaxx (m_window); }
        int     GetMaxY()   { return ::getmaxy (m_window); }
        int     GetWidth()  { return GetMaxX(); }
        int     GetHeight() { return GetMaxY(); }
        void    Move (int x, int y) {  ::wmove (m_window, y, x); }
        void    PutChar (int ch)    { ::waddch (m_window, ch); }
        void    PutCString (const char *s, int len = -1) { ::waddnstr (m_window, s, len); }
        void    Refresh ()  { ::wrefresh (m_window); }
        void    DeferredRefresh ()  { ::wnoutrefresh(m_window); }
        void    SetBackground (int color_pair_idx) { ::wbkgd (m_window,COLOR_PAIR(color_pair_idx)); }
        void    UnderlineOn ()  { AttributeOn(A_UNDERLINE); }
        void    UnderlineOff () { AttributeOff(A_UNDERLINE); }

        void
        Printf (const char *format, ...)  __attribute__ ((format (printf, 2, 3)))
        {
            va_list args;
            va_start (args, format);
            vwprintw(m_window, format, args);
            va_end (args);
        }

        void
        Touch ()
        {
            ::touchwin (m_window);
            if (m_parent)
                m_parent->Touch();
        }

        void
        AddSubWindow (const WindowSP &subwindow_sp, bool make_active)
        {
            subwindow_sp->m_parent = this;
            if (make_active)
            {
                m_prev_active_window_idx = m_curr_active_window_idx;
                m_curr_active_window_idx = m_subwindows.size();
            }
            m_subwindows.push_back(subwindow_sp);
            m_needs_update = true;
        }

        WindowSP
        CreateSubWindow (const char *name, int h, int w, int y, int x, bool make_active)
        {
            WindowSP subwindow_sp;
            if (m_window)
                subwindow_sp.reset(new Window(name, ::subwin (m_window, h, w, y, x), true));
            else
                subwindow_sp.reset(new Window(name, ::newwin (h, w, y, x), true));
            subwindow_sp->m_parent = this;
            if (make_active)
            {
                m_prev_active_window_idx = m_curr_active_window_idx;
                m_curr_active_window_idx = m_subwindows.size();
            }
            m_subwindows.push_back(subwindow_sp);
            m_needs_update = true;
            return subwindow_sp;
        }
        
        bool
        RemoveSubWindow (Window *window)
        {
            Windows::iterator pos, end = m_subwindows.end();
            size_t i = 0;
            for (pos = m_subwindows.begin(); pos != end; ++pos, ++i)
            {
                if ((*pos).get() == window)
                {
                    if (m_prev_active_window_idx == i)
                        m_prev_active_window_idx = UINT32_MAX;
                    else if (m_prev_active_window_idx != UINT32_MAX && m_prev_active_window_idx > i)
                        --m_prev_active_window_idx;

                    if (m_curr_active_window_idx == i)
                        m_curr_active_window_idx = UINT32_MAX;
                    else if (m_curr_active_window_idx != UINT32_MAX && m_curr_active_window_idx > i)
                        --m_curr_active_window_idx;
                    window->Erase();
                    m_subwindows.erase(pos);
                    m_needs_update = true;
                    if (m_parent)
                        m_parent->Touch();
                    else
                        ::touchwin (stdscr);
                    return true;
                }
            }
            return false;
        }
        
        void
        RemoveSubWindows ()
        {
            m_curr_active_window_idx = UINT32_MAX;
            m_prev_active_window_idx = UINT32_MAX;
            for (Windows::iterator pos = m_subwindows.begin();
                 pos != m_subwindows.end();
                 pos = m_subwindows.erase(pos))
            {
                (*pos)->Erase();
            }
            if (m_parent)
                m_parent->Touch();
            else
                ::touchwin (stdscr);
        }

        WINDOW *
        get()
        {
            return m_window;
        }

        operator WINDOW *()
        {
            return m_window;
        }
        
        //----------------------------------------------------------------------
        // Window drawing utilities
        //----------------------------------------------------------------------
        void
        DrawTitleBox (const char *title)
        {
            attr_t attr = 0;
            if (IsActive())
                attr = A_BOLD | COLOR_PAIR(2);
            else
                attr = 0;
            if (attr)
                AttributeOn(attr);

            Box();
            Move (3, 0);
            
            if (title && title[0])
            {
                PutCString ("<");
                PutCString(title);
                PutCString (">");
            }
            if (attr)
                AttributeOff(attr);
        }

        virtual void
        Draw (bool force)
        {
            if (m_delegate_sp && m_delegate_sp->WindowDelegateDraw (*this, force))
                return;

            //Erase();
            for (auto &subwindow_sp : m_subwindows)
            {
                subwindow_sp->Draw(force);
            }
            //DeferredRefresh();
        }
        
        virtual HandleCharResult
        HandleChar (int key)
        {
            // Always check the active window first
            HandleCharResult result = eKeyNotHandled;
            WindowSP active_window_sp = GetActiveWindow ();
            if (active_window_sp)
            {
                result = active_window_sp->HandleChar (key);
                if (result != eKeyNotHandled)
                    return result;
            }
            
            if (m_delegate_sp)
            {
                result = m_delegate_sp->WindowDelegateHandleChar (*this, key);
                if (result != eKeyNotHandled)
                    return result;
            }

            // Then check for any windows that want any keys
            // that weren't handled. This is typically only
            // for a menubar.
            // Make a copy of the subwindows in case any HandleChar()
            // functions muck with the subwindows. If we don't do this,
            // we can crash when iterating over the subwindows.
            Windows subwindows (m_subwindows);
            for (auto subwindow_sp : subwindows)
            {
                if (subwindow_sp->m_can_activate == false)
                {
                    HandleCharResult result = subwindow_sp->HandleChar(key);
                    if (result != eKeyNotHandled)
                        return result;
                }
            }

            return eKeyNotHandled;
        }

        bool
        SetActiveWindow (Window *window)
        {
            const size_t num_subwindows = m_subwindows.size();
            for (size_t i=0; i<num_subwindows; ++i)
            {
                if (m_subwindows[i].get() == window)
                {
                    m_prev_active_window_idx = m_curr_active_window_idx;
                    m_curr_active_window_idx = i;
                    return true;
                }
            }
            return false;
        }

        WindowSP
        GetActiveWindow ()
        {
            if (!m_subwindows.empty())
            {
                if (m_curr_active_window_idx >= m_subwindows.size())
                {
                    if (m_prev_active_window_idx < m_subwindows.size())
                    {
                        m_curr_active_window_idx = m_prev_active_window_idx;
                        m_prev_active_window_idx = UINT32_MAX;
                    }
                    else if (IsActive())
                    {
                        m_prev_active_window_idx = UINT32_MAX;
                        m_curr_active_window_idx = UINT32_MAX;
                        
                        // Find first window that wants to be active if this window is active
                        const size_t num_subwindows = m_subwindows.size();
                        for (size_t i=0; i<num_subwindows; ++i)
                        {
                            if (m_subwindows[i]->GetCanBeActive())
                            {
                                m_curr_active_window_idx = i;
                                break;
                            }
                        }
                    }
                }
                
                if (m_curr_active_window_idx < m_subwindows.size())
                    return m_subwindows[m_curr_active_window_idx];
            }
            return WindowSP();
        }
        
        bool
        GetCanBeActive () const
        {
            return m_can_activate;
        }

        void
        SetCanBeActive (bool b)
        {
            m_can_activate = b;
        }
        
        const WindowDelegateSP &
        GetDelegate () const
        {
            return m_delegate_sp;
        }

        void
        SetDelegate (const WindowDelegateSP &delegate_sp)
        {
            m_delegate_sp = delegate_sp;
        }
        
        Window *
        GetParent () const
        {
            return m_parent;
        }
        
        bool
        IsActive () const
        {
            if (m_parent)
                return m_parent->GetActiveWindow().get() == this;
            else
                return true; // Top level window is always active
        }
        
        void
        SelectNextWindowAsActive ()
        {
            // Move active focus to next window
            const size_t num_subwindows = m_subwindows.size();
            if (m_curr_active_window_idx == UINT32_MAX)
            {
                uint32_t idx = 0;
                for (auto subwindow_sp : m_subwindows)
                {
                    if (subwindow_sp->GetCanBeActive())
                    {
                        m_curr_active_window_idx = idx;
                        break;
                    }
                    ++idx;
                }
            }
            else if (m_curr_active_window_idx + 1 < num_subwindows)
            {
                bool handled = false;
                m_prev_active_window_idx = m_curr_active_window_idx;
                for (size_t idx=m_curr_active_window_idx + 1; idx<num_subwindows; ++idx)
                {
                    if (m_subwindows[idx]->GetCanBeActive())
                    {
                        m_curr_active_window_idx = idx;
                        handled = true;
                        break;
                    }
                }
                if (!handled)
                {
                    for (size_t idx=0; idx<=m_prev_active_window_idx; ++idx)
                    {
                        if (m_subwindows[idx]->GetCanBeActive())
                        {
                            m_curr_active_window_idx = idx;
                            break;
                        }
                    }
                }
            }
            else
            {
                m_prev_active_window_idx = m_curr_active_window_idx;
                for (size_t idx=0; idx<num_subwindows; ++idx)
                {
                    if (m_subwindows[idx]->GetCanBeActive())
                    {
                        m_curr_active_window_idx = idx;
                        break;
                    }
                }
            }
        }

    protected:
        std::string m_name;
        WINDOW *m_window;
        Window *m_parent;
        Windows m_subwindows;
        WindowDelegateSP m_delegate_sp;
        uint32_t m_curr_active_window_idx;
        uint32_t m_prev_active_window_idx;
        bool m_delete;
        bool m_needs_update;
        bool m_can_activate;
        
    private:
        DISALLOW_COPY_AND_ASSIGN(Window);
    };
    
    class MenuDelegate
    {
    public:
        virtual ~MenuDelegate() {}
        
        virtual MenuActionResult
        MenuDelegateAction (Menu &menu) = 0;
    };

    class Menu : public WindowDelegate
    {
    public:
        enum class Type
        {
            Invalid,
            Bar,
            Item,
            Separator
        };
        
        // Menubar or separator constructor
        Menu (Type type);
        
        // Menuitem constructor
        Menu (const char *name,
              const char *key_name,
              int key_value);
        
        virtual ~
        Menu ()
        {
        }

        const MenuDelegateSP &
        GetDelegate () const
        {
            return m_delegate_sp;
        }
        
        void
        SetDelegate (const MenuDelegateSP &delegate_sp)
        {
            m_delegate_sp = delegate_sp;
        }
        
        void
        RecalculateNameLengths();

        void
        AddSubmenu (const MenuSP &menu_sp);
        
        int
        DrawAndRunMenu (Window &window);
        
        void
        DrawMenuTitle (Window &window, bool highlight);

        virtual bool
        WindowDelegateDraw (Window &window, bool force);
        
        virtual HandleCharResult
        WindowDelegateHandleChar (Window &window, int key);

        MenuActionResult
        Action ()
        {
            if (m_delegate_sp)
                return m_delegate_sp->MenuDelegateAction (*this);
            return m_canned_result;
        }
        
        void
        SetCannedResult (MenuActionResult result)
        {
            m_canned_result = result;
        }

        Menus &
        GetSubmenus()
        {
            return m_submenus;
        }

        const Menus &
        GetSubmenus() const
        {
            return m_submenus;
        }

        int
        GetSelectedSubmenuIndex () const
        {
            return m_selected;
        }
        
        void
        SetSelectedSubmenuIndex (int idx)
        {
            m_selected = idx;
        }

        Type
        GetType () const
        {
            return m_type;
        }
        
        int
        GetStartingColumn() const
        {
            return m_start_col;
        }

        void
        SetStartingColumn(int col)
        {
            m_start_col = col;
        }

        int
        GetKeyValue() const
        {
            return m_key_value;
        }
        
        void
        SetKeyValue(int key_value)
        {
            m_key_value = key_value;
        }

        std::string &
        GetName()
        {
            return m_name;
        }

        std::string &
        GetKeyName()
        {
            return m_key_name;
        }

        int
        GetDrawWidth () const
        {
            return m_max_submenu_name_length + m_max_submenu_key_name_length + 8;
        }
    protected:
        std::string m_name;
        std::string m_key_name;
        Type m_type;
        int m_key_value;
        int m_start_col;
        int m_max_submenu_name_length;
        int m_max_submenu_key_name_length;
        int m_selected;
        Menu *m_parent;
        Menus m_submenus;
        WindowSP m_menu_window_sp;
        MenuActionResult m_canned_result;
        MenuDelegateSP m_delegate_sp;
    };

    // Menubar or separator constructor
    Menu::Menu (Type type) :
        m_name (),
        m_key_name (),
        m_type (type),
        m_key_value (0),
        m_start_col (0),
        m_max_submenu_name_length (0),
        m_max_submenu_key_name_length (0),
        m_selected (0),
        m_parent (NULL),
        m_submenus (),
        m_canned_result (MenuActionResult::Success),
        m_delegate_sp()
    {
    }

    // Menuitem constructor
    Menu::Menu (const char *name,
                const char *key_name,
                int key_value) :
        m_name (),
        m_key_name (),
        m_type (Type::Invalid),
        m_key_value (key_value),
        m_start_col (0),
        m_max_submenu_name_length (0),
        m_max_submenu_key_name_length (0),
        m_selected (0),
        m_parent (NULL),
        m_submenus (),
        m_canned_result (MenuActionResult::Success),
        m_delegate_sp()
    {
        if (name && name[0])
        {
            m_name = name;
            m_type = Type::Item;
            if (key_name && key_name[0])
                m_key_name = key_name;
        }
        else
        {
            m_type = Type::Separator;
        }
    }

    void
    Menu::RecalculateNameLengths()
    {
        m_max_submenu_name_length = 0;
        m_max_submenu_key_name_length = 0;
        Menus &submenus = GetSubmenus();
        const size_t num_submenus = submenus.size();
        for (size_t i=0; i<num_submenus; ++i)
        {
            Menu *submenu = submenus[i].get();
            if (m_max_submenu_name_length < submenu->m_name.size())
                m_max_submenu_name_length = submenu->m_name.size();
            if (m_max_submenu_key_name_length < submenu->m_key_name.size())
                m_max_submenu_key_name_length = submenu->m_key_name.size();
        }
    }

    void
    Menu::AddSubmenu (const MenuSP &menu_sp)
    {
        menu_sp->m_parent = this;
        if (m_max_submenu_name_length < menu_sp->m_name.size())
            m_max_submenu_name_length = menu_sp->m_name.size();
        if (m_max_submenu_key_name_length < menu_sp->m_key_name.size())
            m_max_submenu_key_name_length = menu_sp->m_key_name.size();
        m_submenus.push_back(menu_sp);
    }

    void
    Menu::DrawMenuTitle (Window &window, bool highlight)
    {
        if (m_type == Type::Separator)
        {
            window.Move(0, window.GetY());
            window.PutChar(ACS_LTEE);
            int width = window.GetWidth();
            if (width > 2)
            {
                width -= 2;
                for (size_t i=0; i< width; ++i)
                    window.PutChar(ACS_HLINE);
            }
            window.PutChar(ACS_RTEE);
        }
        else
        {
            const int shortcut_key = m_key_value;
            bool underlined_shortcut = false;
            const attr_t hilgight_attr = A_REVERSE;
            if (highlight)
                window.AttributeOn(hilgight_attr);
            if (isprint(shortcut_key))
            {
                size_t lower_pos = m_name.find(tolower(shortcut_key));
                size_t upper_pos = m_name.find(toupper(shortcut_key));
                const char *name = m_name.c_str();
                size_t pos = std::min<size_t>(lower_pos, upper_pos);
                if (pos != std::string::npos)
                {
                    underlined_shortcut = true;
                    if (pos > 0)
                    {
                        window.PutCString(name, pos);
                        name += pos;
                    }
                    const attr_t shortcut_attr = A_UNDERLINE|A_BOLD;
                    window.AttributeOn (shortcut_attr);
                    window.PutChar(name[0]);
                    window.AttributeOff(shortcut_attr);
                    name++;
                    if (name[0])
                        window.PutCString(name);
                }
            }
            
            if (!underlined_shortcut)
            {
                window.PutCString(m_name.c_str());
            }

            if (highlight)
                window.AttributeOff(hilgight_attr);

            if (m_key_name.empty())
            {
                if (!underlined_shortcut && isprint(m_key_value))
                {
                    window.AttributeOn (COLOR_PAIR(3));
                    window.Printf (" (%c)", m_key_value);
                    window.AttributeOff (COLOR_PAIR(3));
                }
            }
            else
            {
                window.AttributeOn (COLOR_PAIR(3));
                window.Printf (" (%s)", m_key_name.c_str());
                window.AttributeOff (COLOR_PAIR(3));
            }
        }
    }
    
    bool
    Menu::WindowDelegateDraw (Window &window, bool force)
    {
        Menus &submenus = GetSubmenus();
        const size_t num_submenus = submenus.size();
        const int selected_idx = GetSelectedSubmenuIndex();
        Menu::Type menu_type = GetType ();
        switch (menu_type)
        {
        case  Menu::Type::Bar:
            {
                window.SetBackground(2);
                window.Move(0, 0);
                for (size_t i=0; i<num_submenus; ++i)
                {
                    Menu *menu = submenus[i].get();
                    if (i > 0)
                        window.PutChar(' ');
                    menu->SetStartingColumn (window.GetX());
                    window.PutCString("| ");
                    menu->DrawMenuTitle (window, false);
                }
                window.PutCString(" |");
                window.DeferredRefresh();
            }
            break;
                
        case Menu::Type::Item:
            {
                int y = 1;
                int x = 3;
                // Draw the menu
                int cursor_x = 0;
                int cursor_y = 0;
                window.Erase();
                window.SetBackground(2);
                window.Box();
                for (size_t i=0; i<num_submenus; ++i)
                {
                    const bool is_selected = i == selected_idx;
                    window.Move(x, y + i);
                    if (is_selected)
                    {
                        // Remember where we want the cursor to be
                        cursor_x = x-1;
                        cursor_y = y+i;
                    }
                    submenus[i]->DrawMenuTitle (window, is_selected);
                }
                window.Move(cursor_x, cursor_y);
                window.DeferredRefresh();
            }
            break;

        default:
        case Menu::Type::Separator:
            break;
        }
        return true; // Drawing handled...
    }
    
    HandleCharResult
    Menu::WindowDelegateHandleChar (Window &window, int key)
    {
        HandleCharResult result = eKeyNotHandled;
        
        Menus &submenus = GetSubmenus();
        const size_t num_submenus = submenus.size();
        const int selected_idx = GetSelectedSubmenuIndex();
        Menu::Type menu_type = GetType ();
        if (menu_type == Menu::Type::Bar)
        {
            MenuSP run_menu_sp;
            switch (key)
            {
                case KEY_DOWN:
                case KEY_UP:
                    // Show last menu or first menu
                    if (selected_idx < num_submenus)
                        run_menu_sp = submenus[selected_idx];
                    else if (!submenus.empty())
                        run_menu_sp = submenus.front();
                    result = eKeyHandled;
                    break;
                    
                case KEY_RIGHT:
                {
                    ++m_selected;
                    if (m_selected >= num_submenus)
                        m_selected = 0;
                    if (m_selected < num_submenus)
                        run_menu_sp = submenus[m_selected];
                    else if (!submenus.empty())
                        run_menu_sp = submenus.front();
                    result = eKeyHandled;
                }
                    break;
                    
                case KEY_LEFT:
                {
                    --m_selected;
                    if (m_selected < 0)
                        m_selected = num_submenus - 1;
                    if (m_selected < num_submenus)
                        run_menu_sp = submenus[m_selected];
                    else if (!submenus.empty())
                        run_menu_sp = submenus.front();
                    result = eKeyHandled;
                }
                    break;
                    
                default:
                    for (size_t i=0; i<num_submenus; ++i)
                    {
                        if (submenus[i]->GetKeyValue() == key)
                        {
                            SetSelectedSubmenuIndex(i);
                            run_menu_sp = submenus[i];
                            result = eKeyHandled;
                            break;
                        }
                    }
                    break;
            }
            
            if (run_menu_sp)
            {
                // Run the action on this menu in case we need to populate the
                // menu with dynamic content and also in case check marks, and
                // any other menu decorations need to be caclulated
                if (run_menu_sp->Action() == MenuActionResult::Quit)
                    return eQuitApplication;

                const int win_width = run_menu_sp->GetDrawWidth();
                const int win_height = run_menu_sp->GetSubmenus().size() + 2;
                if (m_menu_window_sp)
                    window.GetParent()->RemoveSubWindow(m_menu_window_sp.get());
                
                m_menu_window_sp = window.GetParent()->CreateSubWindow (run_menu_sp->GetName().c_str(),
                                                                        win_height,
                                                                        win_width,
                                                                        window.GetMinY() + 1,
                                                                        run_menu_sp->GetStartingColumn(),
                                                                        true);
                m_menu_window_sp->SetDelegate (run_menu_sp);
            }
        }
        else if (menu_type == Menu::Type::Item)
        {
            switch (key)
            {
                case KEY_DOWN:
                    if (m_submenus.size() > 1)
                    {
                        const int start_select = m_selected;
                        while (++m_selected != start_select)
                        {
                            if (m_selected >= num_submenus)
                                m_selected = 0;
                            if (m_submenus[m_selected]->GetType() == Type::Separator)
                                continue;
                            else
                                break;
                        }
                        return eKeyHandled;
                    }
                    break;
                    
                case KEY_UP:
                    if (m_submenus.size() > 1)
                    {
                        const int start_select = m_selected;
                        while (--m_selected != start_select)
                        {
                            if (m_selected < 0)
                                m_selected = num_submenus - 1;
                            if (m_submenus[m_selected]->GetType() == Type::Separator)
                                continue;
                            else
                                break;
                        }
                        return eKeyHandled;
                    }
                    break;
                    
                case KEY_RETURN:
                    if (selected_idx < num_submenus)
                    {
                        if (submenus[selected_idx]->Action() == MenuActionResult::Quit)
                            return eQuitApplication;
                        window.GetParent()->RemoveSubWindow(&window);
                        return eKeyHandled;
                    }
                    break;
                    
                case KEY_ESCAPE: // Beware: pressing escape key has 1 to 2 second delay in case other chars are entered for escaped sequences
                    window.GetParent()->RemoveSubWindow(&window);
                    return eKeyHandled;
                    
                default:
                {
                    bool handled = false;
                    for (size_t i=0; i<num_submenus; ++i)
                    {
                        Menu *menu = submenus[i].get();
                        if (menu->GetKeyValue() == key)
                        {
                            handled = true;
                            SetSelectedSubmenuIndex(i);
                            if (menu->Action() == MenuActionResult::Quit)
                                return eQuitApplication;
                            return eKeyHandled;
                        }
                    }
                }
                    break;
                    
            }
        }
        else if (menu_type == Menu::Type::Separator)
        {
            
        }
        return result;
    }


    class Application
    {
    public:
        Application (FILE *in, FILE *out) :
            m_window_sp(),
            m_screen (NULL),
            m_in (in),
            m_out (out)
        {
            
        }
        
        ~Application ()
        {
            m_window_delegates.clear();
            m_window_sp.reset();
            if (m_screen)
            {
                ::delscreen(m_screen);
                m_screen = NULL;
            }
        }
        
        void
        Initialize ()
        {
            ::setlocale(LC_ALL, "");
            ::setlocale(LC_CTYPE, "");
#if 0
            ::initscr();
#else
            m_screen = ::newterm(NULL, m_out, m_in);
#endif
            ::start_color();
            ::curs_set(0);
            ::noecho();
            ::keypad(stdscr,TRUE);
        }
        
        void
        Terminate ()
        {
            ::endwin();
        }
        
        void
        Run (Debugger &debugger)
        {
            bool done = false;
            int delay_in_tenths_of_a_second = 1;
            
            // Alas the threading model in curses is a bit lame so we need to
            // resort to polling every 0.5 seconds. We could poll for stdin
            // ourselves and then pass the keys down but then we need to
            // translate all of the escape sequences ourselves. So we resort to
            // polling for input because we need to receive async process events
            // while in this loop.
            
            halfdelay(delay_in_tenths_of_a_second); // Poll using some number of tenths of seconds seconds when calling Window::GetChar()

            ListenerSP listener_sp (new Listener ("lldb.IOHandler.curses.Application"));
            ConstString broadcaster_class_target(Target::GetStaticBroadcasterClass());
            ConstString broadcaster_class_process(Process::GetStaticBroadcasterClass());
            ConstString broadcaster_class_thread(Thread::GetStaticBroadcasterClass());
            debugger.EnableForwardEvents (listener_sp);

            bool update = true;
#if defined(__APPLE__)
            std::deque<int> escape_chars;
#endif
            
            while (!done)
            {
                if (update)
                {
                    m_window_sp->Draw(false);
                
                    // All windows should be calling Window::DeferredRefresh() instead
                    // of Window::Refresh() so we can do a single update and avoid
                    // any screen blinking
                    doupdate();
                    update = false;
                }
                
#if defined(__APPLE__)
                // Terminal.app doesn't map its function keys correctly, F1-F4 default to:
                // \033OP, \033OQ, \033OR, \033OS, so lets take care of this here if possible
                int ch;
                if (escape_chars.empty())
                    ch = m_window_sp->GetChar();
                else
                {
                    ch = escape_chars.front();
                    escape_chars.pop_front();
                }
                if (ch == KEY_ESCAPE)
                {
                    int ch2 = m_window_sp->GetChar();
                    if (ch2 == 'O')
                    {
                        int ch3 = m_window_sp->GetChar();
                        switch (ch3)
                        {
                            case 'P': ch = KEY_F(1); break;
                            case 'Q': ch = KEY_F(2); break;
                            case 'R': ch = KEY_F(3); break;
                            case 'S': ch = KEY_F(4); break;
                            default:
                                escape_chars.push_back(ch2);
                                if (ch3 != -1)
                                    escape_chars.push_back(ch3);
                                break;
                        }
                    }
                    else if (ch2 != -1)
                        escape_chars.push_back(ch2);
                }
#else
                int ch = m_window_sp->GetChar();

#endif
                if (ch == -1)
                {
                    if (feof(m_in) || ferror(m_in))
                    {
                        done = true;
                    }
                    else
                    {
                        // Just a timeout from using halfdelay(), check for events
                        EventSP event_sp;
                        while (listener_sp->PeekAtNextEvent())
                        {
                            listener_sp->GetNextEvent(event_sp);
                            
                            if (event_sp)
                            {
                                Broadcaster *broadcaster = event_sp->GetBroadcaster();
                                if (broadcaster)
                                {
                                    //uint32_t event_type = event_sp->GetType();
                                    ConstString broadcaster_class (broadcaster->GetBroadcasterClass());
                                    if (broadcaster_class == broadcaster_class_process)
                                    {
                                        update = true;
                                        continue; // Don't get any key, just update our view
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    HandleCharResult key_result = m_window_sp->HandleChar(ch);
                    switch (key_result)
                    {
                        case eKeyHandled:
                            update = true;
                            break;
                        case eKeyNotHandled:
                            break;
                        case eQuitApplication:
                            done = true;
                            break;
                    }
                }
            }
            
            debugger.CancelForwardEvents (listener_sp);

        }
        
        WindowSP &
        GetMainWindow ()
        {
            if (!m_window_sp)
                m_window_sp.reset (new Window ("main", stdscr, false));
            return m_window_sp;
        }
        
        WindowDelegates &
        GetWindowDelegates ()
        {
            return m_window_delegates;
        }

    protected:
        WindowSP m_window_sp;
        WindowDelegates m_window_delegates;
        SCREEN *m_screen;
        FILE *m_in;
        FILE *m_out;
    };
    

} // namespace curses


using namespace curses;

struct Row
{
    ValueObjectSP valobj;
    Row *parent;
    int row_idx;
    int x;
    int y;
    bool might_have_children;
    bool expanded;
    bool calculated_children;
    std::vector<Row> children;
    
    Row (const ValueObjectSP &v, Row *p) :
    valobj (v),
    parent (p),
    row_idx(0),
    x(1),
    y(1),
    might_have_children (v->MightHaveChildren()),
    expanded (false),
    calculated_children (false),
    children()
    {
    }
    
    size_t
    GetDepth () const
    {
        if (parent)
            return 1 + parent->GetDepth();
        return 0;
    }
    
    void
    Expand()
    {
        expanded = true;
        if (!calculated_children)
        {
            calculated_children = true;
            const size_t num_children = valobj->GetNumChildren();
            for (size_t i=0; i<num_children; ++i)
            {
                children.push_back(Row (valobj->GetChildAtIndex(i, true), this));
            }
        }
    }
    
    void
    Unexpand ()
    {
        expanded = false;
    }
    
    void
    DrawTree (Window &window)
    {
        if (parent)
            parent->DrawTreeForChild (window, this, 0);
        
        if (might_have_children)
        {
            // It we can get UTF8 characters to work we should try to use the "symbol"
            // UTF8 string below
//            const char *symbol = "";
//            if (row.expanded)
//                symbol = "\xe2\x96\xbd ";
//            else
//                symbol = "\xe2\x96\xb7 ";
//            window.PutCString (symbol);
            
            // The ACS_DARROW and ACS_RARROW don't look very nice they are just a
            // 'v' or '>' character...
//            if (expanded)
//                window.PutChar (ACS_DARROW);
//            else
//                window.PutChar (ACS_RARROW);
            // Since we can't find any good looking right arrow/down arrow
            // symbols, just use a diamond...
            window.PutChar (ACS_DIAMOND);
            window.PutChar (ACS_HLINE);
        }
    }

    void
    DrawTreeForChild (Window &window, Row *child, uint32_t reverse_depth)
    {
        if (parent)
            parent->DrawTreeForChild (window, this, reverse_depth + 1);
        
        if (&children.back() == child)
        {
            // Last child
            if (reverse_depth == 0)
            {
                window.PutChar (ACS_LLCORNER);
                window.PutChar (ACS_HLINE);
            }
            else
            {
                window.PutChar (' ');
                window.PutChar (' ');
            }
        }
        else
        {
            if (reverse_depth == 0)
            {
                window.PutChar (ACS_LTEE);
                window.PutChar (ACS_HLINE);
            }
            else
            {
                window.PutChar (ACS_VLINE);
                window.PutChar (' ');
            }
        }
    }
};

struct DisplayOptions
{
    bool show_types;
};


class ValueObjectListDelegate : public WindowDelegate
{
public:
    ValueObjectListDelegate () :
        m_valobj_list (),
        m_rows (),
        m_selected_row (NULL),
        m_selected_row_idx (0),
        m_first_visible_row (0),
        m_num_rows (0),
        m_max_x (0),
        m_max_y (0)
    {
    }
    
    ValueObjectListDelegate (ValueObjectList &valobj_list) :
        m_valobj_list (valobj_list),
        m_rows (),
        m_selected_row (NULL),
        m_selected_row_idx (0),
        m_first_visible_row (0),
        m_num_rows (0),
        m_max_x (0),
        m_max_y (0)
    {
        SetValues (valobj_list);
    }
    
    virtual
    ~ValueObjectListDelegate()
    {
    }

    void
    SetValues (ValueObjectList &valobj_list)
    {
        m_selected_row = NULL;
        m_selected_row_idx = 0;
        m_first_visible_row = 0;
        m_num_rows = 0;
        m_rows.clear();
        m_valobj_list = valobj_list;
        const size_t num_values = m_valobj_list.GetSize();
        for (size_t i=0; i<num_values; ++i)
            m_rows.push_back(Row(m_valobj_list.GetValueObjectAtIndex(i), NULL));
    }
    
    virtual bool
    WindowDelegateDraw (Window &window, bool force)
    {
        m_num_rows = 0;
        m_min_x = 2;
        m_min_y = 1;
        m_max_x = window.GetMaxX();
        m_max_y = window.GetMaxY() - 1;
        
        window.Erase();
        window.DrawTitleBox ("Variables");
        
        const int num_visible_rows = NumVisibleRows();
        const int num_rows = CalculateTotalNumberRows (m_rows);
        
        // If we unexpanded while having something selected our
        // total number of rows is less than the num visible rows,
        // then make sure we show all the rows by setting the first
        // visible row accordingly.
        if (m_first_visible_row > 0 && num_rows < num_visible_rows)
            m_first_visible_row = 0;
        
        // Make sure the selected row is always visible
        if (m_selected_row_idx < m_first_visible_row)
            m_first_visible_row = m_selected_row_idx;
        else if (m_first_visible_row + m_max_y < m_selected_row_idx)
            m_first_visible_row = m_selected_row_idx - m_max_y;
        
        DisplayRows (window, m_rows, g_options);
        
        window.DeferredRefresh();
        
        // Get the selected row
        m_selected_row = GetRowForRowIndex (m_selected_row_idx);
        // Keep the cursor on the selected row so the highlight and the cursor
        // are always on the same line
        if (m_selected_row)
            window.Move (m_selected_row->x,
                         m_selected_row->y);
        
        return true; // Drawing handled
    }
    
    virtual HandleCharResult
    WindowDelegateHandleChar (Window &window, int c)
    {
        switch(c)
        {
            case 'x':
            case 'X':
            case 'o':
            case 's':
            case 'u':
            case 'd':
            case 'D':
            case 'i':
            case 'A':
            case 'p':
            case 'c':
            case 'b':
            case 'B':
            case 'f':
                // Change the format for the currently selected item
                if (m_selected_row)
                    m_selected_row->valobj->SetFormat (FormatForChar (c));
                return eKeyHandled;
                
            case 't':
                // Toggle showing type names
                g_options.show_types = !g_options.show_types;
                return eKeyHandled;
                
            case ',':
            case KEY_PPAGE:
                // Page up key
                if (m_first_visible_row > 0)
                {
                    if (m_first_visible_row > m_max_y)
                        m_first_visible_row -= m_max_y;
                    else
                        m_first_visible_row = 0;
                    m_selected_row_idx = m_first_visible_row;
                }
                return eKeyHandled;
                
            case '.':
            case KEY_NPAGE:
                // Page down key
                if (m_num_rows > m_max_y)
                {
                    if (m_first_visible_row + m_max_y < m_num_rows)
                    {
                        m_first_visible_row += m_max_y;
                        m_selected_row_idx = m_first_visible_row;
                    }
                }
                return eKeyHandled;
                
            case KEY_UP:
                if (m_selected_row_idx > 0)
                    --m_selected_row_idx;
                return eKeyHandled;
            case KEY_DOWN:
                if (m_selected_row_idx + 1 < m_num_rows)
                    ++m_selected_row_idx;
                return eKeyHandled;
                
            case KEY_RIGHT:
                if (m_selected_row)
                {
                    if (!m_selected_row->expanded)
                        m_selected_row->Expand();
                }
                return eKeyHandled;
                
            case KEY_LEFT:
                if (m_selected_row)
                {
                    if (m_selected_row->expanded)
                        m_selected_row->Unexpand();
                    else if (m_selected_row->parent)
                        m_selected_row_idx = m_selected_row->parent->row_idx;
                }
                return eKeyHandled;
                
            case ' ':
                // Toggle expansion state when SPACE is pressed
                if (m_selected_row)
                {
                    if (m_selected_row->expanded)
                        m_selected_row->Unexpand();
                    else
                        m_selected_row->Expand();
                }
                return eKeyHandled;
                
            default:
                break;
        }
        return eKeyNotHandled;
    }
    
protected:
    ValueObjectList m_valobj_list;
    std::vector<Row> m_rows;
    Row *m_selected_row;
    uint32_t m_selected_row_idx;
    uint32_t m_first_visible_row;
    uint32_t m_num_rows;
    int m_min_x;
    int m_min_y;
    int m_max_x;
    int m_max_y;

    static Format
    FormatForChar (int c)
    {
        switch (c)
        {
            case 'x': return eFormatHex;
            case 'X': return eFormatHexUppercase;
            case 'o': return eFormatOctal;
            case 's': return eFormatCString;
            case 'u': return eFormatUnsigned;
            case 'd': return eFormatDecimal;
            case 'D': return eFormatDefault;
            case 'i': return eFormatInstruction;
            case 'A': return eFormatAddressInfo;
            case 'p': return eFormatPointer;
            case 'c': return eFormatChar;
            case 'b': return eFormatBinary;
            case 'B': return eFormatBytesWithASCII;
            case 'f': return eFormatFloat;
        }
        return eFormatDefault;
    }
    
    void
    DisplayRowObject (Window &window,
                      Row &row,
                      DisplayOptions &options,
                      bool highlight,
                      bool last_child)
    {
        ValueObject *valobj = row.valobj.get();
        
        const char *type_name = options.show_types ? valobj->GetTypeName().GetCString() : NULL;
        const char *name = valobj->GetName().GetCString();
        const char *value = valobj->GetValueAsCString ();
        const char *summary = valobj->GetSummaryAsCString ();
        
        window.Move (row.x, row.y);
        
        row.DrawTree (window);
        
        if (highlight)
            window.AttributeOn(A_REVERSE);
        
        if (type_name && type_name[0])
            window.Printf ("(%s) ", type_name);
        
        if (name && name[0])
            window.PutCString(name);
        
        attr_t changd_attr = 0;
        if (valobj->GetValueDidChange())
            changd_attr = COLOR_PAIR(5) | A_BOLD;
        
        if (value && value[0])
        {
            window.PutCString(" = ");
            if (changd_attr)
                window.AttributeOn(changd_attr);
            window.PutCString (value);
            if (changd_attr)
                window.AttributeOff(changd_attr);
        }
        
        if (summary && summary[0])
        {
            window.PutChar(' ');
            if (changd_attr)
                window.AttributeOn(changd_attr);
            window.PutCString(summary);
            if (changd_attr)
                window.AttributeOff(changd_attr);
        }
        
        if (highlight)
            window.AttributeOff (A_REVERSE);
    }
    void
    DisplayRows (Window &window,
                 std::vector<Row> &rows,
                 DisplayOptions &options)
    {
        // >   0x25B7
        // \/  0x25BD
        
        bool window_is_active = window.IsActive();
        for (auto &row : rows)
        {
            const bool last_child = row.parent && &rows[rows.size()-1] == &row;
            // Save the row index in each Row structure
            row.row_idx = m_num_rows;
            if ((m_num_rows >= m_first_visible_row) &&
                ((m_num_rows - m_first_visible_row) < NumVisibleRows()))
            {
                row.x = m_min_x;
                row.y = m_num_rows - m_first_visible_row + 1;
                DisplayRowObject (window,
                                  row,
                                  options,
                                  window_is_active && m_num_rows == m_selected_row_idx,
                                  last_child);
            }
            else
            {
                row.x = 0;
                row.y = 0;
            }
            ++m_num_rows;
            
            if (row.expanded && !row.children.empty())
            {
                DisplayRows (window,
                             row.children,
                             options);
            }
        }
    }
    
    int
    CalculateTotalNumberRows (const std::vector<Row> &rows)
    {
        int row_count = 0;
        for (const auto &row : rows)
        {
            ++row_count;
            if (row.expanded)
                row_count += CalculateTotalNumberRows(row.children);
        }
        return row_count;
    }
    static Row *
    GetRowForRowIndexImpl (std::vector<Row> &rows, size_t &row_index)
    {
        for (auto &row : rows)
        {
            if (row_index == 0)
                return &row;
            else
            {
                --row_index;
                if (row.expanded && !row.children.empty())
                {
                    Row *result = GetRowForRowIndexImpl (row.children, row_index);
                    if (result)
                        return result;
                }
            }
        }
        return NULL;
    }
    
    Row *
    GetRowForRowIndex (size_t row_index)
    {
        return GetRowForRowIndexImpl (m_rows, row_index);
    }
    
    int
    NumVisibleRows () const
    {
        return m_max_y - m_min_y;
    }

    static DisplayOptions g_options;
};

class FrameVariablesWindowDelegate : public ValueObjectListDelegate
{
public:
    FrameVariablesWindowDelegate (Debugger &debugger) :
        ValueObjectListDelegate (),
        m_debugger (debugger),
        m_frame_block (NULL)
    {
    }
    
    virtual
    ~FrameVariablesWindowDelegate()
    {
    }
    
    virtual bool
    WindowDelegateDraw (Window &window, bool force)
    {
        ExecutionContext exe_ctx (m_debugger.GetCommandInterpreter().GetExecutionContext());
        Process *process = exe_ctx.GetProcessPtr();
        Block *frame_block = NULL;
        StackFrame *frame = NULL;
        
        if (process)
        {
            StateType state = process->GetState();
            if (StateIsStoppedState(state, true))
            {
                frame = exe_ctx.GetFramePtr();
                if (frame)
                    frame_block = frame->GetFrameBlock ();
            }
            else if (StateIsRunningState(state))
            {
                return true; // Don't do any updating when we are running
            }
        }
        
        ValueObjectList local_values;
        if (frame_block)
        {
            // Only update the variables if they have changed
            if (m_frame_block != frame_block)
            {
                m_frame_block = frame_block;

                VariableList *locals = frame->GetVariableList(true);
                if (locals)
                {
                    const DynamicValueType use_dynamic = eDynamicDontRunTarget;
                    const size_t num_locals = locals->GetSize();
                    for (size_t i=0; i<num_locals; ++i)
                        local_values.Append(frame->GetValueObjectForFrameVariable (locals->GetVariableAtIndex(i), use_dynamic));
                    // Update the values
                    SetValues(local_values);
                }
            }
        }
        else
        {
            m_frame_block = NULL;
            // Update the values with an empty list if there is no frame
            SetValues(local_values);
        }
        
        return ValueObjectListDelegate::WindowDelegateDraw (window, force);

    }

protected:
    Debugger &m_debugger;
    Block *m_frame_block;
};

class ApplicationDelegate :
    public WindowDelegate,
    public MenuDelegate
{
public:
    ApplicationDelegate (Debugger &debugger) :
        WindowDelegate (),
        MenuDelegate (),
        m_debugger (debugger)
    {
    }
    
    virtual
    ~ApplicationDelegate ()
    {
    }
    virtual bool
    WindowDelegateDraw (Window &window, bool force)
    {
        return false; // Drawing not handled, let standard window drawing happen
    }

    virtual HandleCharResult
    WindowDelegateHandleChar (Window &window, int key)
    {
        if (key == '\t')
        {
            window.SelectNextWindowAsActive();
            return eKeyHandled;
        }
        return eKeyNotHandled;
    }
    
    virtual MenuActionResult
    MenuDelegateAction (Menu &menu)
    {
        if (menu.GetName().compare("Process") == 0)
        {
            // Populate the menu with all of the threads
            Menus &submenus = menu.GetSubmenus();
            ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
            Process *process = exe_ctx.GetProcessPtr();
            if (process && process->IsAlive() && StateIsStoppedState (process->GetState(), true))
            {
                if (submenus.size() == 7)
                    menu.AddSubmenu (MenuSP (new Menu(Menu::Type::Separator)));
                else if (submenus.size() > 8)
                    submenus.erase (submenus.begin() + 8, submenus.end());
                
                ThreadList &threads = process->GetThreadList();
                Mutex::Locker locker (threads.GetMutex());
                size_t num_threads = threads.GetSize();
                for (size_t i=0; i<num_threads; ++i)
                {
                    ThreadSP thread_sp = threads.GetThreadAtIndex(i);
                    char menu_char = '\0';
                    if (i < 9)
                        menu_char = '1' + i;
                    StreamString thread_menu_title;
                    thread_menu_title.Printf("Thread %u", thread_sp->GetIndexID());
                    const char *thread_name = thread_sp->GetName();
                    if (thread_name && thread_name[0])
                        thread_menu_title.Printf (" %s", thread_name);
                    else
                    {
                        const char *queue_name = thread_sp->GetQueueName();
                        if (queue_name && queue_name[0])
                            thread_menu_title.Printf (" %s", queue_name);
                    }
                    menu.AddSubmenu (MenuSP (new Menu(thread_menu_title.GetString().c_str(), NULL, menu_char)));
                }
            }
            else if (submenus.size() > 7)
            {
                // Remove the separator and any other thread submenu items
                // that were previously added
                submenus.erase (submenus.begin() + 7, submenus.end());
            }
            // Since we are adding and removing items we need to recalculate the name lengths
            menu.RecalculateNameLengths();
        }
        return MenuActionResult::Success;
    }
protected:
    Debugger &m_debugger;
};


class StatusBarWindowDelegate : public WindowDelegate
{
public:
    StatusBarWindowDelegate (Debugger &debugger) :
        m_debugger (debugger)
    {
    }
    
    virtual
    ~StatusBarWindowDelegate ()
    {
    }
    virtual bool
    WindowDelegateDraw (Window &window, bool force)
    {
        ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
        Process *process = exe_ctx.GetProcessPtr();
        Thread *thread = exe_ctx.GetThreadPtr();
        StackFrame *frame = exe_ctx.GetFramePtr();
        window.Erase();
        window.SetBackground(2);
        window.Move (0, 0);
        if (process)
        {
            const StateType state = process->GetState();
            window.Printf ("Process: %5" PRIu64 " %10s", process->GetID(), StateAsCString(state));

            if (StateIsStoppedState(state, true))
            {
                window.Move (40, 0);
                if (thread)
                    window.Printf ("Thread: 0x%4.4" PRIx64, thread->GetID());

                window.Move (60, 0);
                if (frame)
                    window.Printf ("Frame: %3u  PC = 0x%16.16" PRIx64, frame->GetFrameIndex(), frame->GetFrameCodeAddress().GetOpcodeLoadAddress (exe_ctx.GetTargetPtr()));
            }
            else if (state == eStateExited)
            {
                const char *exit_desc = process->GetExitDescription();
                const int exit_status = process->GetExitStatus();
                if (exit_desc && exit_desc[0])
                    window.Printf (" with status = %i (%s)", exit_status, exit_desc);
                else
                    window.Printf (" with status = %i", exit_status);
            }
        }
        window.DeferredRefresh();
        return true;
    }

protected:
    Debugger &m_debugger;
};

class SourceFileWindowDelegate : public WindowDelegate
{
public:
    SourceFileWindowDelegate (Debugger &debugger) :
        WindowDelegate (),
        m_debugger (debugger),
        m_sc (),
        m_file_sp (),
        m_line_width (4),
        m_selected_line (0),
        m_pc_line (0),
        m_stop_id (0),
        m_first_visible_line (0),
        m_min_x (0),
        m_min_y (0),
        m_max_x (0),
        m_max_y (0)
    {
    }
    
    
    virtual
    ~SourceFileWindowDelegate()
    {
    }
    
    void
    Update (const SymbolContext &sc)
    {
        m_sc = sc;
    }
    
    uint32_t
    NumVisibleLines () const
    {
        return m_max_y - m_min_y;
    }
    
    virtual bool
    WindowDelegateDraw (Window &window, bool force)
    {
        window.Erase();
        window.DrawTitleBox ("Sources");
        m_min_x = window.GetMinX()+1;
        m_min_y = window.GetMinY()+1;
        m_max_x = window.GetMaxX();
        m_max_y = window.GetMaxY();
        const uint32_t num_visible_lines = NumVisibleLines();
        
        ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
        Process *process = exe_ctx.GetProcessPtr();
        Thread *thread = NULL;
        StackFrameSP frame_sp;
        const bool process_alive = process ? process->IsAlive() : false;
        if (process_alive)
        {
            thread = exe_ctx.GetThreadPtr();
            if (thread)
                frame_sp = thread->GetSelectedFrame();
        }
        const uint32_t stop_id = process ? process->GetStopID() : 0;
        const bool stop_id_changed = stop_id != m_stop_id;
        m_stop_id = stop_id;
        if (frame_sp)
        {
            m_sc = frame_sp->GetSymbolContext(eSymbolContextEverything);
        }
        else
        {
            m_sc.Clear(true);
        }
        
        if (process_alive)
        {
            if (m_sc.line_entry.IsValid())
            {
                m_pc_line = m_sc.line_entry.line;
                if (m_pc_line != UINT32_MAX)
                    --m_pc_line; // Convert to zero based line number...
                // Update the selected line if the stop ID changed...
                if (stop_id_changed)
                    m_selected_line = m_pc_line;

                if (m_file_sp && m_file_sp->FileSpecMatches(m_sc.line_entry.file))
                {
                    // Same file, noting to do, we should either have the
                    // lines or not (source file missing)
                    if (m_selected_line >= m_first_visible_line)
                    {
                        if (m_selected_line >= m_first_visible_line + num_visible_lines)
                            m_first_visible_line = m_selected_line - 10;
                    }
                    else
                    {
                        if (m_selected_line > 10)
                            m_first_visible_line = m_selected_line - 10;
                        else
                            m_first_visible_line = 0;
                    }
                }
                else
                {
                    // File changed, set selected line to the line with the PC
                    m_selected_line = m_pc_line;
                    m_file_sp = m_debugger.GetSourceManager().GetFile(m_sc.line_entry.file);
                    if (m_file_sp)
                    {
                        const size_t num_lines = m_file_sp->GetNumLines();
                        int m_line_width = 1;
                        for (size_t n = num_lines; n >= 10; n = n / 10)
                            ++m_line_width;
                        
                        snprintf (m_line_format, sizeof(m_line_format), " %%%iu ", m_line_width);
                        if (num_lines < num_visible_lines || m_selected_line < num_visible_lines)
                            m_first_visible_line = 0;
                        else
                            m_first_visible_line = m_selected_line - 10;
                    }
                }
            }
        }
        else
        {
            m_pc_line = UINT32_MAX;
        }
        
        if (m_file_sp)
        {
            BreakpointLines bp_lines;
            Target *target = exe_ctx.GetTargetPtr();
            if (target)
            {
                BreakpointList &bp_list = target->GetBreakpointList();
                const size_t num_bps = bp_list.GetSize();
                for (size_t bp_idx=0; bp_idx<num_bps; ++bp_idx)
                {
                    BreakpointSP bp_sp = bp_list.GetBreakpointAtIndex(bp_idx);
                    const size_t num_bps_locs = bp_sp->GetNumLocations();
                    for (size_t bp_loc_idx=0; bp_loc_idx<num_bps_locs; ++bp_loc_idx)
                    {
                        BreakpointLocationSP bp_loc_sp = bp_sp->GetLocationAtIndex(bp_loc_idx);
                        LineEntry bp_loc_line_entry;
                        if (bp_loc_sp->GetAddress().CalculateSymbolContextLineEntry (bp_loc_line_entry))
                        {
                            if (m_file_sp->GetFileSpec() == bp_loc_line_entry.file)
                            {
                                bp_lines.insert(bp_loc_line_entry.line);
                            }
                        }
                    }
                }
            }
            
        
            const attr_t selected_highlight_attr = A_REVERSE;
            const attr_t pc_highlight_attr = COLOR_PAIR(1);

            const size_t num_lines = m_file_sp->GetNumLines();
            for (int i=0; i<num_visible_lines; ++i)
            {
                const uint32_t curr_line = m_first_visible_line + i;
                if (curr_line < num_lines)
                {
                    const int line_y = 1+i;
                    window.Move(1, line_y);
                    const bool is_pc_line = curr_line == m_pc_line;
                    const bool line_is_selected = m_selected_line == curr_line;
                    // Highlight the line as the PC line first, then if the selected line
                    // isn't the same as the PC line, highlight it differently
                    attr_t highlight_attr = 0;
                    attr_t bp_attr = 0;
                    if (is_pc_line)
                        highlight_attr = pc_highlight_attr;
                    else if (line_is_selected)
                        highlight_attr = selected_highlight_attr;
                    
                    if (bp_lines.find(curr_line+1) != bp_lines.end())
                        bp_attr = COLOR_PAIR(2);

                    if (bp_attr)
                        window.AttributeOn(bp_attr);
                    
                    window.Printf (m_line_format, curr_line + 1);

                    if (bp_attr)
                        window.AttributeOff(bp_attr);

                    window.PutChar(ACS_VLINE);
                    // Mark the line with the PC with a diamond
                    if (is_pc_line)
                        window.PutChar(ACS_DIAMOND);
                    else
                        window.PutChar(' ');
                    
                    if (highlight_attr)
                        window.AttributeOn(highlight_attr);
                    const uint32_t line_len = m_file_sp->GetLineLength(curr_line + 1, false);
                    if (line_len > 0)
                        window.PutCString(m_file_sp->PeekLineData(curr_line + 1), line_len);

                    if (is_pc_line && frame_sp && frame_sp->GetConcreteFrameIndex() == 0)
                    {
                        StopInfoSP stop_info_sp;
                        if (thread)
                            stop_info_sp = thread->GetStopInfo();
                        if (stop_info_sp)
                        {
                            const char *stop_description = stop_info_sp->GetDescription();
                            if (stop_description && stop_description[0])
                            {
                                size_t stop_description_len = strlen(stop_description);
                                int desc_x = window.GetWidth() - stop_description_len - 16;
                                window.Printf ("%*s", desc_x - window.GetX(), "");
                                //window.Move(window.GetWidth() - stop_description_len - 15, line_y);
                                window.Printf ("<<< Thread %u: %s ", thread->GetIndexID(), stop_description);
                            }
                        }
                        else
                        {
                            window.Printf ("%*s", window.GetWidth() - window.GetX() - 1, "");
                        }
                    }
                    if (highlight_attr)
                        window.AttributeOff(highlight_attr);

                }
                else
                {
                    break;
                }
            }
        }
        window.DeferredRefresh();
        return true; // Drawing handled
    }
    
    virtual HandleCharResult
    WindowDelegateHandleChar (Window &window, int c)
    {
        const uint32_t num_visible_lines = NumVisibleLines();
        const size_t num_lines = m_file_sp ? m_file_sp->GetNumLines() : 0;

        switch (c)
        {
            case ',':
            case KEY_PPAGE:
                // Page up key
                if (m_first_visible_line > num_visible_lines)
                    m_first_visible_line -= num_visible_lines;
                else
                    m_first_visible_line = 0;
                m_selected_line = m_first_visible_line;
                return eKeyHandled;
                
            case '.':
            case KEY_NPAGE:
                // Page down key
                {
                    if (m_first_visible_line + num_visible_lines < num_lines)
                        m_first_visible_line += num_visible_lines;
                    else if (num_lines < num_visible_lines)
                        m_first_visible_line = 0;
                    else
                        m_first_visible_line = num_lines - num_visible_lines;
                    m_selected_line = m_first_visible_line;
                }
                return eKeyHandled;
                
            case KEY_UP:
                if (m_selected_line > 0)
                {
                    m_selected_line--;
                    if (m_first_visible_line > m_selected_line)
                        m_first_visible_line = m_selected_line;
                }
                return eKeyHandled;

            case KEY_DOWN:
                if (m_selected_line + 1 < num_lines)
                {
                    m_selected_line++;
                    if (m_first_visible_line + num_visible_lines < m_selected_line)
                        m_first_visible_line++;
                }
                return eKeyHandled;
                
            case '\r':
            case '\n':
            case KEY_ENTER:
                // Set a breakpoint and run to the line using a one shot breakpoint
                if (m_file_sp && m_selected_line > 0)
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasProcessScope() && exe_ctx.GetProcessRef().IsAlive())
                    {
                        BreakpointSP bp_sp = exe_ctx.GetTargetRef().CreateBreakpoint (NULL,                      // Don't limit the breakpoint to certain modules
                                                                                      m_file_sp->GetFileSpec(),  // Source file
                                                                                      m_selected_line + 1,       // Source line number (m_selected_line is zero based)
                                                                                      eLazyBoolCalculate,        // Check inlines using global setting
                                                                                      eLazyBoolCalculate,        // Skip prologue using global setting,
                                                                                      false,                     // internal
                                                                                      false);                    // request_hardware
                        // Make breakpoint one shot
                        bp_sp->GetOptions()->SetOneShot(true);
                        exe_ctx.GetProcessRef().Resume();
                    }
                }
                return eKeyHandled;

            case 'b':   // 'b' == toggle breakpoint on currently selected line
                if (m_file_sp && m_selected_line > 0)
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasTargetScope())
                    {
                        BreakpointSP bp_sp = exe_ctx.GetTargetRef().CreateBreakpoint (NULL,                      // Don't limit the breakpoint to certain modules
                                                                                      m_file_sp->GetFileSpec(),  // Source file
                                                                                      m_selected_line + 1,       // Source line number (m_selected_line is zero based)
                                                                                      eLazyBoolCalculate,        // Check inlines using global setting
                                                                                      eLazyBoolCalculate,        // Skip prologue using global setting,
                                                                                      false,                     // internal
                                                                                      false);                    // request_hardware
                    }
                }
                return eKeyHandled;

            case 'd':   // 'd' == detach and let run
            case 'D':   // 'D' == detach and keep stopped
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasProcessScope())
                        exe_ctx.GetProcessRef().Detach(c == 'D');
                }
                return eKeyHandled;;

            case 'k':
                // 'k' == kill
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasProcessScope())
                        exe_ctx.GetProcessRef().Destroy();
                }
                return eKeyHandled;

            case 'c':
                // 'c' == continue
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasProcessScope())
                        exe_ctx.GetProcessRef().Resume();
                }
                return eKeyHandled;
                
            case 'o':
                // 'o' == step out
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasThreadScope() && StateIsStoppedState (exe_ctx.GetProcessRef().GetState(), true))
                    {
                        Process *process = exe_ctx.GetProcessPtr();
                        Thread *thread = exe_ctx.GetThreadPtr();
                        bool abort_other_plans = false;
                        bool stop_other_threads = false;
                        ThreadPlanSP new_plan_sp(thread->QueueThreadPlanForStepOut (abort_other_plans,
                                                                                    NULL,
                                                                                    false,
                                                                                    stop_other_threads,
                                                                                    eVoteYes,
                                                                                    eVoteNoOpinion,
                                                                                    0));
                        
                        new_plan_sp->SetIsMasterPlan(true);
                        new_plan_sp->SetOkayToDiscard(false);
                        
                        // Why do we need to set the current thread by ID here???
                        process->GetThreadList().SetSelectedThreadByID (thread->GetID());
                        process->Resume();
                    }
                }
                return eKeyHandled;
            case 'n':
                // 'n' == step over
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasThreadScope() && StateIsStoppedState (exe_ctx.GetProcessRef().GetState(), true))
                    {
                        Process *process = exe_ctx.GetProcessPtr();
                        Thread *thread = exe_ctx.GetThreadPtr();
                        StackFrameSP frame_sp = thread->GetStackFrameAtIndex (0);
                        if (frame_sp)
                        {
                            bool abort_other_plans = false;
                            lldb::RunMode stop_other_threads = eOnlyThisThread;
                            ThreadPlanSP new_plan_sp;
                            
                            if (frame_sp->HasDebugInformation ())
                            {
                                SymbolContext sc(frame_sp->GetSymbolContext(eSymbolContextEverything));
                                new_plan_sp = thread->QueueThreadPlanForStepOverRange (abort_other_plans,
                                                                                       sc.line_entry.range,
                                                                                       sc,
                                                                                       stop_other_threads);
                            }
                            else
                            {
                                new_plan_sp = thread->QueueThreadPlanForStepSingleInstruction (true,
                                                                                               abort_other_plans, 
                                                                                               stop_other_threads);
                            }
                            new_plan_sp->SetIsMasterPlan(true);
                            new_plan_sp->SetOkayToDiscard(false);
                            
                            // Why do we need to set the current thread by ID here???
                            process->GetThreadList().SetSelectedThreadByID (thread->GetID());
                            process->Resume();
                        }
                    }
                }
                return eKeyHandled;
            case 's':
                // 's' == step into
                {
                    ExecutionContext exe_ctx = m_debugger.GetCommandInterpreter().GetExecutionContext();
                    if (exe_ctx.HasThreadScope() && StateIsStoppedState (exe_ctx.GetProcessRef().GetState(), true))
                    {
                        Process *process = exe_ctx.GetProcessPtr();
                        Thread *thread = exe_ctx.GetThreadPtr();
                        StackFrameSP frame_sp = thread->GetStackFrameAtIndex (0);
                        bool abort_other_plans = false;
                        lldb::RunMode stop_other_threads = eOnlyThisThread;
                        ThreadPlanSP new_plan_sp;
                        
                        if (frame_sp && frame_sp->HasDebugInformation ())
                        {
                            bool avoid_code_without_debug_info = true;
                            SymbolContext sc(frame_sp->GetSymbolContext(eSymbolContextEverything));
                            new_plan_sp = thread->QueueThreadPlanForStepInRange (abort_other_plans,
                                                                                 sc.line_entry.range,
                                                                                 sc,
                                                                                 NULL,
                                                                                 stop_other_threads,
                                                                                 avoid_code_without_debug_info);
                        }
                        else
                        {
                            new_plan_sp = thread->QueueThreadPlanForStepSingleInstruction (false,
                                                                                           abort_other_plans, 
                                                                                           stop_other_threads);
                        }
                        
                        new_plan_sp->SetIsMasterPlan(true);
                        new_plan_sp->SetOkayToDiscard(false);
                        
                        // Why do we need to set the current thread by ID here???
                        process->GetThreadList().SetSelectedThreadByID (thread->GetID());
                        process->Resume();
                    }
                }
                return eKeyHandled;
            default:
                break;
        }
        return eKeyNotHandled;
    }
    
protected:
    typedef std::set<uint32_t> BreakpointLines;

    Debugger &m_debugger;
    SymbolContext m_sc;
    SourceManager::FileSP m_file_sp;
    char m_line_format[8];
    int m_line_width;
    uint32_t m_selected_line;       // The selected line
    uint32_t m_pc_line;             // The line with the PC
    uint32_t m_stop_id;
    int m_first_visible_line;
    int m_min_x;
    int m_min_y;
    int m_max_x;
    int m_max_y;

};

DisplayOptions ValueObjectListDelegate::g_options = { true };

IOHandlerCursesGUI::IOHandlerCursesGUI (Debugger &debugger) :
    IOHandler (debugger)
{
}

void
IOHandlerCursesGUI::Activate ()
{
    IOHandler::Activate();
    if (!m_app_ap)
    {
        m_app_ap.reset (new Application (GetInputFILE(), GetOutputFILE()));
        
        
        // This is both a window and a menu delegate
        std::shared_ptr<ApplicationDelegate> app_delegate_sp(new ApplicationDelegate(m_debugger));
        
        
        MenuSP lldb_menu_sp(new Menu("LLDB" , "F1", KEY_F(1)));
        MenuSP exit_menuitem_sp(new Menu("Exit", NULL, 'x'));
        exit_menuitem_sp->SetCannedResult(MenuActionResult::Quit);
        lldb_menu_sp->AddSubmenu (MenuSP (new Menu("About LLDB", NULL, 'a')));
        lldb_menu_sp->AddSubmenu (MenuSP (new Menu(Menu::Type::Separator)));
        lldb_menu_sp->AddSubmenu (exit_menuitem_sp);
        
        MenuSP target_menu_sp(new Menu("Target" ,"F2", KEY_F(2)));
        target_menu_sp->AddSubmenu (MenuSP (new Menu("Create", NULL, 'c')));
        target_menu_sp->AddSubmenu (MenuSP (new Menu("Delete", NULL, 'd')));
        
        MenuSP process_menu_sp(new Menu("Process", "F3", KEY_F(3)));
        process_menu_sp->SetDelegate(std::static_pointer_cast<MenuDelegate>(app_delegate_sp));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Attach"  , NULL, 'a')));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Detach"  , NULL, 'd')));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Launch"  , NULL, 'l')));
        process_menu_sp->AddSubmenu (MenuSP (new Menu(Menu::Type::Separator)));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Continue", NULL, 'c')));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Halt"    , NULL, 'h')));
        process_menu_sp->AddSubmenu (MenuSP (new Menu("Kill"    , NULL, 'k')));
        
        MenuSP thread_menu_sp(new Menu("Thread", "F4", KEY_F(4)));
        thread_menu_sp->AddSubmenu (MenuSP (new Menu("Step In"  , NULL, 'i')));
        thread_menu_sp->AddSubmenu (MenuSP (new Menu("Step Over", NULL, 'v')));
        thread_menu_sp->AddSubmenu (MenuSP (new Menu("Step Out" , NULL, 'o')));
        
        MenuSP view_menu_sp(new Menu("View", "F5", KEY_F(5)));
        view_menu_sp->AddSubmenu (MenuSP (new Menu("Backtrace", NULL, 'b')));
        view_menu_sp->AddSubmenu (MenuSP (new Menu("Registers", NULL, 'r')));
        view_menu_sp->AddSubmenu (MenuSP (new Menu("Source"   , NULL, 's')));
        view_menu_sp->AddSubmenu (MenuSP (new Menu("Threads"  , NULL, 't')));
        view_menu_sp->AddSubmenu (MenuSP (new Menu("Variables", NULL, 'v')));
        
        MenuSP help_menu_sp(new Menu("Help", "F6", KEY_F(6)));
        help_menu_sp->AddSubmenu (MenuSP (new Menu("GUI Help", NULL, 'g')));
        
        m_app_ap->Initialize();
        WindowSP &main_window_sp = m_app_ap->GetMainWindow();
        
        MenuSP menubar_sp(new Menu(Menu::Type::Bar));
        menubar_sp->AddSubmenu (lldb_menu_sp);
        menubar_sp->AddSubmenu (target_menu_sp);
        menubar_sp->AddSubmenu (process_menu_sp);
        menubar_sp->AddSubmenu (thread_menu_sp);
        menubar_sp->AddSubmenu (view_menu_sp);
        menubar_sp->AddSubmenu (help_menu_sp);
        
        WindowSP menubar_window_sp = main_window_sp->CreateSubWindow("menubar", 1, main_window_sp->GetWidth(), 0, 0, false);
        // Let the menubar get keys if the active window doesn't handle the
        // keys that are typed so it can respond to menubar key presses.
        menubar_window_sp->SetCanBeActive(false); // Don't let the menubar become the active window
        menubar_window_sp->SetDelegate(menubar_sp);
        init_pair (1, COLOR_WHITE   , COLOR_BLUE  );
        init_pair (2, COLOR_BLACK   , COLOR_WHITE );
        init_pair (3, COLOR_MAGENTA , COLOR_WHITE );
        init_pair (4, COLOR_MAGENTA , COLOR_BLACK );
        init_pair (5, COLOR_RED     , COLOR_BLACK );
        
        const int main_window_view_h = main_window_sp->GetHeight() - 1; // Subtract 1 for menubar
        const int main_window_view_w = main_window_sp->GetWidth();
        int source_window_height = (main_window_view_h / 3) * 2;
        int locals_window_height = main_window_view_h - source_window_height;
        WindowSP source_window_sp (main_window_sp->CreateSubWindow("source",
                                                                   source_window_height,
                                                                   main_window_view_w,
                                                                   1,
                                                                   0,
                                                                   true));
        WindowSP locals_window_sp (main_window_sp->CreateSubWindow("locals",
                                                                   locals_window_height - 1,
                                                                   main_window_view_w,
                                                                   1 + source_window_height,
                                                                   0,
                                                                   false));
        WindowSP status_window_sp (main_window_sp->CreateSubWindow("status",
                                                                   1,
                                                                   main_window_view_w,
                                                                   source_window_height + locals_window_height,
                                                                   0,
                                                                   false));
        status_window_sp->SetCanBeActive(false); // Don't let the status bar become the active window
        main_window_sp->SetDelegate (std::static_pointer_cast<WindowDelegate>(app_delegate_sp));
        source_window_sp->SetDelegate (WindowDelegateSP(new SourceFileWindowDelegate(m_debugger)));
        locals_window_sp->SetDelegate (WindowDelegateSP(new FrameVariablesWindowDelegate(m_debugger)));
        status_window_sp->SetDelegate (WindowDelegateSP(new StatusBarWindowDelegate(m_debugger)));
    }
}

void
IOHandlerCursesGUI::Deactivate ()
{
    m_app_ap->Terminate();
}

void
IOHandlerCursesGUI::Run ()
{
    m_app_ap->Run(m_debugger);
    SetIsDone(true);
}


IOHandlerCursesGUI::~IOHandlerCursesGUI ()
{
    
}

void
IOHandlerCursesGUI::Hide ()
{
}


void
IOHandlerCursesGUI::Refresh ()
{
}


void
IOHandlerCursesGUI::Interrupt ()
{
}


void
IOHandlerCursesGUI::GotEOF()
{
}

