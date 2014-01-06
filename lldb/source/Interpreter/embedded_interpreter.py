import __builtin__
import code
import lldb
import sys
import traceback

try:
    import readline
    import rlcompleter
except ImportError:
    have_readline = False
else:
    have_readline = True
    if 'libedit' in readline.__doc__:
        readline.parse_and_bind('bind ^I rl_complete')
    else:
        readline.parse_and_bind('tab: complete')

g_builtin_override_called = False

class LLDBQuitter(object):
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        self()
    def __call__(self, code=None):
        global g_builtin_override_called
        g_builtin_override_called = True
        raise SystemExit(-1)

def setquit():
    '''Redefine builtin functions 'quit()' and 'exit()' to print a message and raise an EOFError exception.'''
    # This function will be called prior to each interactive
    # interpreter loop or each single line, so we set the global
    # g_builtin_override_called to False so we know if a SystemExit
    # is thrown, we can catch it and tell the difference between
    # a call to "quit()" or "exit()" and something like
    # "sys.exit(123)"
    global g_builtin_override_called
    g_builtin_override_called = False
    __builtin__.quit = LLDBQuitter('quit')
    __builtin__.exit = LLDBQuitter('exit')

def run_python_interpreter (dict):
    global g_builtin_override_called
    setquit()
    try:
        code.interact ("Python Interactive Interpreter. To exit, type 'quit()', 'exit()' or Ctrl-D.", None, dict)
    except SystemExit as e:
        if not g_builtin_override_called:
            print 'Script exited with %s' %(e)

# When running one line, we might place the string to run in this string
# in case it would be hard to correctly escape a string's contents

g_run_one_line_str = None

def run_python_interpreter (dict):
   # Pass in the dictionary, for continuity from one session to the next.
   code.interact(banner="Python Interactive Interpreter. To exit, type 'quit()', 'exit()' or Ctrl-D.", local=dict)

def run_one_line (dict, input_string):
    global g_builtin_override_called
    global g_run_one_line_str
    setquit()
    try:
        repl = code.InteractiveConsole(dict);
        if input_string:
            repl.runsource (input_string)
        elif g_run_one_line_str:
            repl.runsource (g_run_one_line_str)

    except SystemExit as e:
        if not g_builtin_override_called:
            print 'Script exited with %s' %(e)
