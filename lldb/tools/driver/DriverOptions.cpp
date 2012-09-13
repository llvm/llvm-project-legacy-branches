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

#include "lldb/API/SBHostOS.h"
using namespace lldb;

typedef struct
{
    uint32_t usage_mask;                     // Used to mark options that can be used together.  If (1 << n & usage_mask) != 0
                                             // then this option belongs to option set n.
    bool required;                           // This option is required (in the current usage level)
    const char * long_option;                // Full name for this option.
    char short_option;                       // Single character for this option.
    int option_has_arg;                      // no_argument, required_argument or optional_argument
    uint32_t completion_type;                // Cookie the option class can use to do define the argument completion.
    lldb::CommandArgumentType argument_type; // Type of argument this option takes
    const char *  usage_text;                // Full text explaining what this options does and what (if any) argument to
                                             // pass it.
} OptionDefinition;

#define LLDB_3_TO_5 LLDB_OPT_SET_3|LLDB_OPT_SET_4|LLDB_OPT_SET_5
#define LLDB_4_TO_5 LLDB_OPT_SET_4|LLDB_OPT_SET_5

static OptionDefinition g_options[] =
{
    { LLDB_OPT_SET_1,    true , "help"           , 'h', no_argument      , NULL,  eArgTypeNone,         
        "Prints out the usage information for the LLDB debugger." },
    { LLDB_OPT_SET_2,    true , "version"        , 'v', no_argument      , NULL,  eArgTypeNone,         
        "Prints out the current version number of the LLDB debugger." },
    { LLDB_OPT_SET_3,    true , "arch"           , 'a', required_argument, NULL,  eArgTypeArchitecture, 
        "Tells the debugger to use the specified architecture when starting and running the program.  <architecture> must "
        "be one of the architectures for which the program was compiled." },
    { LLDB_OPT_SET_3,    true , "file"           , 'f', required_argument, NULL,  eArgTypeFilename,     
        "Tells the debugger to use the file <filename> as the program to be debugged." },
    { LLDB_OPT_SET_4,    true , "attach-name"    , 'n', required_argument, NULL,  eArgTypeProcessName,  
        "Tells the debugger to attach to a process with the given name." },
    { LLDB_OPT_SET_4,    true , "wait-for"       , 'w', no_argument      , NULL,  eArgTypeNone,         
        "Tells the debugger to wait for a process with the given pid or name to launch before attaching." },
    { LLDB_OPT_SET_5,    true , "attach-pid"     , 'p', required_argument, NULL,  eArgTypePid,          
        "Tells the debugger to attach to a process with the given pid." },
    { LLDB_3_TO_5,       false, "script-language", 'l', required_argument, NULL,  eArgTypeScriptLang,   
        "Tells the debugger to use the specified scripting language for user-defined scripts, rather than the default.  "
        "Valid scripting languages that can be specified include Python, Perl, Ruby and Tcl.  Currently only the Python "
        "extensions have been implemented." },
    { LLDB_3_TO_5,       false, "debug"          , 'd', no_argument      , NULL,  eArgTypeNone,         
        "Tells the debugger to print out extra information for debugging itself." },
    { LLDB_3_TO_5,       false, "source"         , 's', required_argument, NULL,  eArgTypeFilename,     
        "Tells the debugger to read in and execute the file <file>, which should contain lldb commands." },
    { LLDB_3_TO_5,       false, "editor"         , 'e', no_argument      , NULL,  eArgTypeNone,         
        "Tells the debugger to open source files using the host's \"external editor\" mechanism." },
    { LLDB_3_TO_5,       false, "no-lldbinit"    , 'x', no_argument      , NULL,  eArgTypeNone,         
        "Do not automatically parse any '.lldbinit' files." },
    { 0,                 false, NULL             , 0  , 0                , NULL,  eArgTypeNone,         NULL }
};

static const uint32_t last_option_set_with_args = 2;

// This function takes INDENT, which tells how many spaces to output at the front
// of each line; TEXT, which is the text that is to be output. It outputs the 
// text, on multiple lines if necessary, to RESULT, with INDENT spaces at the 
// front of each line.  It breaks lines on spaces, tabs or newlines, shortening 
// the line if necessary to not break in the middle of a word. It assumes that 
// each output line should contain a maximum of OUTPUT_MAX_COLUMNS characters.

void
OutputFormattedUsageText (FILE *out, int indent, const char *text, int output_max_columns)
{
    int len = strlen (text);
    std::string text_string (text);

    // Force indentation to be reasonable.
    if (indent >= output_max_columns)
        indent = 0;

    // Will it all fit on one line?

    if (len + indent < output_max_columns)
        // Output as a single line
        fprintf (out, "%*s%s\n", indent, "", text);
    else
    {
        // We need to break it up into multiple lines.
        int text_width = output_max_columns - indent - 1;
        int start = 0;
        int end = start;
        int final_end = len;
        int sub_len;

        while (end < final_end)
        {
              // Dont start the 'text' on a space, since we're already outputting the indentation.
              while ((start < final_end) && (text[start] == ' '))
                  start++;

              end = start + text_width;
              if (end > final_end)
                  end = final_end;
              else
              {
                  // If we're not at the end of the text, make sure we break the line on white space.
                  while (end > start
                         && text[end] != ' ' && text[end] != '\t' && text[end] != '\n')
                      end--;
              }
              sub_len = end - start;
              std::string substring = text_string.substr (start, sub_len);
              fprintf (out, "%*s%s\n", indent, "", substring.c_str());
              start = end + 1;
        }
    }
}

void
ShowUsage (FILE *out, OptionDefinition *option_table, Driver::OptionData data)
{
    uint32_t screen_width = 80;
    uint32_t indent_level = 0;
    const char *name = "lldb";
    
    fprintf (out, "\nUsage:\n\n");

    indent_level += 2;


    // First, show each usage level set of options, e.g. <cmd> [options-for-level-0]
    //                                                   <cmd> [options-for-level-1]
    //                                                   etc.

    uint32_t num_options;
    uint32_t num_option_sets = 0;
    
    for (num_options = 0; option_table[num_options].long_option != NULL; ++num_options)
    {
        uint32_t this_usage_mask = option_table[num_options].usage_mask;
        if (this_usage_mask == LLDB_OPT_SET_ALL)
        {
            if (num_option_sets == 0)
                num_option_sets = 1;
        }
        else
        {
            for (uint32_t j = 0; j < LLDB_MAX_NUM_OPTION_SETS; j++)
            {
                if (this_usage_mask & 1 << j)
                {
                    if (num_option_sets <= j)
                        num_option_sets = j + 1;
                }
            }
        }
    }

    for (uint32_t opt_set = 0; opt_set < num_option_sets; opt_set++)
    {
        uint32_t opt_set_mask;
        
        opt_set_mask = 1 << opt_set;
        
        if (opt_set > 0)
            fprintf (out, "\n");
        fprintf (out, "%*s%s", indent_level, "", name);
        bool is_help_line = false;
        
        for (uint32_t i = 0; i < num_options; ++i)
        {
            if (option_table[i].usage_mask & opt_set_mask)
            {
                CommandArgumentType arg_type = option_table[i].argument_type;
                const char *arg_name = SBCommandInterpreter::GetArgumentTypeAsCString (arg_type);
                // This is a bit of a hack, but there's no way to say certain options don't have arguments yet...
                // so we do it by hand here.
                if (option_table[i].short_option == 'h')
                    is_help_line = true;
                    
                if (option_table[i].required)
                {
                    if (option_table[i].option_has_arg == required_argument)
                        fprintf (out, " -%c <%s>", option_table[i].short_option, arg_name);
                    else if (option_table[i].option_has_arg == optional_argument)
                        fprintf (out, " -%c [<%s>]", option_table[i].short_option, arg_name);
                    else
                        fprintf (out, " -%c", option_table[i].short_option);
                }
                else
                {
                    if (option_table[i].option_has_arg == required_argument)
                        fprintf (out, " [-%c <%s>]", option_table[i].short_option, arg_name);
                    else if (option_table[i].option_has_arg == optional_argument)
                        fprintf (out, " [-%c [<%s>]]", option_table[i].short_option, arg_name);
                    else
                        fprintf (out, " [-%c]", option_table[i].short_option);
                }
            }
        }
        if (!is_help_line && (opt_set <= last_option_set_with_args))
            fprintf (out, " [[--] <PROGRAM-ARG-1> [<PROGRAM_ARG-2> ...]]");
    }

    fprintf (out, "\n\n");

    // Now print out all the detailed information about the various options:  long form, short form and help text:
    //   -- long_name <argument>
    //   - short <argument>
    //   help text

    // This variable is used to keep track of which options' info we've printed out, because some options can be in
    // more than one usage level, but we only want to print the long form of its information once.

    Driver::OptionData::OptionSet options_seen;
    Driver::OptionData::OptionSet::iterator pos;

    indent_level += 5;

    for (uint32_t i = 0; i < num_options; ++i)
    {
        // Only print this option if we haven't already seen it.
        pos = options_seen.find (option_table[i].short_option);
        if (pos == options_seen.end())
        {
            CommandArgumentType arg_type = option_table[i].argument_type;
            const char *arg_name = SBCommandInterpreter::GetArgumentTypeAsCString (arg_type);

            options_seen.insert (option_table[i].short_option);
            fprintf (out, "%*s-%c ", indent_level, "", option_table[i].short_option);
            if (arg_type != eArgTypeNone)
                fprintf (out, "<%s>", arg_name);
            fprintf (out, "\n");
            fprintf (out, "%*s--%s ", indent_level, "", option_table[i].long_option);
            if (arg_type != eArgTypeNone)
                fprintf (out, "<%s>", arg_name);
            fprintf (out, "\n");
            indent_level += 5;
            OutputFormattedUsageText (out, indent_level, option_table[i].usage_text, screen_width);
            indent_level -= 5;
            fprintf (out, "\n");
        }
    }

    indent_level -= 5;

    fprintf (out, "\n%*s(If you don't provide -f then the first argument will be the file to be debugged"
                  "\n%*s so '%s -- <filename> [<ARG1> [<ARG2>]]' also works."
                  "\n%*s Remember to end the options with \"--\" if any of your arguments have a \"-\" in them.)\n\n",
             indent_level, "", 
             indent_level, "",
             name, 
             indent_level, "");
}

void
BuildGetOptTable (OptionDefinition *expanded_option_table, std::vector<struct option> &getopt_table, 
                  uint32_t num_options)
{
    if (num_options == 0)
        return;

    uint32_t i;
    uint32_t j;
    std::bitset<256> option_seen;

    getopt_table.resize (num_options + 1);

    for (i = 0, j = 0; i < num_options; ++i)
    {
        char short_opt = expanded_option_table[i].short_option;
        
        if (option_seen.test(short_opt) == false)
        {
            getopt_table[j].name    = expanded_option_table[i].long_option;
            getopt_table[j].has_arg = expanded_option_table[i].option_has_arg;
            getopt_table[j].flag    = NULL;
            getopt_table[j].val     = expanded_option_table[i].short_option;
            option_seen.set(short_opt);
            ++j;
        }
    }

    getopt_table[j].name    = NULL;
    getopt_table[j].has_arg = 0;
    getopt_table[j].flag    = NULL;
    getopt_table[j].val     = 0;

}

Driver::OptionData::OptionData () :
    m_args(),
    m_script_lang (lldb::eScriptLanguageDefault),
    m_crash_log (),
    m_source_command_files (),
    m_debug_mode (false),
    m_print_version (false),
    m_print_help (false),
    m_wait_for(false),
    m_process_name(),
    m_process_pid(LLDB_INVALID_PROCESS_ID),
    m_use_external_editor(false),
    m_seen_options()
{
}

Driver::OptionData::~OptionData ()
{
}

void
Driver::OptionData::Clear ()
{
    m_args.clear ();
    m_script_lang = lldb::eScriptLanguageDefault;
    m_source_command_files.clear ();
    m_debug_mode = false;
    m_print_help = false;
    m_print_version = false;
    m_use_external_editor = false;
    m_wait_for = false;
    m_process_name.erase();
    m_process_pid = LLDB_INVALID_PROCESS_ID;
}

void
Driver::ResetOptionValues ()
{
    m_option_data.Clear ();
}

// Check the arguments that were passed to this program to make sure they are valid and to get their
// argument values (if any).  Return a boolean value indicating whether or not to start up the full
// debugger (i.e. the Command Interpreter) or not.  Return FALSE if the arguments were invalid OR
// if the user only wanted help or version information.

SBError
Driver::ParseArgs (int argc, const char *argv[], FILE *out_fh, bool &exit)
{
    ResetOptionValues ();

    SBCommandReturnObject result;

    SBError error;
    std::string option_string;
    struct option *long_options = NULL;
    std::vector<struct option> long_options_vector;
    uint32_t num_options;

    for (num_options = 0; g_options[num_options].long_option != NULL; ++num_options)
        /* Do Nothing. */;

    if (num_options == 0)
    {
        if (argc > 1)
            error.SetErrorStringWithFormat ("invalid number of options");
        return error;
    }

    BuildGetOptTable (g_options, long_options_vector, num_options);

    if (long_options_vector.empty())
        long_options = NULL;
    else
        long_options = &long_options_vector.front();

    if (long_options == NULL)
    {
        error.SetErrorStringWithFormat ("invalid long options");
        return error;
    }

    // Build the option_string argument for call to getopt_long.

    for (int i = 0; long_options[i].name != NULL; ++i)
    {
        if (long_options[i].flag == NULL)
        {
            option_string.push_back ((char) long_options[i].val);
            switch (long_options[i].has_arg)
            {
                default:
                case no_argument:
                    break;
                case required_argument:
                    option_string.push_back (':');
                    break;
                case optional_argument:
                    option_string.append ("::");
                    break;
            }
        }
    }

    // This is kind of a pain, but since we make the debugger in the Driver's constructor, we can't
    // know at that point whether we should read in init files yet.  So we don't read them in in the
    // Driver constructor, then set the flags back to "read them in" here, and then if we see the
    // "-n" flag, we'll turn it off again.  Finally we have to read them in by hand later in the
    // main loop.
    
    m_debugger.SkipLLDBInitFiles (false);
    m_debugger.SkipAppInitFiles (false);

    // Prepare for & make calls to getopt_long.
#if __GLIBC__
    optind = 0;
#else
    optreset = 1;
    optind = 1;
#endif
    int val;
    while (1)
    {
        int long_options_index = -1;
        val = ::getopt_long (argc, const_cast<char **>(argv), option_string.c_str(), long_options, &long_options_index);

        if (val == -1)
            break;
        else if (val == '?')
        {
            m_option_data.m_print_help = true;
            error.SetErrorStringWithFormat ("unknown or ambiguous option");
            break;
        }
        else if (val == 0)
            continue;
        else
        {
            m_option_data.m_seen_options.insert ((char) val);
            if (long_options_index == -1)
            {
                for (int i = 0;
                     long_options[i].name || long_options[i].has_arg || long_options[i].flag || long_options[i].val;
                     ++i)
                {
                    if (long_options[i].val == val)
                    {
                        long_options_index = i;
                        break;
                    }
                }
            }

            if (long_options_index >= 0)
            {
                const char short_option = (char) g_options[long_options_index].short_option;

                switch (short_option)
                {
                    case 'h':
                        m_option_data.m_print_help = true;
                        break;

                    case 'v':
                        m_option_data.m_print_version = true;
                        break;

                    case 'c':
                        m_option_data.m_crash_log = optarg;
                        break;

                    case 'e':
                        m_option_data.m_use_external_editor = true;
                        break;

                    case 'x':
                        m_debugger.SkipLLDBInitFiles (true);
                        m_debugger.SkipAppInitFiles (true);
                        break;

                    case 'f':
                        {
                            SBFileSpec file(optarg);
                            if (file.Exists())
                            {
                                m_option_data.m_args.push_back (optarg);
                            }
                            else if (file.ResolveExecutableLocation())
                            {
                                char path[PATH_MAX];
                                file.GetPath (path, sizeof(path));
                                m_option_data.m_args.push_back (path);
                            }
                            else
                                error.SetErrorStringWithFormat("file specified in --file (-f) option doesn't exist: '%s'", optarg);
                        }
                        break;

                    case 'a':
                        if (!m_debugger.SetDefaultArchitecture (optarg))
                            error.SetErrorStringWithFormat("invalid architecture in the -a or --arch option: '%s'", optarg);
                        break;

                    case 'l':
                        m_option_data.m_script_lang = m_debugger.GetScriptingLanguage (optarg);
                        break;

                    case 'd':
                        m_option_data.m_debug_mode = true;
                        break;

                    case 'n':
                        m_option_data.m_process_name = optarg;
                        break;
                    
                    case 'w':
                        m_option_data.m_wait_for = true;
                        break;
                        
                    case 'p':
                        {
                            char *remainder;
                            m_option_data.m_process_pid = strtol (optarg, &remainder, 0);
                            if (remainder == optarg || *remainder != '\0')
                                error.SetErrorStringWithFormat ("Could not convert process PID: \"%s\" into a pid.",
                                                                optarg);
                        }
                        break;
                    case 's':
                        {
                            SBFileSpec file(optarg);
                            if (file.Exists())
                                m_option_data.m_source_command_files.push_back (optarg);
                            else if (file.ResolveExecutableLocation())
                            {
                                char final_path[PATH_MAX];
                                file.GetPath (final_path, sizeof(final_path));
                                std::string path_str (final_path);
                                m_option_data.m_source_command_files.push_back (path_str);
                            }
                            else
                                error.SetErrorStringWithFormat("file specified in --source (-s) option doesn't exist: '%s'", optarg);
                        }
                        break;

                    default:
                        m_option_data.m_print_help = true;
                        error.SetErrorStringWithFormat ("unrecognized option %c", short_option);
                        break;
                }
            }
            else
            {
                error.SetErrorStringWithFormat ("invalid option with value %i", val);
            }
            if (error.Fail())
            {
                return error;
            }
        }
    }
    
    if (error.Fail() || m_option_data.m_print_help)
    {
        ShowUsage (out_fh, g_options, m_option_data);
        exit = true;
    }
    else if (m_option_data.m_print_version)
    {
        ::fprintf (out_fh, "%s\n", m_debugger.GetVersionString());
        exit = true;
    }
    else if (! m_option_data.m_crash_log.empty())
    {
        // Handle crash log stuff here.
    }
    else if (m_option_data.m_process_name.empty() && m_option_data.m_process_pid == LLDB_INVALID_PROCESS_ID)
    {
        // Any arguments that are left over after option parsing are for
        // the program. If a file was specified with -f then the filename
        // is already in the m_option_data.m_args array, and any remaining args
        // are arguments for the inferior program. If no file was specified with
        // -f, then what is left is the program name followed by any arguments.

        // Skip any options we consumed with getopt_long
        argc -= optind;
        argv += optind;

        if (argc > 0)
        {
            for (int arg_idx=0; arg_idx<argc; ++arg_idx)
            {
                const char *arg = argv[arg_idx];
                if (arg)
                    m_option_data.m_args.push_back (arg);
            }
        }
        
    }
    else
    {
        // Skip any options we consumed with getopt_long
        argc -= optind;
        //argv += optind; // Commented out to keep static analyzer happy

        if (argc > 0)
            ::fprintf (out_fh, "Warning: program arguments are ignored when attaching.\n");
    }

    return error;
}

void
Driver::HandleCommandLine(SBCommandReturnObject& result)
{
    // Now we handle options we got from the command line
    char command_string[PATH_MAX * 2];
    const size_t num_source_command_files = GetNumSourceCommandFiles();
    if (num_source_command_files > 0)
    {
        for (size_t i=0; i < num_source_command_files; ++i)
        {
            const char *command_file = GetSourceCommandFileAtIndex(i);
            ::snprintf (command_string, sizeof(command_string), "command source '%s'", command_file);
            m_debugger.GetCommandInterpreter().HandleCommand (command_string, result, false);
            if (GetDebugMode())
            {
                result.PutError (m_debugger.GetErrorFileHandle());
                result.PutOutput (m_debugger.GetOutputFileHandle());
            }
        }
    }

    const size_t num_args = m_option_data.m_args.size();
    
    if (!num_args)
        return;

    char arch_name[64];
    if (m_debugger.GetDefaultArchitecture (arch_name, sizeof (arch_name)))
        ::snprintf (command_string, 
                    sizeof (command_string), 
                    "target create --arch=%s \"%s\"", 
                    arch_name,
                    m_option_data.m_args[0].c_str());
    else
        ::snprintf (command_string, 
                    sizeof(command_string), 
                    "target create \"%s\"", 
                    m_option_data.m_args[0].c_str());

    m_debugger.HandleCommand (command_string);
                
    if (num_args > 1)
    {
        m_debugger.HandleCommand ("settings clear target.run-args");
        char arg_cstr[1024];
        for (size_t arg_idx = 1; arg_idx < num_args; ++arg_idx)
        {
            ::snprintf (arg_cstr, 
                        sizeof(arg_cstr), 
                        "settings append target.run-args \"%s\"", 
                        m_option_data.m_args[arg_idx].c_str());
            m_debugger.HandleCommand (arg_cstr);
        }
    }
}

