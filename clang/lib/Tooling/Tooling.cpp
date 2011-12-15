//===--- Tooling.cpp - Running clang standalone tools ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements functions to run clang tools standalone instead
//  of running them as a plugin.
//
//===----------------------------------------------------------------------===//

#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/JSONParser.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include <map>
#include <cstdio>

namespace clang {
namespace tooling {

FrontendActionFactory::~FrontendActionFactory() {}

// FIXME: This file contains structural duplication with other parts of the
// code that sets up a compiler to run tools on it, and we should refactor
// it to be based on the same framework.

// Exists solely for the purpose of lookup of the main executable.
static int StaticSymbol;

/// \brief Builds a clang driver initialized for running clang tools.
static clang::driver::Driver *NewDriver(clang::DiagnosticsEngine *Diagnostics,
                                        const char *BinaryName) {
  // This just needs to be some symbol in the binary.
  void *const SymbolAddr = &StaticSymbol;
  const llvm::sys::Path ExePath =
      llvm::sys::Path::GetMainExecutable(BinaryName, SymbolAddr);

  const std::string DefaultOutputName = "a.out";
  clang::driver::Driver *CompilerDriver = new clang::driver::Driver(
      ExePath.str(), llvm::sys::getDefaultTargetTriple(),
      DefaultOutputName, false, *Diagnostics);
  CompilerDriver->setTitle("clang_based_tool");
  return CompilerDriver;
}

/// \brief Retrieves the clang CC1 specific flags out of the compilation's jobs.
/// Returns NULL on error.
static const clang::driver::ArgStringList *GetCC1Arguments(
    clang::DiagnosticsEngine *Diagnostics,
    clang::driver::Compilation *Compilation) {
  // We expect to get back exactly one Command job, if we didn't something
  // failed. Extract that job from the Compilation.
  const clang::driver::JobList &Jobs = Compilation->getJobs();
  if (Jobs.size() != 1 || !isa<clang::driver::Command>(*Jobs.begin())) {
    llvm::SmallString<256> error_msg;
    llvm::raw_svector_ostream error_stream(error_msg);
    Compilation->PrintJob(error_stream, Compilation->getJobs(), "; ", true);
    Diagnostics->Report(clang::diag::err_fe_expected_compiler_job)
        << error_stream.str();
    return NULL;
  }

  // The one job we find should be to invoke clang again.
  const clang::driver::Command *Cmd =
      cast<clang::driver::Command>(*Jobs.begin());
  if (llvm::StringRef(Cmd->getCreator().getName()) != "clang") {
    Diagnostics->Report(clang::diag::err_fe_expected_clang_command);
    return NULL;
  }

  return &Cmd->getArguments();
}

/// \brief Returns a clang build invocation initialized from the CC1 flags.
static clang::CompilerInvocation *NewInvocation(
    clang::DiagnosticsEngine *Diagnostics,
    const clang::driver::ArgStringList &CC1Args) {
  clang::CompilerInvocation *Invocation = new clang::CompilerInvocation;
  clang::CompilerInvocation::CreateFromArgs(
      *Invocation, CC1Args.data() + 1, CC1Args.data() + CC1Args.size(),
      *Diagnostics);
  Invocation->getFrontendOpts().DisableFree = false;
  return Invocation;
}

/// \brief Converts a string vector representing a Command line into a C
/// string vector representing the Argv (including the trailing NULL).
std::vector<char*> CommandLineToArgv(const std::vector<std::string> *Command) {
  std::vector<char*> Result(Command->size() + 1);
  for (std::vector<char*>::size_type I = 0; I < Command->size(); ++I) {
    Result[I] = const_cast<char*>((*Command)[I].c_str());
  }
  Result[Command->size()] = NULL;
  return Result;
}

bool RunSyntaxOnlyToolOnCode(
    clang::FrontendAction *ToolAction, llvm::StringRef Code) {
  const char *const FileName = "input.cc";
  const char *const CommandLine[] = {
      "clang-tool", "-fsyntax-only", FileName
  };
  std::map<std::string, std::string> FileContents;
  FileContents[FileName] = Code;

  FileManager Files((FileSystemOptions()));
  ToolInvocation Invocation(
      std::vector<std::string>(
          CommandLine,
          CommandLine + sizeof(CommandLine)/sizeof(CommandLine[0])),
      ToolAction, &Files);
  Invocation.MapVirtualFiles(FileContents);
  return Invocation.Run();
}

namespace {

// A parser for JSON escaped strings of command line arguments with \-escaping
// for quoted arguments (see the documentation of UnescapeJsonCommandLine(...)).
class CommandLineArgumentParser {
 public:
  CommandLineArgumentParser(llvm::StringRef CommandLine)
      : Input(CommandLine), Position(Input.begin()-1) {}

  std::vector<std::string> Parse() {
    bool HasMoreInput = true;
    while (HasMoreInput && nextNonWhitespace()) {
      std::string Argument;
      HasMoreInput = ParseStringInto(Argument);
      CommandLine.push_back(Argument);
    }
    return CommandLine;
  }

 private:
  // All private methods return true if there is more input available.

  bool ParseStringInto(std::string &String) {
    do {
      if (*Position == '"') {
        if (!ParseQuotedStringInto(String)) return false;
      } else {
        if (!ParseFreeStringInto(String)) return false;
      }
    } while (*Position != ' ');
    return true;
  }

  bool ParseQuotedStringInto(std::string &String) {
    if (!Next()) return false;
    while (*Position != '"') {
      if (!SkipEscapeCharacter()) return false;
      String.push_back(*Position);
      if (!Next()) return false;
    }
    return Next();
  }

  bool ParseFreeStringInto(std::string &String) {
    do {
      if (!SkipEscapeCharacter()) return false;
      String.push_back(*Position);
      if (!Next()) return false;
    } while (*Position != ' ' && *Position != '"');
    return true;
  }

  bool SkipEscapeCharacter() {
    if (*Position == '\\') {
      return Next();
    }
    return true;
  }

  bool nextNonWhitespace() {
    do {
      if (!Next()) return false;
    } while (*Position == ' ');
    return true;
  }

  bool Next() {
    ++Position;
    if (Position == Input.end()) return false;
    // Remove the JSON escaping first. This is done unconditionally.
    if (*Position == '\\') ++Position;
    return Position != Input.end();
  }

  const llvm::StringRef Input;
  llvm::StringRef::iterator Position;
  std::vector<std::string> CommandLine;
};

} // end namespace

std::vector<std::string> UnescapeJsonCommandLine(
    llvm::StringRef JsonEscapedCommandLine) {
  CommandLineArgumentParser parser(JsonEscapedCommandLine);
  return parser.Parse();
}

CompileCommand FindCompileArgsInJsonDatabase(
    llvm::StringRef FileName, llvm::StringRef JsonDatabase,
    std::string &ErrorMessage) {
  llvm::JSONParser Parser(JsonDatabase);
  llvm::JSONValue *Root = Parser.parseRoot();
  if (Root == NULL) {
    ErrorMessage = Parser.getErrorMessage();
    return CompileCommand();
  }
  llvm::JSONArray *Array = llvm::dyn_cast<llvm::JSONArray>(Root);
  if (Array == NULL) {
    ErrorMessage = "Expected array.";
    return CompileCommand();
  }
  for (llvm::JSONArray::const_iterator AI = Array->begin(), AE = Array->end();
       AI != AE; ++AI) {
    const llvm::JSONObject *Object = llvm::dyn_cast<llvm::JSONObject>(*AI);
    if (Object == NULL) {
      ErrorMessage = "Expected object.";
      return CompileCommand();
    }
    llvm::StringRef EntryDirectory;
    llvm::StringRef EntryFile;
    llvm::StringRef EntryCommand;
    for (llvm::JSONObject::const_iterator KVI = Object->begin(),
                                          KVE = Object->end();
         KVI != KVE; ++KVI) {
      const llvm::JSONValue *Value = (*KVI)->Value;
      if (Value == NULL) {
        ErrorMessage = "Expected value.";
        return CompileCommand();
      }
      const llvm::JSONString *ValueString =
        llvm::dyn_cast<llvm::JSONString>(Value);
      if (ValueString == NULL) {
        ErrorMessage = "Expected string as value.";
        return CompileCommand();
      }
      if ((*KVI)->Key->getRawText() == "directory") {
        EntryDirectory = ValueString->getRawText();
      } else if ((*KVI)->Key->getRawText() == "file") {
        EntryFile = ValueString->getRawText();
      } else if ((*KVI)->Key->getRawText() == "command") {
        EntryCommand = ValueString->getRawText();
      } else {
        ErrorMessage = (llvm::Twine("Unknown key: \"") +
                        (*KVI)->Key->getRawText() + "\"").str();
        return CompileCommand();
      }
    }
    if (EntryFile == FileName) {
      CompileCommand Result;
      Result.Directory = EntryDirectory;
      Result.CommandLine = UnescapeJsonCommandLine(EntryCommand);
      return Result;
    }
  }
  ErrorMessage = "ERROR: No matching command found.";
  return CompileCommand();
}

/// \brief Returns the absolute path of 'File', by prepending it with
/// 'BaseDirectory' if 'File' is not absolute. Otherwise returns 'File'.
/// If 'File' starts with "./", the returned path will not contain the "./".
/// Otherwise, the returned path will contain the literal path-concatenation of
/// 'BaseDirectory' and 'File'.
///
/// \param File Either an absolute or relative path.
/// \param BaseDirectory An absolute path.
static std::string GetAbsolutePath(
    llvm::StringRef File, llvm::StringRef BaseDirectory) {
  assert(llvm::sys::path::is_absolute(BaseDirectory));
  if (llvm::sys::path::is_absolute(File)) {
    return File;
  }
  llvm::StringRef RelativePath(File);
  if (RelativePath.startswith("./")) {
    RelativePath = RelativePath.substr(strlen("./"));
  }
  llvm::SmallString<1024> AbsolutePath(BaseDirectory);
  llvm::sys::path::append(AbsolutePath, RelativePath);
  return AbsolutePath.str();
}

ToolInvocation::ToolInvocation(
    llvm::ArrayRef<std::string> CommandLine, FrontendAction *ToolAction,
    FileManager *Files)
    : CommandLine(CommandLine.vec()), ToolAction(ToolAction), Files(Files) {
}

void ToolInvocation::MapVirtualFiles(
    const std::map<std::string, std::string> &FileContents) {
  MappedFileContents.insert(FileContents.begin(), FileContents.end());
}

bool ToolInvocation::Run() {
  const std::vector<char*> Argv = CommandLineToArgv(&CommandLine);
  const char *const BinaryName = Argv[0];
  DiagnosticOptions DefaultDiagnosticOptions;
  TextDiagnosticPrinter DiagnosticPrinter(
      llvm::errs(), DefaultDiagnosticOptions);
  DiagnosticsEngine Diagnostics(llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs>(
      new DiagnosticIDs()), &DiagnosticPrinter, false);

  const llvm::OwningPtr<clang::driver::Driver> Driver(
      NewDriver(&Diagnostics, BinaryName));
  // Since the input might only be virtual, don't check whether it exists.
  Driver->setCheckInputsExist(false);
  const llvm::OwningPtr<clang::driver::Compilation> Compilation(
      Driver->BuildCompilation(llvm::ArrayRef<const char*>(
          &Argv[0], Argv.size() - 1)));
  const clang::driver::ArgStringList *const CC1Args = GetCC1Arguments(
      &Diagnostics, Compilation.get());
  if (CC1Args == NULL) {
    return false;
  }
  llvm::OwningPtr<clang::CompilerInvocation> Invocation(
      NewInvocation(&Diagnostics, *CC1Args));
  return RunInvocation(BinaryName, Compilation.get(),
                       Invocation.take(), *CC1Args, ToolAction.take());
}

bool ToolInvocation::RunInvocation(
    const char *BinaryName,
    clang::driver::Compilation *Compilation,
    clang::CompilerInvocation *Invocation,
    const clang::driver::ArgStringList &CC1Args,
    clang::FrontendAction *ToolAction) {
  llvm::OwningPtr<clang::FrontendAction> ScopedToolAction(ToolAction);
  // Show the invocation, with -v.
  if (Invocation->getHeaderSearchOpts().Verbose) {
    llvm::errs() << "clang Invocation:\n";
    Compilation->PrintJob(llvm::errs(), Compilation->getJobs(), "\n", true);
    llvm::errs() << "\n";
  }

  // Create a compiler instance to handle the actual work.
  clang::CompilerInstance Compiler;
  Compiler.setInvocation(Invocation);
  Compiler.setFileManager(Files);
  // FIXME: What about LangOpts?

  // Create the compilers actual diagnostics engine.
  Compiler.createDiagnostics(CC1Args.size(),
                             const_cast<char**>(CC1Args.data()));
  if (!Compiler.hasDiagnostics())
    return false;

  Compiler.createSourceManager(*Files);
  AddFileMappingsTo(Compiler.getSourceManager());

  // Infer the builtin include path if unspecified.
  if (Compiler.getHeaderSearchOpts().UseBuiltinIncludes &&
      Compiler.getHeaderSearchOpts().ResourceDir.empty()) {
    // This just needs to be some symbol in the binary.
    void *const SymbolAddr = &StaticSymbol;
    Compiler.getHeaderSearchOpts().ResourceDir =
        clang::CompilerInvocation::GetResourcesPath(BinaryName, SymbolAddr);
  }

  const bool Success = Compiler.ExecuteAction(*ToolAction);

  Compiler.resetAndLeakFileManager();
  return Success;
}

void ToolInvocation::AddFileMappingsTo(SourceManager &Sources) {
  for (std::map<std::string, std::string>::const_iterator
           It = MappedFileContents.begin(), End = MappedFileContents.end();
       It != End; ++It) {
    // Inject the code as the given file name into the preprocessor options.
    const llvm::MemoryBuffer *Input =
        llvm::MemoryBuffer::getMemBuffer(It->second.c_str());
    // FIXME: figure out what '0' stands for.
    const FileEntry *FromFile = Files->getVirtualFile(
        It->first, Input->getBufferSize(), 0);
    // FIXME: figure out memory management ('true').
    Sources.overrideFileContents(FromFile, Input, true);
  }
}

ClangTool::ClangTool(int argc, char **argv)
    : Files((FileSystemOptions())) {
  if (argc < 3) {
    llvm::outs() << "Usage: " << argv[0] << " <cmake-output-dir> "
                 << "<file1> <file2> ...\n";
    exit(1);
  }
  AddTranslationUnits(argv[1], std::vector<std::string>(argv + 2, argv + argc));
}

void ClangTool::AddTranslationUnits(
    llvm::StringRef JsonDatabaseDirectory,
    llvm::ArrayRef<std::string> TranslationUnits) {
  llvm::SmallString<1024> JsonDatabasePath(JsonDatabaseDirectory);
  llvm::sys::path::append(JsonDatabasePath, "compile_commands.json");
  llvm::OwningPtr<llvm::MemoryBuffer> JsonDatabase;
  llvm::error_code Result =
      llvm::MemoryBuffer::getFile(JsonDatabasePath, JsonDatabase);
  if (Result != 0) {
    llvm::outs() << "Error while opening JSON database: " << Result.message()
                 << "\n";
    exit(1);
  }
  llvm::StringRef BaseDirectory(::getenv("PWD"));
  for (unsigned I = 0; I < TranslationUnits.size(); ++I) {
    llvm::SmallString<1024> File(GetAbsolutePath(
        TranslationUnits[I], BaseDirectory));
    std::string ErrorMessage;
    clang::tooling::CompileCommand LookupResult =
        clang::tooling::FindCompileArgsInJsonDatabase(
            File.str(), JsonDatabase->getBuffer(), ErrorMessage);
    if (!ErrorMessage.empty()) {
      llvm::outs() << "Error while parsing JSON database: " << ErrorMessage
                   << "\n";
      exit(1);
    }
    if (!LookupResult.CommandLine.empty()) {
      if (!LookupResult.Directory.empty()) {
        // FIXME: What should happen if CommandLine includes -working-directory
        // as well?
        LookupResult.CommandLine.push_back(
            "-working-directory=" + LookupResult.Directory);
      }
      CommandLines.push_back(make_pair(File.str(), LookupResult.CommandLine));
    } else {
      // FIXME: There are two use cases here: doing a fuzzy
      // "find . -name '*.cc' |xargs tool" match, where as a user I don't care
      // about the .cc files that were not found, and the use case where I
      // specify all files I want to run over explicitly, where this should
      // be an error. We'll want to add an option for this.
      llvm::outs() << "Skipping " << File << ". Command line not found.\n";
    }
  }
}

void ClangTool::MapVirtualFiles(
    const std::map<std::string, std::string> &FileContents) {
  MappedFileContents.insert(FileContents.begin(), FileContents.end());
}

int ClangTool::Run(FrontendActionFactory *ActionFactory) {
  bool ProcessingFailed = false;
  for (unsigned I = 0; I < CommandLines.size(); ++I) {
    std::string File = CommandLines[I].first;
    std::vector<std::string> &CommandLine = CommandLines[I].second;
    llvm::outs() << "Processing: " << File << ".\n";
    ToolInvocation Invocation(CommandLine, ActionFactory->New(), &Files);
    Invocation.MapVirtualFiles(MappedFileContents);
    if (!Invocation.Run()) {
      llvm::outs() << "Error while processing " << File << ".\n";
      ProcessingFailed = true;
    }
  }
  return ProcessingFailed ? 1 : 0;
}

} // end namespace tooling
} // end namespace clang
