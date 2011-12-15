//===--- Tooling.h - Framework for standalone Clang tools -------*- C++ -*-===//
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

#ifndef LLVM_CLANG_TOOLING_TOOLING_H
#define LLVM_CLANG_TOOLING_TOOLING_H

#include "clang/Basic/FileManager.h"
#include "clang/Driver/Util.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <map>
#include <string>
#include <vector>

namespace clang {

namespace driver {
class Compilation;
} // end namespace driver

class CompilerInvocation;
class SourceManager;
class FrontendAction;

namespace tooling {

/// Interface to generate clang::FrontendActions.
class FrontendActionFactory {
public:
  virtual ~FrontendActionFactory();

  /// Returns a new clang::FrontendAction. The caller takes ownership of the
  /// returned action.
  virtual clang::FrontendAction *New() = 0;
};

/// \brief Returns a new FrontendActionFactory for a given type.
///
/// T must extend clang::FrontendAction.
///
/// Example:
/// FrontendActionFactory *Factory =
///   NewFrontendActionFactor<clang::SyntaxOnlyAction>();
template <typename T>
FrontendActionFactory *NewFrontendActionFactory();

/// \brief Returns a new FrontendActionFactory any type that provides an
/// implementation of NewFrontendAction().
///
/// FactoryT must implement: FrontendAction *NewFrontendAction().
///
/// Example:
/// struct ProvidesFrontendActions {
///   FrontendActionFactory *NewFrontendAction();
/// } Factory;
/// FrontendActionFactory *FactoryAdapter =
///   NewFrontendActionFactor(&Factory);
template <typename FactoryT>
FrontendActionFactory *NewFrontendActionFactory(FactoryT *ActionFactory);

/// \brief Runs (and deletes) the tool on 'Code' with the -fsynatx-only flag.
///
/// \param ToolAction The action to run over the code.
/// \param Code C++ code.
///
/// \return - True if 'ToolAction' was successfully executed.
bool RunSyntaxOnlyToolOnCode(
    clang::FrontendAction *ToolAction, llvm::StringRef Code);

/// \brief Converts a vector<string> into a vector<char*> suitable to pass
/// to main-style functions taking (int Argc, char *Argv[]).
std::vector<char*> CommandLineToArgv(const std::vector<std::string> *Command);

/// \brief Specifies the working directory and command of a compilation.
struct CompileCommand {
  /// \brief The working directory the command was executed from.
  std::string Directory;

  /// \brief The command line that was executed.
  std::vector<std::string> CommandLine;
};

/// \brief Converts a JSON escaped command line to a vector of arguments.
///
/// \param JsonEscapedCommandLine The escaped command line as a string. This
/// is assumed to be escaped as a JSON string (e.g. " and \ are escaped).
/// In addition, any arguments containing spaces are assumed to be \-escaped.
///
/// For example, the input (|| denoting non C-escaped strings):
///   |./call  a  \"b \\\" c \\\\ \"  d|
/// would yield:
///   [ |./call|, |a|, |b " c \ |, |d| ].
std::vector<std::string> UnescapeJsonCommandLine(
    llvm::StringRef JsonEscapedCommandLine);

/// \brief Looks up the compile command for 'FileName' in 'JsonDatabase'.
///
/// \param FileName The path to an input file for which we want the compile
/// command line. If the 'JsonDatabase' was created by CMake, this must be
/// an absolute path inside the CMake source directory which does not have
/// symlinks resolved.
///
/// \param JsonDatabase A JSON formatted list of compile commands. This lookup
/// command supports only a subset of the JSON standard as written by CMake.
///
/// \param ErrorMessage If non-empty, an error occurred and 'ErrorMessage' will
/// be set to contain the error message. In this case CompileCommand will
/// contain an empty directory and command line.
///
/// \see JsonCompileCommandLineDatabase
CompileCommand FindCompileArgsInJsonDatabase(
    llvm::StringRef FileName, llvm::StringRef JsonDatabase,
    std::string &ErrorMessage);

/// \brief Utility to run a FrontendAction in a single clang invocation.
class ToolInvocation {
 public:
  /// \brief Create a tool invocation.
  ///
  /// \param CommandLine The command line arguments to clang.
  /// \param ToolAction The action to be executed. Class takes ownership.
  /// \param Files The FileManager used for the execution. Class does not take
  /// ownership.
  ToolInvocation(
      llvm::ArrayRef<std::string> CommandLine, FrontendAction *ToolAction,
      FileManager *Files);

  /// \brief Map virtual files to be used while running the tool.
  ///
  /// \param FileContents A map from file names to the file content of the
  /// mapped virtual files.
  void MapVirtualFiles(const std::map<std::string, std::string> &FileContents);

  /// \brief Run the clang invocation.
  ///
  /// \returns True if there were no errors during execution.
  bool Run();

 private:
  void AddFileMappingsTo(SourceManager &SourceManager);

  bool RunInvocation(
      const char *BinaryName,
      clang::driver::Compilation *Compilation,
      clang::CompilerInvocation *Invocation,
      const clang::driver::ArgStringList &CC1Args,
      clang::FrontendAction *ToolAction);

  std::vector<std::string> CommandLine;
  llvm::OwningPtr<FrontendAction> ToolAction;
  FileManager *Files;
  // Maps <file name> -> <file content>.
  std::map<std::string, std::string> MappedFileContents;
};

/// \brief Utility to run a FrontendAction over a set of files.
///
/// This class is written to be usable for command line utilities.
class ClangTool {
 public:
  /// \brief Construct a clang tool from a command line.
  ///
  /// This will parse the command line parameters and print an error message
  /// and exit the program if the command line does not specify the required
  /// parameters.
  ///
  /// Usage:
  /// $ tool-name <cmake-output-dir> <file1> <file2> ...
  ///
  /// where <cmake-output-dir> is a CMake build directory in which a file named
  /// compile_commands.json exists (enable -DCMAKE_EXPORT_COMPILE_COMMANDS in
  /// CMake to get this output).
  ///
  /// <file1> ... specify the paths of files in the CMake source tree. This
  /// path is looked up in the compile command database. If the path of a file
  /// is absolute, it needs to point into CMake's source tree. If the path is
  /// relative, the current working directory needs to be in the CMake source
  /// tree and the file must be in a subdirectory of the current working
  /// directory. "./" prefixes in the relative files will be automatically
  /// removed, but the rest of a relative path must be a suffix of a path in
  /// the compile command line database.
  ///
  /// For example, to use a tool on all files in a subtree of the source
  /// tree, use:
  ///
  ///   /path/in/subtree $ find . -name '*.cpp' |
  ///       xargs tool-name /path/to/source
  ///
  /// \param argc The argc argument from main.
  /// \param argv The argv argument from main.
  ClangTool(int argc, char **argv);

  /// \brief Map virtual files to be used while running the tool.
  ///
  /// \param FileContents A map from file names to the file content of the
  /// mapped virtual files.
  void MapVirtualFiles(const std::map<std::string, std::string> &FileContents);

  /// Runs a frontend action over all files specified in the command line.
  ///
  /// \param ActionFactory Factory generating the frontend actions. The function
  /// takes ownership of this parameter. A new action is generated for every
  /// processed translation unit.
  int Run(FrontendActionFactory *ActionFactory);

  /// \brief Returns the file manager used in the tool.
  ///
  /// The file manager is shared between all translation units.
  FileManager &GetFiles() { return Files; }

 private:
  /// \brief Add translation units to run the tool over.
  ///
  /// Translation units not found in JsonDatabaseDirectory (see constructore)
  /// will be skipped.
  void AddTranslationUnits(
      llvm::StringRef JsonDatabaseDirectory,
      llvm::ArrayRef<std::string> TranslationUnits);

  // We store command lines as pair (file name, command line).
  typedef std::pair< std::string, std::vector<std::string> > CommandLine;
  std::vector<CommandLine> CommandLines;

  FileManager Files;
  // Maps <file name> -> <file content>.
  std::map<std::string, std::string> MappedFileContents;
};

template <typename T>
FrontendActionFactory *NewFrontendActionFactory() {
  class SimpleFrontendActionFactory : public FrontendActionFactory {
  public:
    virtual clang::FrontendAction *New() { return new T; }
  };

  return new SimpleFrontendActionFactory;
}

template <typename FactoryT>
FrontendActionFactory *NewFrontendActionFactory(FactoryT *ActionFactory) {
  class FrontendActionFactoryAdapter : public FrontendActionFactory {
  public:
    explicit FrontendActionFactoryAdapter(FactoryT *ActionFactory)
      : ActionFactory(ActionFactory) {}

    virtual clang::FrontendAction *New() {
      return ActionFactory->NewFrontendAction();
    }

  private:
    FactoryT *ActionFactory;
  };

  return new FrontendActionFactoryAdapter(ActionFactory);
}

} // end namespace tooling
} // end namespace clang

#endif // LLVM_CLANG_TOOLING_TOOLING_H

