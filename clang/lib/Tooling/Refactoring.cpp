//===--- Refactoring.cpp - Framework for clang refactoring tools ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Implements tools to support refactorings.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/DiagnosticOptions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Rewriter.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/raw_os_ostream.h"

namespace clang {
namespace tooling {

Replacement::Replacement(llvm::StringRef FilePath, unsigned Offset,
                         unsigned Length, llvm::StringRef ReplacementText)
    : FilePath(FilePath), Offset(Offset),
      Length(Length), ReplacementText(ReplacementText) {}

Replacement::Replacement(SourceManager &Sources, SourceLocation Start,
                         unsigned Length, llvm::StringRef ReplacementText) {
  SetFromSourceLocation(Sources, Start, Length, ReplacementText);
}

Replacement::Replacement(SourceManager &Sources, const CharSourceRange &Range,
                         llvm::StringRef ReplacementText) {
  SetFromSourceRange(Sources, Range, ReplacementText);
}

bool Replacement::Apply(Rewriter &Rewrite) const {
  SourceManager &SM = Rewrite.getSourceMgr();
  const FileEntry *Entry = SM.getFileManager().getFile(FilePath);
  if (Entry == NULL)
    return false;
  FileID ID;
  // FIXME: Use SM.translateFile directly.
  SourceLocation Location = SM.translateFileLineCol(Entry, 1, 1);
  ID = Location.isValid() ?
    SM.getFileID(Location) :
    SM.createFileID(Entry, SourceLocation(), SrcMgr::C_User);
  // FIXME: We cannot check whether Offset + Length is in the file, as
  // the remapping API is not public in the RewriteBuffer.
  const SourceLocation Start =
    SM.getLocForStartOfFile(ID).
    getLocWithOffset(Offset);
  // ReplaceText returns false on success.
  // ReplaceText only fails if the source location is not a file location, in
  // which case we already returned false earlier.
  bool RewriteSucceeded = !Rewrite.ReplaceText(Start, Length, ReplacementText);
  assert(RewriteSucceeded);
  return RewriteSucceeded;
}

bool Replacement::Less::operator()(const Replacement &R1,
                                   const Replacement &R2) const {
  if (R1.FilePath != R2.FilePath) return R1.FilePath < R2.FilePath;
  if (R1.Offset != R2.Offset) return R1.Offset < R2.Offset;
  if (R1.Length != R2.Length) return R1.Length < R2.Length;
  return R1.ReplacementText < R2.ReplacementText;
}

void Replacement::SetFromSourceLocation(SourceManager &Sources,
                                        SourceLocation Start, unsigned Length,
                                        llvm::StringRef ReplacementText) {
  const std::pair<FileID, unsigned> DecomposedLocation =
      Sources.getDecomposedLoc(Start);
  const FileEntry *Entry = Sources.getFileEntryForID(DecomposedLocation.first);
  this->FilePath = Entry != NULL ? Entry->getName() : "invalid-location";
  this->Offset = DecomposedLocation.second;
  this->Length = Length;
  this->ReplacementText = ReplacementText;
}

void Replacement::SetFromSourceRange(SourceManager &Sources,
                                     const CharSourceRange &Range,
                                     llvm::StringRef ReplacementText) {
  SetFromSourceLocation(Sources, Range.getBegin(),
                        getRangeSize(Sources, Range), ReplacementText);
}

bool ApplyAllReplacements(Replacements &Replaces, Rewriter &Rewrite) {
  bool Result = true;
  for (Replacements::const_iterator I = Replaces.begin(),
                                    E = Replaces.end();
       I != E; ++I) {
    Result = I->Apply(Rewrite) && Result;
  }
  return Result;
}

bool SaveRewrittenFiles(Rewriter &Rewrite) {
  for (Rewriter::buffer_iterator I = Rewrite.buffer_begin(),
                                 E = Rewrite.buffer_end();
       I != E; ++I) {
    // FIXME: This code is copied from the FixItRewriter.cpp - I think it should
    // go into directly into Rewriter (there we also have the Diagnostics to
    // handle the error cases better).
    const FileEntry *Entry =
        Rewrite.getSourceMgr().getFileEntryForID(I->first);
    std::string ErrorInfo;
    llvm::raw_fd_ostream FileStream(
        Entry->getName(), ErrorInfo, llvm::raw_fd_ostream::F_Binary);
    if (!ErrorInfo.empty())
      return false;
    I->second.write(FileStream);
    FileStream.flush();
  }
  return true;
}

int getRangeSize(SourceManager &Sources, const CharSourceRange &Range) {
  std::pair<FileID, unsigned> Start =
      Sources.getDecomposedLoc(Range.getBegin());
  std::pair<FileID, unsigned> End =
      Sources.getDecomposedLoc(Range.getEnd());
  if (Start.first != End.first) return -1;
  if (Range.isTokenRange())
    // FIXME: Bring in the correct LangOptions.
    End.second += Lexer::MeasureTokenLength(Range.getEnd(), Sources,
                                            LangOptions());
  return End.second - Start.second;
}

RefactoringTool::RefactoringTool(int argc, char **argv) : Tool(argc, argv) {}

Replacements &RefactoringTool::GetReplacements() { return Replace; }

int RefactoringTool::Run(FrontendActionFactory *ActionFactory) {
  int Result = Tool.Run(ActionFactory);
  LangOptions DefaultLangOptions;
  DiagnosticOptions DefaultDiagnosticOptions;
  TextDiagnosticPrinter DiagnosticPrinter(llvm::errs(),
                                          DefaultDiagnosticOptions);
  DiagnosticsEngine Diagnostics(
      llvm::IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()),
      &DiagnosticPrinter, false);
  SourceManager Sources(Diagnostics, Tool.GetFiles());
  Rewriter Rewrite(Sources, DefaultLangOptions);
  if (!ApplyAllReplacements(Replace, Rewrite)) {
    llvm::errs() << "Could not apply replacements.\n";
    return 1;
  }
  if (!SaveRewrittenFiles(Rewrite)) {
    llvm::errs() << "Could not save rewritten files.\n";
    return 1;
  }
  return Result;
}

} // end namespace tooling
} // end namespace clang
