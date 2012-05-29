//===--- Parser.cpp - Matcher expression parser -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple matcher expression parser.
// The parser understand matcher expressions of the form:
//   MatcherName(Arg0, Arg1, ..., ArgN)
// as well as simple types like string, character, number and boolean literals.
// The parser does not know how to process the matchers. It delegates this task
// to a TokenProcessor object received as an argument.
//
//===----------------------------------------------------------------------===//

#include <string>
#include <vector>

#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "llvm/ADT/Twine.h"

namespace clang {
namespace ast_matchers {
namespace dynamic {

namespace {

class CodeTokenizer {
 public:
  explicit CodeTokenizer(llvm::StringRef MatcherCode)
      : Code(MatcherCode), StartOfLine(MatcherCode), Line(1) { }

  Parser::TokenInfo getNextToken() {
    consumeWhitespace();
    Parser::TokenInfo Result;
    Result.StartLine = Line;
    Result.StartColumn = Column();

    if (Code.empty()) {
      // Nothing to do.
    } else if (isSingleCharToken(Code[0])) {
      Result.Token = Code.substr(0, 1);
      Code = Code.drop_front();

    } else if (Code[0] == '"' || Code[0] == '\'') {
      // Parse a string literal.
      Result.Token = consumeStringLiteral();
      Code = Code.drop_front(Result.Token.size());

    } else {
      size_t TokenLength = 0;
      while (TokenLength < Code.size() &&
             !isspace(Code[TokenLength]) &&
             !isSingleCharToken(Code[TokenLength]))
        ++TokenLength;
      Result.Token = Code.substr(0, TokenLength);
      Code = Code.drop_front(TokenLength);
    }

    Result.EndLine = Line;
    Result.EndColumn = Column() - 1;
    return Result;
  }

  bool isDone() const { return Code.empty(); }

 private:
  llvm::StringRef consumeStringLiteral() const {
    bool InEscape = false;
    const char Marker = Code[0];
    for (size_t Length = 1; Length < Code.size(); ++Length) {
      if (InEscape) {
        InEscape = false;
        continue;
      }
      if (Code[Length] == '\\') {
        InEscape = true;
        continue;
      }
      if (Code[Length] == Marker) {
        return Code.substr(0, Length + 1);
      }
    }
    return Code;
  }

  void consumeWhitespace() {
    while (!Code.empty() && isspace(Code[0])) {
      if (Code[0] == '\n') {
        ++Line;
        StartOfLine = Code.drop_front();
      }
      Code = Code.drop_front();
    }
  }
  static bool isSingleCharToken(const char Char) {
    return Char == ',' || Char == '(' || Char == ')';
  }

  llvm::StringRef Code;
  llvm::StringRef StartOfLine;
  int Line;
  int Column() const {
    return Code.data() - StartOfLine.data() + 1;
  }
};

char UnescapeCharSequence(llvm::StringRef* Escaped) {
  if (Escaped->empty()) return 0;
  char Char = (*Escaped)[0];
  *Escaped = Escaped->drop_front();
  if (Char != '\\') {
    return Char;
  }

  if (Escaped->empty()) return 0;
  Char = (*Escaped)[0];
  *Escaped = Escaped->drop_front();
  // TODO: Does not escape octal/hex sequences. Eg. "\123"
  switch (Char) {
    case 'a':  Char = '\a';  break;
    case 'b':  Char = '\b';  break;
    case 'f':  Char = '\f';  break;
    case 'n':  Char = '\n';  break;
    case 'r':  Char = '\r';  break;
    case 't':  Char = '\t';  break;
    case 'v':  Char = '\v';  break;
    case '\\': Char = '\\';  break;
    case '?':  Char = '\?';  break;
    case '\'': Char = '\'';  break;
    case '"':  Char = '\"';  break;
  }
  return Char;
}

GenericValue ParseString(const Parser::TokenInfo& Token,
                         Parser::TokenProcessor* Processor) {
  if (Token.Token.size() < 2 ||
      !Token.Token.startswith("\"") || !Token.Token.endswith("\"")) {
    return GenericError(
        llvm::Twine("Error parsing string token: <" + Token.Token + ">").str());
  }

  llvm::StringRef Escaped = Token.Token.drop_front().drop_back();
  std::string Unescaped;

  while (!Escaped.empty()) {
    Unescaped.push_back(UnescapeCharSequence(&Escaped));
  }

  GenericValue Value = Unescaped;
  return Processor->processValueToken(Value, Token);
}

GenericValue ParseChar(const Parser::TokenInfo& Token,
                       Parser::TokenProcessor* Processor) {
  if (Token.Token.size() < 3 ||
      !Token.Token.startswith("'") || !Token.Token.endswith("'")) {
    return GenericError(
        llvm::Twine("Error parsing char token: <" + Token.Token + ">").str());
  }
  llvm::StringRef Escaped = Token.Token.drop_front().drop_back();
  const int Unescaped = UnescapeCharSequence(&Escaped);
  if (!Escaped.empty()) {
    return GenericError(
        llvm::Twine("Error parsing char token: <" + Token.Token + ">").str());
  }

  GenericValue Value = Unescaped;
  return Processor->processValueToken(Value, Token);
}

GenericValue ParseNumber(const Parser::TokenInfo& Token,
                         Parser::TokenProcessor* Processor) {
  long long SignedLong;
  if (!Token.Token.getAsInteger(0, SignedLong)) {
    return Processor->processValueToken(NumberHolder(SignedLong), Token);
  }
  unsigned long long UnsignedLong;
  if (!Token.Token.getAsInteger(0, UnsignedLong)) {
    return Processor->processValueToken(NumberHolder(UnsignedLong), Token);
  }

  // TODO: Figure out a way to do this without copying the token.
  const std::string AsString = Token.Token;
  char* endptr = NULL;
  double Double = strtod(AsString.c_str(), &endptr);
  if (endptr[0] == 0)
    return Processor->processValueToken(NumberHolder(Double), Token);

  return GenericError(
      llvm::Twine("Error parsing number token: <" + Token.Token + ">").str());
}

GenericValue ParseToken(const Parser::TokenInfo& Token,
                        CodeTokenizer* Tokenizer,
                        Parser::TokenProcessor* Processor);

GenericValue ParseMatcher(const Parser::TokenInfo& Token,
                          CodeTokenizer* Tokenizer,
                          Parser::TokenProcessor* Processor) {
  const Parser::TokenInfo& OpenToken = Tokenizer->getNextToken();
  if (OpenToken.Token != "(") {
    return GenericError(
        llvm::Twine("Error parsing matcher. Found token <" +
                    OpenToken.Token + "> while looking for '('").str());
  }

  std::vector<GenericValue> Args;
  Parser::TokenInfo EndToken;
  while (!Tokenizer->isDone()) {
    Parser::TokenInfo NextToken = Tokenizer->getNextToken();
    if (NextToken.Token == ")") {
      // End of args.
      EndToken = NextToken;
      break;
    }
    if (Args.size() > 0) {
      // We must find a , token to continue.
      if (NextToken.Token != ",") {
        return GenericError(
            llvm::Twine("Error parsing matcher. Found token <" +
                        NextToken.Token + "> while looking for ','").str());
      }
      NextToken = Tokenizer->getNextToken();
    }
    const GenericValue Arg = ParseToken(NextToken, Tokenizer, Processor);
    if (Arg.is<GenericError>()) {
      return GenericError(
          llvm::Twine("Error parsing argument " + llvm::Twine(Args.size()) +
                      " for matcher " + Token.Token + ": " +
                      Arg.get<GenericError>().Message).str());
    }
    Args.push_back(Arg);
  }

  if (EndToken.Token == "") {
    return GenericError("Error parsing matcher. Found end-of-code while "
                        "looking for ')'");
  }

  // Merge the start and end infos.
  Parser::TokenInfo NewInfo = Token;
  NewInfo.EndLine = EndToken.EndLine;
  NewInfo.EndColumn = EndToken.EndColumn;
  const GenericValue Result = Processor->processMatcherToken(
      Token.Token, Args, NewInfo);
  if (Result.is<GenericError>()) {
    return GenericError(
        llvm::Twine("Error building matcher " + Token.Token +
                    ": " + Result.get<GenericError>().Message).str());
  }

  return Result;
}

GenericValue ParseToken(const Parser::TokenInfo& Token,
                        CodeTokenizer* Tokenizer,
                        Parser::TokenProcessor* Processor) {
  if (Token.Token.empty()) {
    return GenericError("End of code found while looking for token.");
  }

  if (Token.Token[0] == '"') return ParseString(Token, Processor);
  if (Token.Token[0] == '\'') return ParseChar(Token, Processor);
  if (isdigit(Token.Token[0]) ||
      Token.Token[0] == '-' || Token.Token[0] == '+') {
    return ParseNumber(Token, Processor);
  }

  // TODO: Do this better when we have more constants.
  // TODO: Add more constants (like INT_MAX and stuff like that).
  if (Token.Token == "true")
    return Processor->processValueToken(true, Token);
  if (Token.Token == "false")
    return Processor->processValueToken(false, Token);

  return ParseMatcher(Token, Tokenizer, Processor);
}

}  // anonymous namespace

GenericValue Parser::parseMatcher(llvm::StringRef Code,
                                  Parser::TokenProcessor* Processor) {
  CodeTokenizer Tokenizer(Code);
  return ParseToken(Tokenizer.getNextToken(), &Tokenizer, Processor);
}

}  // namespace dynamic
}  // namespace ast_matchers
}  // namespace clang
