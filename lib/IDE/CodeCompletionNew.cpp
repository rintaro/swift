
#include "swift/IDE/CodeCompletion.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include <algorithm>
#include <iterator>

using namespace swift;
using namespace ide;

struct CompletionParseStatus {
  enum class ReparseMode {
    FunctionBody,
    DeclMember,
    Toplevel,
  };

  size_t ParseOffset;
  DeclContext *DC;
  ReparseMode Mode;
};

class CCTokenContextAnalyzer : private ASTVisitor<CCTokenContextAnalyzer, bool, bool, bool> {
  friend ASTVisitor<CCTokenContextAnalyzer, bool, bool, bool>;
  struct ContextInfo {
    DeclContext *DC;
    ContextInfo(DeclContext *DC) : DC(DC) {}
  };

  class ContextRAII {
    CCTokenContextAnalyzer &Analyzer;
  public:
    ContextRAII(CCTokenContextAnalyzer &Analyzer, DeclContext *DC) : Analyzer(Analyzer) {
      Analyzer.ContextStack.emplace_back(DC);
    }
    ~ContextRAII() {
      Analyzer.ContextStack.pop_back();
    }
  };

  SourceLoc CCTokenLoc;
  SourceManager &SM;
  llvm::SmallVector<ContextInfo, 0> ContextStack;


public:
  Optional<CompletionParseStatus> FoundStatus;

  CCTokenContextAnalyzer(SourceLoc CCTokenLoc, SourceManager &SM)
  :CCTokenLoc(CCTokenLoc), SM(SM) {}

  DeclContext *getCurDeclContext() {
    return ContextStack.back().DC;
  }

  size_t getOffset(SourceLoc Loc) {
    return SM.getLocOffsetInBuffer(Loc, SM.findBufferContainingLoc(Loc));
  }

  bool delayDeclAt(SourceLoc Loc) {
    //llvm::errs() << "delayDeclAt: \n";
    //getCurDeclContext()->dumpContext();
    FoundStatus = { getOffset(Loc), getCurDeclContext(),
                    CompletionParseStatus::ReparseMode::DeclMember };
    return true;
  }

  bool delayFunctionBody(AbstractFunctionDecl *AFD) {
    //llvm::errs() << "delayFunctionBody: \n";
    //AFD->dumpContext();
    FoundStatus = { getOffset(AFD->getBodySourceRange().Start), AFD,
                    CompletionParseStatus::ReparseMode::FunctionBody };
    return true;
  }

  bool delayTopLevelCode(SourceLoc Loc) {
    //llvm::errs() << "delayTopLevelCode: \n";
    //getCurDeclContext()->dumpContext();
    FoundStatus = { getOffset(Loc), getCurDeclContext(),
                    CompletionParseStatus::ReparseMode::Toplevel };
    return true;
  }

  enum class RelativePosition {
    Before,
    Inside,
    After,
  };

  RelativePosition getCCTokPositionRelativeToBraceRange(SourceRange braceRange) {
    // The CC token is before the start of body or at '{'.
    if (!SM.isBeforeInBuffer(braceRange.Start, CCTokenLoc))
      return RelativePosition::Before;

    // The CC token is before the body or at the '}'.
    if (!SM.isBeforeInBuffer(braceRange.End, CCTokenLoc))
      return RelativePosition::Inside;

    // Body is not terminated by '}'. Consider it's in the brace.
    auto lastTok = Lexer::getTokenAtLocation(SM, braceRange.End);
    if (!lastTok.is(tok::r_brace))
      return RelativePosition::Inside;

    return RelativePosition::After;
  }

  /// Find a declaration from \p Decls where the CC token is between the *start*
  /// of the decl and the next sibling.
  template <typename Range>
  Decl *findInterestingDecl(Range Decls) {
    Decl *foundDecl = nullptr;
    for (Decl *D : Decls) {
      if (D->isImplicit() || D->getSourceRange().isInvalid())
        continue;
      if (D->escapedFromIfConfig())
        continue;
      if (isa<VarDecl>(D))
        continue;
      if (isa<AccessorDecl>(D))
        continue;
      auto DeclStartLoc = D->getSourceRangeIncludingAttrs().Start;
      if (!SM.isBeforeInBuffer(DeclStartLoc, CCTokenLoc))
        break;
      foundDecl = D;
    }
    return foundDecl;
  }

  bool visitSourceFile(SourceFile *SF) {
    assert(SF->getBufferID() &&
           "Can't perform code-completion in non-managed Buffer");
    ContextRAII Context(*this, SF);

    if (auto D = findInterestingDecl(SF->Decls)) {
      // The CC token is between the *start* of `FoundDecl` and the next sibling.
      // At this point, we cannot say the CC token is related to the FoundElem,
      // or it's introducing a new declaration *after* the FoundElem. Let's dig
      // into it.
      if (visit(D))
        return true;
      auto CharEndOfLastTok = Lexer::getLocForEndOfToken(SM, D->getEndLoc());
      return delayTopLevelCode(CharEndOfLastTok);
    } else {
      // Otherwise, Decls doesn't have any written declarations.
      auto startOfFile =  SM.getLocForBufferStart(SF->getBufferID().getValue());
      delayTopLevelCode(startOfFile);
    }

    return false;
  }

  /// Visit \c NominalTypeDecl or \c ExtensionDecl.
  template <typename DeclTy>
  bool visitIterableGenericDecl(DeclTy *D) {

    SourceRange braceRange = D->getBraces();
    switch (getCCTokPositionRelativeToBraceRange(braceRange)) {
      case RelativePosition::Before:
        // Reparse the decl itself.
        return delayDeclAt(D->getSourceRangeIncludingAttrs().Start);
      case RelativePosition::Inside: {
        if (braceRange.Start == braceRange.End) {
          // There's no '{' when parsing. Reparse the decl itself.
          return delayDeclAt(D->getSourceRangeIncludingAttrs().Start);
        }
        ContextRAII Context(*this, D);
        if (Decl *foundElem = findInterestingDecl(D->getMembers())) {
          // Dig into the member.
          ContextRAII Context(*this, D);
          if (visit(foundElem))
            return true;
          // Reparse from the end of the decl.
          auto CharEndOfLastTok = Lexer::getLocForEndOfToken(SM, foundElem->getEndLoc());
          return delayDeclAt(CharEndOfLastTok);
        } else {
          // There's no interesting member. reparse inside the brace '{'.
          auto afterLBraceLoc = Lexer::getLocForEndOfToken(SM, braceRange.Start);
          return delayDeclAt(afterLBraceLoc);
        }
      }
      case RelativePosition::After:
        return false;
    }
  }

  bool visitNominalTypeDecl(NominalTypeDecl *NTD) {
    return visitIterableGenericDecl(NTD);
  }
  bool visitExtensionDecl(ExtensionDecl *ED) {
    return visitIterableGenericDecl(ED);
  }

  bool visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
    auto bodyRange = AFD->getBodySourceRange();
    if (bodyRange.isInvalid())
      return delayDeclAt(AFD->getSourceRangeIncludingAttrs().Start);

    switch (getCCTokPositionRelativeToBraceRange(bodyRange)) {
      case RelativePosition::Before:
      case RelativePosition::Inside:
        return delayDeclAt(AFD->getSourceRangeIncludingAttrs().Start);
//        return delayFunctionBody(AFD);
      case RelativePosition::After:
        return false;
    }
  }

  bool visitIfConfigDecl(IfConfigDecl *ICD) {
    assert(!getCurDeclContext()->isLocalContext() &&
           "We don't dig into local context");

    // If the CC token is past '#end', parse from the end of '#end'.
    if (!ICD->hadMissingEnd()) {
      auto EndCharLoc = Lexer::getLocForEndOfToken(SM, ICD->getEndLoc());
      if (!SM.isBeforeInBuffer(CCTokenLoc, EndCharLoc)) {
        return false;
      }
    }

    // Find the clause where the CC token is in.
    const IfConfigClause *foundClause = nullptr;
    SourceLoc foundDirectiveEndLoc;
    for (auto &clause : ICD->getClauses()) {
      if (!SM.isBeforeInBuffer(clause.Loc, CCTokenLoc)) {
        break;
      }

      // If the CC token is at the condition part of the directive, Reparse the
      // ICD itself.
      SourceLoc directiveEndLoc = Lexer::getLocForEndOfLine(SM, clause.Loc);
      if (SM.isBeforeInBuffer(CCTokenLoc, directiveEndLoc)) {
        return delayDeclAt(ICD->getStartLoc());
      }

      foundClause = &clause;
      foundDirectiveEndLoc = directiveEndLoc;
    }
    assert(foundClause && "Empty IfConfigDecl is impossible");

    auto declRange = makeTransformRange(foundClause->Elements,
                                        [](const ASTNode N) {
                                          // In non-local context, ICD must
                                          // contain only decls.
                                          return N.get<Decl *>();
                                        });
    auto ParsePos = foundDirectiveEndLoc;

    if (auto *D = findInterestingDecl(declRange)) {
      if (visit(D))
        return true;
      // The CC token is at *after* 'D'
      ParsePos = Lexer::getLocForEndOfToken(SM, D->getEndLoc());
    }

    //getCurDeclContext()->dumpContext();
    if (getCurDeclContext()->isTypeContext()) {
      return delayDeclAt(ParsePos);
    } else if (getCurDeclContext()->isModuleScopeContext()) {
      return delayTopLevelCode(ParsePos);
    } else {
      llvm_unreachable("Invalid decl context");
    }
  }

  bool visitVarDecl(VarDecl *VD) {
    llvm_unreachable("Shouldn't visit VarDecl");
  }

  bool visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    return delayTopLevelCode(TLCD->getStartLoc());
  }

  bool visitDecl(Decl *D) {
    assert(D->getSourceRange().isValid());
    // For other decls, reparse the decl itself.
    SourceLoc ParsePos = D->getSourceRangeIncludingAttrs().Start;

    if (getCurDeclContext()->isTypeContext()) {
      return delayDeclAt(ParsePos);
    } else if (getCurDeclContext()->isModuleScopeContext()) {
      return delayTopLevelCode(ParsePos);
    } else {
      llvm_unreachable("Invalid decl context");
    }

  }
};

Optional<CompletionParseStatus>
analyzeCodeCompletionPoint(SourceFile *SF, size_t Offset) {
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  Optional<uint> BufferID = SF->getBufferID();
  assert(BufferID.hasValue());
  auto CCTokenLoc = SM.getLocForOffset(*BufferID, Offset);
  CCTokenContextAnalyzer Analyzer(CCTokenLoc, SM);
  Analyzer.visitSourceFile(SF);
  return Analyzer.FoundStatus;
}

unsigned int createCodeCompletionBuffer(SourceFile *SF, size_t Offset) {
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  auto OldBufferID = SF->getBufferID().getValue();
  auto InputSrcText = SM.getEntireTextForBuffer(OldBufferID);

  auto NewBufferSize = InputSrcText.size() + 1;
  auto NewBuffer = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(
      NewBufferSize, SM.getIdentifierForBuffer(OldBufferID));
  char *Ptr = NewBuffer->getBufferStart();
  Ptr = std::copy(InputSrcText.begin(), InputSrcText.begin() + Offset, Ptr);
  *Ptr = '\0';
  std::copy(InputSrcText.begin() + Offset, InputSrcText.end(), Ptr + 1);
  unsigned int NewBufferID = SM.addNewSourceBuffer(std::move(NewBuffer));
  SM.setCodeCompletionPoint(NewBufferID, Offset);
  return NewBufferID;
}

bool swift::ide::performCodeCompletion(SourceFile *SF, size_t Offset,
                                       CodeCompletionCallbacksFactory *Factory) {
  ASTContext &Ctx = SF->getASTContext();
//  SF->dump(llvm::errs());
  SourceManager &SM = Ctx.SourceMgr;

  // Get information from the pre-parsed AST.
  auto Info = analyzeCodeCompletionPoint(SF, Offset).getValue();

  // Create a CC token embedded buffer.
  auto NewBufferID = createCodeCompletionBuffer(SF, Offset);

  // Create a lexer that starts from the position determined by the analyzer.
  auto CCTokenLoc = SM.getLocForOffset(NewBufferID, Offset);
  auto BeginLoc = SM.getLocForOffset(NewBufferID, Info.ParseOffset);


  //
  std::unique_ptr<Lexer> Lex;
  Lex.reset(new Lexer(Ctx.LangOpts, SM, NewBufferID, nullptr,
                      /*InSILMode*/ false, HashbangMode::Allowed,
                      CommentRetentionMode::None,
                      TriviaRetentionMode::WithoutTrivia));
  
  auto BeginState = Lex->getStateForBeginningOfTokenLoc(BeginLoc);
  Lex->restoreState(BeginState);
  Parser TheParser(std::move(Lex), *SF);
  TheParser.SyntaxContext->disable();

  // Prime lexer.
  TheParser.consumeTokenWithoutFeedingReceiver();

  // Hack. We don't care 'PreviousLoc' at start position of decl, statement, or
  // function body.
  TheParser.PreviousLoc = TheParser.Tok.getLoc();

  std::unique_ptr<CodeCompletionCallbacks> CodeCompletion;
  CodeCompletion.reset(Factory->createCodeCompletionCallbacks(TheParser));
  TheParser.setCodeCompletionCallbacks(CodeCompletion.get());
  Scope initScope(&TheParser, ScopeKind::TopLevel, false);

  // Parser need at least one scope in it.
  // TODO: Do we need to emulate the actual scope hierarchy?

//  llvm::errs() << "Tok: '";
//  llvm::errs().write_escaped(TheParser.Tok.getText());
//  llvm::errs() << "'\n";
  switch (Info.Mode) {
    case CompletionParseStatus::ReparseMode::FunctionBody: {
//      llvm::errs() << "ReparseMode::FunctionBody\n";
      auto AFD = cast<AbstractFunctionDecl>(Info.DC);
//      llvm::errs() << SM.extractText(Lexer::getCharSourceRangeFromSourceRange(SM, AFD->getSourceRange()));
      llvm::errs() << "\n---------\n";
      TheParser.parseAbstractFunctionBody(AFD);
      //AFD->getBody()->dump();
      break;
    }
    case CompletionParseStatus::ReparseMode::Toplevel: {
      llvm::errs() << "ReparseMode::ToplevelDecl\n";
      bool PreviousHadSemi = true;
      SmallVector<ASTNode, 2> Items;
      do {
//        llvm::errs() << "Tok: '";
//        llvm::errs().write_escaped(TheParser.Tok.getText());
//        llvm::errs() << "'\n";
        TheParser.parseBraceItem(PreviousHadSemi,
                                 BraceItemListKind::TopLevelCode, true, Items);
      } while (!TheParser.Tok.is(tok::eof) &&
               !SM.isBeforeInBuffer(CCTokenLoc, TheParser.Tok.getLoc()));
      break;
    }
    case CompletionParseStatus::ReparseMode::DeclMember: {
      llvm::errs() << "ReparseMode::DeclMember\n";

      Parser::ParseDeclOptions Options;
      if (Info.DC->isModuleScopeContext()) {
        Options |= Parser::ParseDeclFlags::PD_AllowTopLevel;
      } else {
        assert(Info.DC->getAsDecl() != nullptr);
        switch (Info.DC->getAsDecl()->getKind()) {
          case DeclKind::Enum:
            Options |= Parser::ParseDeclFlags::PD_HasContainerType;
            Options |= Parser::ParseDeclFlags::PD_AllowEnumElement;
            Options |= Parser::ParseDeclFlags::PD_InEnum;
            break;
          case DeclKind::Struct:
            Options |= Parser::ParseDeclFlags::PD_HasContainerType;
            Options |= Parser::ParseDeclFlags::PD_InStruct;
            break;
          case DeclKind::Class:
            Options |= Parser::ParseDeclFlags::PD_HasContainerType;
            Options |= Parser::ParseDeclFlags::PD_AllowDestructor;
            Options |= Parser::ParseDeclFlags::PD_InClass;
            break;
          case DeclKind::Protocol:
            Options |= Parser::ParseDeclFlags::PD_HasContainerType;
            Options |= Parser::ParseDeclFlags::PD_DisallowInit;
            Options |= Parser::ParseDeclFlags::PD_InProtocol;
            break;
          case DeclKind::Extension:
            Options |= Parser::ParseDeclFlags::PD_HasContainerType;
            Options |= Parser::ParseDeclFlags::PD_InExtension;
            break;
          default:
            llvm_unreachable("Invalid DeclContext for");
        }
      }

      Parser::ContextChange CC(TheParser, Info.DC);
      bool PreviousHadSemi = true;
      do {
        llvm::errs() << "Tok: '";
        llvm::errs().write_escaped(TheParser.Tok.getText());
        llvm::errs() << "'\n";
        TheParser.parseDeclItem(PreviousHadSemi, Options, [&](Decl *D) {
        });
      } while (TheParser.Tok.isNot(tok::eof, tok::r_brace, tok::pound_elseif,
                                   tok::pound_else, tok::pound_endif) &&
               !SM.isBeforeInBuffer(CCTokenLoc, TheParser.Tok.getLoc()));
      break;
    }
  }
  CodeCompletion->doneParsing();
  return false;
}
