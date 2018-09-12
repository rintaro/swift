
#include "swift/IDE/CodeCompletion.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/SubSystems.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include <algorithm>
#include <iterator>

using namespace swift;
using namespace ide;

struct CompletionParseStatus {
  size_t ParseOffset;
  DeclContext *DC;
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

  bool completionAt(SourceLoc Loc) {
    FoundStatus = { getOffset(Loc), getCurDeclContext() };
    return true;
  }

  bool completionAfter(ASTNode Node) {
    auto CharEndOfLastTok = Lexer::getLocForEndOfToken(SM, Node.getEndLoc());
    return completionAt(CharEndOfLastTok);
  }


  bool visit(ASTNode Node) {
    if (auto *D = Node.dyn_cast<Decl *>())
      return visit(D);
    if (auto *S = Node.dyn_cast<Stmt *>())
      return visit(S);
    if (auto *E = Node.dyn_cast<Expr *>())
      return visit(E);
    llvm_unreachable("unknown ASTNode");
  }

  bool isInterestingRange(SourceRange R) {
    if (!SM.isBeforeInBuffer(R.Start, CCTokenLoc))
      return false;
    return true;
  }

  bool visitIfInteresting(Stmt *S) {
    if (!isInterestingRange(S->getSourceRange()))
      return false;
    return visit(S);
  }

  bool isAfter(SourceLoc EndLoc) {
    auto EndCharLoc = Lexer::getLocForEndOfToken(SM, EndLoc);
    return SM.isBeforeInBuffer(EndCharLoc, CCTokenLoc);
  }


  enum class RelativePosition {
    Unknown,
    Before,
    Inside,
    After,
  };

  RelativePosition getCCTokPositionRelativeToBraceRange(SourceRange braceRange) {
    // The CC token is before the start of body or at '{'.
    if (!SM.isBeforeInBuffer(braceRange.Start, CCTokenLoc))
      return RelativePosition::Before;

    if (braceRange.Start == braceRange.End)
      return RelativePosition::Unknown;

    // The CC token is before the body or at the '}'.
    if (!SM.isBeforeInBuffer(braceRange.End, CCTokenLoc))
      return RelativePosition::Inside;

    // Body is not terminated by '}'. Consider it's in the brace.
    auto lastTok = Lexer::getTokenAtLocation(SM, braceRange.End);
    if (!lastTok.is(tok::r_brace))
      return RelativePosition::Inside;

    return RelativePosition::After;
  }

  bool isInterestingBrace(Stmt *S) {
    auto *BS = dyn_cast_or_null<BraceStmt>(S);
    if (!BS || BS->isImplicit() || BS->getSourceRange().isInvalid())
      return false;
    if (BS->getLBraceLoc() == BS->getRBraceLoc())
      return false;
    return true;
  }

  RelativePosition getRelationWithBrace(Stmt *S) {
    if (!isInterestingBrace(S))
      return RelativePosition::Unknown;
    else
      return getCCTokPositionRelativeToBraceRange(S->getSourceRange());
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

  /// Find an AST node (decl, stmt, or expr) from \p Nodes where the CC token is
  /// between the *start* of the item and the next sibling.
  ASTNode findInterestingASTNode(ArrayRef<ASTNode> Nodes) {
    ASTNode foundNode = ASTNode();
    for (ASTNode Node : Nodes) {
      if (Node.isImplicit() || Node.getSourceRange().isInvalid())
        continue;
      auto StartLoc = Node.getStartLoc();
      if (auto D = Node.dyn_cast<Decl *>()) {
        if (D->escapedFromIfConfig())
          continue;
        if (isa<VarDecl>(D))
          continue;
        if (isa<AccessorDecl>(D))
          continue;
        StartLoc = D->getSourceRangeIncludingAttrs().Start;
      }
      if (!SM.isBeforeInBuffer(StartLoc, CCTokenLoc))
        break;
      foundNode = Node;
    }
    return foundNode;
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
      return completionAt(CharEndOfLastTok);
    }

    // Otherwise, Decls doesn't have any written declarations.
    auto startOfFile =  SM.getLocForBufferStart(SF->getBufferID().getValue());
    return completionAt(startOfFile);
  }

  bool visitIfStmt(IfStmt *IS) {
    if (isInterestingBrace(IS->getThenStmt()) &&
        visit(IS->getThenStmt()))
      return true;
    auto *ElseStmt = IS->getElseStmt();
    if (visitIfInteresting(ElseStmt))
      return true;
    // We complete 'else' after if statement. So we re-parse this statement.
    return completionAt(IS->getStartLoc());
  }

  bool visitGuardStmt(GuardStmt *GS) {
    if (isInterestingBrace(GS->getBody())) {
      if (visit(GS->getBody()))
        return true;
      if (isAfter(GS->getEndLoc()))
        return completionAfter(GS);
    }
    return completionAt(GS->getStartLoc());
  }

  bool visitWhileStmt(WhileStmt *WS) {
    if (isInterestingBrace(WS->getBody())) {
      if (visit(cast<BraceStmt>(WS->getBody())))
        return true;
      if (isAfter(WS->getEndLoc()))
        return completionAfter(WS);
    }
    return completionAt(WS->getStartLoc());
  }

  bool visitDoStmt(DoStmt *DS) {
    if (isInterestingBrace(DS->getBody())) {
      if (visit(DS->getBody()))
        return true;
      if (isAfter(DS->getEndLoc()))
        return completionAfter(DS);
    }
    return completionAt(DS->getStartLoc());
  }

  bool visitDoCatchStmt(DoCatchStmt *DCS) {
    if (isInterestingBrace(DCS->getBody())) {
      if (visit(DCS->getBody()))
        return true;
    }
    bool LastCatchHadValidBrace = false;
    for (auto CS : DCS->getCatches()) {
      LastCatchHadValidBrace = isInterestingBrace(CS->getBody());
      if (LastCatchHadValidBrace && visit(CS->getBody()))
        return true;
    }
    if (LastCatchHadValidBrace && isAfter(DCS->getEndLoc()))
      return completionAfter(DCS);
    return completionAt(DCS->getStartLoc());
  }

  bool visitRepeatWhileStmt(RepeatWhileStmt *RWS) {
    if (isInterestingBrace(RWS->getBody())) {
      if (visitBraceStmt(cast<BraceStmt>(RWS->getBody())))
        return true;
    }
    // In repeat while stmt, the completion may belongs to the condition part.
    // Reparse from this statement.
    return completionAt(RWS->getStartLoc());
  }

  bool visitForEachStmt(ForEachStmt *FES) {
    if (isInterestingBrace(FES->getBody())) {
      if (visit(FES->getBody()))
        return true;
      if (isAfter(FES->getEndLoc()))
        return completionAfter(FES);
    }
    return completionAt(FES->getStartLoc());
  }

  bool visitSwitchStmt(SwitchStmt *SS) {
    if (!SM.isBeforeInBuffer(SS->getLBraceLoc(), CCTokenLoc))
      return completionAt(SS->getStartLoc());
    for (ASTNode Elem : SS->getRawCases()) {
      if (auto *CS = dyn_cast_or_null<CaseStmt>(Elem.dyn_cast<Stmt *>())) {
        if (!SM.isBeforeInBuffer(CS->getLabelItemsRange().End, CCTokenLoc))
          // CCToken is before the end of this label items.
          break;
        if (!SM.isBeforeInBuffer(CCTokenLoc, CS->getBody()->getEndLoc()))
          // CCToken is after this case.
          continue;
      } else  {
        visit(Elem);
      }
    }
    if (isAfter(SS->getRBraceLoc()))
      return completionAfter(SS);
    return completionAt(SS->getStartLoc());
  }

  /// Visit \c NominalTypeDecl or \c ExtensionDecl.
  template <typename DeclTy>
  bool visitIterableGenericDecl(DeclTy *D) {
    SourceRange braceRange = D->getBraces();
    switch (getCCTokPositionRelativeToBraceRange(braceRange)) {
      case RelativePosition::Before:
      case RelativePosition::Unknown:
        // Reparse the decl itself.
        completionAt(D->getStartLoc());
        break;
      case RelativePosition::Inside: {
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


  bool visitBraceStmt(BraceStmt *BS) {
    switch (getCCTokPositionRelativeToBraceRange(BS->getSourceRange())) {
      case RelativePosition::Before:
      case RelativePosition::Unknown:
        return false;
      case RelativePosition::Inside:
        if (ASTNode foundElem = findInterestingASTNode(BS->getElements())) {
          if (visit(foundElem))
            return true;
          return delayStmtAfter(foundElem)
        } else {
          return delayStmtAt(Lexer::getLocForEndOfToken(SM, BS->getEndLoc()));
        }
      case RelativePosition::After:

        break;
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

  // Parser need at least one scope in it.
  // TODO: Do we need to emulate the actual scope hierarchy?
  Scope initScope(&TheParser, ScopeKind::TopLevel, false);

//  llvm::errs() << "Tok: '";
//  llvm::errs().write_escaped(TheParser.Tok.getText());
//  llvm::errs() << "'\n";

  switch (Info.DC->getContextKind()) {
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::AbstractFunctionDecl: {
    SmallVector<ASTNode, 1> Entries;
    bool PreviousHadSemi = false;
    do {
      parseBraceItem(Entries);
      if (finished())
        doneParsing();
      Entries.clear();
    }
    break;
  }
  case DeclContextKind::GenericTypeDecl:
  case DeclContextKind::ExtensionDecl: {
    Parser::ParseDeclOptions Options(PD_HasContainerType);
    switch (Info.DC->getAsDecl->getKind()) {
      case DeclKind::Enum:
        Options |= Parser::ParseDeclFlags::PD_AllowEnumElement;
        Options |= Parser::ParseDeclFlags::PD_InEnum;
        break;
      case DeclKind::Struct:
        Options |= Parser::ParseDeclFlags::PD_InStruct;
        break;
      case DeclKind::Class:
        Options |= Parser::ParseDeclFlags::PD_AllowDestructor;
        Options |= Parser::ParseDeclFlags::PD_InClass;
        break;
      case DeclKind::Protocol:
        Options |= Parser::ParseDeclFlags::PD_DisallowInit;
        Options |= Parser::ParseDeclFlags::PD_InProtocol;
        break;
      case DeclKind::Extension:
        Options |= Parser::ParseDeclFlags::PD_InExtension;
        break;
      default:
        llvm_unreachable("Invalid DeclContext");
    }
    SmallVector<Decl *, 2> Decls
    bool PreviousHadSemi = false;
    do {
      parseDeclItem(PreviousHadSemi, Options, [&](Decl *D) { Decls.push_back(D); });
    } while (!finished());

  }
  }
  CodeCompletion->doneParsing();
  return false;
}
