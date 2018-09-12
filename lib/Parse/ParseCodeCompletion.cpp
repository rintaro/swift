#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include <algorithm>
#include <iterator>



void Parser::doCodeCompletion(size_t Offset, DeclContext *DC,
                              CodeCompletionCallbacksFactory *Factory,
                              bool InConfigBlock) {
  // Disable syntax context;
  SyntaxContext->disable();
  llvm::SaveAndRestore<bool> NoInterfaceToken(IsParsingInterfaceTokens, false);

  std::unique_ptr<CodeCompletionCallbacks> Callbacks;
  Callbacks.reset(Factory->createCodeCompletionCallbacks(*this));
  setCodeCompletionCallbacks(Callbacks.get());

  auto CCTokenLoc = SourceMgr.getLocForOffset(L->getBufferID(), Offset);

  auto finished = [&]() -> bool {
    SourceMgr.isBeforeInBuffer(CCTokenLoc, Tok.getLoc());
  };

  switch (DC->getContextKind()) {
    case DeclContextKind::FileUnit:
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::AbstractFunctionDecl: {
      bool IsTopLevel = DC->isModuleScopeContext();
      BraceItemListKind Kind = IsTopLevel ? BraceItemListKind::TopLevelCode : BraceItemListKind::Brace;
      SmallVector<ASTNode, 1> Entries;
      bool PreviousHadSemi = false;
      do {
        parseBraceItem(PreviousHadSemi, Kind, IsTopLevel, Entries);
        
      } while (!finished());
      break;
    }
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::ExtensionDecl: {
      Parser::ParseDeclOptions Options(PD_HasContainerType);
      switch (DC->getAsDecl()->getKind()) {
        case DeclKind::Enum:
          Options |= ParseDeclOptions(PD_AllowEnumElement | PD_InEnum);
          break;
        case DeclKind::Struct:
          Options |= ParseDeclOptions(PD_InStruct);
          break;
        case DeclKind::Class:
          Options |= ParseDeclOptions(PD_AllowDestructor | PD_InClass);
          break;
        case DeclKind::Protocol:
          Options |= ParseDeclOptions(PD_DisallowInit | PD_InProtocol);
          break;
        case DeclKind::Extension:
          Options |= ParseDeclOptions(PD_InExtension);
          break;
        default:
          llvm_unreachable("Invalid DeclContext");
      }
      SmallVector<Decl *, 2> Decls;
      bool PreviousHadSemi = false;
      do {
        parseDeclItem(PreviousHadSemi, Options,
                      [&](Decl *D) { Decls.push_back(D); });
      } while (!finished());
    }
    default:
      llvm_unreachable("invalid decl context for code completion");
  }
  CodeCompletion->doneParsing();

}
