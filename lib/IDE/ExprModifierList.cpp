//===--- ExprModifierList.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/ExprModifierList.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"

using namespace swift;
using namespace ide;

namespace {
class ExprModifierListCallbacks : public CodeCompletionCallbacks {
  ArrayRef<const char *> ExpectedTypeNames;
  ExprModifierListConsumer &Consumer;
  SourceLoc Loc;
  Expr *ParsedExpr = nullptr;
  DeclContext *CurDeclContext = nullptr;

  void getModifiers(Type T, ArrayRef<Type> expectedTypes,
                    SmallVectorImpl<ValueDecl *> &result);

public:
  ExprModifierListCallbacks(Parser &P,
                            ArrayRef<const char *> ExpectedTypeNames,
                            ExprModifierListConsumer &Consumer)
      : CodeCompletionCallbacks(P), ExpectedTypeNames(ExpectedTypeNames),
        Consumer(Consumer) {}

  // Only handle callbacks for suffix completions.
  // {
  void completeDotExpr(Expr *E, SourceLoc DotLoc) override;
  void completePostfixExpr(Expr *E, bool hasSpace) override;
  // }

  // Ignore other callbacks.
  // {
  void completeExpr() override{};
  void completeExprSuper(SuperRefExpr *SRE) override {};
  void completeExprSuperDot(SuperRefExpr *SRE) override {};
  void completeInPrecedenceGroup(SyntaxKind SK) override {};
  void completePoundAvailablePlatform() override {};
  void completeExprKeyPath(KeyPathExpr *KPE, SourceLoc DotLoc) override {}
  void completeTypeSimpleBeginning() override {}
  void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) override {}
  void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) override {}
  void completeDeclAttrKeyword(Decl *D, bool Sil, bool Param) override {}
  void completeDeclAttrParam(DeclAttrKind DK, int Index) override {}
  void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords) override {}
  void completeImportDecl(
      std::vector<std::pair<Identifier, SourceLoc>> &Path) override {}
  void completeAfterPoundExpr(CodeCompletionExpr *E,
                              Optional<StmtKind> ParentKind) override {}
  void completeAfterPoundDirective() override {}
  void completePlatformCondition() override {}
  void completeGenericParams(TypeLoc TL) override {}
  void completeAfterIfStmt(bool hasElse) override {}
  void completeAccessorBeginning() override{};

  void completeStmtOrExpr() override{};
  void completePostfixExprBeginning(CodeCompletionExpr *E) override {}
  void completeForEachSequenceBeginning(CodeCompletionExpr *E) override {};
  void completeCaseStmtBeginning() override {};

  void completeAssignmentRHS(AssignExpr *E) override {};
  void completeCallArg(CodeCompletionExpr *E) override {};
  void completeReturnStmt(CodeCompletionExpr *E) override {};
  void completeYieldStmt(CodeCompletionExpr *E,
                         Optional<unsigned> yieldIndex) override {};

  void completeUnresolvedMember(CodeCompletionExpr *E,
                                SourceLoc DotLoc) override {};
  void completeCaseStmtDotPrefix() override {};

  void completePostfixExprParen(Expr *E, Expr *CodeCompletionE) override {};
  // }

  void doneParsing() override;
};

void ExprModifierListCallbacks::completeDotExpr(Expr *E, SourceLoc DotLoc) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ExprModifierListCallbacks::completePostfixExpr(Expr *E, bool hasSpace) {
  CurDeclContext = P.CurDeclContext;
  ParsedExpr = E;
}

void ExprModifierListCallbacks::doneParsing() {
  if (!ParsedExpr)
    return;

  typeCheckContextUntil(
      CurDeclContext,
      CurDeclContext->getASTContext().SourceMgr.getCodeCompletionLoc());

  Type T = ParsedExpr->getType();

  // Type check the expression if needed.
  if (!T || T->is<ErrorType>()) {
    prepareForRetypechecking(ParsedExpr);
    ConcreteDeclRef ReferencedDecl = nullptr;
    auto optT = getTypeOfCompletionContextExpr(P.Context, CurDeclContext,
                                               CompletionTypeCheckKind::Normal,
                                               ParsedExpr, ReferencedDecl);
    if (!optT)
      return;
    T = *optT;
  }

  if (!T || T->is<ErrorType>() || T->is<UnresolvedType>())
    return;

  // Collect the modifiers.
  ExprModifierListResult result(T);
  getModifiers(T, {}, result.Modifiers);

  Consumer.handleResult(result);
}

void ExprModifierListCallbacks::getModifiers(
    Type T, ArrayRef<Type> expectedTypes,
    SmallVectorImpl<ValueDecl *> &result) {
  if (!T->mayHaveMembers())
    return;

  class LocalConsumer : public VisibleDeclConsumer {
    DeclContext *DC;
    LazyResolver *TypeResolver;
    ModuleDecl *CurModule;

    /// The type of the parsed expression.
    Type T;

    /// The list of expected types.
    ArrayRef<Type> ExpectedTypes;

    /// Result sink to populate.
    SmallVectorImpl<ValueDecl *> &Result;

    /// Returns true if \p VD is a instance method which return type is
    /// convertible to the requested types.
    bool isMatchingModifier(ValueDecl *VD) {
      if (!isa<FuncDecl>(VD))
        return false;
      if (VD->isStatic() || VD->isOperator())
        return false;

      auto declTy = T->getTypeOfMember(CurModule, VD);
      if (declTy->is<ErrorType>())
        return false;

      // Strip '(Self.Type) ->' and parameters.
      declTy = declTy->castTo<AnyFunctionType>()->getResult();
      declTy = declTy->castTo<AnyFunctionType>()->getResult();

      // Returns exact the same type, it's obviously a modifier.
      if (declTy->isEqual(T))
        return true;

      // The return type is convertible to the requested types.
      for (auto ty : ExpectedTypes) {
        if (swift::isConvertibleTo(declTy, ty, *DC))
          return true;
      }

      return false;
    }

  public:
    LocalConsumer(DeclContext *DC, Type T, ArrayRef<Type> expectedTypes,
                  SmallVectorImpl<ValueDecl *> &result)
        : DC(DC), TypeResolver(DC->getASTContext().getLazyResolver()),
          CurModule(DC->getParentModule()), T(T), ExpectedTypes(expectedTypes),
          Result(result) {}

    void foundDecl(ValueDecl *VD, DeclVisibilityKind reason) {
      if (isMatchingModifier(VD) && !VD->shouldHideFromEditor())
        Result.push_back(VD);
    }

  } LocalConsumer(CurDeclContext, T, expectedTypes, result);

  lookupVisibleMemberDecls(LocalConsumer, MetatypeType::get(T), CurDeclContext,
                           CurDeclContext->getASTContext().getLazyResolver(),
                           /*includeInstanceMembers=*/false);
}

} // anonymous namespace.

void PrintingExprModifierListConsumer::handleResult(
    const ExprModifierListResult &result) {
  OS << "-----BEGIN EXPR MODIFIER LIST-----\n";

  OS << "- TypeName: ";
  result.ExprType.print(OS);
  OS << "\n";

  OS << "- Modifiers: ";
  if (result.Modifiers.empty())
    OS << " []";
  OS << "\n";
  for (auto VD : result.Modifiers) {
    auto funcTy = cast<FuncDecl>(VD)->getMethodInterfaceType();
    funcTy = result.ExprType->getTypeOfMember(VD->getDeclContext()->getParentModule(), VD, funcTy);
    auto resultTy = funcTy->castTo<FunctionType>()->getResult();

    OS << "   - Name: ";
    VD->getFullName().print(OS);
    OS << "\n";

    OS << "     TypeName: ";
    resultTy.print(OS);
    OS << "\n";

    StringRef BriefDoc = VD->getBriefComment();
    if (!BriefDoc.empty()) {
      OS << "     DocBrief: \"";
      OS << VD->getBriefComment();
      OS << "\"\n";
    }
  }

  OS << "-----END EXPR MODIFIER LIST-----\n";
}

CodeCompletionCallbacksFactory *
swift::ide::makeExprModifierListCallbacksFactory(
    ArrayRef<const char *> expectedTypeNames,
    ExprModifierListConsumer &Consumer) {

  // CC callback factory which produces 'ContextInfoCallbacks'.
  class ExprModifierListCallbacksFactoryImpl
      : public CodeCompletionCallbacksFactory {
    ArrayRef<const char *> ExpectedTypeNames;
    ExprModifierListConsumer &Consumer;

  public:
    ExprModifierListCallbacksFactoryImpl(
        ArrayRef<const char *> ExpectedTypeNames,
        ExprModifierListConsumer &Consumer)
        : ExpectedTypeNames(ExpectedTypeNames), Consumer(Consumer) {}

    CodeCompletionCallbacks *createCodeCompletionCallbacks(Parser &P) override {
      return new ExprModifierListCallbacks(P, ExpectedTypeNames, Consumer);
    }
  };

  return new ExprModifierListCallbacksFactoryImpl(expectedTypeNames,
                                                  Consumer);
}
