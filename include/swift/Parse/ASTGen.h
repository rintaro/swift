//===--- ASTGen.h ---------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_ASTGEN_H
#define SWIFT_PARSE_ASTGEN_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
/// Generates AST nodes from Syntax nodes.
class ASTGen {
  ASTContext &Context;

  /// Type cache to prevent multiple transformations of the same syntax node.
  llvm::DenseMap<syntax::SyntaxNodeId, TypeRepr *> TypeCache;

  PersistentParserState **ParserState;

  // FIXME: remove when Syntax can represent all types and ASTGen can handle them
  /// Types that are not representable as syntax nodes.
  llvm::DenseMap<SourceLoc, TypeRepr *> Types;

public:
  ASTGen(ASTContext &Context, PersistentParserState **ParserState)
      : Context(Context), ParserState(ParserState) {}

  SourceLoc generate(syntax::TokenSyntax Tok, SourceLoc &Loc);

  Expr *generate(syntax::IntegerLiteralExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::FloatLiteralExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::NilLiteralExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::BooleanLiteralExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::PoundFileExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::PoundLineExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::PoundColumnExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::PoundFunctionExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::PoundDsohandleExprSyntax &Expr, SourceLoc &Loc);
  Expr *generate(syntax::UnknownExprSyntax &Expr, SourceLoc &Loc);

  TypeRepr *generate(syntax::TypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::SomeTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::CompositionTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::SimpleTypeIdentifierSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::MemberTypeIdentifierSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::DictionaryTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::ArrayTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::TupleTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::AttributedTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::FunctionTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::MetatypeTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::OptionalTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::ImplicitlyUnwrappedOptionalTypeSyntax Type, SourceLoc &Loc);
  TypeRepr *generate(syntax::UnknownTypeSyntax Type, SourceLoc &Loc);

  TypeRepr *generate(syntax::GenericArgumentSyntax Arg, SourceLoc &Loc);
  llvm::SmallVector<TypeRepr *, 4>
  generate(syntax::GenericArgumentListSyntax Args, SourceLoc &Loc);

  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

private:
  Expr *generateMagicIdentifierLiteralExpression(syntax::TokenSyntax PoundToken,
                                                 SourceLoc &Loc);

  TupleTypeRepr *generateTuple(syntax::TokenSyntax LParen,
                               syntax::TupleTypeElementListSyntax Elements,
                               syntax::TokenSyntax RParen, SourceLoc &Loc,
                               bool IsFunction = false);

  void gatherTypeIdentifierComponents(
      syntax::TypeSyntax Component, SourceLoc &Loc,
      llvm::SmallVectorImpl<ComponentIdentTypeRepr *> &Components);

  template <typename T>
  TypeRepr *generateSimpleOrMemberIdentifier(T Type, SourceLoc &Loc);

  template <typename T>
  ComponentIdentTypeRepr *generateIdentifier(T Type, SourceLoc &Loc);

  StringRef copyAndStripUnderscores(StringRef Orig);

  static SourceLoc advanceLocBegin(const SourceLoc &Loc,
                                   const syntax::Syntax &Node);
  static SourceLoc advanceLocEnd(const SourceLoc &Loc,
                                 const syntax::TokenSyntax &Token);
  static SourceLoc advanceLocAfter(const SourceLoc &Loc,
                                   const syntax::Syntax &Node);

  static MagicIdentifierLiteralExpr::Kind getMagicIdentifierLiteralKind(tok Kind);

  ValueDecl *lookupInScope(DeclName Name);

  TypeRepr *cacheType(syntax::TypeSyntax Type, TypeRepr *TypeAST);

  TypeRepr *lookupType(syntax::TypeSyntax Type);

public:
  TypeRepr *addType(TypeRepr *Type, const SourceLoc &Loc);

  bool hasType(const SourceLoc &Loc) const;

  TypeRepr *getType(const SourceLoc &Loc) const;
};
} // namespace swift

#endif // SWIFT_PARSE_ASTGEN_H
