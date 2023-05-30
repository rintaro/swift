//===--- AnyFreestandingMacroExpansion.h ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ANY_FREESTANDING_MACRO_EXPANSION_H
#define SWIFT_AST_ANY_FREESTANDING_MACRO_EXPANSION_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"

namespace swift {

/// A wrapper of either 'MacroExpansionExpr' or 'MacroExpansionDecl'.
class AnyFreestandingMacroExpansion {
  PointerUnion<MacroExpansionExpr *, MacroExpansionDecl *> Expansion;

  AnyFreestandingMacroExpansion(decltype(Expansion) Expansion)
      : Expansion(Expansion) {}

public:
  AnyFreestandingMacroExpansion(MacroExpansionExpr *mee) : Expansion(mee) {
    assert(mee && "should have a expression");
  }
  AnyFreestandingMacroExpansion(MacroExpansionDecl *med) : Expansion(med) {
    assert(med && "should have a declaration");
  }

  MacroExpansionExpr *getExpr() const {
    return Expansion.dyn_cast<MacroExpansionExpr *>();
  }
  MacroExpansionDecl *getDecl() const {
    return Expansion.dyn_cast<MacroExpansionDecl *>();
  }
  ASTNode getASTNode() const {
    if (auto mee = getExpr())
      return mee;
    return getDecl();
  }

#undef FORWARD_VARIANT
#define FORWARD_VARIANT(NAME)                                                  \
  if (auto *mee = Expansion.dyn_cast<MacroExpansionExpr *>())                  \
    return mee->NAME();                                                        \
  return Expansion.get<MacroExpansionDecl *>()->NAME();

  DeclContext *getDeclContext() const { FORWARD_VARIANT(getDeclContext); }
  SourceRange getSourceRange() const { FORWARD_VARIANT(getSourceRange); }
  SourceLoc getLoc() const { FORWARD_VARIANT(getLoc); }
  ConcreteDeclRef getMacroRef() const { FORWARD_VARIANT(getMacroRef); }
  ArgumentList *getArgs() const { FORWARD_VARIANT(getArgs); }
};

} // namespace swift

#endif // SWIFT_AST_ANY_FREESTANDING_MACRO_EXPANSION_H
