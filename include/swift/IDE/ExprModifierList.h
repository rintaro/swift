//===--- ExprModifierList.h --- ---------------------------------*- C++ -*-===//
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

#ifndef SWIFT_IDE_EXPRMODIFIERLIST_H
#define SWIFT_IDE_EXPRMODIFIERLIST_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"

namespace swift {
class CodeCompletionCallbacksFactory;

namespace ide {

/// A result item for context info query.
class ExprModifierListResult {
public:
  /// The resolved type of the expression.
  Type ExprType;

  /// "Modifier"s that can be called on the expression.
  SmallVector<ValueDecl *, 0> Modifiers;

  ExprModifierListResult(Type ExprType) : ExprType(ExprType) {}
};

/// An abstract base class for consumers of context info results.
class ExprModifierListConsumer {
public:
  virtual ~ExprModifierListConsumer() {}
  virtual void handleResult(const ExprModifierListResult &result) = 0;
};

/// Printing consumer
class PrintingExprModifierListConsumer : public ExprModifierListConsumer {
  llvm::raw_ostream &OS;

public:
  PrintingExprModifierListConsumer(llvm::raw_ostream &OS) : OS(OS) {}

  void handleResult(const ExprModifierListResult &result) override;
};

/// Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeExprModifierListCallbacksFactory(ArrayRef<const char *> expectedTypeNames,
                                     ExprModifierListConsumer &Consumer);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_EXPRMODIFIERLIST_H
