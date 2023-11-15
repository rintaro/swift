//===--- Stmts.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  func generate(stmt node: StmtSyntax) -> BridgedStmt {
    switch node.as(StmtSyntaxEnum.self) {
    case .breakStmt:
      break
    case .continueStmt:
      break
    case .deferStmt:
      break
    case .discardStmt:
      break
    case .doStmt:
      break
    case .expressionStmt(let node):
      return self.generate(expressionStmt: node)
    case .fallThroughStmt:
      break
    case .forStmt:
      break
    case .guardStmt:
      break
    case .labeledStmt:
      break
    case .missingStmt:
      break
    case .repeatStmt:
      break
    case .returnStmt(let node):
      return self.generate(returnStmt: node).asStmt
    case .thenStmt:
      break
    case .throwStmt:
      break
    case .whileStmt:
      break
    case .yieldStmt:
      break
    }
    return self.generateWithLegacy(node)
  }

  public func generate(codeBlock node: CodeBlockSyntax) -> BridgedBraceStmt {
    BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: node.leftBrace.bridgedSourceLoc(in: self),
      elements: self.generate(codeBlockItemList: node.statements),
      rBraceLoc: node.rightBrace.bridgedSourceLoc(in: self)
    )
  }

  func makeIfStmt(_ node: IfExprSyntax) -> BridgedIfStmt {
    // FIXME: handle multiple coniditons.
    // FIXME: handle non-expression conditions.
    let conditions = node.conditions.map(self.generate(conditionElement:))
    assert(conditions.count == 1)

    return .createParsed(
      self.ctx,
      ifKeywordLoc: node.ifKeyword.bridgedSourceLoc(in: self),
      condition: conditions.first!.castToExpr,
      thenStmt: self.generate(codeBlock: node.body).asStmt,
      elseLoc: node.elseKeyword.bridgedSourceLoc(in: self),
      elseStmt: self.map(node.elseBody) {
        switch $0 {
        case .codeBlock(let node):
          return self.generate(codeBlock: node).asStmt
        case .ifExpr(let node):
          return self.makeIfStmt(node).asStmt
        }
      }
    )
  }

  public func generate(expressionStmt node: ExpressionStmtSyntax) -> BridgedStmt {
    switch Syntax(node.expression).as(SyntaxEnum.self) {
    case .ifExpr(let e):
      return makeIfStmt(e).asStmt
    default:
      fatalError("Unhandled case!")
    }
  }

  public func generate(returnStmt node: ReturnStmtSyntax) -> BridgedReturnStmt {
    .createParsed(
      self.ctx,
      returnKeywordLoc: node.returnKeyword.bridgedSourceLoc(in: self),
      expr: self.map(node.expression, generate(expr:))
    )
  }
}
