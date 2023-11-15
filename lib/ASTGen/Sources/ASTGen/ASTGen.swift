//===--- ASTGen.swift -----------------------------------------------------===//
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
import BasicBridging
import ParseBridging
// Needed to use BumpPtrAllocator
@_spi(BumpPtrAllocator) import SwiftSyntax

import struct SwiftDiagnostics.Diagnostic

extension UnsafePointer {
  public var raw: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: self)
  }
}

enum ASTNode {
  case decl(BridgedDecl)
  case stmt(BridgedStmt)
  case expr(BridgedExpr)

  var castToExpr: BridgedExpr {
    guard case .expr(let bridged) = self else {
      fatalError("Expected an expr")
    }
    return bridged
  }

  var castToStmt: BridgedStmt {
    guard case .stmt(let bridged) = self else {
      fatalError("Expected a stmt")
    }
    return bridged
  }

  var castToDecl: BridgedDecl {
    guard case .decl(let bridged) = self else {
      fatalError("Expected a decl")
    }
    return bridged
  }

  var bridged: BridgedASTNode {
    switch self {
    case .expr(let e):
      return BridgedASTNode(raw: e.raw, kind: .expr)
    case .stmt(let s):
      return BridgedASTNode(raw: s.raw, kind: .stmt)
    case .decl(let d):
      return BridgedASTNode(raw: d.raw, kind: .decl)
    }
  }
}

/// Little utility wrapper that lets us have some mutable state within
/// immutable structs, and is therefore pretty evil.
@propertyWrapper
class Boxed<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

struct ASTGenVisitor {
  fileprivate let diagnosticEngine: BridgedDiagnosticEngine

  let base: UnsafeBufferPointer<UInt8>

  @Boxed private(set) var declContext: BridgedDeclContext

  let ctx: BridgedASTContext

  fileprivate let allocator: SwiftSyntax.BumpPtrAllocator = .init(slabSize: 256)

  /// Fallback legacy parser used when ASTGen doesn't have the generate(_:)
  /// implementation for the AST node kind.
  let legacyParse: BridgedLegacyParser

  init(
    diagnosticEngine: BridgedDiagnosticEngine,
    sourceBuffer: UnsafeBufferPointer<UInt8>,
    declContext: BridgedDeclContext,
    astContext: BridgedASTContext,
    legacyParser: BridgedLegacyParser
  ) {
    self.diagnosticEngine = diagnosticEngine
    self.base = sourceBuffer
    self.declContext = declContext
    self.ctx = astContext
    self.legacyParse = legacyParser
  }

  public func generate(sourceFile node: SourceFileSyntax) -> [BridgedDecl] {
    var out = [BridgedDecl]()

    for element in node.statements {
      let loc = element.bridgedSourceLoc(in: self)
      let swiftASTNodes = generate(codeBlockItem: element)
      switch swiftASTNodes {
      case .decl(let d):
        out.append(d)
      case .stmt(let s):
        let topLevelDecl = BridgedTopLevelCodeDecl.createParsed(
          self.ctx,
          declContext: self.declContext,
          startLoc: loc,
          stmt: s,
          endLoc: loc
        )
        out.append(topLevelDecl.asDecl)
      case .expr(let e):
        let topLevelDecl = BridgedTopLevelCodeDecl.createParsed(
          self.ctx,
          declContext: self.declContext,
          startLoc: loc,
          expr: e,
          endLoc: loc
        )
        out.append(topLevelDecl.asDecl)
      default:
        fatalError("Top level nodes must be decls, stmts, or exprs.")
      }
    }

    return out
  }
}

extension ASTGenVisitor {
  /// Replaces the current declaration context with `declContext` for the duration of its execution, and calls `body`.
  @inline(__always)
  func withDeclContext(_ declContext: BridgedDeclContext, _ body: () -> Void) {
    let oldDeclContext = self.declContext
    self.declContext = declContext
    body()
    self.declContext = oldDeclContext
  }
}

extension ASTGenVisitor {
  /// Emits the given diagnostic via the C++ diagnostic engine.
  @inline(__always)
  func diagnose(_ diagnostic: Diagnostic) {
    emitDiagnostic(
      diagnosticEngine: self.diagnosticEngine,
      sourceFileBuffer: self.base,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }
}

// Misc visits.
// TODO: Some of these are called within a single file/method; we may want to move them to the respective files.
extension ASTGenVisitor {
  public func generate(memberBlockItem node: MemberBlockItemSyntax) -> BridgedDecl {
    generate(decl: node.decl)
  }

  public func generate(initializerClause node: InitializerClauseSyntax) -> BridgedExpr {
    generate(expr: node.value)
  }

  public func generate(conditionElement node: ConditionElementSyntax) -> ASTNode {
    // FIXME: returning ASTNode is wrong, non-expression conditions are not ASTNode.
    switch node.condition {
    case .availability(_):
      break
    case .expression(let node):
      return .expr(self.generate(expr: node))
    case .matchingPattern(_):
      break
    case .optionalBinding(_):
      break
    }
    fatalError("unimplemented")
  }

  public func generate(codeBlockItem node: CodeBlockItemSyntax) -> ASTNode {
    switch node.item {
    case .decl(let node):
      return .decl(self.generate(decl: node))
    case .stmt(let node):
      return .stmt(self.generate(stmt: node))
    case .expr(let node):
      return .expr(self.generate(expr: node))
    }
  }

  public func generate(arrayElement node: ArrayElementSyntax) -> BridgedExpr {
    generate(expr: node.expression)
  }

  @inline(__always)
  func generate(codeBlockItemList node: CodeBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate(codeBlockItem: $0).bridged }.bridgedArray(in: self)
  }
}

// Optional node handling.
extension ASTGenVisitor {
  /// Generate an AST node from an optional node using the generator function.
  ///
  /// Usage:
  ///   self.map(node.initializer, generate(expr:))
  @inline(__always)
  func map<Node: SyntaxProtocol, Result: HasNullable>(
    _ node: Node?,
    _ body: (Node) -> Result
  ) -> Result.Nullable {
    return Result.asNullable(node.map(body))
  }

  /// Generate an AST node array from an optional collection node using the
  /// generator function. If `nil`, returns an empty array.
  ///
  /// Usage:
  ///   self.map(node.genericWhereClasuse, generate(genericWhereClause:))
  @inline(__always)
  func map<Node: SyntaxCollection>(
    _ node: Node?,
    _ body: (Node) -> BridgedArrayRef
  ) -> BridgedArrayRef {
    return node.map(body) ?? .init()
  }
}

extension Collection {
  /// Like ``Sequence.compactMap(_:)``, but returns a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  ///
  /// - Note: The purpose of this method is to make up for the performance toll of calling ``Collection.bridgedArray``
  ///   on a ``LazyFilterSequence`` due to the `count` access.
  func compactMap<T>(in astgen: ASTGenVisitor, _ transform: (Element) -> T?) -> BridgedArrayRef {
    if self.isEmpty {
      return .init()
    }

    let baseAddress = astgen.allocator.allocate(T.self, count: self.count).baseAddress!
    do {
      // A loop instead of `initialize(from: self.lazy.compactMap(transform))` because we aren't
      // doing a great job optimizing the latter.
      var currentAddress = baseAddress
      for element in self {
        guard let transformed = transform(element) else {
          continue
        }

        currentAddress.initialize(to: transformed)
        currentAddress += 1
      }
    }

    return .init(data: baseAddress, count: self.count)
  }
}

extension LazyCollectionProtocol {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    if self.isEmpty {
      return .init()
    }

    let buffer = astgen.allocator.allocate(Element.self, count: self.count)
    _ = buffer.initialize(from: self)

    return .init(data: buffer.baseAddress, count: self.count)
  }
}

// 'ReversedCollection' does not conform to 'LazyCollectionProtocol', and cannot here because it only
// conditionally conforms to 'LazySequenceProtocol' in the standard library.
// FIXME: We could make it conform unconditionally
extension ReversedCollection {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  @inline(__always)
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    self.lazy.bridgedArray(in: astgen)
  }
}

extension Optional where Wrapped: LazyCollectionProtocol {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  @inline(__always)
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    guard let self else {
      return .init()
    }

    return self.bridgedArray(in: astgen)
  }
}

/// Generate AST nodes for all top-level entities in the given source file.
@_cdecl("swift_ASTGen_buildTopLevelASTNodes")
public func buildTopLevelASTNodes(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  dc: BridgedDeclContext,
  ctx: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  outputContext: UnsafeMutableRawPointer,
  callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: dc,
    astContext: ctx,
    legacyParser: legacyParser
  )
  .generate(sourceFile: sourceFile.pointee.syntax)
  .forEach { callback($0.raw, outputContext) }
}

/// Generate an AST node at the given source location. Returns the generated
/// ASTNode and mutate the pointee of `endLocPtr` to the end of the node.
private func _build<Node: SyntaxProtocol, Result>(
  generator: (ASTGenVisitor) -> (Node) -> Result,
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> Result? {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)

  // Find the type syntax node.
  guard
    let node = findSyntaxNodeInSourceFile(
      sourceFilePtr: sourceFilePtr,
      // FIXME: findSyntaxNodeInSourceFile should receive `BridgedSourceLoc`.
      sourceLocationPtr: sourceLoc.getOpaquePointerValue()?.assumingMemoryBound(to: UInt8.self),
      type: Node.self,
      wantOutermost: true
    )
  else {
    // FIXME: Produce an error
    return nil
  }

  // Fill in the end location.
  endLocPtr.pointee = sourceLoc.advanced(by: node.totalLength.utf8Length)

  // Convert the syntax node.
  return generator(ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser
  ))(node)
}

@_cdecl("swift_ASTGen_buildTypeRepr")
@usableFromInline
func buildTypeRepr(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    generator: ASTGenVisitor.generate(type:),
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )?.raw
}

@_cdecl("swift_ASTGen_buildDecl")
@usableFromInline
func buildDecl(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    generator: ASTGenVisitor.generate(decl:),
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )?.raw
}

@_cdecl("swift_ASTGen_buildExpr")
@usableFromInline
func buildExpr(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    generator: ASTGenVisitor.generate(expr:),
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )?.raw
}

@_cdecl("swift_ASTGen_buildStmt")
@usableFromInline
func buildStmt(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    generator: ASTGenVisitor.generate(stmt:),
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )?.raw
}
