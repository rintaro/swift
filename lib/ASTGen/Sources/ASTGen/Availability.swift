//===--- Attrs.swift ------------------------------------------------------===//
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
import SwiftDiagnostics
import SwiftParserDiagnostics
import SwiftIfConfig
@_spi(Compiler) import SwiftParser
@_spi(RawSyntax) @_spi(Compiler) import SwiftSyntax

@_cdecl("swift_ASTGen_parseAvailabilityMacroDefinition")
func parseAvailabilityMacroDefinition(
  ctx: BridgedASTContext,
  dc: BridgedDeclContext,
  diagEngine: BridgedDiagnosticEngine,
  buffer: BridgedStringRef,
  outPtr: UnsafeMutablePointer<BridgedAvailabilityMacroDefinition>
) {
  let buffer = UnsafeBufferPointer(start: buffer.data, count: buffer.count)

  // Parse.
  var parser = Parser(buffer)
  let parsed = AvailabilityMacroDefinitionSyntax.parse(from: &parser)

  // Emit diagnostics.
  let diagnostics = ParseDiagnosticsGenerator.diagnostics(for: parsed)
  for diagnostic in diagnostics {
    emitDiagnostic(
      diagnosticEngine: diagEngine,
      sourceFileBuffer: buffer,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }
  
  // Generate.
  let config = CompilerBuildConfiguration(ctx: ctx, sourceBuffer: buffer)
  let configuredRegions = parsed.configuredRegions(in: config)
  // FIXME: 'declContext' and 'configuredRegions' are never used.
  let generator = ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: buffer,
    declContext: dc,
    astContext: ctx,
    configuredRegions: configuredRegions
  )
  let generated = generator.generate(availabilityMacroDefinition: parsed)
  outPtr.pointee = generated
}

@_cdecl("swift_ASTGen_freeAvailabilityMacroDefinition")
func freeAvailabilityMacroDefinition(
  defintion: BridgedAvailabilityMacroDefinition
) {
  freeBridgedString(bridged: defintion.name)

  let specs = defintion.specs
  let specsBuffer = UnsafeMutableBufferPointer(
    start: UnsafeMutablePointer(mutating: specs.data?.assumingMemoryBound(to: BridgedAvailabilitySpec.self)),
    count: specs.count
  )
  specsBuffer.deinitialize()
  specsBuffer.deallocate()
}

extension ASTGenVisitor {
  func generate(availabilityMacroDefinition node: AvailabilityMacroDefinitionSyntax) -> BridgedAvailabilityMacroDefinition {

    let name = allocateBridgedString(node.nameAndVersion.platform.text)
    let version = self.generate(versionTuple: node.nameAndVersion.version)
    let specs = self.generateAvailabilitySpecList(args: node.specs)

    let specsBuffer = UnsafeMutableBufferPointer<BridgedAvailabilitySpec>.allocate(capacity: specs.count)
    _ = specsBuffer.initialize(from: specs)

    return BridgedAvailabilityMacroDefinition(
      name: name,
      version: version?.bridged ?? BridgedVersionTuple(),
      specs: BridgedArrayRef(data: UnsafeRawPointer(specsBuffer.baseAddress), count: specsBuffer.count)
    )
  }

  func generateAvailabilitySpecList(args node: AvailabilityArgumentListSyntax) -> [BridgedAvailabilitySpec] {
    var result: [BridgedAvailabilitySpec] = []

    for parsed in node {
      switch parsed.argument {
      case .token(let tok) where tok.rawText == "*":
        let spec = BridgedOtherPlatformAvailabilitySpec.createParsed(
          self.ctx,
          loc: self.generateSourceLoc(tok)
        )
        result.append(spec.asAvailabilitySpec)
      case .availabilityVersionRestriction(let platformVersion):
        let nameLoc = self.generateSourceLoc(platformVersion.platform)
        let version = self.generate(versionTuple: platformVersion.version)
        let versionRange = self.generateSourceRange(platformVersion.version)

        func platformAgnostic(_ kind: BridgedAvailabilitySpecKind) {
          let spec = BridgedPlatformAgnosticVersionConstraintAvailabilitySpec.createParsed(
            self.ctx,
            kind: kind,
            nameLoc: nameLoc,
            version: version?.bridged ?? BridgedVersionTuple(),
            versionRange: versionRange
          )
          result.append(spec.asAvailabilitySpec)
        }

        switch platformVersion.platform.rawText {
        case "swift":
          platformAgnostic(.languageVersionConstraint)
        case "_PackageVersion":
          platformAgnostic(.packageDescriptionVersionConstraint)
        case let name:
          let platform = BridgedPlatformKind(from: name.bridged)
          guard platform != .none else {
            // TODO: Diagnostics.
            preconditionFailure("invalid platform kind")
          }
          let spec = BridgedPlatformVersionConstraintAvailabilitySpec.createParsed(
            self.ctx,
            platform: platform,
            platformLoc: nameLoc,
            version: version?.bridged ?? BridgedVersionTuple(),
            runtimeVersion: version?.bridged ?? BridgedVersionTuple(),
            versionRange: versionRange
          )
          result.append(spec.asAvailabilitySpec)
        }
      default:
        // TODO: Diagnostics.
        preconditionFailure("invalid argument kind for availability spec")
      }
    }

    return result
  }
}
