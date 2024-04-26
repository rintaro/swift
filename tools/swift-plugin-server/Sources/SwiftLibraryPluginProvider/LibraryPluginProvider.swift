import CSwiftPluginServer
import SwiftSyntaxMacros
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

private struct LibraryPluginError: Error, CustomStringConvertible {
  var description: String
  init(message: String) {
    self.description = message
  }
}

@_spi(PluginMessage)
public class LibraryPluginProvider: PluginProvider {
  struct LoadedLibraryPlugin {
    var libraryPath: String
    var handle: UnsafeMutableRawPointer
  }

  
  struct MacroRef: Hashable {
    var moduleName: String
    var typeName: String
  }

  /// Loaded dylib handles associated with the module name.
  var loadedLibraryPlugins: [String: LoadedLibraryPlugin] = [:]

  /// Resolved cached macros.
  var resolvedMacros: [MacroRef: Macro.Type] = [:]

  public init() {}

  public func loadPluginLibrary(libraryPath: String, moduleName: String) throws {
    var errorMessage: UnsafePointer<CChar>?
    guard let dlHandle = PluginServer_load(libraryPath, &errorMessage) else {
      throw LibraryPluginError(message: "loader error: " + String(cString: errorMessage!))
    }
    loadedLibraryPlugins[moduleName] = LoadedLibraryPlugin(
      libraryPath: libraryPath,
      handle: dlHandle
    )
  }

  public func resolveMacro(moduleName: String, typeName: String) throws -> SwiftSyntaxMacros.Macro.Type {
    let macroRef = MacroRef(moduleName: moduleName, typeName: typeName)
    if let resolved = resolvedMacros[macroRef] {
      return resolved
    }

    // Find 'dlopen'ed library for the module name.
    guard let plugin = loadedLibraryPlugins[moduleName] else {
      // NOTE: This should be unreachable. Compiler should not use this server
      // unless the plugin loading succeeded.
      throw LibraryPluginError(message: "(plugin-server) plugin not loaded for module '\(moduleName)'")
    }

    // Lookup the type metadata.
    var errorMessage: UnsafePointer<CChar>?
    guard let macroTypePtr = PluginServer_lookupMacroTypeMetadataByExternalName(
      moduleName, typeName, plugin.handle, &errorMessage
    ) else {
      throw LibraryPluginError(message: "macro implementation type '\(moduleName).\(typeName)' could not be found in library plugin '\(plugin.libraryPath)'")
    }

    // THe type must be a 'Macro' type.
    let macroType = unsafeBitCast(macroTypePtr, to: Any.Type.self)
    guard let macro = macroType as? Macro.Type else {
      throw LibraryPluginError(message: "type '\(moduleName).\(typeName)' is not a valid macro implementation type in library plugin '\(plugin.libraryPath)'")
    }

    // Cache the resolved type.
    resolvedMacros[macroRef] = macro
    return macro
  }

  /// This 'PluginProvider' implements 'loadLibraryMacro()'.
  public var features: [SwiftCompilerPluginMessageHandling.PluginFeature] {
    [.loadPluginLibrary]
  }
}
