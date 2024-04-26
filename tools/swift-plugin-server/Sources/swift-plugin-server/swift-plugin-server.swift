//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(PluginMessage) import SwiftLibraryPluginProvider
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

let globalHandler = CompilerPluginMessageHandler(
  provider: LibraryPluginProvider()
)

@main
struct SwiftPluginServer {
  /// @main entry point.
  static func main() throws {
    let connection = try StandardIOMessageConnection()
    let listener = CompilerPluginMessageListener(
      connection: connection,
      provider: LibraryPluginProvider()
    )
    listener.main()
  }
}

