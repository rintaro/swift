//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling
@_spi(PluginMessage) import SwiftLibraryPluginProvider

import Darwin

/// Entry point.
///
/// Compiler 'dlopen' this 'SwiftInProcPluginServer' library, and 'dlsym' this
/// function. When the compiler wants to use dylib plugins, it calls this
/// function with the same message as `swift-plugin-server`.
///
/// The caller must `free` the returned buffer
@_cdecl("swift_inproc_plugins_handle_message")
public func handleMessage(
  _ inputData: UnsafePointer<UInt8>!,
  _ inputLength: Int,
  _ outputData: UnsafeMutablePointer<UnsafePointer<UInt8>?>!,
  _ outputLength: UnsafeMutablePointer<Int>!
) {
  do {
    let input = UnsafeBufferPointer(start: inputData, count: inputLength)
    let output = try InProcPluginServer.shared.handleMessage(input)

    // FIXME: Better way to return buffer?
    precondition(outputData.pointee == nil)
    let outData = malloc(output.count)!
    output.withUnsafeBufferPointer { buf -> Void in
      outData.initializeMemory(
        as: UInt8.self,
        from: buf.baseAddress!,
        count: buf.count
      )
      outputData.pointee = UnsafePointer(outData.assumingMemoryBound(to: UInt8.self))
      outputLength.pointee = output.count
    }

  } catch {
    fatalError("Internal Error: \(error)")
  }
}

/// Singleton "plugin server".
struct InProcPluginServer {
  let handler: CompilerPluginMessageHandler<LibraryPluginProvider>

  init() {
    self.handler = CompilerPluginMessageHandler(
      provider: LibraryPluginProvider()
    )
  }

  func handleMessage(_ input: UnsafeBufferPointer<UInt8>) throws -> [UInt8] {
    let request = try JSON.decode(HostToPluginMessage.self, from: input)
    let response =  handler.handleMessage(request)
    return try JSON.encode(response)
  }

  static let shared = Self()
}

