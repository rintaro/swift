// swift-tools-version: 5.6

import PackageDescription

let swiftSourceDirectory = #filePath
  .split(separator: "/", omittingEmptySubsequences: false)
  .dropLast(3) // Remove 'tools', 'swift-plugin-server', 'Package.swift'
  .joined(separator: "/")

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
  ],
  targets: [
    .target(
      name: "CSwiftPluginServer",
      cSettings: [
        .unsafeFlags([
          "-I", "\(swiftSourceDirectory)/include",
          "-I", "\(swiftSourceDirectory)/stdlib/public/SwiftShims",
          "-I", "\(swiftSourceDirectory)/../llvm-project/llvm/include",
          "-I", "\(swiftSourceDirectory)/../build/Default/llvm/include",
          "-I", "\(swiftSourceDirectory)/../build/Default/swift/include",
        ])
      ],
      cxxSettings: [
        .unsafeFlags([
          "-I", "\(swiftSourceDirectory)/include",
          "-I", "\(swiftSourceDirectory)/stdlib/public/SwiftShims",
          "-I", "\(swiftSourceDirectory)/../llvm-project/llvm/include",
          "-I", "\(swiftSourceDirectory)/../build/Default/llvm/include",
          "-I", "\(swiftSourceDirectory)/../build/Default/swift/include",
        ])
      ]
    ),
    .target(
      name: "SwiftLibraryPluginProvider",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
        "CSwiftPluginServer",
      ]
    ),
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        "SwiftLibraryPluginProvider",
      ]
    ),
    .target(
      name: "SwiftInProcPluginServer",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        "SwiftLibraryPluginProvider",
      ]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
