if (SWIFT_BUILD_SWIFT_SYNTAX)

  add_swift_host_library(_swiftCSwiftPluginServer STATIC
    Sources/CSwiftPluginServer/PluginServer.cpp
    LLVM_LINK_COMPONENTS support
  )
  target_link_libraries(_swiftCSwiftPluginServer PRIVATE
    swiftDemangling
  )
  target_include_directories(_swiftCSwiftPluginServer PUBLIC
    Sources/CSwiftPluginServer/include
  )

  add_pure_swift_host_library(SwiftLibraryPluginProvider STATIC
    Sources/SwiftLibraryPluginProvider/LibraryPluginProvider.swift
    DEPENDENCIES
      _swiftCSwiftPluginServer
    SWIFT_DEPENDENCIES
      SwiftSyntaxMacros
  )

  add_pure_swift_host_library(SwiftInProcPluginServer SHARED
    Sources/SwiftInProcPluginServer/InProcPluginServer.swift
    SWIFT_DEPENDENCIES
      SwiftLibraryPluginProvider
  )

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftLibraryPluginProvider
  )
endif()
