# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

set(string_processing_dependencies)
if (SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING)
  list(APPEND string_processing_dependencies _MatchingEngine)
endif()

add_swift_compiler_module(Parse
  DEPENDS
    Basic
    ${string_processing_dependencies}
  SOURCES
    Parse.swift
    Regex.swift)

