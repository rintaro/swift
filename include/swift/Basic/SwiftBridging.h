//===--- SwiftBridging.h ----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SWIFTBRIDGING_H
#define SWIFT_BASIC_SWIFTBRIDGING_H

#if __has_include(<swift/bridging>)
#include <swift/bridging>
#else

#define SWIFT_SELF_CONTAINED
#define SWIFT_RETURNS_INDEPENDENT_VALUE
#define SWIFT_SHARED_REFERENCE(_retain, _release)
#define SWIFT_IMMORTAL_REFERENCE
#define SWIFT_UNSAFE_REFERENCE
#define SWIFT_NAME(_name)
#define SWIFT_CONFORMS_TO_PROTOCOL(_moduleName_protocolName)
#define SWIFT_COMPUTED_PROPERTY
#define SWIFT_MUTATING
#define SWIFT_UNCHECKED_SENDABLE
#define SWIFT_NONCOPYABLE
#define SWIFT_NONESCAPABLE
#define SWIFT_ESCAPABLE
#define SWIFT_RETURNS_RETAINED
#define SWIFT_RETURNS_UNRETAINED

#endif

#if __has_attribute(swift_name)
// 'SWIFT_NAME' but accepting a string literal.
// Workaround for SWIFT_NAME is not clang-format friendly.
#define SWIFT_NAME_S(NAME) __attribute__((swift_name(NAME)))
#else
#define SWIFT_NAME_S(NAME)
#endif

#endif // SWIFT_BASIC_SWIFTBRIDGING_H
