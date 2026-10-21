/// The resilience domain is serialized only when it differs from the package
/// name.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg \
// RUN:   -resilience-domain MyDomain %t/File.swift \
// RUN:   -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: %llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-DOMAIN
// CHECK-DOMAIN: <MODULE_PACKAGE_NAME abbrevid={{[0-9]+}}/> blob data = 'MyLoggingPkg'
// CHECK-DOMAIN: <MODULE_RESILIENCE_DOMAIN abbrevid={{[0-9]+}}/> blob data = 'MyDomain'

/// A resilience domain without a package name is serialized.
// RUN: %target-swift-frontend -module-name Logging \
// RUN:   -resilience-domain MyDomain %t/File.swift \
// RUN:   -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: %llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-DOMAIN-ONLY
// CHECK-DOMAIN-ONLY-NOT: MODULE_PACKAGE_NAME
// CHECK-DOMAIN-ONLY: <MODULE_RESILIENCE_DOMAIN abbrevid={{[0-9]+}}/> blob data = 'MyDomain'

/// A resilience domain equal to the package name is not serialized.
// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg \
// RUN:   -resilience-domain MyLoggingPkg %t/File.swift \
// RUN:   -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: %llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-DEFAULT

/// Without a resilience domain, it's not serialized.
// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg \
// RUN:   %t/File.swift \
// RUN:   -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: %llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-DEFAULT
// CHECK-DEFAULT: <MODULE_PACKAGE_NAME abbrevid={{[0-9]+}}/> blob data = 'MyLoggingPkg'
// CHECK-DEFAULT-NOT: MODULE_RESILIENCE_DOMAIN

/// An empty resilience domain is an error.
// RUN: not %target-swift-frontend -module-name Logging -resilience-domain "" \
// RUN:   %t/File.swift -typecheck 2>&1 | %FileCheck %s -check-prefix CHECK-EMPTY
// CHECK-EMPTY: error: resilience-domain is empty

//--- File.swift
public func log(level: Int) {}
