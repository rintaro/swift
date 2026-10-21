/// Package dependencies of a resilient module are loaded by clients in the same
/// package or in the same resilience domain.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -package-name pkgA -resilience-domain shared

/// Same resilience domain, different package.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name pkgB -resilience-domain shared \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// Package name matching the resilience domain.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name shared \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// Same package, different resilience domain.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name pkgA -resilience-domain other \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// Different package, different resilience domain.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name pkgB -resilience-domain other \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=HIDDEN-DEP %s

// VISIBLE-DEP: loaded module 'HiddenDep'
// HIDDEN-DEP-NOT: loaded module 'HiddenDep'

//--- HiddenDep.swift

//--- PackageDep.swift
package import HiddenDep

//--- Client.swift
import PackageDep
