// A non-stdlib module's documentation groups are recorded in the index store
// as if they were submodules, so each grouped declaration's record carries the
// group as a dotted suffix on the module name while the unit keeps the bare
// module name.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: split-file %s %t

// --- Build the system framework with group information. Every source file
// --- must appear in the group info, so use one group per file.
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -emit-module-doc \
// RUN:     -group-info-path %t/group.json \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/FileA.swift %t/FileB.swift

// --- Index a client that imports it.
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-ignore-stdlib \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/Client.swift

// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

// The unit is named after the bare module, while each record is attributed to
// its group as a submodule-style module name.
// CHECK-DAG: Unit | system | SomeModule | {{.*}}SomeModule.swiftmodule
// CHECK-DAG: Record | system | SomeModule.GroupA | {{.*}} | {{.*}}_GroupA-
// CHECK-DAG: Record | system | SomeModule.GroupB | {{.*}} | {{.*}}_GroupB-

//--- group.json
{
  "GroupA": ["FileA.swift"],
  "GroupB": ["FileB.swift"]
}

//--- FileA.swift
public func groupedFuncA() {}

//--- FileB.swift
public func groupedFuncB() {}

//--- Client.swift
import SomeModule
