/// Package CMO bodies and non-resilient access are available to clients in the
/// same resilience domain, regardless of their package names.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -package-name pkgA -resilience-domain shared \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -O -wmo -allow-non-resilient-access -package-cmo \
// RUN:   -emit-module-path %t/Lib.swiftmodule

/// A client in a different package but in the same resilience domain.
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -package-name pkgB -resilience-domain shared \
// RUN:   -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination \
// RUN:   | %FileCheck %s -check-prefix=CHECK-IN-DOMAIN

/// A client in the same package as the library's package name, without a
/// resilience domain, is in a different resilience domain.
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -package-name pkgA \
// RUN:   -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination \
// RUN:   | %FileCheck %s -check-prefix=CHECK-OUT-OF-DOMAIN

/// A client whose package name matches the library's resilience domain is in
/// the same resilience domain.
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -package-name shared \
// RUN:   -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination \
// RUN:   | %FileCheck %s -check-prefix=CHECK-IN-DOMAIN

/// A client in a different package and a different resilience domain.
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -package-name pkgB -resilience-domain other \
// RUN:   -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination \
// RUN:   | %FileCheck %s -check-prefix=CHECK-OUT-OF-DOMAIN

/// Bodies serialized for package are deserialized only in the same resilience
/// domain, where they access the resilient struct's stored property directly.
// CHECK-IN-DOMAIN-LABEL: sil public_external @$s3Lib6PubStrV5valueSivg : $@convention(method) (@in_guaranteed PubStr) -> Int {
// CHECK-IN-DOMAIN:         struct_element_addr %0, #PubStr.value
// CHECK-IN-DOMAIN-LABEL: sil public_external @$s3Lib3PubCyACSicfc : $@convention(method) (Int, @owned Pub) -> @owned Pub {

// CHECK-OUT-OF-DOMAIN: sil @$s3Lib6PubStrV5valueSivg : $@convention(method) (@in_guaranteed PubStr) -> Int{{$}}
// CHECK-OUT-OF-DOMAIN: sil @$s3Lib3PubCyACSicfc : $@convention(method) (Int, @owned Pub) -> @owned Pub{{$}}

/// Tables serialized for package are deserialized only in the same resilience
/// domain.
// CHECK-IN-DOMAIN: sil_vtable Pub {
// CHECK-OUT-OF-DOMAIN-NOT: sil_vtable Pub {

//--- Lib.swift
public struct PubStr {
  public var value: Int
  public init(_ value: Int) { self.value = value }
}

public class Pub {
  public var pubVar: Int
  public init(_ arg: Int) {
    pubVar = arg
  }
}

//--- Client.swift
import Lib

public func getValue(_ s: PubStr) -> Int {
  return s.value
}

public func makePub(_ arg: Int) -> Int {
  return Pub(arg).pubVar
}
