@_spi(Hello)
public struct World {}

// RUN: %sourcekitd-test -req=cursor -pos=2:15 %s -- %s | %FileCheck -check-prefix=CHECK %s
// CHECK-INT: source.lang.swift.ref.struct
// CHECK-INT: s:11cursor_init1AV
// CHECK-INT: SECONDARY SYMBOLS BEGIN
// CHECK-INT-NEXT: source.lang.swift.ref.function.constructor {{[^|]*}}
// CHECK-INT-NEXT: init(arg:)
// CHECK-INT-NEXT: s:11cursor_init1AV3argACSi_tcfc
// CHECK-INT-NEXT: source.lang.swift
// CHECK-INT: -----
// CHECK-INT-NEXT: SECONDARY SYMBOLS END

