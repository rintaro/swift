// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_1 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_2 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_3 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_4 | %FileCheck %s -check-prefix=CHECK_1

protocol P {
  func protocolMethod -> Self
}
extension P {
  func protocolMethod() -> Self { return self }
}

class C : P {
  static func staticMethod() -> Self {}
  func instanceMethod() -> C
}

func testing(obj: C) {
  let _ = obj #^TEST_1^#
  let _ = obj .#^TEST_2^#
  let _ = obj.instanceMethod()#^TEST_3^#
  let _ = obj.instanceMethod().#^TEST_4^#
}

// CHECK_1:      -----BEGIN EXPR MODIFIER LIST-----
// CHECK_1_NEXT: - TypeName: C
// CHECK_1_NEXT: - Modifiers:
// CHECK_1_NEXT:    - Name: instanceMethod()
// CHECK_1_NEXT:      TypeName: C
// CHECK_1_NEXT:    - Name: protocolMethod()
// CHECK_1_NEXT:      TypeName: C
// CHECK_1_NEXT: -----END EXPR MODIFIER LIST-----
