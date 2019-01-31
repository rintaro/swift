// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_1 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_2 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_3 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_4 | %FileCheck %s -check-prefix=CHECK_1

// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_1 -modifier-list-expected-types Target | %FileCheck %s -check-prefix=CHECK_2
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_2 -modifier-list-expected-types Target | %FileCheck %s -check-prefix=CHECK_2
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_3 -modifier-list-expected-types Target | %FileCheck %s -check-prefix=CHECK_2
// RUN: %target-swift-ide-test -modifier-list -source-filename %s -code-completion-token=TEST_4 -modifier-list-expected-types Target | %FileCheck %s -check-prefix=CHECK_2

protocol Target {}

protocol P {
  func protocolMethod -> Self
}
extension P {
  func protocolMethod() -> Self { return self }
}

class C : P {
  static func staticMethod() -> Self { fatalError() }
  func instanceMethod() -> C { fatalError() }
  func methodForTarget() -> Target { fatalError() }
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

// CHECK_2:      -----BEGIN EXPR MODIFIER LIST-----
// CHECK_2_NEXT: - TypeName: C
// CHECK_2_NEXT: - Modifiers:
// CHECK_2_NEXT:    - Name: instanceMethod()
// CHECK_2_NEXT:      TypeName: C
// CHECK_2_NEXT:    - Name: methodForTarget()
// CHECK_2_NEXT:      TypeName: Target
// CHECK_2_NEXT:    - Name: protocolMethod()
// CHECK_2_NEXT:      TypeName: C
// CHECK_2_NEXT: -----END EXPR MODIFIER LIST-----
