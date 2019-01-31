protocol Target1 {}
protocol Target2 {}
protocol Target3 {}

protocol P {
  associatedtype Assoc
  func protocolMethod -> Self
}
extension P {
  func protocolMethod(asc: Assoc) -> Self { return self }
}

class C : P {
  typealias Assoc = String
  static func staticMethod() -> Self {}
  func instanceMethod(x: Int) -> C {}
  func methodForTarget1() -> Target1 {}
  func methodForTarget2() -> Target2 {}
}

func testing(obj: C) {
  let _ = obj.
}

// RUN: %sourcekitd-test -req=exprmodifierlist -pos=22:14 %s -- -module-name MyModule %s > %t.response
// RUN: diff -u %s.response %t.response
// RUN: %sourcekitd-test -req=exprmodifierlist -pos=22:14 %s -req-opts=expectedtypes='Target1;Target3' -- -module-name MyModule %s > %t.withtype.response
// RUN: diff -u %s.withtype.response %t.withtype.response
