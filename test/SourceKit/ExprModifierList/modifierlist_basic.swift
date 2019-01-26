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
  func instanceMethod(x: Int) -> C
}

func testing(obj: C) {
  let _ = obj.
}

// RUN: %sourcekitd-test -req=exprmodifierlist -pos=16:14 %s -- -module-name MyModule %s > %t.response
// RUN: diff -u %s.response %t.response
