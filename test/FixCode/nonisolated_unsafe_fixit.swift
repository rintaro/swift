// RUN: %target-typecheck-verify-swift -swift-version 6

// https://github.com/swiftlang/swift/issues/87342

class MyClass {
  nonisolated(unsafe) var computedValue: Int? {
  // expected-warning@-1 {{'nonisolated(unsafe)' has no effect on property 'computedValue', consider using 'nonisolated'}}{{3-22=nonisolated}}
    get { 0 }
  }

  nonisolated(unsafe) func myMethod() {}
  // expected-warning@-1 {{'nonisolated(unsafe)' has no effect on instance method 'myMethod()', consider using 'nonisolated'}}{{3-22=nonisolated}}
}
