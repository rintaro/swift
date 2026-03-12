// RUN: %target-typecheck-verify-swift %s

// RUN: %target-typecheck-verify-swift -Wwarning PerformanceHints -verify-additional-prefix perfhints-
// RUN: %target-typecheck-verify-swift -Wwarning UntypedThrows -verify-additional-prefix perfhints-

enum MyError: Error {
case failed
}

// expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
func untypedThrows() throws { }

// expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
func rethrowingFunction(param: () throws -> Void) rethrows { }

// expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
typealias FnType = () throws -> Void

func untypedThrowsInBody() {
  // expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
  do throws {
    throw MyError.failed
  } catch {
  }

  // expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
  _ = { (x) throws in x + 1 }
}

struct SomeStruct {
  // expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
  init() throws { }

  var value: Int {
    // expected-perfhints-warning@+1{{untyped throws performs heap allocation on each 'throw'; add a thrown error type with '(type)'}}
    get throws {
      0
    }
  }
}
