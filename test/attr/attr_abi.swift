// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -enable-experimental-feature ABIAttribute -parse-as-library

// REQUIRES: swift_feature_ABIAttribute
// REQUIRES: swift_feature_Extern

//
// Same-kind checking
//

@abi(func funcForFunc_abi())
func funcForFunc() {}

@abi(var varForVar_abi: Int)
var varForVar: Int = 0

@abi(func funcForVar_abi())
var funcForVar: Int = 0

@abi(var varForFunc_abi: Int)
func varForFunc() {}

//
// Function arity checking
//

@abi(func param00_generic00() -> Int)
func param00_generic00() -> Int { fatalError() }

@abi(func param10_generic00(_: Int) -> Int)
func param10_generic00() -> Int { fatalError() }

@abi(func param01_generic00() -> Int)
func param01_generic00(_: Int) -> Int { fatalError() }

@abi(func param11_generic00(_: Int) -> Int)
func param11_generic00(_: Int) -> Int { fatalError() }



@abi(func param00_generic10<T>() -> T)
func param00_generic10() -> Int { fatalError() }

@abi(func param10_generic10<T>(_: Int) -> T)
func param10_generic10() -> Int { fatalError() }

@abi(func param01_generic10<T>() -> T)
func param01_generic10(_: Int) -> Int { fatalError() }

@abi(func param11_generic10<T>(_: Int) -> T)
func param11_generic10(_: Int) -> Int { fatalError() }



@abi(func param00_generic01() -> Int)
func param00_generic01<T>() -> T { fatalError() }

@abi(func param10_generic01(_: Int) -> Int)
func param10_generic01<T>() -> T { fatalError() }

@abi(func param01_generic01() -> Int)
func param01_generic01<T>(_: Int) -> T { fatalError() }

@abi(func param11_generic01(_: Int) -> Int)
func param11_generic01<T>(_: Int) -> T { fatalError() }



@abi(func param00_generic11<T>() -> T)
func param00_generic11<T>() -> T { fatalError() }

@abi(func param10_generic11<T>(_: Int) -> T)
func param10_generic11<T>() -> T { fatalError() }

@abi(func param01_generic11<T>() -> T)
func param01_generic11<T>(_: Int) -> T { fatalError() }

@abi(func param11_generic11<T>(_: Int) -> T)
func param11_generic11<T>(_: Int) -> T { fatalError() }

//
// Throws effect checking
//

@abi(func throws00(_: () throws -> Void))
func throws00(_: () throws -> Void) {}

@abi(func throws10(_: () throws -> Void) throws)
func throws10(_: () throws -> Void) {}

@abi(func throws20(_: () throws -> Void) rethrows)
func throws20(_: () throws -> Void) {}

@abi(func throws01(_: () throws -> Void))
func throws01(_: () throws -> Void) throws {}

@abi(func throws11(_: () throws -> Void) throws)
func throws11(_: () throws -> Void) throws {}

@abi(func throws21(_: () throws -> Void) rethrows)
func throws21(_: () throws -> Void) throws {}

@abi(func throws02(_: () throws -> Void))
func throws02(_: () throws -> Void) rethrows {}

@abi(func throws12(_: () throws -> Void) throws)
func throws12(_: () throws -> Void) rethrows {}

@abi(func throws22(_: () throws -> Void) rethrows)
func throws22(_: () throws -> Void) rethrows {}

//
// Async effect checking
//

@abi(func async00())
func async00() {}

@abi(func async10() async)
func async10() {}

@abi(func async01())
func async01() async {}

@abi(func async11() async)
func async11() async {}

//
// PBD shape checking
//

@abi(var x1, y1: Int)
var x1: Int = 0

@abi(var x2: Int)
var x2 = 0, y2: Int = 0

@abi(var (x3, y3): (Int, Int), (a3, b3): (Int, Int))
var (x3, y3): (Int, Int) = (0, 0), a3: Int = 0

@abi(var (x4, y4): (Int, Int), a4: Int)
var (x4, y4): (Int, Int) = (0, 0), (a4, b4): (Int, Int) = (0, 0)

//
// Conflict diagnostics
//

@abi(func noConflictWithSelf())
func noConflictWithSelf() {}

@abi(func noConflictWithMutualChanges1())
func noConflictWithMutualChanges2() {}

@abi(func noConflictWithMutualChanges2())
func noConflictWithMutualChanges1() {}

@abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
func conflictsWithOtherABI1() {}

@abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
func conflictsWithOtherABI2() {}

@abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
func conflictsWithNonABI_formal() {}

func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

@abi(var var_noConflictWithSelf: Int)
var var_noConflictWithSelf: Int = 0

@abi(var var_noConflictWithMutualChanges1: Int)
var var_noConflictWithMutualChanges2: Int = 0

@abi(var var_noConflictWithMutualChanges2: Int)
var var_noConflictWithMutualChanges1: Int = 0

@abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
var var_conflictsWithOtherABI1: Int = 0

@abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
var var_conflictsWithOtherABI2: Int = 0

@abi(var var_conflictsWithNonABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
var var_conflictsWithNonABI_formal: Int = 0

var var_conflictsWithNonABI: Int = 0 // expected-note {{'var_conflictsWithNonABI' previously declared here}}

struct Foo {
  @abi(func noConflictWithSelf())
  func noConflictWithSelf() {}

  @abi(func noConflictWithMutualChanges1())
  func noConflictWithMutualChanges2() {}

  @abi(func noConflictWithMutualChanges2())
  func noConflictWithMutualChanges1() {}

  @abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
  func conflictsWithOtherABI1() {}

  @abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
  func conflictsWithOtherABI2() {}

  @abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
  func conflictsWithNonABI_formal() {}

  func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

  @abi(var var_noConflictWithSelf: Int)
  var var_noConflictWithSelf: Int = 0

  @abi(var var_noConflictWithMutualChanges1: Int)
  var var_noConflictWithMutualChanges2: Int = 0

  @abi(var var_noConflictWithMutualChanges2: Int)
  var var_noConflictWithMutualChanges1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
  var var_conflictsWithOtherABI1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
  var var_conflictsWithOtherABI2: Int = 0

  @abi(var var_conflictsWithNonABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
  var var_conflictsWithNonABI_formal: Int = 0

  var var_conflictsWithNonABI: Int = 0 // expected-note {{'var_conflictsWithNonABI' previously declared here}}
}

func fn() {
  // TODO: Figure out if @abi makes sense in local scope and, if so, when we
  //       should complain about redecls.

  @abi(func noConflictWithSelf())
  func noConflictWithSelf() {}

  @abi(func noConflictWithMutualChanges1())
  func noConflictWithMutualChanges2() {}

  @abi(func noConflictWithMutualChanges2())
  func noConflictWithMutualChanges1() {}

  @abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
  func conflictsWithOtherABI1() {}

  @abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
  func conflictsWithOtherABI2() {}

  @abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
  func conflictsWithNonABI_formal() {}

  func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

  var a = 1 // expected-note {{'a' previously declared here}}
  var a = 1 // expected-error {{invalid redeclaration of 'a'}}

  @abi(var var_noConflictWithSelf: Int)
  var var_noConflictWithSelf: Int = 0

  @abi(var var_noConflictWithMutualChanges1: Int)
  var var_noConflictWithMutualChanges2: Int = 0

  @abi(var var_noConflictWithMutualChanges2: Int)
  var var_noConflictWithMutualChanges1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
  var var_conflictsWithOtherABI1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
  var var_conflictsWithOtherABI2: Int = 0

  // Diagnosed in the opposite order from usual due to being in a local scope where visibility may be limited:
  @abi(var var_conflictsWithNonABI: Int) // expected-note {{'var_conflictsWithNonABI' previously declared here}}
  var var_conflictsWithNonABI_formal: Int = 0

  var var_conflictsWithNonABI: Int = 0 // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
}

//
// Incorrect usage
//

@abi(func bar()) // expected-note {{attribute already specified here}}
@abi(func foo()) // expected-error {{duplicate attribute}}
func duplicateABIAttr() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested_abi() {
  @abi(func _exit(_ code: UInt32) -> Void)
  func exit(_ code : UInt32) -> Void {}
  exit(0)
}

@abi(var x = 1) // expected-error {{initial value is not allowed here}} expected-error {{type annotation missing in pattern}}
var x = 1

//
// Examples of expected use cases
//

@abi(func originallySendable() -> @Sendable () -> Void)
func originallySendable() -> sending () -> Void { fatalError() }

@abi(func originallyGenericSendable<T: Sendable>() -> T)
func originallyGenericSendable<T>() -> sending T { fatalError() }

@abi(func originallyAnySendable() -> any Sendable)
func originallyAnySendable() -> sending Any { fatalError() }

@abi(nonisolated func explicitIsolationChanged() -> @Sendable () -> Void)
@MainActor func explicitIsolationChanged() -> sending () -> Void { fatalError() }
