// RUN: %target-typecheck-verify-swift

struct A : Equatable {
  func foo() {}
  static func == (lhs: A, rhs: A) -> Bool { return true }
}

var a : A?
var b : A ? // expected-error {{consecutive statements on a line}} {{10-10=;}} expected-error {{expected expression}}

var c = a?  // expected-error {{'?' must be followed by a call, member lookup, or subscript}}
var d : ()? = a?.foo()

var e : (() -> A)?
var f = e?()

struct B<T> {}
var g = B<A!>()

// SR-3961 Force-unwrap and inequality operator ambiguit
a!=A() // expected-warning{{unspaced binary operator '=' after '!' is ambiguous}} {{3-3= }} {{4-4= }}
a?=A() // expected-warning{{unspaced binary operator '=' after '?' is ambiguous}} {{3-3= }} {{4-4= }}
a! = A()
a? = A()
a != A() // expected-warning {{result of operator '!=' is unused}}
a ?= A() // expected-error {{use of unresolved operator '?='}}

infix operator +-+
infix operator ?+-+
infix operator !+-+
a!+-+A() // expected-warning{{unspaced binary operator '+-+' after '!' is ambiguous}} {{3-3= }} {{6-6= }} expected-error {{use of unresolved operator '+-+'}}
a?+-+A() // expected-warning{{unspaced binary operator '+-+' after '?' is ambiguous}} {{3-3= }} {{6-6= }} expected-error {{use of unresolved operator '+-+'}}
a !+-+ A() // expected-error {{use of unresolved operator '!+-+'}}
a ?+-+ A() // expected-error {{use of unresolved operator '?+-+'}}
a! +-+ A() // expected-error {{use of unresolved operator '+-+'}}
a? +-+ A() // expected-error {{use of unresolved operator '+-+'}}
