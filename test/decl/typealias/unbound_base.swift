// RUN: %target-typecheck-verify-swift

struct OtherGeneric<U> {}

struct Constrained<T> {
  typealias NonGeneric = Int where T == Int
  typealias FakeGeneric = T where T == Int

  typealias Unbound = OtherGeneric where T == Int
  typealias Generic = OtherGeneric where T == Int
}

extension Constrained where T == Int {
  typealias NonGenericInExtension = Int
  typealias FakeGenericInExtension = T

  typealias UnboundInExtension = OtherGeneric
  typealias GenericInExtension = OtherGeneric
}

func use(_: Constrained.NonGeneric,
         _: Constrained.FakeGeneric,
         _: Constrained.Unbound<String>,
         _: Constrained.Generic<String>,
         _: Constrained.NonGenericInExtension,
         _: Constrained.UnboundInExtension<String>,
         _: Constrained.GenericInExtension<String>) {

  // FIXME: Get these working too
#if false
  let _ = Constrained.NonGeneric.self
  let _ = Constrained.FakeGeneric.self
  let _ = Constrained.Unbound<String>.self
  let _ = Constrained.Generic<String>.self

  let _ = Constrained.NonGenericInExtension.self
  let _ = Constrained.FakeGenericInExtension.self
  let _ = Constrained.UnboundInExtension<String>.self
  let _ = Constrained.GenericInExtension<String>.self
#endif

  let _: Constrained.NonGeneric = 123
  let _: Constrained.FakeGeneric = 123
  let _: Constrained.NonGenericInExtension = 123
  let _: Constrained.FakeGenericInExtension = 123

  let _: Constrained.Unbound = OtherGeneric<String>()
  let _: Constrained.Generic = OtherGeneric<String>()

  let _: Constrained.UnboundInExtension = OtherGeneric<String>()
  let _: Constrained.GenericInExtension = OtherGeneric<String>()
}

struct Use {
  let a1: Constrained.NonGeneric
  let b1: Constrained.FakeGeneric
  let c1: Constrained.Unbound<String>
  let d1: Constrained.Generic<String>
  let a2: Constrained.NonGenericInExtension
  let b2: Constrained.FakeGenericInExtension
  let c2: Constrained.UnboundInExtension<String>
  let d2: Constrained.GenericInExtension<String>
}

extension Constrained.NonGeneric {}
extension Constrained.Unbound {}
extension Constrained.Generic {}

extension Constrained.NonGenericInExtension {}
extension Constrained.UnboundInExtension {}
extension Constrained.GenericInExtension {}
