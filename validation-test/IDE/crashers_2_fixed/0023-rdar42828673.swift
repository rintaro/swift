// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

public enum MyEnum {
  case foo
}

public func use(_: Any?, _: MyEnum) {}

struct Foo<Outer> {
  func test<Value>(_ value: Value?) {
    use(value, .#^COMPLETE^#foo)
  }
}
