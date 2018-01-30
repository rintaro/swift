// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree | %utils/pretty-json > %t
// RUN: diff -u %t %S/Inputs/serialize_struct_decl.json

struct Foo {
  let   bar : Int

  let baz : Array < Int >
      }
