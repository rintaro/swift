// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree | %utils/pretty-json > %t
// RUN: diff -u %t %S/Inputs/serialize_multiple_decls.json

struct A {
}

struct B {
}
