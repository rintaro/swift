// RUN: %empty-directory(%t)

// RUN: DUMP_ARGS="-dump-parse -dump-ast-format default-with-decl-contexts"
// RUN: %target-swift-frontend %s ${DUMP_ARGS} -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen \
// RUN:    | %utils/sanitize-address.py > %t/astgen.ast
// RUN: %target-swift-frontend %s ${DUMP_ARGS} -disable-availability-checking -enable-experimental-move-only \
// RUN:    | %utils/sanitize-address.py > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: not %target-swift-frontend %s ${DUMP_ARGS} -disable-availability-checking -enable-experimental-move-only -parse-as-library -enable-experimental-feature ParserASTGen \
// RUN:    | %utils/sanitize-address.py > %t/astgen.library.ast
// RUN: not %target-swift-frontend %s ${DUMP_ARGS} -disable-availability-checking -enable-experimental-move-only -parse-as-library \
// RUN:    | %utils/sanitize-address.py > %t/cpp-parser.library.ast

// RUN: %diff -u %t/astgen.library.ast %t/cpp-parser.library.ast

// REQUIRES: swift_feature_ParserASTGen

// rdar://116686158
// UNSUPPORTED: asan

_ = ##"foo bar"##
_ = ##/foo bar/##

if let first = [1,2,3].first {
  foo(x: first)
}

func foo(x: Int) {}

let a = 42

var b = {
  12 + a
}() {
  didSet { print("didSet") }
}
