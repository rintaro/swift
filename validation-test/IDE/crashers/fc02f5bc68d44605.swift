// {"kind":"complete","original":"90f5eddd","signature":"$sSo5swiftO9SourceLocV0A6ASTGenE2at2inAD20_CompilerSwiftSyntax16AbsolutePositionV_SRys5UInt8VGtcfCTf4nnd_n","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
// REQUIRES: swift_swift_parser
extension a { b {
#^^#macro c =
#^d^#
