// RUN: %target-typecheck-verify-swift -typecheck %s

for var i = 0; i < 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}

for var i = 0; i < 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-30=}}

for var i = 0; i <= 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-21= ... }} {{23-28=}}

for var i = 0; i <= 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-21= ... }} {{23-31=}}

for var i = 10; i > 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in stride(from: }} {{15-21=, to: }} {{22-27=, by: -1)}}

for var i = 10; i > 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in stride(from: }} {{15-21=, to: }} {{22-29=, by: -}} {{30-30=)}}

for var i = 10; i >= 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in stride(from: }} {{15-22=, through: }} {{23-28=, by: -1)}}

for var i = 10; i >= 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in stride(from: }} {{15-22=, through: }} {{23-30=, by: -}} {{31-31=)}}
