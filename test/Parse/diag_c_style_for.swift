// RUN: %target-typecheck-verify-swift

for var a = 0; a < 10; a += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-30=}}
}

for var b = 0; b < 10; b += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-30=}}
}

for var c=1;c != 5 ;c += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{12-18= ..< }} {{19-27=}}
}

for var d=100;d<5;d+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{14-17= ..< }} {{18-23=}}
}

// next three aren't auto-fixable
for var e = 3; e > 4; e+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

for var f = 3; f < 4; f-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in stride(from: }} {{14-20=, to: }} {{21-26=, by: -}} {{27-27=)}}
}

for var i = 6; i > 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in stride(from: }} {{14-20=, to: }} {{21-26=, by: -}} {{27-27=)}}
}

for var i = 100; i != 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in stride(from: }} {{16-23=, to: }} {{24-29=, by: -}} {{30-30=)}}
}

let start = Int8(4)
let count = Int8(10)
var other = Int8(2)

for ; other<count; other+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// this should be fixable, and keep the type
for (var number : Int8 = start; number < count; number+=1) { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-10=}} {{23-26= in }} {{31-42= ..< }} {{47-59=}}
  print(number)
}

// should produce extra note
for (var m : Int8 = start; m < count; m+=1) { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-10=}} {{18-21= in }} {{26-32= ..< }} {{37-44=}}
  m += 3
}

for var o = 2; o < 888; o += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{23-31=}}
}

for var o = 2; o < 888; o += 11 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in stride(from: }} {{14-20=, to: }} {{23-30=, by: }} {{32-32=)}}
}

// could theoretically fix this with "..."
for var p = 2; p <= 8; p+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-21= ... }} {{22-28=}}
}
