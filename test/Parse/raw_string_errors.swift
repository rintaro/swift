// RUN: %target-typecheck-verify-swift

#"\##("invalid")"#
// expected-error@-1{{too many '#' characters in delimited escape}}
// expected-error@-2{{invalid escape sequence in literal}}

####"invalid"###
// expected-error@-1{{unterminated string literal}}

###"invalid"######
// expected-error@-1{{too many '#' characters in closing delimiter}}{{16-17=}}
// expected-error@-2{{consecutive statements on a line must be separated by ';'}}
// expected-error@-3{{expected expression}}
// expected-warning@-4{{string literal is unused}}

##"""##
  foobar
  ##"""##
// expected-error@-3{{multi-line string literal content must begin on a new line}}{{6-6=\n}}
// expected-error@-2{{multi-line string literal closing delimiter must begin on a new line}}{{5-5=\n}}
