swat0:

TODO
- virtuals
- inc! and dec! on fields
- test code and demos
- speed up subtype test

FEATURES
- supports most primitive wasm operations
- supports many Scheme special forms
- import/export of functions and globals
- single-module compilation
- sealed virtual functions

NON-FEATURES
- strings
- arrays
- import/export of types
- globals of pointer type
- non-native types (host types)

TBD - probably
- type checks at the call-in boundary?
- module:id import syntax

TBD - probably not
- cond
- let*
- less bizarre trap operator
- return statement
- auto widening of i32 -> f64, i32 -> i64, f32 -> f64, null


swat1:

FOR SURE
- multi-module compilation
- type import and export
- open virtual functions
- arrays

MAYBE
- non-native types (host types)
- globals of pointer type
- strings
- anything from the TBD categories for swat0
- anything from the TODO category in MANUAL.md or FUTURE.md
- ad-hoc polymorphism
- lists: type (T) is list of T, (@ p n) is nth, (car p), (cdr p) etc predefined,
  if x:(T) then (cons p x) => (T), we could have list-ref, length also.  We'd
  synthesize class %List.T: (defclass %List.T (car T) (cdr %List.T)) and
  overload car, cdr, cons, and the others
