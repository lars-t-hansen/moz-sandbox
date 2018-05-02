swat0:

TODO - FEATURES & must-have
- virtuals
- more test code and demos
- type checks at the call-in boundary?
- export classes
  - they show up as factory functions M.make.Cls(), and with
    access to fields thru the std TypedObject mechanism

FEATURES
- supports most primitive wasm operations
- supports many Scheme special forms
- import/export of functions and globals
- single-module compilation
- sealed virtual functions

THEN:
- speed up subtype test in JS code, maybe avoid going out of line [flag -O]
- speed up vcalls, maybe avoid going out of line [flag -O]
- speed up upcasts, don't emit code [flag -O]
- ditto downcasts, probably - for now
- if the desc is in flat memory then reading the desc means obtaining an index
- note, right now JS performs a null check on obj reference, does this move into wasm somehow?

NON-FEATURES
- strings
- arrays
- lists
- tuples
- import classes
- globals of pointer type
- non-native types (host types)

TBD - probably not
- less bizarre trap operator
- return statement
- auto widening of i32 -> f64, i32 -> i64, f32 -> f64, null
- multi-arity ops for better ergonomics


swat1:

FOR SURE
- multi-module compilation
- type import and export
- open virtual functions
- arrays

MAYBE
- non-native types (host types)
- globals of pointer type
- sequences with common syntax (strings, arrays, lists)
- anything from the TBD categories for swat0
- anything from the TODO category in MANUAL.md or FUTURE.md
- ad-hoc polymorphism
- lists: type (T) is list of T, (@ p n) is nth, (car p), (cdr p) etc predefined,
  if x:(T) then (cons p x) => (T), we could have list-ref, length also.  We'd
  synthesize class %List.T: (defclass %List.T (car T) (cdr %List.T)) and
  overload car, cdr, cons, and the others


General:

Some of the cracks are starting to show.  For example,

- Larceny insists on printing some symbols with quoting, eg,
  |+infinity| where we want just +infinity, and there are hacks to
  work around this.

- Obviously if the input syntax is wrong we'll see this as an error
  signaled from Lareny's reader, not a meaningful syntax error

- No line number information is available when swat wants to print
  an error
