swat0:

TODO - FEATURES & must-have
- virtuals
- more test code and demos
- type checks at the call-in boundary
- export classes
  - they show up as factory functions M.make.Cls(), and with
    access to fields thru the std TypedObject mechanism
- vectors:
  - Type (Vector T), shorthand @T
  - Constructor (new (Vector T) num init-value-opt) / (new @T num init-value-opt)
  - Constructor (vector E1 E2 ... En) where the E all have to be the same type T and
    we'll have a (Vector T) as a result
  - Could have (make-vector n E) to construct @T where T is type of E, E not optional
  - Accessor (vector-ref E1 E2) / (@ E1 E2) where E1 is the pointer and E2 is the offset
  - Setters (vector-set! E1 E2 V) / (set! (@ E1 E2) n), (inc! (@ E1 E2)), (dec! (@ E1 E2))
  - probably we want anyref at this point?
  - (vector->string v) where v is @i32

FEATURES
- supports most primitive wasm operations
- supports many Scheme special forms
- import/export of functions and globals
- single-module compilation
- sealed virtual functions

# DEMOS

* The lack of reference-type globals is a real hardship
* Really want something that deals with the DOM
** Snake is kind of nice
** Not much of a class hierarchy...  Really arrays would be most efficient
* maybe port Life / Mandelbrot / PSON from AssemblyScript?  Life / Mandel are
  not obvious because they are very array-oriented, so we need arrays.
* obviously we can do a self-compiler, but it's a big project.

THEN:
- speed up subtype test in JS code, maybe avoid going out of line [flag -O]
- speed up vcalls, maybe avoid going out of line [flag -O]
- speed up upcasts, don't emit code [flag -O]
- ditto downcasts, probably - for now
- if the desc is in flat memory then reading the desc means obtaining an index
- note, right now JS performs a null check on obj reference, does this move into wasm somehow?

NON-FEATURES
- lists
- tuples / multiple values
- symbols
- import classes
- enums, though very very tempting
- globals of pointer type
- non-native types (host types)
   
swat1:

TBD - probably not
- less bizarre trap operator
- return statement
- auto widening of i32 -> f64, i32 -> i64, f32 -> f64, null
- multi-arity ops for better ergonomics
  High value:  + - (including generating 'neg') * < <= > >= = <u <=u > >u =u max min bitand bitor bitxor
  Also string-append
  In Scheme, (/ x) == (/ 1 x) and / is multi-arity
  Quotient and Remainder are not.
  Might preserve that here.
  Might preserve quotient and remainder as names.
  Might allow / on FP numbers, or yield FP results even from int operands?
- eqv? should translate to eq? or = depending on types of operands...
- = on references to class types, this needs to go through JS.
  Though it's possible the syntax here is "eq?" and that "=" is
  reserved for numbers.  Also see note on eqv? above

FOR SURE
- multi-module compilation
- type import and export
- open virtual functions
- string operations
  - list->string, vector->string and vice versa, once we have those
  - i32->string, string->i32, etc

MAYBE
- non-native types (host types)
- globals of pointer type - could simulate now with JS callouts but let's just
  wait for SM support
- sequences with common syntax (strings, arrays, lists)
- anything from the TBD categories
- anything from the TODO category in MANUAL.md or FUTURE.md
- ad-hoc polymorphism (overloading)
- lists: type (T) is list of T, (@ p n) is nth, (car p), (cdr p) etc predefined,
  if x:(T) then (cons p x) => (T), we could have list-ref, length also.  We'd
  synthesize class %List.T: (defclass %List.T (car T) (cdr %List.T)) and
  overload car, cdr, cons, and the others


General:

Some of the cracks are starting to show.

Notably, no line number information is available when swat wants to
print an error.

We could hack around this.  Suppose we read the source code as a
string.  Now we can scan forward until we find a top-level phrase,
skipping comments and so on, and then read the top-level phrase
prefix.  Then we have line number info at least at that point; we can
use the reader for each individual top-level form, and will (maybe?)
have line number information for those, if by no other means then at
least by scanning the source text forward again?  We have
port-position, so probably.
