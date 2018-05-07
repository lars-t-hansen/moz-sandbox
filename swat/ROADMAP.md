# swat v1 

## Missing language features and implementation bugs

* Type checks at the call-in boundary + defined semantics for visible unexported types
* Bounds checks on string and array accesses
  * string-ref, substring
  * vector-ref, vector-set!
* Virtual function cleanup
  * Missing handling of the "default" case
  * Idiosyncratic "closed" syntax, defvirtual + defmethod would be less weird and "open" and forward-looking
  * Should be exportable at least, no reason not to
* Exportable classes
  * They should show up as factory functions M.make.Cls(), and with access to fields thru the std TypedObject mechanism
* Some way of invoking JS methods on host objects, so that we don't have to go to JS for DOM access.
  * Ad-hoc / limited is OK for now, anything's better than what we have
  * Note, not just invocation but also property reference, eg for event.charCode
  * Language operator, eg (=> obj "getElementById" arg ...) where obj is anyref and the arg ... are of any type.
    Symbols would be nicer and we can require a quoted form here but it seems like a headache right now.
  * Less obscure: (ffi-get obj name), (ffi-set! obj name value), (ffi-call obj name arg ...)
  * Want a way to reference functions, to install event handlers?  OK if this is only exported functions...  Suppose we
    can just reference an exported function and get an anyref back, representing the value that would have been seen on
    the outside of the module?
* Important missing syntax
  * `do` loops - what we have is insanely primitive and `while` is just ugly
* Very high value missing language operators
  * vector->string, string->vector
  * <number>->string, string-><number> for number types
  * eq? on compatible reference types (probably not anyref)

## Quality etc

### Required

* Move to separate repo for better issue tracking and visibility
* More documentation, esp about how to use for web development
* Compile all the way to .wasm
* More test code
* Less brittle compiler / better error messages by reading phrase-at-a-time so that
  we at least have a starting line number

### Desirable

* speed up subtype test in JS code, maybe avoid going out of line [flag -O]
* speed up vcalls, maybe avoid going out of line [flag -O]
* speed up upcasts, don't emit code [flag -O]
* ditto downcasts, probably - for now
* if the desc is in flat memory then reading the desc means obtaining an index
* note, right now JS performs a null check on obj reference, does this move into wasm somehow?
* clean up toolchain: r7rs support?  chez scheme or other scheme system support?
* more demos
  
## Demo ideas

* The lack of reference-type globals is a real hardship
* Really want something that deals with the DOM
* maybe port Life / Mandelbrot / PSON from AssemblyScript?  Life / Mandel are
  not obvious because they are very array-oriented, so we need arrays.
* obviously we can do a self-compiler, but it's a big project.

# swat later, or opportunistically

## Globals of reference type

Currently not supported because of missing support in SpiderMonkey.
We could hack around it but it's just as easy to wait until support is
available.

## Host types

I envision something like `(defhost TypeName (predicate Fn))` where
TypeName is the name of the type and `Fn` names a function from anyref
to i32 that determines whether an object is of the given type.  This
can be used to improve type checking around host types.

There might be more operators but a predicate is a good start.

## Vector operations

* (vector E0 E1 ...) constructs a vector of type T common to the E
* vector-copy, with optional start and end
* vector-append

## Lists

* Immutable
* Type syntax (List T)
* Constructor (list E0 E1 ...), (cons E0 E1) where E0 is T and E1 is (List T)
* cons, car, cdr, ...
* list->vector, vector->list, list-copy, list-ref, list-head, list-tail, reverse, append
* operators with function arguments: map, for-each
  * For initial cut, require either reference to global function or *literal* lambda expression, which cannot be used in other contexts

## Tuples / multiple values

* Immutable fixed-size records with integer-named fields
* Type syntax (Values T0 T1 ...) maybe, or perhaps just (T0 T1 ...), tricky
* Constructor (values T0 T1 ...)
* Accessors *0 *1 etc, as for fields
* Destructuring tuples with let-values and let*-values

## Boxes

* Sum type containers, speculative

## Symbols

* Because symbols are a good idea

## Multi-module + meaningful type import and export

## Enums

## Less bizarre trap operator

We want this to be written `???` and we want it to have a type that is
compatible with other types so that we don't have to provide an
explicit type.

## Return statement

'nuff said.

# Auto widening

At least i32 -> f64, i32 -> i64, f32 -> f64, null -> any reference type

# Multi-arity ops for better ergonomics

* High value:  + - (including generating 'neg') * < <= > >= = <u <=u > >u =u max min bitand bitor bitxor
* Also string-append
* In Scheme, (/ x) == (/ 1 x) and / is multi-arity; might preserve that here.
* In Scheme, quotient and remainder are not multi-arity.
* Might preserve quotient and remainder as names.
* Might allow / on FP numbers, or yield FP results even from int operands?
* eqv? should translate to eq? or = depending on types of operands?

# Ad-hoc polymorphism

No reason not to, except language complexity.
