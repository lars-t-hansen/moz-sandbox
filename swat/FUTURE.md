// -*- fill-column: 80 -*-

# Host object access

We want to:

* invoke methods on host objects
* get and set properties on host objects
* pass swat functions to host objects, eg for event handlers
* do all of this with at least superficial type safety, ie, something better
  than "anyref" for everything is desirable

We can do this with an operator-based interface, eg

* (ffi-get obj name)
* (ffi-set! obj name value)
* (ffi-send obj name arg ...)

where "name" is a string.  This will work but is pretty clunky.

We can allow an exported function to be referenced by name and turn into an
anyref or perhaps a special HostFunction type.

Then in the Snake game we'd see things like:

```
(defvar interval i32 0)
(defun+ (ontick) ...)
(defun (init)
  (ffi-send window setInterval ontick 20)
  ...)
```

A more general idea is to define host types and then "just use" those.

```
(defhost- DOMWindow
  (method (setInterval (handler HostFunction) (timeout i32) -> i32)))

(defhost- DOMKeyboardEvent
  (property charCode i32))

(defun (init (w DOMWindow))
  (=> w setInterval ontick 20))

(defun (onclick (ev DOMKeyboardEvent))
  (*charCode ev))
```

where `=>` is just syntax for sending a message to a host object.  For field
access we just use *name as for normal types.

Yeah, poor man's WebIDL, but why not?

Unclear how far this goes but it might go far enough for now.  It's always
'defhost-' because these types are always imported.

## Host types

The host type idea can be generalized.

We define an imported opaque type with a predicate.  We use "defhost" rather
than "defclass" to make the distinction that the representation may be something
other than a class; it is an object, maybe with identity.

The predicates are always (anyref) -> i32.  They are just functions; they don't
have to be imported.

(defhost- DOMNode (predicate domnode?))

(defhost- DOMHTMLNode (extends DOMNode)
  (predicate domhtmmlnode?)
  (upcast #t))

(defun- (domnode (n anyref) -> i32))
(defun- (domhtmlnode (n anyref) -> i32))


# Miscellaneous very unstructured notes on possible evolution

;;;
;;; Working on: Snake
;;;
;;;  - virtual functions
;;;    - remaining work items
;;;      - proper error function
;;;      - lots and lots of test code for all the operators and operations
;;;      - avoid callouts to JS for virtual dispatch, by using flat memory
;;;
;;;  - We don't have:
;;;    - enough test code, including type checks that should fail
;;;
;;;    - type checks at call boundaries from JS to Wasm (in class.swat, we
;;;      can currently pass an Ipso to something that takes a Box, and it
;;;      will not throw, but this is wrong).  For classes and strings.
;;;
;;;      In general there's something iffy about exporting a function that takes
;;;      a Box when a Box cannot be exported however!  In some sense, only
;;;      functions that take Object should be exportable.  (Functions that
;;;      return Box are OK, there's an implied widening.)
;;;
;;;      We can imagine exporting factory methods that return Box, which
;;;      allows the host to call back in with a Box, though.  Still seems
;;;      like we'd want Box to be exported.



  - Type (Vector T), shorthand @T
  - Constructor (new (Vector T) num init-value-opt) / (new @T num init-value-opt)
  - Constructor (vector E1 E2 ... En) where the E all have to be the same type T and
    we'll have a (Vector T) as a result
  - Could have (make-vector n E) to construct @T where T is type of E, E not optional
  - Accessor (vector-ref E1 E2) / (@ E1 E2) where E1 is the pointer and E2 is the offset
  - Setters (vector-set! E1 E2 V) / (set! (@ E1 E2) n), (inc! (@ E1 E2)), (dec! (@ E1 E2))
  - probably we want anyref at this point?
  - (vector->string v) where v is @i32




## Boxes

- (box v) takes any v other than void and returns a Box, which is a built-in opaque type
- (typecase b ((id typename) expr ...) ... (else ...)) takes a box b and type tests it
- for eg Scheme lists we'd have (List Box)
- one can use is and as on boxes
- boxes are like anyref but for all kinds of values

## Tuples and values

We should unify tuples and multiple-values.  We can then have list of tuples,
for example, and we can return tuples as multiple values, and we can destructure
tuples with let-values if we want.

Type syntax: (t1 t2 t3) where the t are type names [maybe] [i wanted (t) for list of t but maybe ()t or (t ...) is fine]
         or: (Values t1 t2 t3)
Constructor: (values a b c)
Accessor: (*0 t) (*1 t) ... where t is a tuple value, the * syntax signifying these are more like records
Destructuring: (let-values (((a b c) t) ...) ...)
Type testing: no?
Type casting: no?
In principle we could "cast up" to a prefix and "cast down" if there was a previous upcast

Tuple types are structurally equivalent.  Fields are always immutable.

We can do multiple values now by boxing (if contains references) or transmitting
through flat memory (by returning an index for an area or having a dedicated
address, which is probably just as well).  Probably boxing is fine for now to
simplify the interaction of multiple-value return with tuples; can optimize
later.

## Desugaring / ensugaring

- rephrase (*x E) in terms of form (field-ref x E),
  ditto (set! (*x E) v), as (field-set! x E v), paves
  way for arrays and lists.

- Type @T is syntax for canonical (array T); (@ E N) is (array-ref E N);
  (set! (@ E N) V) is (array-set! E N V).

- Then we get type (list T) as a natural extension and can invent
  syntax for that, eg (T); (@ E N) is (list-ref E N), etc.  Shades
  of generic sequences...

  We'd want operators map and for-each to work on generic sequences
  too; desugar to map-list/map-array; for-each-list/for-each-array.
  assq, not so much...

  Speaking of lists, null? will work correctly, which is nice.


## Miscellaneous to-do items

```
;;; TODO (whenever)
;;;   - return statement?  For this we need another unreachable type, like we want
;;;     for unreachable.  Or we could implement as a branch to outermost block,
;;;     though that's not very "wasm".
;;;   - more subtle conversion ops
;;;   - allow certain global references as inits to locally defined globals
;;;   - block optimization pass on function body:
;;;      - strip unlabeled blocks inside other blocks everywhere,
;;;        including implicit outermost block.
;;;      - remove whatever ad-hoc solutions we have now
;;;   - Multi-arity when it makes sense (notably + - * relationals and or bit(and,or,xor))
;;;   - tee in some form?  (tee! ...)  Or just make this the set! semantics and then
;;;     lower to set_local/set_global if result is discarded?
;;;   - Memories + flat memory??  Do we really care? Maybe call ops <-i8 and ->i8, etc
;;;     The flat memories can reduce some of the pain in the implementation, so maybe
;;;     best reserved for that.
;;;   - Produce wabt-compatible output
;;;   - Multiple-value return:  -> (t0 t1 ...) return type annotation; LET-VALUES or
;;;     destructuring in LET; VALUES; maybe some way of applying / splatting a captured
;;;     list of values (long tail).  Until we actually have these in wasm we can simulate
;;;     using objects or globals or flat memory (though not for references)
;;;   - deftype, benefits ref types
;;;   - poor man's enums (actually we'd ideally have something stronger)
;;;       (defenum TyTy I32 F64 Ref)
;;;     is exactly equivalent to
;;;       (deftype TyTy i32)
;;;       (defconst TyTy.I32 0)
;;;       (defconst TyTy.F64 1)
;;;       (defconst TyTy.Ref 2)
;;;   - possible to have inline wasm code, eg, (inline type ...) with some kind of
;;;     name->number substitution for locals, globals, functions, types
;;;   - more limited, possible to use the wasm names for operators in some contexts,
;;;     with a literal-ish meaning
;;;   - boolean type, but not as an alias as i32.  We want a "strong enum" probably
;;;     with a type name and support for #t and #f, and type checking, and bool->i32
;;;     and i32->bool or similar primitives.
```