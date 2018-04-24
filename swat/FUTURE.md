// -*- fill-column: 80 -*-

// Miscellaneous to-do items

;;; TODO (whenever)
;;;   - return statement?  For this we need another unreachable type, like we want
;;;     for unreachable.  Or we could implement as a branch to outermost block,
;;;     though that's not very "wasm".
;;;   - more subtle conversion ops
;;;   - allow certain global references as inits to locally defined globals
;;;   - cond-like macro
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
;;;   - Allow imports from arbitrary module names by adopting eg (func- mod:fn ...) syntax
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

--------------------------------------------------

Class types:

Toplevel   ::= ... | Class

Class      ::= (Class-Kwd Id Base-Type Field ... Virtual-Decl ...)
Base-Type  ::= (extends Id) | Empty
Struct-Kwd ::= defclass+ | defclass- | defclass
Field      ::= Decl
Virtual-Decl ::= (virtual Virtual-Signature)
Virtual-Signature ::= (Id self Decl ...) | (Id self Decl ... -> Type)

    A Class production introduces a named class type.  These types form a tree,
    with the predefined class type AnyClass at the root.  If no base type is
    provided then AnyClass is assumed.  Since AnyClass has no fields or methods
    the implicit base type does not matter, but it allows us to use eg
    "*AnyClass" as an "any pointer type" type.

    [We prefer 'class' over 'struct' since we want to reserve struct for a
    different, maybe structural, type systems.  cf the distinction between
    Objects and Records in Modula-3.]

    Syntactically no field can be called "extends" or "virtual", as these
    are keywords within the definition.

    Each Decl introduces a field and its type.  Field names must be distinct
    in a class.

    The first argument 'self' is assumed to be of the type of the class within
    which the virtual declaration occurs; ie, the meaning of 'self' in a type T
    is really (self *T).  Overriding methods for subtypes of the class type will
    discriminate on this argument.
    
Func-Kwd   ::= ... | defmethod

    For defmethod, there must be at least one Decl in the signature and the Id
    of the first Decl must be "self", and the type of that argument must be a
    pointer-to-class whose type hierarchy has a definition of a virtual that
    matches the signature.  All other arguments and return types must equal
    the corresponding in the declaration of the virtual.

    For calls, the concrete type T of the first argument is examined, and the
    method defined for T or for a base type U of T s.t. there is no closer
    method to T is selected.

Type       ::= Primitive | RefType
Primitive  ::= i32 | i64 | f32 | f64
RefType    ::= string | ClassName | @RefType | @Primitive
ClassName ::= Id

    Syntactically, the * and @ must be next to the type name without any
    intervening spaces.  (In Scheme terms, a pointer to i32 is the single symbol
    "*i32".)

    The ClassName must name a defined class, or AnyClass, the empty class.

    An ArrayType represents a pointer to a heap-allocated array (so ArrayType is
    in a sense also a reftype); @@i32 is an array of arrays of i32s, while *@i32
    is a location holding a reference to an array of i32s.
    
    Note that class types cannot be used as the types of variables or
    parameters; only references to class types can.  Similarly, we cannot have
    arrays of class types, only arrays of references to class types.

    Type compatibility:

      - a ref-to-class-A is automatically cast to a ref-to-class-B if A is a subclass
        of B and the context requires B.

      - these contexts are:
         - function call parameter
	 - function return
	 - assignment to variable, field, or array element
	 - result of expression list (begin, let, case arm)
	 - result of select or two-armed if
	 
New        ::= (new RefType Expr ...) | (new ArrayType Expr ElemInit)
ElemInit   ::= Expr | Empty

   Something fishy here... why would the operator take the reftype and not
   the base type?  ie, (new i32 4) would create the cell; this returns *i32.
   (new Point 10 20) returns *Point. (@new i32 10) creates an array; it returns @i32.

   (new RefType ...) creates an object on the heap and initializes it with the
   initializing expressions, which must be no more than the fields.  Fields not
   mentioned are zero/null initialized, and array fields are given objects of
   zero length.  So (new *i32 3) creates a cell holding a single i32, and
   (new *@i32 (new @i32 5)) creates a cell holding array, initially an array of
   length 5.

   For arrays, the default value is used for all fields, or they are given the
   canonical default value for the type.

FieldRef   ::= (*Id Expr) | (* Expr)
ArrayRef   ::= (@ Expr Expr)

   In a FieldRef (*Id Expr), the Expr must have a RefType *T and T must be
   a class that has a field named Id.

   In a FieldRef (* Expr), the Expr must have a RefType either *Primitive
   or **T for some type T.

   In an ArrayRef, the first Expr must have type @T and the second is the
   index to access.

Set        ::= (set! Lvalue Expr)
Inc        ::= (inc! Lvalue)
Dec        ::= (dec! Lvalue)
Lvalue     ::= Id | FieldRef | ArrayRef

  These supersede the existing definitions by generalizing the notion of lvalue.

  (set! (* p) (new @i32 10))  works fine if p is *@i32.
  (set! (*x pt) 33)
  (inc! (*count r))


--------------------------------------------------

Implementation:

(new i32 5) becomes

(defun- (stdlib:new_i32 (n init) -> *i32))
...
(new_i32 5)

(new Point 10 20) becomes


--------------------------------------------------

Structs and references, see wabbit.swat and vfunc.swat

Really we need some kind of array type, *[]T, and aref syntax, which must
combine with field getters/setters somehow.  Brackets a problem?  Larceny treats
them as parens.  Reader macro?  Or maybe *T... is OK.  It's a bad hack.  What
about references?  (... x n) is not pretty.  (@ x n) works.  Then (new @*T k) to
create?  Clearly @*T will work as array of references to T syntax.  Indeed @i32
will, and should, work.

Some sort of vtable thing, see notes elsewhere

Look to flatjs for inspiration about syntax.

--------------------------------------------------



Callish    ::= ... | FieldRef | Cast | Typetest

Cast       ::= (->Id Expr)
Typetest   ::= (Id? Expr)

  Here Id must be a struct type name.

Syntax     ::= ... | New | Null

New        ::= (new T Expr ...) where T is a struct type and the Expr number as many as the fields
Null       ::= (null RefType)

   ...

Operator ::= ... | Ref-Op
Ref-Op   ::= null?


(define (expand-null cx expr env)
  (check-list-oneof expr '(1 2) "Bad null expression" expr)
  (if (null? (cdr expr))
      (values '(ref.null anyref) '(ref %any%))
      (let ((t (parse-struct-name cx (cadr expr))))
	(values `(ref.null ,t) `(ref ,(cadr expr))))))

(define (parse-struct-name cx name)
  ;; TODO: verify the type exists
  ;; Return anyref because we have no other types yet
  'anyref)

(define (expand-new cx expr env)
  ;; length 2 or more
  ;; first operand must name struct type T
  ;; other operands must have types that match the field types of T in order, can automatically upcast
  ...)

(define (expand-null? cx expr env)
  (check-list expr 2 "Bad null? expression" expr)
  (values `(ref.isnull ,(expand-expr cx (cadr expr) env)) 'i32))

(define (expand-field-access cx expr env)
  ;; length 2
  ;; type of operand is a (ref T), not anyref
  ;; the type T must have a field named by the accessor
  ...)

(define (expand-struct-predicate cx expr env)
  ...)

(define (expand-struct-cast cx expr env)
  ...)

(define (field-accessor? x)
  #f)

(define (struct-predicate? x)
  #f)

(define (struct-cast? x)
  #f)


(define (struct-ref-name? t)
  (let* ((name (symbol->string t))
	 (len  (string-length name)))
    (and (> len 1)
	 (char=? (string-ref name 0) #\*)
	 (struct-type? (string->symbol (substring name 1 len))))))


TypeAlias  ::= (deftype Id Type)

    Define Id as an alias for the given type, which can't reference the
    alias being introduced here.

    Until further notice, Type is restricted to being an Id.

    TODO: This is aspirational - not implemented.

