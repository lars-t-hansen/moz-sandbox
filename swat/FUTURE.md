// -*- fill-column: 80 -*-

Toplevel   ::= ... | Struct

Struct     ::= (Struct-Kwd Id Base-Type Field ... Virtual-Decl ...)
Base-Type  ::= (extends Id) | Empty
Struct-Kwd ::= defstruct+ | defstruct- | defstruct
Field      ::= Decl
Virtual-Decl ::= (virtual Virtual-Signature)
Virtual-Signature ::= (Id self Decl ...) | (Id self Decl ... -> Type)

    A Struct production introduces a named structure type.  These types form
    a tree, with the predefined type Object at the root.  If no base type
    is provided then Object is assumed.  Since Object has no fields or methods
    the implicit base type does not matter, but it allows us to use eg "*Object"
    as an "any pointer type" type.

    Syntactically no field can be called "extends" or "virtual", as these
    are keywords within the definition.

    Each Decl introduces a field and its type.  Field names must be distinct
    in a structure.

    The first argument 'self' is assumed to be of the type of the structure
    within which the virtual declaration occurs; ie, the meaning of 'self' in a
    type T is really (self *T).  Overriding methods for subtypes of the
    structure type will discriminate on this argument.
    
Func-Kwd   ::= ... | defmethod

    For defmethod, there must be at least one Decl in the signature and the Id
    of the first Decl must be "self", and the type of that argument must be a
    pointer-to-struct whose type hierarchy has a definition of a virtual that
    matches the signature.  All other arguments and return types must equal
    the corresponding in the declaration of the virtual.

    For calls, the concrete type T of the first argument is examined, and the
    method defined for T or for a base type U of T s.t. there is no closer
    method to T is selected.

Type       ::= Primitive | Alias | RefType | ArrayType
Primitive  ::= i32 | i64 | f32 | f64
Alias      ::= Id
RefType    ::= *Type | *StructName
ArrayType  ::= @Type
StructName ::= Id

    Syntactically, the * and @ must be next to the type name without any
    intervening spaces.  (In Scheme terms, a pointer to i32 is the single symbol
    "*i32".)

    The StructName must name a defined structure, or Object, the empty structure.

    An ArrayType represents a pointer to a heap-allocated array (so ArrayType is
    in a sense also a reftype); @@i32 is an array of arrays of i32s, while *@i32
    is a location holding a reference to an array of i32s.
    
    Note that struct types cannot be used as the types of variables or
    parameters; only references to struct types can.  Similarly, we cannot have
    arrays of struct types, only arrays of references to struct types.

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
   a structure that has a field named Id.

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

