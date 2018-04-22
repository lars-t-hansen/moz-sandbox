// -*- fill-column: 80 -*-

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

Toplevel   ::= ... | Struct

Struct     ::= (Struct-Kwd Base-Type Id Decl ... Virtual-Decl ...)
Base-Type  ::= (extends Id) | Empty
Struct-Kwd ::= defstruct+ | defstruct- | defstruct
Virtual-Decl ::= (virtual Virtual-Signature)
Virtual-Signature ::= (Id self Decl ...) | (Id self Decl ... -> ReturnType)

    Introduce a named structure type.  These form a tree, with Object at the
    root.  If no base type is provided then Object is assumed.  Since Object
    has no fields or methods this does not matter, but it allows us to
    use eg "*Object" as an "any pointer type" type.

    Each Decl introduces a field and its type.  Field names must be distinct
    in a structure.

Type       ::= ... | *Id

  Here Id must be a struct type name.

Func-Kwd   ::= ... | defmethod

    For defmethod, there must be at least one Decl and the Id of the first
    Decl must be "self", and the type of that argument must be a
    pointer-to-struct whose type hierarchy has a definition of a virtual
    that matches the signature.

Callish    ::= ... | FieldRef | Cast | Typetest

FieldRef   ::= (*Id E)
Cast       ::= (->Id Expr)
Typetest   ::= (Id? Expr)

  Here Id must be a struct type name.

Set        ::= (set! Lvalue Expr)
Inc        ::= (inc! Lvalue)
Dec        ::= (dec! Lvalue)
Lvalue     ::= Id | FieldRef

  These supersede the existing definitions by generalizing the notion of lvalue.

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

