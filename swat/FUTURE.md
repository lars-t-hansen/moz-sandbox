Callish    ::= ... | FieldRef | Cast | Typetest

Type       ::= ... | *Id

Struct     ::= (Struct-Kwd Base-Type Id Decl ... Virtual-Decl ...)
Base-Type  ::= <: Id | Empty
Struct-Kwd ::= defstruct+ | defstruct- | defstruct
Virtual-Decl ::= (virtual Virtual-Signature)
Virtual-Signature ::= (Id self Decl ...) | (Id self Decl ... -> ReturnType)

    Introduce a named structure type.  These form a tree, with Object at the
    root.  If no base type is provided then Object is assumed.  Since Object
    has no fields or methods this does not matter, but it allows us to
    use eg "*Object" as an "any pointer type" type.

    Each Decl introduces a field and its type.  Field names must be distinct
    in a structure.
    
Set        ::= (set! Lvalue Expr)
Inc        ::= (inc! Lvalue)
Dec        ::= (dec! Lvalue)
Lvalue     ::= Id | FieldRef

   ...

New        ::= (new T Expr ...) where T is a struct type and the Expr number as many as the fields
Null       ::= (null RefType)
;;
   ...


FieldRef   ::= (*Id E)
Cast       ::= (->Id Expr)
Typetest   ::= (Id? Expr)

  Here Id must be a struct type name.


    For defmethod, there must be at least one Decl and the Id of the first
    Decl must be "self", and the type of that argument must be a
    pointer-to-struct whose type hierarchy has a definition of a virtual
    that matches the signature.
