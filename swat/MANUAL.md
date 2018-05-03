# Swat Language Definition

## Introduction

Swat is an evolving Scheme/Lisp-syntaxed WebAssembly superstructure.

The goal is to offer a reasonable superstructure, not to be able to express
everything Wasm can express.  Notably Swat has an expression discipline where
Wasm has a less structured stack discipline.

There are some hacks here.  "Vigor is better than rigor, unless you're already
dead."

See the .swat programs for examples.  See below for a reference.

swat.sch translates Swat programs to WebAssembly text format (the format
accepted by Firefox's wasmTextToBinary, not wabt at this point).  Usually you
run it via the `swat` script.

## JS API

When `swat` is run with the `--js` option it produces JS code that can be loaded
into the SpiderMonkey shell and run.  A swat module called `M` begets a global
JS variable also called `M`.  This variable holds an object that has a number of
fields, of which these are notable:

* `module`, which is the Wasm module object;
* `lib`, which is an object that should be passed under the `lib` name in the
  imports object when instantiating `module`.

Other fields of `M` should be considered private to the implementation.

See eg "fib.swat" for an example of how to use the JS API.  When compiled with
`--js`, the translation appears in the file "fib.wast.js".

## Definition

Note on the BNF format: initial-lower-case symbols and parens are literal.
"..."  denotes zero or more.  Vertical bars denote alternatives.

```
Program    ::= Component ...
Component  ::= Module | JS
JS         ::= (js SchemeString)

    When compiling to .wast, all modules but the first are ignored, as is any JS
    clause.  When compiling to .wast.js, each compiled module is stored in a JS
    variable corresponding to its name, and JS code is emitted between modules.

    The JS clause is a means of including arbitrary testing / driver code in
    .wast.js output.

Module     ::= (defmodule Id Toplevel ...)
Toplevel   ::= Global | Func | Class | Virtual

    The module ID is ignored except when compiling to .js.wast.  Toplevel
    clauses can be present in any order and are reordered as required by the
    wasm format.

    Keywords that define top-level things in modules can optionally be suffixed
    with "+", denoting an exported entity, and "-", denoting an imported entity.

    Imported entities don't have bodies or initializers.

    An imported entity can have a name of the form A:B.  In this case, A names
    the module and B is the top-level name within the module.  If the : is at
    the beginning of the name the module name is the empty string.  This is
    equivalent to omitting the A: part altogether.

    Top-level names must all be distinct: there is a single name space.

    TODO: As noted later, virtuals and classes cannot currently be imported
    or exported.

Global     ::= (Global-Kwd Id Type Global-Init)
Global-Kwd ::= defvar | defvar+ | defvar- | defconst | defconst+ | defconst-
Global-Init::= Number | Empty

    Mutable global variables are defined with defvar; immutable with defconst.

    For the imported kinds there can be no initializer; for the other kinds
    there must be an initializer.

    TODO: Only numeric types are supported, this is a restriction in the
    SpiderMonkey shell that will be lifted.

    TODO: It should be possible to initialize a global with the value of another
    immutable imported global, since wasm allows this.

    TODO: The type is redundant when there's an initializer, it should be
    possible to leave it out in that case.

Type       ::= Primitive | RefType
Primitive  ::= i32 | i64 | f32 | f64
RefType    ::= ClassName
ClassName  ::= Id

    The types of variables.

Func       ::= (Func-Kwd Signature Expr ...)
Func-Kwd   ::= defun | defun+ | defun-
Signature  ::= (Id Decl ...) | (Id Decl ... -> Type)
Decl       ::= (Id Type)

    If the return type is omitted then the function's type is void.

    Parameter names must be unique in the signature.

    A defun- does not have a body.

Virtual    ::= (defvirtual Signature VirtualCase ...)
VirtualCase ::= (ClassName Id)

    The Signature must have at least one formal, the name of the first argument
    must be 'self', and the type of the first argument must be a ClassName.

    In the VirtualCase, the Id must be the name of a function that has a
    signature that is identical to the one in the Signature in all arguments
    except the first.  The type of the first argument of that function must be a
    subtype of the type of the first argument in the Signature, and a supertype
    of ClassName.  (Not necessarily a proper subtype or supertype; typically it
    is identical to ClassName.)  The return type of the function must be a
    subtype of the return type of the virtual's.

    Each ClassName in a VirtualCase must be a subtype of the type of the first
    argument of Signature.

    The ClassNames must have no duplicates.

    When a virtual is called, with a first argument whose concrete dynamic type
    is T, a method is selected among those present s.t. the ClassName is a
    supertype of T, and there is no more specific ClassName that is also a
    supertype of T.

    TODO: There's no reason why a virtual can't be imported or exported, except
    we can't currently import or export class types.

Class      ::= (defclass ClassName Extends Field ...)
Extends    ::= (extends ClassName) | Empty
Field      ::= (id Type)

    A defclass clause defines a structured type.  Types form a tree underneath
    the common predefined type Object, which has no fields.

    Field names must be unique after merging the fields from the base classes
    and the present class.

    Automatic widening: When a value of static type A is used in a context that
    requires static type B, and A is a subclass of B, then the value is silently
    reinterpreted (without a change in representation) as being of type B.
    
    The contexts where automatic widening is applied are:

         - passing a value in a function call or new operator invocation
	 - returning a value from a function
	 - assigning a value to variable or object field

    There is no automatic widening when resulting a value from select, two-armed
    if, cond, or case; the arms of these must all have the same static type. Use
    a TypeCast expression to force a widening where one is not performed
    automatically.

    TODO: It is probably sane to widen for select, if, or case, if at least
    one arm has type Object.

    When a class that is not exported is mentioned in the signature of a
    function that is exported or is the thye of a global that is exported, then
    the class's name becomes known outside the module, but no information about
    the class is revealed.  It is thus possible to treat classes as ADTs, where
    a module exports constructors on classes and operations on their instances.

    TODO: No import and export of classes yet.  Class export will result in
    information being made available on the emitted module namespace, eg,
    M.make.Box is a factory for an exported Box type; for a given box instance
    b, b.x would read its x field.  Class import is out of scope for swat0.

Expr       ::= Syntax | Callish | Primitive
Maybe-expr ::= Expr | Empty
Syntax     ::= Begin | If | Cond | Set | Inc | Dec | Let | Let* | Loop | Break |
               Continue | While | Case | And | Or | Trap | Null | New | TypeTest |
	       TypeCast
Callish    ::= Builtin | Call | FieldRef
Primitive  ::= VarRef | Number 

   Expressions that are used in a void context (ie appear in the middle of a
   sequence of expressions or are the last expression in a function body)
   are automatically dropped; there is no drop operator to ignore a value.

VarRef     ::= Id

   References a location introduced by defvar, defconst, a function parameter,
   or let.

Begin      ::= (begin Expr Expr ...)

   Expression sequence yielding the value of its last expression, if any.

If         ::= (if Expr Expr) | (if Expr Expr Expr)

   The two-armed variant is always void, the three-armed variant yields the
   common type of its two arms.

Cond       ::= (cond Clause ... Else)
Clause     ::= (CondExpr Expr ...)
CondExpr   ::= Expr with type i32
Else       ::= (else Expr ...) | Empty

   Multi-armed conditional.  The CondExprs are tested in order; the first one to
   yield a non-zero result is selected and its clause body - the Expr ... - are
   evaluated in order.  An empty clause body denotes a void value.  The types of
   all the arms must match; if there is no "else" clause then the types of the
   arms must all be void.

Set        ::= (set! Lvalue Expr)
Inc        ::= (inc! Lvalue)
Dec        ::= (dec! Lvalue)
Lvalue     ::= Id | FieldRef

   set! sets the variable to the value of the expression.  inc! adds 1 (of the
   appropriate type) to the variable; dec! subtracts 1.

   If the Lvalue is a FieldRef then the pointer in the FieldRef designates an
   object, and the field designated by the name in the FieldRef in that object
   is updated.

   TODO: inc! and dec! should take an operand, but it is optional and
   defaults to '1'.

   TODO: Should inc! and dec! return a value, eg, old value, new value?  Check
   the CL hyperspec.

Let        ::= (let ((Id Init) ...) Expr Expr ...)
Init       ::= Expr

   Evaluate the Inits in left-to-right order in the scope outside the LET.  Then
   bind their values to the corresponding Id, and then evaluate the Exprs in the
   extended environment.

Let*       ::= (let* ((Id Init) ...) Expr Expr ...)

   Evaluate the first Init in the scope outside the LET*.  Then extend the
   environment with the Id/Value binding resulting from that evaluation, and
   move on to the next initializer.  And so on.  Finally evaluate the Exprs in
   the fully extended environment.

Loop       ::= (loop Id Expr Expr ...)
Break      ::= (break Id Maybe-expr)
Continue   ::= (continue Id)

   Break and continue must name an enclosing loop.  Labels are lexically scoped.

   If break carries an expression then its type is the type of the loop as
   an expression; if there are multiple breaks then their types must agree.

   TODO: It might be OK to make the label optional, even though technically
   this introduces a syntactic ambiguity.  We'd disambiguate in favor of it
   being a label.  In that case break and continue might also apply to "while".

While      ::= (while Expr Expr ...)

   The type of while is always void.  Evaluates the body while the first
   expression is nonzero.

Case       ::= (case Expr CaseCase ... CaseElse)
CaseCase   ::= ((CaseVal ...) Expr ...)
CaseElse   ::= (else Expr ...) | Empty
CaseVal    ::= Number | Id

   The dispatch expression must have type i32.  The arms must all have the same
   type.  If there is no else clause then the type of all arms must be void.

   The case values must be i32 constants or names of immutable non-imported i32
   globals with constant initializers.

   The resolved case values must all be distinct.  They can be negative.

   TODO: Expand this to i64 at least.

   TODO: Better code generation if the switch is sparse (and maybe in other
   cases), we may end up inadvertently generating a lot of code.

And        ::= (and Expr Expr)
Or         ::= (or Expr Expr)

   Early-out boolean operators.  and returns 0 or the value of the last Expr.
   or returns the first non-zero Expr, or 0.

Trap       ::= (trap) | (trap Type)

   Triggers a wasm unreachable trap.  The type must fit the context where the
   trap is used.  If the type is omitted it is void.

   TODO: Requiring the type is a hack; wasm has a more elegant solution with the
   unreachable type, we might adopt that.

Null       ::= (null ClassName)

   Produces a null reference of the appropriate class type.

   TODO: Requiring the type name is a hack; we can remove this once we have a
   firmer sense of automatic widening / upcasts.

New        ::= (new ClassName Expr ...)

   Allocate a new instance of ClassName and initialize its fields with the
   expressions.  Every field must have an initializer.

TypeTest   ::= (is ClassName Expr)

   Expr must have static class type T where T is a supertype or subtype of
   ClassName.

   Let V be the value of Expr.  If T is a subtype of ClassName, or if V's
   dynamic type is ClassName or a subtype of ClassName then return 1, otherwise 0.

   TODO: This predicate should be written (or should be sugared as) "ClassName?"
   for each ClassName, and "Object?" should be predefined of course.

TypeCast   ::= (as ClassName Expr)

   Expr must have static class type T where T is a supertype or subtype of
   ClassName.

   Let V be the value of Expr.  If T is a subtype of ClassName, or if V's
   dynamic type is ClassName or a subtype of ClassName then return V, with
   static type ClassName; otherwise trap.

Builtin    ::= (Operator Expr ...)
Operator   ::= Number-op | Int-op | Float-op | Conv-op | Ref-op
Number-op  ::= + | - | * | div | < | <= | > | >= | = | != | zero? | nonzero? | select
Int-op     ::= divu | rem | remu | <u | <=u | >u | >=u | not | bitand | bitor | bitxor | bitnot |
               shl | shr | shru | rotl | rotr | clz | ctz | popcnt | extend8 | extend16 | extend32
Float-op   ::= max | neg | min | abs | sqrt | ceil | floor | copysign | nearest | trunc
Conv-op    ::= i32->i64 | u32->i64 | i64->i32 | f32->f64 | f64->f32 |
               f64->i32 | f64->i64 | i32->f64 | i64->f64 | f32->i32 | f32->i64 | i32->f32 | i64->f32 |
               f32->bits | bits->f32 | f64->bits | bits->f64
Ref-op     ::= null?

   i32->i64 sign-extends, while u32->i64 zero-extends.  The float
   conversions are generally trapping where they might be lossy.

   The ->bits and bits-> operations return / take integers of the
   appropriate size.

   The syntax for select is the "natural" one, (select cond true-value
   false-value).

   TODO: more unsigned conversions
   TODO: more saturating / nontrapping conversions.
   TODO: max, min, neg, and abs should be synthesized for integer operands.
   TODO: rem should be synthesized for floating operands.
   TODO: Some Scheme implementations prefer fxand, fxor, etc for the bitwise ops;
         possibly recent standards have standard names for these and some of
	 the other operations here, and we should have those at least as aliases.
   TODO: (neg x) should be written (- x)
   TODO: More generally, operations that are meaningfully multi-arity - which
         is many of them, as in scheme - should be supported as multi-arity.
   TODO: nan?, finite?, infinite? would be useful
   TODO: allow i32 values in some contexts where wasm requires i64 but this is
         fairly nuts, eg as the second operand of shifts and rotates.  Or
         possibly just allow automatic i32->i64 and f32->f64 promotion for
         operators if the other operand requires it.  (And i32->f64?)  Also
	 see below, about constants.

FieldRef   ::= (*Id Expr)

    Expr must evaluate to an object value (reference to instance of class).
    That class must have a field named by the Id in the grammar above.  The *
    and the Id comprise a single symbol.

Call       ::= (Id Expr ...)

    Id must name a function defined by defun.  Calls the named function with
    the given arguments.

Number     ::= SchemeIntegerLiteral | SchemeFloatingLiteral | SchemeCharLiteral | Prefixed
Prefixed   ::= A symbol comprising the prefixes "I.", "L.", "F.", or "D."
               followed by characters that can be parsed as integer values
               (for I and L) or floating values (for F and D).  I denotes
               i32, L denotes i64, F denotes f32, D denotes f64.  So
               F.3.1415e-2 is the f32 representing approximately Pi / 100.

   A SchemeCharLiteral is converted to its i32 representation.

   A SchemeIntegerLiteral on its own denotes an i32 if it is small enough,
   otherwise i64.

   A SchemeFloatingLiteral on its own denotes an f64.

   NaN is written as "+nan.0" or "-nan.0" (they denote the same value).
   Infinities are written +inf.0 and -inf.0 respectively.  All these can
   be prefixed with "D." or "F."

   TODO: A constant that can be widened without loss of precision in a given
   type context should be widened.  eg, assuming i is int64, (+ i 0) should just
   work.  This happens frequently.  Also see above, about general automatic
   widening.

Id         ::= SchemeSymbol but not Prefixed or Reserved or Compound

   Sometimes it is useful for this symbol to have JS-compatible syntax,
   notably for module names.

   TODO: Check that names conform to the restrictions.
   
Compound   ::= SchemeSymbol with embedded ":" or "/"

   Reserved for import names that name a module and similar uses.

Reserved   ::= SchemeSymbol starting with "%" or "$" or "_", or SchemeSymbol
               naming one of the built-in primitive types i32, i64, f32, f64,
	       and anyref

   These names are used internally.

   Names starting and ending with "%" are used to denote the original
   denotations of predefined names; we could fix this but we're lazy.

   Names starting and ending with "_" are used as hidden fields
   holding system data in class instances, for the time being.

   Names starting with "$" are used to name blocks and loops and types.

   The built-in type names are used in emitted wast code to tag blocks
   and loops and "if" expressions with their type, and there is
   nothing we can do to fix this.  We could work around it by renaming
   everything else but we are (again) lazy.
```