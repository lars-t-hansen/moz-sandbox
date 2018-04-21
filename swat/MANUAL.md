# Swat Language Definition

## Introduction

Note on the BNF format: initial-lower-case symbols and parens are literal.
"..."  denotes zero or more.  Vertical bars denote alternatives.

Some hacks here.  "Vigor is better than rigor, unless you're already dead."

## Definition

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
Toplevel   ::= Global | Func | TypeAlias

    The module ID is ignored except when compiling to .js.wast.  Toplevel
    clauses can be present in any order and are reordered as required by the
    wasm format.

    Keywords that define top-level things in modules can optionally be
    suffixed with "+", denoting an exported entity, and "-", denoting an
    imported entity.  Imported entities don't have bodies or initializers.
    At the moment, imports are all from the module called "".

    Top-level names must all be distinct: there is a single name space.

    TODO: Allow imports from named modules, using eg a module:entity
    syntax.

Global     ::= (Global-Kwd Id Type Global-Init)
Global-Kwd ::= defvar | defvar+ | defvar- | defconst | defconst+ | defconst-
Global-Init::= Number | Empty

    Mutable global variables are defined with defvar; immutable with
    defconst.

    For the imported kinds there can be no initializer; for the other kinds
    there must be an initializer.

    TODO: It should be possible to initialize a global with the value of
    another immutable imported global, since wasm allows this.

    TODO: The type is redundant when there's an initializer, it should
    be possible to leave it out in that case.

Type       ::= i32 | i64 | f32 | f64 | Id

    In type "Id", Id must be a type alias.  These are created by deftype, or are
    built in.  Built-in aliases are:

           bool is an alias for i32

TypeAlias  ::= (deftype Id Type)

    Define Id as an alias for the given type, which can't reference the
    alias being introduced here.

    Until further notice, Type is restricted to being an Id.

    TODO: This is aspirational - not implemented.

Func       ::= (Func-Kwd Signature Expr ...)
Func-Kwd   ::= defun | defun+ | defun-
Signature  ::= (Id Decl ...) | (Id Decl ... -> ReturnType)
Decl       ::= (Id Type)
ReturnType ::= Type | void

    If the return type is omitted then it defaults to void.

    Parameter names must be unique in the signature.

    A defun- does not have a body.

Expr       ::= Syntax | Callish | Primitive
Maybe-expr ::= Expr | Empty
Syntax     ::= Begin | If | Set | Inc | Dec | Let | Loop | Break | Continue |
               While | Case | And | Or | Trap
Callish    ::= Builtin | Call
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

Set        ::= (set! VarRef Expr)
Inc        ::= (inc! VarRef)
Dec        ::= (dec! VarRef)

   set! sets the variable to the value of the expression.  inc! adds 1 (of the
   appropriate type) to the variable; dec! subtracts 1.

   TODO: inc! and dec! should take an operand, but it is optional and
   defaults to '1'.

Let        ::= (let ((Id Expr) ...) Expr Expr ...)

   Bind the Decls with given values in the body of the let.  Initializers are
   evaluated in the scope outside the LET, in left-to-right order.

   TODO: We really should have both let and let*.

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

Trap       ::= (trap ReturnType)

   Triggers a wasm unreachable trap.  The type must fit the context where the
   trap is used.

   TODO: Requiring the type is a hack.

Builtin    ::= (Operator Expr ...)
Operator   ::= Number-op | Int-op | Float-op | Conv-op 
Number-op  ::= + | - | * | div | < | <= | > | >= | = | != | zero? | nonzero? | select
Int-op     ::= divu | rem | remu | <u | <=u | >u | >=u | not | bitand | bitor | bitxor | bitnot |
               shl | shr | shru | rotl | rotr | clz | ctz | popcnt | extend8 | extend16 | extend32
Float-op   ::= max | neg | min | abs | sqrt | ceil | floor | copysign | nearest | trunc
Conv-op    ::= i32->i64 | u32->i64 | i64->i32 | f32->f64 | f64->f32 |
               f64->i32 | f64->i64 | i32->f64 | i64->f64 | f32->i32 | f32->i64 | i32->f32 | i64->f32 |
               f32->bits | bits->f32 | f64->bits | bits->f64
      
   i32->i64 sign-extends, while u32->i64 zero-extends.  These are
   generally trapping conversions where they might be lossy.

   The ->bits and bits-> operations return / take integers of the
   appropriate size.

   The syntax for select is the "natural" one, (select cond true-value
   false-value).

   TODO: more unsigned conversions
   TODO: more saturating / nontrapping conversions.
   TODO: max, min, neg, and abs should be synthesized for integer operands.
   TODO: rem should be synthesized for floating operands.
   TODO: nan?, finite?, infinite? might be useful
   TODO: allow i32 values in some contexts where wasm requires i64 but this is
         fairly nuts, eg as the second operand of shifts and rotates.  Or
         possibly just allow automatic i32->i64 and f32->f64 promotion for
         operators if the other operand requires it.  (And i32->f64?)  Also
	 see below, about constants.

Call       ::= (Id Expr ...)

    Id must name a function defined by defun.  Calls the named function with
    the given arguments.

Number     ::= SchemeIntegerLiteral | SchemeFloatingLiteral | Prefixed
Prefixed   ::= A symbol comprising the prefixes "I.", "L.", "F.", or "D."
               followed by characters that can be parsed as integer values
               (for I and L) or floating values (for F and D).  I denotes
               i32, L denotes i64, F denotes f32, D denotes f64.  So
               F.3.1415e-2 is the f32 representing approximately Pi / 100.

   A SchemeIntegerLiteral on its own denotes an i32 if it is small enough,
   otherwise i64.

   A SchemeFloatingLiteral on its own denotes an f64.

   TODO: A constant that can be widened without loss of precision in a given
   type context should be widened.  eg, assuming i is int64, (+ i 0) should just
   work.  This happens frequently.  Also see above, about general automatic
   widening.

   TODO: Syntax for NaN and infinities would be helpful.

Id         ::= SchemeSymbol but not Prefixed

   Sometimes it is useful for this symbol to have JS-compatible syntax,
   notably for module names.
   
   It's probably best to avoid using separators like ":" or "/" in
   identifiers since we might want to use those in the future.
```