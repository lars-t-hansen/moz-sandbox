;;; -*- fill-column: 80 -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public License,
;;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;;; obtain one at <http://mozilla.org/MPL/2.0/>.
;;;
;;; This is r5rs-ish Scheme, tested with Larceny (http://larcenists.org).

;;; Swat is an evolving Scheme/Lisp-syntaxed WebAssembly superstructure.
;;;
;;; The goal is to offer a reasonable superstructure, but not to be able to
;;; express everything.  Notably swat has an expression discipline where wasm
;;; has a less structured stack discipline.
;;;
;;; See the .swat programs for examples.  See below for a reference.
;;;
;;; This program translates swat programs to WebAssembly text format (the format
;;; accepted by Firefox's wasmTextToBinary, not necessarily wabt at this point).
;;;
;;; See end for misc ways to run this, and see the shell script "swat" for a
;;; command line interface.
;;;
;;; See end for TODO lists.

;;; Language definition
;;; ===================
;;;
;;; Note on the BNF format: initial-lower-case symbols and parens are literal.
;;; "..."  denotes zero or more.  Vertical bars denote alternatives.
;;;
;;; Some hacks here.  "Vigor is better than rigor, unless you're already dead."
;;;
;;; Program    ::= Component ...
;;; Component  ::= Module | JS
;;; JS         ::= (js SchemeString)
;;;
;;;     When compiling to .wast, all modules but the first are ignored, as is
;;;     any JS clause.  When compiling to .wast.js, each compiled module is
;;;     stored in a JS variable corresponding to its name, and JS code is
;;;     emitted between modules.
;;;
;;;     The JS clause is a means of including arbitrary testing / driver code in
;;;     .wast.js output.
;;;
;;; Module     ::= (defmodule Id Toplevel ...)
;;; Toplevel   ::= Global | Func | Struct | TypeAlias
;;;
;;;     The module ID is ignored except when compiling to .js.wast.  Toplevel
;;;     clauses can be present in any order and are reordered as required by the
;;;     wasm format.
;;;
;;;     Keywords that define top-level things in modules can optionally be
;;;     suffixed with "+", denoting an exported entity, and "-", denoting an
;;;     imported entity.  Imported entities don't have bodies or initializers.
;;;     At the moment, imports are all from the module called "".
;;;
;;;     Top-level names must all be distinct: there is a single name space.
;;;
;;;     TODO: Allow imports from named modules, using eg a module:entity
;;;     syntax.
;;;
;;; Global     ::= (Global-Kwd Id Type Global-Init)
;;; Global-Kwd ::= defvar | defvar+ | defvar- | defconst | defconst+ | defconst-
;;; Global-Init::= Number | Empty
;;;
;;;     Mutable global variables are defined with defvar; immutable with
;;;     defconst.
;;;
;;;     For the imported kinds there can be no initializer; for the other kinds
;;;     there must be an initializer.
;;;
;;;     TODO: It should be possible to initialize a global with the value of
;;;     another immutable imported global, since wasm allows this.
;;;
;;;     TODO: The type is redundant when there's an initializer, it should
;;;     be possible (or required) to leave it out in that case.
;;;
;;; Type       ::= i32 | i64 | f32 | f64 | *Id | Id
;;;
;;;     In type "*Id", Id must reference a type defined with defstruct or the
;;;     built-in type Object.
;;;
;;;     In type "Id", Id must be a type alias.  These are created by deftype or
;;;     defenum, or are built in.  Built-in aliases are:
;;;
;;;            bool is an alias for i32
;;;
;;; TypeAlias  ::= (deftype Id Type)
;;;
;;;     Define Id as an alias for the given type, which can't reference the
;;;     alias being introduced here.
;;;
;;;     TODO: For the time being, Type is restricted to being an Id.
;;;
;;; Struct     ::= (Struct-Kwd Base-Type Id Decl ... Virtual-Decl ...)
;;; Base-Type  ::= <: Id | Empty
;;; Struct-Kwd ::= defstruct+ | defstruct- | defstruct
;;; Virtual-Decl ::= (virtual Virtual-Signature)
;;; Virtual-Signature ::= (Id self Decl ...) | (Id self Decl ... -> ReturnType)
;;;
;;;     Introduce a named structure type.  These form a tree, with Object at the
;;;     root.  If no base type is provided then Object is assumed.  Since Object
;;;     has no fields or methods this does not matter, but it allows us to
;;;     use eg "*Object" as an "any pointer type" type.
;;;
;;;     Each Decl introduces a field and its type.  Field names must be distinct
;;;     in a structure.
;;;     
;;; Func       ::= (Func-Kwd Signature Expr ...)
;;; Func-Kwd   ::= defun | defun+ | defun- | defmethod
;;; Signature  ::= (Id Decl ...) | (Id Decl ... -> ReturnType)
;;; Decl       ::= (Id Type)
;;; ReturnType ::= Type | void
;;;
;;;     If the return type is omitted then it defaults to void.
;;;
;;;     Parameter names must be unique.
;;;
;;;     A defun- does not have a body.
;;;
;;;     For defmethod, there must be at least one Decl and the Id of the first
;;;     Decl must be "self", and the type of that argument must be a
;;;     pointer-to-struct whose type hierarchy has a definition of a virtual
;;;     that matches the signature.
;;;
;;; Expr       ::= Syntax | Callish | Primitive
;;; Maybe-expr ::= Expr | Empty
;;; Syntax     ::= Begin | If | Set | Inc | Dec | Let | Loop | Break | Continue |
;;;                While | And | Or | New | Null
;;; Callish    ::= Builtin | Call | FieldRef | Cast | Typetest
;;; Primitive  ::= Id | Number | Unreachable
;;;
;;;    Expressions that are used in a void context (ie appear in the middle of a
;;;    sequence of expressions or are the last expression in a function body)
;;;    are automatically dropped; there is no drop operator to ignore a value.
;;;
;;; Begin      ::= (begin Expr Expr ...)
;;;
;;;    Expression sequence yielding the value of its last expression.
;;;
;;; If         ::= (if Expr Expr) | (if Expr Expr Expr)
;;;
;;;    The two-armed variant is always void, the three-armed variant yields the
;;;    common type of its two arms.
;;;
;;; Set        ::= (set! Lvalue Expr)
;;; Inc        ::= (inc! Lvalue)
;;; Dec        ::= (dec! Lvalue)
;;; Lvalue     ::= Id | FieldRef
;;;
;;;    ...
;;;
;;;    TODO: inc! and dec! should take an operand, but it is optional and
;;;    defaults to '1'.
;;;
;;; Let        ::= (let ((Id Expr) ...) Expr Expr ...)
;;;
;;;    Bind the Decls with given values in the body of the let.  Initializers
;;;    are evaluated in left-to-right order.  Names are are scoped as let* in Scheme.
;;;
;;;    TODO: We really should have both let and let*, and let should follow normal
;;;    scoping rules.
;;;
;;; Loop       ::= (loop Id Expr Expr ...)
;;; Break      ::= (break Id Maybe-expr)
;;; Continue   ::= (continue Id)
;;;
;;;    Break and continue must name an enclosing loop.  Labels are scoped.
;;;
;;;    If break carries an expression then its type is the type of the loop as
;;;    an expression; if there are multiple breaks then their types must agree.
;;;
;;;    TODO: It might be OK to make the label optional, even though technically
;;;    this introduces a syntactic ambiguity.  We'd disambiguate in favor of it
;;;    being a label.  In that case break and continue might also apply to "while".
;;;
;;; While      ::= (while Expr Expr ...)
;;;
;;;    The type of while is always void.  Evaluates the body while the first
;;;    expression is nonzero.
;;;
;;; And        ::= (and Expr Expr)
;;; Or         ::= (or Expr Expr)
;;;
;;;    Early-out boolean operators.
;;;
;;; New        ::= (new T Expr ...) where T is a struct type and the Expr number as many as the fields
;;; Null       ::= (null RefType)
;;
;;;    ...
;;;
;;; Builtin    ::= (Operator Expr ...)
;;; Operator   ::= Number-op | Int-op | Float-op | Conv-op | Ref-op
;;; Number-op  ::= + | - | * | div | < | <= | > | >= | = | != | zero? | nonzero?
;;; Int-op     ::= divu | mod | modu | <u | <=u | >u | >=u | not | bitand | bitor | bitxor | bitnot |
;;;                shl | shr | shru | rotl | rotr | clz | ctz | popcnt | extend8 | extend16 | extend32
;;; Float-op   ::= max | neg | min | abs | sqrt | ceil | floor | copysign | nearest | trunc
;;; Ref-op     ::= null?
;;; Conv-op    ::= i32->i64 | u32->i64 | i64->i32
;;;
;;;    i32->i64 sign-extends, while u32->i64 zero-extends.
;;;
;;;    TODO: Conversion operators are not defined yet.
;;;
;;;    TODO: max, min, neg, and abs should be synthesized for integer operands.
;;;
;;; FieldRef   ::= (*Id E)
;;; Cast       ::= (->Id Expr)
;;; Typetest   ::= (Id? Expr)
;;;
;;;   Here Id must be a struct type name.
;;;
;;; Call       ::= (Id Expr ...)  where Id is not something recognized by other productions
;;;
;;; Number     ::= SchemeIntegerLiteral | SchemeFloatingLiteral | Prefixed
;;; Prefixed   ::= A symbol comprising the prefixes "I.", "L.", "F.", or "D."
;;;                followed by characters that can be parsed as integer values
;;;                (for I and L) or floating values (for F and D).  I denotes
;;;                i32, L denotes i64, F denotes f32, D denotes f64.  So
;;;                F.3.1415e-2 is the f32 representing approximately Pi / 100.
;;;
;;;    A SchemeIntegerLiteral on its own denotes an i32 if it is small enough,
;;;    otherwise i64.
;;;
;;;    A SchemeFloatingLiteral on its own denotes an f64
;;;
;;;    TODO: A constant that can be widened without loss of precision in a given
;;;    type context should be widened.  eg, assuming i is int64, (+ i 0) should
;;;    just work.  This happens frequently.
;;;
;;; Id         ::= SchemeSymbol but not Unreachable or Prefixed
;;;
;;;    Sometimes it is useful for this symbol to have JS-compatible syntax,
;;;    notably for module names.
;;;    
;;;    It's probably best to avoid using separators like ":" or "/" in
;;;    identifiers since we might want to use those in the future.
;;;
;;; Unreachable ::= The SchemeSymbol "???"
;;;
;;;    ??? is the traditional shorthand for code that is unfinished or should
;;;    not run for other reasons.
;;;
;;;    TODO: This currently returns void, but that's wrong - it must be some
;;;    kind of "any" type that we can match to anything.  Or we can make this
;;;    into a form, (unreachable ty)

;;; Keywords and pre-defined global type names that can't be used for new global
;;; names.
;;;
;;; TODO: In addition, built-in operator names actually shadow program bindings,
;;; though that could easily be fixed.
;;;
;;; TODO: local bindings should have an exclusion list too.  In general scoping
;;; is a little bit of a mess, since the name represents the denotation in too
;;; many cases.

(define *predefined-names*
  '(Object bool i32 i64 f32 f64 defun defun+ defun- defvar defvar+ defvar-
    defconst defconst+ defconst- deftype defstruct defmodule begin if set!
    inc! dec! let loop break continue while and or null new))

;;; Translation context
;;;
;;; During translation all these lists are in reverse order, newest
;;; element first.

(define (make-cx)
  (vector #f				; Slots storage (during body expansion)
	  '()  				; Globals  ((name . global) ...)
	  '()				; Structs  ((name . struct) ...)
	  '()				; Funcs:   ((name . func) ...)
	  0				; Next function ID
	  *predefined-names*            ; Names    (name ...)
	  #f 				; Block ID (during body expansion)
	  0                             ; Next global ID
	  '()                           ; Active labeled loops
	  '((i32  . i32)		; Canonical type names
	    (f32  . f32)
	    (i64  . i64)
	    (f64  . f64)
	    (bool . i32))))

(define (cx.slots cx)            (vector-ref cx 0))
(define (cx.slots-set! cx v)     (vector-set! cx 0 v))
(define (cx.globals cx)          (vector-ref cx 1))
(define (cx.globals-set! cx v)   (vector-set! cx 1 v))
(define (cx.structs cx)          (vector-ref cx 2))
(define (cx.funcs cx)            (vector-ref cx 3))
(define (cx.funcs-set! cx v)     (vector-set! cx 3 v))
(define (cx.func-id cx)          (vector-ref cx 4))
(define (cx.func-id-set! cx v)   (vector-set! cx 4 v))
(define (cx.names cx)            (vector-ref cx 5))
(define (cx.names-set! cx v)     (vector-set! cx 5 v))
(define (cx.block-id cx)         (vector-ref cx 6))
(define (cx.block-id-set! cx v)  (vector-set! cx 6 v))
(define (cx.global-id cx)        (vector-ref cx 7))
(define (cx.global-id-set! cx v) (vector-set! cx 7 v))
(define (cx.loops cx)            (vector-ref cx 8))
(define (cx.loops-set! cx v)     (vector-set! cx 8 v))
(define (cx.type-names cx)       (vector-ref cx 9))
(define (cx.type-names-set! cx v)(vector-set! cx 9 v))

(define (new-block-id cx tag)
  (let ((n (cx.block-id cx)))
    (cx.block-id-set! cx (+ n 1))
    (string->symbol (string-append "$" tag "_" (number->string n)))))

;;; Modules

;;; To handle imports and mutual references we must do two prepasses
;;; over the module to define functions in the table.  The first
;;; handles imports, which are given the lowest indices.  The second
;;; handles the others.  Then the third phase expands bodies.

(define (expand-module m)
  (check-list-atleast m 2 "Bad module" m)
  (check-head m 'defmodule)
  (check-symbol (cadr m) "Bad module name" m)
  (let ((cx   (make-cx))
	(name (symbol->string (cadr m)))
	(body (cddr m)))
    (for-each (lambda (d)
		(check-list-atleast d 1 "Bad top-level phrase" d)
		(case (car d)
		  ((defun-)
		   (expand-func-phase1 cx d))
		  ((defconst- defvar-)
		   (expand-global-phase1 cx d))
		  ((defun defun+ defconst defconst+ defvar defvar+)
		   #t)
		  (else
		   (fail "Unknown top-level phrase" d))))
	      body)
    (for-each (lambda (d)
		(case (car d)
		  ((defun defun+)
		   (expand-func-phase1 cx d))
		  ((defconst defconst+ defvar defvar+)
		   (expand-global-phase1 cx d))))
	      body)
    (for-each (lambda (d)
		(case (car d)
		  ((defun defun+)
		   (expand-func-phase2 cx d))
		  ((defconst defconst+ defvar defvar+)
		   (expand-global-phase2 cx d))))
	      body)
    (values name
	    (cons 'module
		  (append
		   (map (lambda (g)
			  (let* ((g (cdr g))
				 (t (if (global.mut? g) `(mut ,(global.type g)) (global.type g))))
			    (if (global.import? g)
				`(import "" ,(symbol->string (global.name g)) (global ,t))
				`(global ,@(if (global.export? g) `((export ,(symbol->string (global.name g)))) '())
					 ,t
					 ,(global.init g)))))
			(reverse (cx.globals cx)))
		   (map (lambda (f)
			  (let ((func (cdr f)))
			    (if (func.import? func)
				`(import "" ,(symbol->string (func.name func))
					 ,(assemble-function func '()))
				(func.defn func))))
			(reverse (cx.funcs cx))))))))

;;; Functions

(define (make-func name import? export? id params result slots locals)
  (vector name import? export? id params result slots locals #f))

(define (func.name f) (vector-ref f 0))
(define (func.import? f) (vector-ref f 1))
(define (func.export? f) (vector-ref f 2))
(define (func.id f) (vector-ref f 3))
(define (func.params f) (vector-ref f 4))
(define (func.result f) (vector-ref f 5))
(define (func.slots f) (vector-ref f 6))
(define (func.locals f) (vector-ref f 7))
(define (func.defn f) (vector-ref f 8))
(define (func.defn-set! f v) (vector-set! f 8 v))

(define (define-name! cx name)
  (if (memq name (cx.names cx))
      (fail "Duplicate global name" name))
  (cx.names-set! cx (cons name (cx.names cx))))

(define (define-function! cx name import? export? params result-type slots locals)
  (let* ((id   (cx.func-id cx))
	 (func (make-func name import? export? id params result-type slots locals)))
    (cx.func-id-set! cx (+ id 1))
    (cx.funcs-set!   cx (cons (cons name func) (cx.funcs cx)))
    func))

;; Here we really want to keep (cdr (flatten-blocks body)), not just body

(define (assemble-function func body)
  (let* ((f body)
	 (f (if (eq? (func.result func) 'void)
		f
		(cons `(result ,(func.result func)) f)))
	 (f (append (func.params func) f))
	 (f (if (func.export? func)
		(cons `(export ,(symbol->string (func.name func))) f)
		f))
	 (f (cons 'func f)))
    (func.defn-set! func f)
    f))

(define (expand-func-phase1 cx f)
  (check-list-atleast f 2 "Bad function" f)
  (let* ((signature (cadr f))
	 (_         (check-list-atleast signature 1 "Bad signature" signature))
	 (name      (car signature))
	 (_         (check-symbol name "Bad function name" name))
	 (export?   (eq? (car f) 'defun+))
	 (import?   (eq? (car f) 'defun-))
	 (body      (cddr f))
	 (slots     (make-slots)))
    (if (and import? (not (null? body)))
	(fail "Import function can't have a body" f))
    (define-name! cx name)
    (cx.slots-set! cx #f)
    (cx.block-id-set! cx #f)
    (let loop ((xs     (cdr signature))
	       (locals '())
	       (params '()))
      (cond ((null? xs)
	     (define-function! cx name import? export? (reverse params) 'void slots locals))

	    ((eq? (car xs) '->)
	     (check-list xs 2 "Bad signature" signature)
	     (let ((t (parse-result-type cx (cadr xs))))
	       (define-function! cx name import? export? (reverse params) t slots locals)))

	    (else
	     (let ((first (car xs)))
	       (if (not (and (list? first) (= 2 (length first)) (symbol? (car first))))
		   (fail "Bad parameter" first))
	       (let ((t    (parse-type cx (cadr first)))
		     (name (car first)))
		 (if (assq name locals)
		     (fail "Duplicate parameter" name))
		 (let-values (((slot _) (claim-param slots t)))
		   (loop (cdr xs)
			 (cons (cons name (make-binding name slot t)) locals)
			 (cons `(param ,t) params))))))))))

(define (expand-func-phase2 cx f)
  (let* ((signature (cadr f))
	 (name      (car signature))
	 (export?   (eq? (car f) 'defun+))
	 (import?   (eq? (car f) 'defun-))
	 (body      (cddr f)))
    (let* ((func   (cdr (assq name (cx.funcs cx))))
	   (locals (func.locals func))
	   (slots  (func.slots func)))
      (cx.slots-set! cx slots)
      (cx.block-id-set! cx 0)
      (assemble-function func (expand-body cx body (func.result func) locals)))))

(define (expand-body cx body expected-type locals)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) locals)))
    (let ((drop? (and (eq? expected-type 'void)
		      (not (eq? result-type 'void)))))
      `(,@(get-slot-decls (cx.slots cx)) ,expanded ,@(if drop? '(drop) '())))))

;;; Globals

(define (make-global name import? export? mut? id type)
  (vector name id type import? export? mut? #f))

(define (global.name x) (vector-ref x 0))
(define (global.id x) (vector-ref x 1))
(define (global.type x) (vector-ref x 2))
(define (global.import? x) (vector-ref x 3))
(define (global.export? x) (vector-ref x 4))
(define (global.mut? x) (vector-ref x 5))
(define (global.init x) (vector-ref x 6))
(define (global.init-set! x v) (vector-set! x 6 v))

(define (define-global! cx name import? export? mut? type)
  (let* ((id   (cx.global-id cx))
	 (glob (make-global name import? export? mut? id type)))
    (cx.global-id-set! cx (+ id 1))
    (cx.globals-set!   cx (cons (cons name glob) (cx.globals cx)))
    glob))

(define (expand-global-phase1 cx g)
  (check-list-oneof g '(3 4) "Bad global" g)
  (let* ((name    (cadr g))
	 (_       (check-symbol name "Bad global name" name))
	 (export? (memq (car g) '(defconst+ defvar+)))
	 (import? (memq (car g) '(defconst- defvar-)))
	 (mut?    (memq (car g) '(defvar defvar+ defvar-)))
	 (type    (parse-type cx (caddr g)))
	 (init    (if (null? (cdddr g)) #f (cadddr g)))
	 (_       (if init (check-constant init))))
    (if (and import? init)
	(fail "Import global can't have an initializer"))
    (define-name! cx name)
    (define-global! cx name import? export? mut? type)))

;; We could expand the init during phase 1 but we'll want to broaden
;; inits to encompass global imports soon.

(define (expand-global-phase2 cx g)
  (let* ((name    (cadr g))
	 (init    (if (null? (cdddr g)) #f (cadddr g))))
    (if init
	(let ((defn (cdr (assq name (cx.globals cx)))))
	  (global.init-set! defn (expand-constant-expr cx init))))))

;;; Local bindings

(define (make-binding name slot type) (vector name slot type))
(define (binding.name x) (vector-ref x 0))
(define (binding.slot x) (vector-ref x 1))
(define (binding.type x) (vector-ref x 2))

;;; Local slots storage

(define (make-slots)
  (vector (list 0 '()) '() '() '() '()))

(define (slots.tracker x) (vector-ref x 0))

(define *slots-i32* 1)
(define *slots-i64* 2)
(define *slots-f32* 3)
(define *slots-f64* 4)

(define (make-tracker)
  (list 0 '()))

(define (tracker.next x) (car x))
(define (tracker.next-set! x v) (set-car! x v))
(define (tracker.defined x) (cadr x))
(define (tracker.defined-set! x v) (set-car! (cdr x) v))

(define (slot-index-for-type t)
  (case t
    ((i32) *slots-i32*)
    ((i64) *slots-i64*)
    ((f32) *slots-f32*)
    ((f64) *slots-f64*)
    (else ???)))

;; returns (slot-id garbage)
(define (claim-param slots t)
  (do-claim-slot slots t #f))

;; returns (slot-id undo-info)
(define (claim-local slots t)
  (do-claim-slot slots t #t))

(define (unclaim-locals slots undos)
  (for-each (lambda (u)
	      (let* ((index (car u))
		     (slot  (cdr u)))
		(vector-set! slots index (cons slot (vector-ref slots index)))))
	    undos))

(define (get-slot-decls slots)
  (map (lambda (t)
	 `(local ,t))
       (reverse (tracker.defined (slots.tracker slots)))))

(define (do-claim-slot slots t record?)
  (let* ((index (slot-index-for-type t))
	 (spare (vector-ref slots index))
	 (slot  (if (not (null? spare))
		    (let ((number (car spare)))
		      (vector-set! slots index (cdr spare))
		      number)
		    (let* ((tracker (slots.tracker slots))
			   (number  (tracker.next tracker)))
		      (tracker.next-set! tracker (+ number 1))
		      (if record?
			  (tracker.defined-set! tracker (cons t (tracker.defined tracker))))
		      number))))
    (values slot (cons index slot))))

;;; Types

(define (parse-type cx t)
  (cond ((assq t (cx.type-names cx)) =>
	 cdr)
	((eq? t 'anyref)
	 '(ref %any%))
	((struct-ref-name? t)
	 (let* ((name (symbol->string t))
		(len  (string-length name)))
	   `(ref ,(string->symbol (substring name 1 len)))))
	(else
	 (fail "Bad type" t))))

(define (struct-ref-name? t)
  (let* ((name (symbol->string t))
	 (len  (string-length name)))
    (and (> len 1)
	 (char=? (string-ref name 0) #\*)
	 (struct-type? (string->symbol (substring name 1 len))))))

(define (parse-result-type cx t)
  (if (eq? t 'void)
      t
      (parse-type cx t)))

;;; Expressions

(define (expand-expr cx expr locals)
  (cond ((symbol? expr)
	 (expand-symbol cx expr locals))
	((number? expr)
	 (expand-number cx expr))
	((form? expr)
	 (expand-form cx expr locals))
	(else
	 (fail "Unknown expression" expr))))

(define (expand-symbol cx expr locals)
  (cond ((numbery-symbol? expr)
	 (expand-numbery-symbol cx expr))
	((eq? expr '???)
	 (values '(unreachable) 'void))
	((assq expr locals) =>
	 (lambda (x)
	   (let ((binding (cdr x)))
	     (values `(get_local ,(binding.slot binding)) (binding.type binding)))))
	((assq expr (cx.globals cx)) =>
	 (lambda (x)
	   (let ((global (cdr x)))
	     (values `(get_global ,(global.id global)) (global.type global)))))
	(else
	 (fail "Bad syntax" expr))))

(define min-i32 (- (expt 2 31)))
(define max-i32 (- (expt 2 31) 1))

(define (expand-constant-expr cx expr)
  (cond ((numbery-symbol? expr)
	 (expand-numbery-symbol cx expr))
	((number? expr)
	 (expand-number cx expr))
	(else ???)))

(define (expand-numbery-symbol cx expr)
  (let* ((name (symbol->string expr))
	 (val  (string->number (substring name 2 (string-length name)))))
    (case (string-ref name 0)
      ((#\I) (values `(i32.const ,val) 'i32))
      ((#\L) (values `(i64.const ,val) 'i64))
      ((#\F) (values `(f32.const ,val) 'f32))
      ((#\D) (values `(f64.const ,val) 'f64))
      (else ???))))

(define (expand-number cx expr)
  (cond ((and (integer? expr) (exact? expr))
	 (if (<= min-i32 expr max-i32)
	     (values `(i32.const ,expr) 'i32)
	     (values `(i64.const ,expr) 'i64)))
	((number? expr)
	 (values `(f64.const ,expr) 'f64))
	(else
	 (fail "Bad syntax" expr))))

(define (expand-form cx expr locals)
  (case (car expr)
    ((begin)     (expand-begin cx expr locals))
    ((if)        (expand-if cx expr locals))
    ((set!)      (expand-set! cx expr locals))
    ((inc! dec!) (expand-inc!dec! cx expr locals))
    ((let)       (expand-let cx expr locals))
    ((loop)      (expand-loop cx expr locals))
    ((break)     (expand-break cx expr locals))
    ((continue)  (expand-continue cx expr locals))
    ((while)     (expand-while cx expr locals))
    ((and)       (expand-and cx expr locals))
    ((or)        (expand-or cx expr locals))
    ((null)      (expand-null cx expr locals))
    ((new)       (expand-new cx expr locals))
    ((not)       (expand-not cx expr locals))
    ((zero?)     (expand-zero? cx expr locals))
    ((nonzero?)  (expand-nonzero? cx expr locals))
    ((clz ctz popcnt neg abs sqrt ceil floor nearest trunc extend8 extend16 extend32)
     (expand-unop cx expr locals))
    ((bitnot)    (expand-bitnot cx expr locals))
    ((null?)     (expand-null? cx expr locals))
    ((+ - * div divu mod modu bitand bitor bitxor shl shr shru rotl rotr max min copysign)
     (expand-binop cx expr locals))
    ((< <u <= <=u > >u >= >=u = !=)
     (expand-relop cx expr locals))
    ((i32->i64 u32->i64 i64->i32)
     (expand-conversion cx expr locals))
    (else
     (let ((op (car expr)))
       (cond ((field-accessor? op)   (expand-field-access cx expr locals))
	     ((struct-predicate? op) (expand-struct-predicate cx expr locals))
	     ((struct-cast? op)      (expand-struct-cast cx expr locals))
	     (else                   (expand-call cx expr locals)))))))

(define (expand-begin cx expr locals)
  (check-list-atleast expr 2 "Bad 'begin'" expr)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) locals)))
    (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
      (cond ((null? exprs)
	     (cond ((= (length body) 1)
		    (values (car body) ty))
		   ((eq? ty 'void)
		    (values `(block ,@(reverse body)) 'void))
		   (else
		    (values `(block ,ty ,@(reverse body)) ty))))
	    ((not (eq? ty 'void))
	     (loop exprs (cons 'drop body) 'void))
	    (else
	     (let-values (((e1 t1) (expand-expr cx (car exprs) locals)))
	       (loop (cdr exprs) (cons e1 body) t1)))))))

(define (expand-if cx expr locals)
  (check-list-oneof expr '(3 4) "Bad 'if'" expr)
  (let-values (((test t0) (expand-expr cx (cadr expr) locals)))
    (case (length expr)
      ((3)
       (let-values (((consequent t1) (expand-expr cx (caddr expr) locals)))
	 (values `(if ,test ,consequent) 'void)))
      ((4)
       (let*-values (((consequent t1) (expand-expr cx (caddr expr) locals))
		     ((alternate  t2) (expand-expr cx (cadddr expr) locals)))
	 (check-same-type t1 t2)
	 (values `(if ,t1 ,test ,consequent ,alternate) t1)))
      (else
       (fail "Bad 'if'" expr)))))

(define (expand-set! cx expr locals)
  (check-list expr 3 "Bad 'set!'" expr)
  (let* ((name (cadr expr))
	 (_    (check-lvalue cx name))
	 (val  (caddr expr)))
    (let-values (((e0 t0) (expand-expr cx val locals)))
      (let ((probe (assq name locals)))
	(if probe
	    (let ((binding (cdr probe)))
	      (values `(set_local ,(binding.slot binding) ,e0) 'void))
	    (let ((probe (assq name (cx.globals cx))))
	      (if probe
		  (let ((global (cdr probe)))
		    (values `(set_global ,(global.id global) ,e0) 'void))
		  (fail "No binding found for" name))))))))

(define (expand-inc!dec! cx expr locals)
  (check-list expr 2 "Bad inc/dec" expr)
  (let* ((op   (car expr))
	 (name (cadr expr))
	 (_    (check-lvalue cx name)))
    (let ((probe (assq name locals)))
      (if probe
	  (let* ((binding (cdr probe))
		 (type    (binding.type binding))
		 (slot    (binding.slot binding))
		 (one     (typed-constant type 1))
		 (op      (operatorize type (if (eq? op 'inc!) '+ '-))))
	    (values `(set_local ,slot (,op (get_local ,slot) ,one))
		    'void))
	  (let ((probe (assq name (cx.globals cx))))
	    (if probe
		(let* ((global (cdr probe))
		       (type   (global.type global))
		       (id     (global.id global))
		       (one    (typed-constant type 1))
		       (op     (operatorize type (if (eq? op 'inc!) '+ '-))))
		  (values `(set_global ,id (,op (get_global ,id) ,one))
			  'void))
		(fail "No binding found for" name)))))))

(define (expand-let cx expr locals)
  (check-list-atleast expr 3 "Bad 'let'" expr)
  (let* ((bindings (cadr expr))
	 (body     (cddr expr)))
    (let loop ((bindings bindings) (locals locals) (code '()) (undos '()))
      (if (null? bindings)
	  (let-values (((e0 t0) (expand-expr cx `(begin ,@body) locals)))
	    (unclaim-locals (cx.slots cx) undos)
	    (let ((type (if (not (eq? t0 'void)) (list t0) '())))
	      (if (not (null? code))
		  (if (and (pair? e0) (eq? (car e0) 'begin))
		      (values `(block ,@type ,@(reverse code) ,@(cdr e0)) t0)
		      (values `(block ,@type ,@(reverse code) ,e0) t0))
		  (values e0 t0))))
	  (let ((binding (car bindings)))
	    (check-list binding 2 "Bad binding" binding)
	    (let* ((name (car binding))
		   (_    (check-symbol name "Bad local name" name))
		   (init (cadr binding)))
	      (let*-values (((e0 t0)     (expand-expr cx init locals))
			    ((slot undo) (claim-local (cx.slots cx) t0)))
		(loop (cdr bindings)
		      (cons (cons name (make-binding name slot t0)) locals)
		      (cons `(set_local ,slot ,e0) code)
		      (cons undo undos)))))))))

(define (expand-loop cx expr locals)
  (check-list-atleast expr 3 "Bad 'loop'" expr)
  (let* ((id   (cadr expr))
	 (_    (check-symbol id "Bad loop id" id))
	 (body (cddr expr)))
    (call-with-loop cx id
		    (lambda (loop)
		      (let-values (((e0 t0) (expand-expr cx `(begin ,@body) locals)))
			(values `(block ,(loop.break loop) ,@(if (eq? (loop.type loop) 'void) '() (list (loop.type loop)))
					(loop ,(loop.continue loop)
					      ,e0
					      (br ,(loop.continue loop))))
				(loop.type loop)))))))

(define (expand-break cx expr locals)
  (check-list-oneof expr '(2 3) "Bad 'break'" expr)
  (let* ((id  (cadr expr))
	 (_   (check-symbol id "Bad loop id" id))
	 (e   (if (null? (cddr expr)) #f (caddr expr)))
	 (loop (lookup-loop cx id)))
    (if e
	(let-values (((e0 t0) (expand-expr cx e locals)))
	  (loop.record-type! loop t0)
	  (values `(br ,(loop.break loop) ,e0) 'void))
	(begin
	  (loop.record-type! loop 'void)
	  (values `(br ,(loop.break loop)) 'void)))))

(define (expand-continue cx expr locals)
  (check-list expr 2 "Bad 'continue'" expr)
  (let* ((id   (cadr expr))
	 (_    (check-symbol id "Bad loop id" id))
	 (loop (lookup-loop cx id)))
    (values `(br ,(loop.continue loop)) 'void)))

(define (expand-while cx expr locals)
  (check-list-atleast expr 2 "Bad 'while'" expr)
  (let* ((block-id (new-block-id cx "brk"))
	 (loop-id  (new-block-id cx "cnt")))
    (values `(block ,block-id
		    (loop ,loop-id
			  (br_if ,block-id (i32.eqz ,(expand-expr cx (cadr expr) locals)))
			  ,(let-values (((e0 t0) (expand-expr cx `(begin ,@(cddr expr)) locals)))
			     e0)
			  (br ,loop-id)))
	    'void)))

(define (expand-and cx expr locals)
  (check-list expr 3 "Bad 'and'" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		((op2 t2) (expand-expr cx (caddr expr) locals)))
    (values `(if i32 ,op1 ,op2 (i32.const 0)) 'i32)))

(define (expand-or cx expr locals)
  (check-list expr 3 "Bad 'or'" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		((op2 t2) (expand-expr cx (caddr expr) locals)))
    (values `(if i32 ,op1 (i32.const 1) ,op2) 'i32)))

(define (expand-zero? cx expr locals)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
    (values `(,(operatorize t1 '=) ,op1 ,(typed-constant t1 0)) 'i32)))

(define (expand-nonzero? cx expr locals)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
    (values `(,(operatorize t1 '!=) ,op1 ,(typed-constant t1 0)) 'i32)))

(define (expand-bitnot cx expr locals)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
    (values `(,(operatorize t1 'bitxor) ,op1 ,(typed-constant t1 -1)) t1)))

(define (expand-not cx expr locals)
  (check-list expr 2 "Bad 'not'" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
    (values `(i32.eqz ,op1) 'i32)))

(define (expand-unop cx expr locals)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
    (values `(,(operatorize t1 (car expr)) ,op1) t1)))

(define (expand-binop cx expr locals)
  (check-list expr 3 "Bad binary operator" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		((op2 t2) (expand-expr cx (caddr expr) locals)))
    (check-same-type t1 t2)
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) t1)))

(define (expand-relop cx expr locals)
  (check-list expr 3 "Bad binary operator" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		((op2 t2) (expand-expr cx (caddr expr) locals)))
    (check-same-type t1 t2)
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) 'i32)))

(define (expand-conversion cx expr locals)
  (check-list expr 2 "Bad conversion" expr)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) locals)))
    (case (car expr)
      ((i32->i64) (values `(i64.extend_s/i32 ,e0) 'i64))
      ((u32->i64) (values `(i64.extend_u/i32 ,e0) 'i64))
      ((i64->i32) (values `(i32.wrap/i64 ,e0) 'i32))
      (else ???))))

(define (expand-call cx expr locals)
  (let ((probe (assq (car expr) (cx.funcs cx))))
    (if (not probe)
	(fail "Not a function" name))
    (values `(call ,(func.id (cdr probe))
		   ,@(map (lambda (e)
			    (let-values (((new-e ty) (expand-expr cx e locals)))
			      new-e))
			  (cdr expr)))
	    (func.result (cdr probe)))))

(define (expand-null cx expr locals)
  (check-list-oneof expr '(1 2) "Bad null expression" expr)
  (if (null? (cdr expr))
      (values '(ref.null anyref) '(ref %any%))
      (let ((t (parse-struct-name cx (cadr expr))))
	(values `(ref.null ,t) `(ref ,(cadr expr))))))

(define (parse-struct-name cx name)
  ;; TODO: verify the type exists
  ;; Return anyref because we have no other types yet
  'anyref)

(define (expand-new cx expr locals)
  ;; length 2 or more
  ;; first operand must name struct type T
  ;; other operands must have types that match the field types of T in order, can automatically upcast
  ...)

(define (expand-null? cx expr locals)
  (check-list expr 2 "Bad null? expression" expr)
  (values `(ref.isnull ,(expand-expr cx (cadr expr) locals)) 'i32))

(define (expand-field-access cx expr locals)
  ;; length 2
  ;; type of operand is a (ref T), not anyref
  ;; the type T must have a field named by the accessor
  ...)

(define (expand-struct-predicate cx expr locals)
  ...)

(define (expand-struct-cast cx expr locals)
  ...)

(define (field-accessor? x)
  #f)

(define (struct-predicate? x)
  #f)

(define (struct-cast? x)
  #f)

(define (check-lvalue cx name)
  (symbol? name))

(define (make-loop id break-name continue-name type)
  (vector id break-name continue-name type))

(define (loop.id x) (vector-ref x 0))
(define (loop.break x) (vector-ref x 1))
(define (loop.continue x) (vector-ref x 2))
(define (loop.type x) (vector-ref x 3))
(define (loop.type-set! x v) (vector-set! x 3 v))

(define (loop.record-type! x t)
  (let ((current (loop.type x)))
    (cond ((not current)
	   (loop.type-set! x t))
	  ((eq? t current))
	  (else
	   (fail "Type mismatch for loop" (loop.id x))))))

(define (call-with-loop cx id fn)
  (let ((loop (make-loop id (new-block-id cx "brk") (new-block-id cx "cnt") #f)))
    (cx.loops-set! cx (cons (cons id loop) (cx.loops cx)))
    (let-values ((vs (fn loop)))
      (cx.loops-set! cx (cdr (cx.loops cx)))
      (apply values vs))))

(define (lookup-loop cx id)
  (let ((probe (assq id (cx.loops cx))))
    (if probe
	(cdr probe)
	(fail "Not an active loop" id))))

(define (operatorize t op)
  (string->symbol (string-append (symbol->string t) (op-name op))))

;; FIXME: some of these names are type-dependent; eg for "<=", it's "le_s" and
;; "le_u" for ints but just "le" for floats.  Presumably the same for "div".

(define (op-name x)
  (case x
    ((clz) ".clz")
    ((ctz) ".ctz")
    ((popcnt) ".popcnt")
    ((+) ".add")
    ((-) ".sub")
    ((*) ".mul")
    ((div) ".div_s")
    ((divu) ".div_u")
    ((mod) ".mod_s")
    ((modu) ".mod_u")
    ((<) ".lt_s")
    ((<u) ".lt_u")
    ((<=) ".le_s")
    ((<=u) ".le_u")
    ((>) ".gt_s")
    ((>u) ".gt_u")
    ((>=) ".ge_s")
    ((>=u) ".ge_u")
    ((=) ".eq")
    ((!=) ".ne")
    ((bitand) ".and")
    ((bitor) ".or")
    ((bitxor) ".xor")
    ((shl) ".shl")
    ((shr) ".shr_s")
    ((shru) ".shr_u")
    ((div) ".div_s")
    ((divu) ".div_u")
    ((mod) ".rem_s")
    ((modu) ".rem_u")
    ((rotl) ".rotl")
    ((rotr) ".rotr")
    ((neg) ".neg")
    ((abs) ".abs")
    ((min) ".min")
    ((max) ".max")
    ((sqrt) ".sqrt")
    ((ceil) ".ceil")
    ((floor) ".floor")
    ((nearest) ".nearest")
    ((trunc) ".trunc")
    ((extend8) ".extend8_s")
    ((extend16) ".extend16_s")
    ((extend32) ".extend32_s")
    (else ???)))

(define (typed-constant type value)
  `(,(string->symbol (string-append (symbol->string type) ".const")) ,value))

;;; Sundry

(define (form? expr)
  (and (list? expr) (not (null? expr))))

(define (numbery-symbol? x)
  (and (symbol? x)
       (let ((name (symbol->string x)))
	 (and (> (string-length name) 2)
	      (char=? #\. (string-ref name 1))
	      (memv (string-ref name 0) '(#\I #\L #\F #\D))))))

(define (check-same-type t1 t2)
  (if (not (eq? t1 t2))
      (fail "Not same type" t1 t2)))

(define (check-head x k)
  (if (not (eq? (car x) k))
      (fail "Expected keyword" k "but saw" (car x))))

(define (check-list-atleast l n . rest)
  (if (or (not (list? l)) (not (>= (length l) n)))
      (if (not (null? rest))
	  (apply fail rest)
	  (fail "List of insufficient length, need at least" n " " l))))

(define (check-list l n . rest)
  (if (or (not (list? l)) (not (= (length l) n)))
      (if (not (null? rest))
	  (apply fail rest)
	  (fail "List of wrong length" l))))

(define (check-list-oneof l ns . rest)
  (if (not (and (list? l) (memv (length l) ns)))
      (if (not (null? rest))
	  (apply fail rest)
	  (fail "List of wrong length" l))))

(define (check-symbol x . rest)
  (if (not (symbol? x))
      (if (not (null? rest))
	  (apply fail rest)
	  (fail "Expected a symbol but got" x))))

(define (check-constant x . rest)
  (if (not (or (number? x) (numbery-symbol? x)))
      (if (not (null? rest))
	  (apply fail rest)
	  (fail "Expected a constant number but got" x))))
	       
(define (handle-failure thunk)
  (call-with-current-continuation
   (lambda (k)
     (set! *leave* k)
     (let ((result (thunk)))
       (set! *leave* #f)
       result))))

(define *leave* #f)

(define (fail msg . irritants)
  (display "FAIL: ")
  (display msg)
  (for-each (lambda (x)
	      (display " ")
	      (display x))
	    irritants)
  (newline)
  (if *leave*
      (*leave*)
      (error "FAILED!")))

;;; Driver for scripts

(define (swat-noninteractive)
  (define js-mode #f)
  (define stdout-mode #f)
  (define files '())
  (handle-failure
   (lambda ()
     (let loop ((args (cdr (vector->list (command-line-arguments)))))
       (if (not (null? args))
	   (let* ((arg (car args))
		  (len (string-length arg)))
	     (cond ((or (string=? arg "--js") (string=? arg "-j"))
		    (set! js-mode #t)
		    (loop (cdr args)))
		   ((or (string=? arg "--stdout") (string=? arg "-s"))
		    (set! stdout-mode #t)
		    (loop (cdr args)))
		   ((or (string=? arg "--help") (string=? arg "-h"))
		    (display "Usage: swat options file ...") (newline)
		    (display "Options:") (newline)
		    (display "  -j  --js      Generate .wast.js") (newline)
		    (display "  -s  --stdout  Print output to stdout") (newline)
		    (exit 0))
		   ((and (> len 0) (char=? (string-ref arg 0) #\-))
		    (fail "Bad option" arg "  Try --help"))
		   ((and (> len 5) (string=? (substring arg (- len 5) len) ".swat"))
		    (set! files (cons arg files))
		    (loop (cdr args)))
		   (else
		    (fail "Bad file name" arg))))))
     (for-each (lambda (filename)
		 (call-with-input-file filename
		   (lambda (in)
		     (if stdout-mode
			 (process-file in (current-output-port) js-mode)
			 (call-with-output-file (input-name->output-name filename js-mode)
			   (lambda (out)
			     (process-file in out js-mode)))))))
	       files))))

(define (process-file in out js-mode)
  (do ((phrase (read in) (read in)))
      ((eof-object? phrase))
    (cond ((and (pair? phrase) (eq? (car phrase) 'defmodule))
	   (let-values (((name code) (expand-module phrase)))
	     (write-module out name code js-mode)))
	  ((and (pair? phrase) (eq? (car phrase) 'js))
	   (write-js out (cadr phrase) js-mode))
	  (else
	   (fail "Bad toplevel phrase" phrase)))))

(define (input-name->output-name filename js-mode)
  (if js-mode
      (string-append (substring filename 0 (- (string-length filename) 5)) ".wast.js") 
      (string-append (substring filename 0 (- (string-length filename) 5)) ".wast")))

(define (write-module out name code js-mode)
  (if js-mode
      (begin
	(display (string-append "var " name " = new WebAssembly.Module(wasmTextToBinary(`") out)
	(newline out))
      (begin
	(display (string-append ";; " name) out)
	(newline out)))
  (pretty-print code out)
  (if js-mode
      (begin
	(display "`));" out)
	(newline out))))

(define (write-js out code js-mode)
  (if js-mode
      (begin
	(display code out)
	(newline out))))

;;; Driver for testing and interactive use

(define (swat filename)
  (handle-failure
   (lambda ()
     (call-with-input-file filename
       (lambda (f)
	 (let ((phrase (read f)))
	   (let-values (((name code) (expand-module phrase)))
	     (display (string-append ";; " name))
	     (newline)
	     (pretty-print code))))))))

;;; TODO for v1

;;;   - More test cases!!
;;;   - conversion ops - lots of these with subtle semantic differences, need good naming.
;;;     probably nontrapping should be default and trapping should get more complicated names?
;;;                f32->i32 | f32->u32 | f64->i32 | f64->u32 | i64->i32 | f32->i64 | f32->u64 |
;;;                f64->i64 | f64->u64 | i32->i64 | u32->i64 | f32-bits | f64-bits | ...
;;;   - select, (select e A B)
;;;   - Switch/Case, (case E ((c0 c1 ...) E ...) ... (else ...))
;;;     Importantly, non-imported constant globals can be referenced as the c ...
;;;   - return statement?  Not very scheme-y...
;;;
;;; TODO for v2
;;;   - Structs and references, see wabbit.swat and vfunc.swat
;;;   - Some sort of vtable thing, see notes elsewhere
;;;
;;; TODO (whenever)
;;;   - allow certain global references as inits to locally defined globals
;;;   - for defconst and defvar with initializer, the type should be inferred
;;;   - cond-like macro
;;;   - let*
;;;   - block optimization pass on function body:
;;;      - strip unlabeled blocks inside other blocks everywhere,
;;;        including implicit outermost block.
;;;      - remove whatever ad-hoc solutions we have now
;;;   - max/min/abs are only defined on floating types now, we could synthesize them
;;;     for integer types
;;;   - Multi-arity when it makes sense (notably + - * relationals and or bit(and,or,xor))
;;;   - tee in some form?  (tee! ...)  Or just make this the set! semantics and then
;;;     lower to set_local/set_global if result is discarded?
;;;   - Memories + flat memory??  Do we really care? Maybe call ops <-i8 and ->i8, etc
;;;   - Better syntax checking + errors
;;;   - Produce wabt-compatible output
;;;   - Allow imports from arbitrary module names by adopting eg (func- mod:fn ...) syntax
;;;   - Multiple-value return:  -> (t0 t1 ...) return type annotation; LET-VALUES or
;;;     destructuring in LET; VALUES; maybe some way of applying / splatting a captured
;;;     list of values (long tail).  Until we actually have these in wasm we can simulate
;;;     using objects or globals or flat memory (though not for references)
;;;   - poor man's enums:
;;;       (defenum TyTy I32 F64 Ref)
;;;     is exactly equivalent to
;;;       (defconst TyTy.I32 0)
;;;       (defconst TyTy.F64 1)
;;;       (defconst TyTy.Ref 2)
;;;       (defalias TyTy i32)
;;;   - (defalias bool i32)
