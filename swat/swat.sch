;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at <http://mozilla.org/MPL/2.0/>.


;;; Swat is an evolving Scheme-syntaxed WebAssembly superstructure.
;;;
;;; This program translates swat programs to WebAssembly text format
;;; (the format accepted by Firefox's wasmTextToBinary, not
;;; necessarily wabt at this point).
;;;
;;; The goal is to offer a reasonable superstructure, but not to be
;;; able to express everything.  Notably swat has an expression
;;; discipline where wasm has a less structured stack discipline.

;;; TODO for v1
;;;   - Test cases
;;;   - Let
;;;   - Loop, Break, Continue
;;;   - Global variables (imports, local, exports)
;;;
;;; TODO for v2
;;;   - Structs and references
;;;   - Some sort of vtable thing
;;;
;;; TODO (whenever)
;;;   - select
;;;   - zero? nonzero?  [type change semantics as for conversions]
;;;   - conversion ops (i32->i64, etc)
;;;   - sign extension ops
;;;   - any other missing int operations
;;;   - any other missing floating point operations
;;;   - Multi-arity when it makes sense (notably + - * relationals and or bit(and,or,xor))
;;;   - tee in some form?  (tee! ...)  Or just make this the set! semantics and then
;;;     lower to set_local/set_global if result is discarded?
;;;   - Switch/Case
;;;   - Memories + flat memory??
;;;   - Better syntax checking + errors
;;;   - Produce wabt-compatible output
;;;   - Allow imports from arbitrary module names by adopting eg (func- mod:fn ...) syntax
;;;   - Make sure this works in eg SCM and other r4rs-ish implementations

;;; BNF format: lower-case words and parens are literal.  "..."
;;; denotes zero or more.  Vertical bars denote alternatives.  Symbols
;;; follow Scheme syntactic rules, ie, can contain most punctuation.
;;;
;;; Module     ::= (module Func ...)
;;; Func       ::= (Func-Kwd Signature Expr ...)
;;; Func-Kwd   ::= func | func+ | func-
;;; Signature  ::= (Id Decl ...) | (Id Decl ... -> ReturnType)
;;; Decl       ::= (Id Type)
;;; Type       ::= i32 | i64 | f32 | f64
;;; ReturnType ::= Type | void
;;; Expr       ::= Begin | If | Set | Let | Loop | Break | Continue | While | And | Or | Operator | Call | Id | Number
;;; Begin      ::= (begin Expr Expr ...)
;;; If         ::= (if Expr Expr) | (if Expr Expr Expr)
;;; Set        ::= (set! Id Expr)
;;; Let        ::= (let ((Decl Expr) ...) Expr Expr ...)
;;; Loop       ::= (loop Id Expr ...)
;;; Break      ::= (break Id)
;;; Continue   ::= (continue Id)
;;; While      ::= (while Expr Expr ...)
;;; And        ::= (and Expr Expr)
;;; Or         ::= (or Expr Expr)
;;; Operator   ::= (Op Expr ...)
;;; Op         ::= + | - | * | div | divu | mod | modu | < | <= | > | >= | = | != | <u | <=u | >u | >=u |
;;;                not | bitand | bitor | bitxor | shl | shr | shru | rotl | rotr | clz | ctz | popcnt
;;; Call       ::= (Id Expr ...)
;;; Number     ::= SchemeIntegerLiteral | SchemeFloatingLiteral | Prefixed
;;; Prefixed   ::= A symbol comprising the letters "I.", "L.", "F.", or
;;;                "D." followed by characters that can be parsed as
;;;                integer values (for I and L) or floating values
;;;                (for F and D).  I denotes i32, L denotes i64, F
;;;                denotes f32, D denotes f64.  So F.3.1415e-2 is the
;;;                f32 representing approximately Pi / 100.
;;;
;;; Syntactic constraints:
;;;
;;; - Function names can't be duplicated
;;; - Param names can't be duplicated
;;; - A return type can also be omitted to specify 'void'
;;; - A func- does not have a body
;;; - A SchemeIntegerLiteral on its own denotes an i32 if it is small enough,
;;;   otherwise i64.
;;; - A SchemeFloatingLiteral on its own denotes an f64
;;; - The init expression in "let" (the one paired with each Decl) can be
;;;   omitted, it defaults to zero of the appropriate type
;;; - break and continue must name an enclosing loop
;;;
;;; Sundry semantics:
;;;
;;; - Functions can be present in arbitrary order.
;;; - "func+" denotes an export, with the given name.
;;; - "func-" denotes an import, with the given name, from the "" module.
;;; - Types are inferred bottom-up, in order to synthesize types for
;;;   operators and blocks.
;;; - Types are checked when it matters to us but otherwise we just pass
;;;   things through and hope the next step takes care of it.
;;; - "let" bindings are as for let* in Scheme: left-to-right, with each
;;;   previous binding in scope when the next decl is processed.

(define (swat filename)
  (call-with-current-continuation
   (lambda (k)
     (set! *leave* k)
     (call-with-input-file filename
       (lambda (f)
	 (let ((phrase (read f)))
	   (pretty-print (expand-module phrase))))))))

;;; Translation context

(define (make-cx)
  (vector 0				; Next local index
	  '()  				; Globals  ((name . global) ...)
	  '()				; Structs  ((name . struct) ...)
	  '()				; Funcs:   ((name . func) ...)
	  0				; Next function index
	  '()))				; Names    (name ...)
	  
(define (cx.local-slot cx)       (vector-ref cx 0))
(define (cx.globals cx)          (vector-ref cx 1))
(define (cx.structs cx)          (vector-ref cx 2))
(define (cx.funcs cx)            (vector-ref cx 3))
(define (cx.funcs-set! cx v)     (vector-set! cx 3 v))
(define (cx.func-slot cx)        (vector-ref cx 4))
(define (cx.func-slot-set! cx v) (vector-set! cx 4 v))
(define (cx.names cx)            (vector-ref cx 5))
(define (cx.names-set! cx v)     (vector-set! cx 5 v))

;;; Modules

;;; To handle imports and mutual references we must do two prepasses
;;; over the module to define functions in the table.  The first
;;; handles imports, which are given the lowest indices.  The second
;;; handles the others.  Then the third phase expands bodies.

(define (expand-module m)
  (check-list-atleast m 1 "Bad module" m)
  (check-head m 'module)
  (let ((cx (make-cx)))
    (for-each (lambda (d)
		(check-list-atleast d 1 "Bad top-level phrase" d)
		(case (car d)
		  ((func-)
		   (expand-func-phase1 cx d))
		  ((func func+) #t)
		  (else
		   (fail "Unknown top-level phrase" d))))
	      (cdr m))
    (for-each (lambda (d)
		(case (car d)
		  ((func func+)
		   (expand-func-phase1 cx d))))
	      (cdr m))
    (for-each (lambda (d)
		(case (car d)
		  ((func func+)
		   (expand-func-phase2 cx d))))
	      (cdr m))
    (cons 'module
	  (map (lambda (x)
		 (let ((func (cdr x)))
		   (if (func.import? func)
		       `(import "" ,(symbol->string (func.name func))
				,(assemble-function func '()))
		       (func.defn func))))
	       (reverse (cx.funcs cx))))))

;;; Functions

(define (make-func name import? export? slot params result defn)
  (vector name import? export? slot params result defn))

(define (func.name f) (vector-ref f 0))
(define (func.import? f) (vector-ref f 1))
(define (func.export? f) (vector-ref f 2))
(define (func.slot f) (vector-ref f 3))
(define (func.params f) (vector-ref f 4))
(define (func.result f) (vector-ref f 5))
(define (func.defn f) (vector-ref f 6))
(define (func.defn-set! f v) (vector-set! f 6 v))

(define (define-name! cx name)
  (if (memq name (cx.names cx))
      (fail "Duplicate global name" name))
  (cx.names-set! cx (cons name (cx.names cx))))

(define (define-function cx name import? export? params result-type stuff)
  (let* ((slot (cx.func-slot cx))
	 (func (make-func name import? export? slot params result-type stuff)))
    (cx.func-slot-set! cx (+ slot 1))
    (cx.funcs-set!     cx (cons (cons name func) (cx.funcs cx)))
    func))

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
	 (export?   (eq? (car f) 'func+))
	 (import?   (eq? (car f) 'func-))
	 (body      (cddr f)))
    (if (and import? (not (null? body)))
	(fail "Import function can't have a body" f))
    (define-name! cx name)
    (let loop ((xs     (cdr signature))
	       (locals '())
	       (slots  (make-slots))
	       (params '()))
      (cond ((null? xs)
	     (define-function cx name import? export? (reverse params) 'void (cons locals slots)))

	    ((eq? (car xs) '->)
	     (check-list xs 2 "Bad signature" signature)
	     (let ((t (parse-result-type (cadr xs))))
	       (define-function cx name import? export? (reverse params) t (cons locals slots))))

	    (else
	     (let ((first (car xs)))
	       (if (not (and (list? first) (= 2 (length first)) (symbol? (car first))))
		   (fail "Bad parameter" first))
	       (let ((t    (parse-type (cadr first)))
		     (name (car first)))
		 (if (assq name locals)
		     (fail "Duplicate parameter" name))
		 (let-values (((slots slot) (claim-param slots t)))
		   (loop (cdr xs)
			 (cons (cons name (make-binding name slot t)) locals)
			 slots
			 (cons `(param ,t) params))))))))))

(define (expand-func-phase2 cx f)
  (let* ((signature (cadr f))
	 (name      (car signature))
	 (export?   (eq? (car f) 'func+))
	 (import?   (eq? (car f) 'func-))
	 (body      (cddr f)))
    (let* ((func   (cdr (assq name (cx.funcs cx))))
	   (locals (car (func.defn func)))
	   (slots  (cdr (func.defn func))))
      (assemble-function func (expand-body cx body (func.result func) locals slots)))))

(define (expand-body cx body expected-type locals slots)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) locals slots)))
    `(,@(get-slot-decls slots) ,expanded ,@(if (eq? expected-type 'void) '(drop) '()))))

;;; Locals and globals

(define (make-binding name slot type) (vector name slot type))
(define (binding.name x) (vector-ref x 0))
(define (binding.slot x) (vector-ref x 1))
(define (binding.type x) (vector-ref x 2))

(define (make-slots)
  (vector '() '() '() '() (list 0 '())))

(define *slots-i32* 0)
(define *slots-i64* 1)
(define *slots-f32* 2)
(define *slots-f64* 3)
(define *slots-tracker* 4)

(define (claim-param slots t)
  (do-claim-slot slots t #f))

(define (claim-local slots t)
  (do-claim-slot slots t #t))

(define (get-slot-decls slots)
  (reverse (cadr (vector-ref slots *slots-tracker*))))

(define (do-claim-slot slots t record?)
  (let ((index (case t
		 ((i32) *slots-i32*)
		 ((i64) *slots-i64*)
		 ((f32) *slots-f32*)
		 ((f64) *slots-f64*)
		 (else (fail "Bad type" t)))))
    (let ((spare (vector-ref slots index)))
      (if (not (null? spare))
	  (let ((number (car spare))
		(new    (vector-copy slots)))
	    (vector-set! new *index* (cdr spare))
	    (values new number))
	  (let* ((tracker (vector-ref slots *slots-tracker*))
		 (number  (car tracker))
		 (defn    `(local ,t)))
	    (set-car! tracker (+ number 1))
	    (if record?
		(set-car! (cdr tracker) (cons defn (cadr tracker))))
	    (values slots number))))))

;;; Types

(define (parse-type t)
  (case t
    ((i32 i64 f32 f64) t)
    (else
     (fail "Bad type" t))))

(define (parse-result-type t)
  (if (eq? t 'void)
      t
      (parse-type t)))

;;; Expressions

(define (expand-expr cx expr locals slots)
  (cond ((symbol? expr)
	 (expand-symbol cx expr locals slots))
	((number? expr)
	 (expand-number cx expr locals slots))
	((form? expr)
	 (expand-form cx expr locals slots))
	(else
	 (fail "Unknown expression" expr))))

(define (expand-symbol cx expr locals slots)
  (let ((name (symbol->string expr)))
    (cond ((and (> (string-length name) 2)
		(char=? #\. (string-ref name 1))
		(memv (string-ref name 0) '(#\I #\L #\F #\D)))
	   (let ((val (string->number (substring name 2 (string-length name)))))
	     (case (string-ref name 0)
	       ((#\i) (values `(i32.const ,val) 'i32))
	       ((#\l) (values `(i64.const ,val) 'i64))
	       ((#\f) (values `(f32.const ,val) 'f32))
	       ((#\d) (values `(f64.const ,val) 'f64)))))
	  ((assq expr locals) =>
	   (lambda (x)
	     (let ((binding (cdr x)))
	       (values `(get_local ,(binding.slot binding)) (binding.type binding)))))
	  ((assq expr (cx.globals cx)) =>
	   (lambda (x)
	     (let ((binding (cdr x)))
	       (values `(get_global ,(binding.slot binding)) (binding-type binding)))))
	  (else
	   (fail "Bad syntax" expr)))))

(define min-i32 (- (expt 2 31)))
(define max-i32 (- (expt 2 31) 1))

(define (expand-number cx expr locals slots)
  (cond ((and (integer? expr) (exact? expr))
	 (if (<= min-i32 expr max-i32)
	     (values `(i32.const ,expr) 'i32)
	     (values `(i64.const ,expr) 'i64)))
	((number? expr)
	 (values `(f64.const ,expr) 'f64))
	(else
	 (fail "Bad syntax" expr))))

(define (expand-form cx expr locals slots)
  (case (car expr)
    ((begin)
     (check-list-atleast expr 2 "Bad 'begin'" expr)
     (let-values (((e0 t0) (expand-expr cx (cadr expr) locals slots)))
       (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
	 (if (null? exprs)
	     (if (= (length body) 1)
		 (values (car body) ty)
		 (values `(block ,ty ,@(reverse body)) ty))
	     (let-values (((e1 t1) (expand-expr cx (car exprs) locals slots)))
	       (loop (cdr exprs) (cons e1 body) t1))))))

    ((if)
     (check-list-oneof expr '(3 4) "Bad 'if'" expr)
     (let-values (((test t0) (expand-expr cx (cadr expr) locals slots)))
       (case (length expr)
	 ((3)
	  (let-values (((consequent t1) (expand-expr cx (caddr expr) locals slots)))
	    (values `(if ,test ,consequent) 'void)))
	 ((4)
	  (let*-values (((consequent t1) (expand-expr cx (caddr expr) locals slots))
			((alternate  t2) (expand-expr cx (cadddr expr) locals slots)))
	    (check-same-type t1 t2)
	    (values `(if ,t1 ,test ,consequent ,alternate) t1)))
	 (else
	  (fail "Bad 'if'" expr)))))

    ((set!)
     (check-list expr 3 "Bad 'set!'" expr)
     (let ((name (cadr expr)))
       (let-values (((e0 t0) (expand-expr cx (caddr expr) locals)))
	 (let ((probe (assq name locals)))
	   (if probe
	       (values `(set_local ,(binding.slot probe) ,e0) 'void))
	   (let ((probe (assq name (cx.globals cx))))
	     (if probe
		 (values `(set_global ,(binding.slot probe) ,e0) 'void)
		 (fail "No binding found for" name)))))))

    ((let*)
     ;; (let* (((id type) init) ...) expr ...)
     ;; where init can be omitted and defaults to 0 of the appropriate type
     (...))

    ((loop)
     ...)

    ((break)
     ...)

    ((continue)
     ...)

    ((and)
     (check-list expr 3 "Bad 'and'" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals slots))
		   ((op2 t2) (expand-expr cx (caddr expr) locals slots)))
       (values `(if i32 ,op1 ,op2 (i32.const 0)) 'i32)))

    ((or)
     (check-list expr 3 "Bad 'or'" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals slots))
		   ((op2 t2) (expand-expr cx (caddr expr) locals slots)))
       (values `(if i32 (i32.eqz ,op1) (i32.const 0) ,op2) 'i32)))

    ((while)
     (check-list-atleast expr 2 "Bad 'while'" expr)
     (let* ((block-id (new-block-id cx))
	    (loop-id (new-block-id cx)))
       (values `(block ,block-id
		       (loop ,loop-id
			     (br_if ,block-id ,(expand-expr cx (cadr expr) locals slots))
			     ,@(map (lambda (e)
				      (expand-expr cx e locals slots))
				    (cddr expr))
			     (br ,loop-id)))
	       'void)))

    ((not)
     (check-list expr 2 "Bad 'not'" expr)
     (let-values (((op1 t1) (expand-expr cx (cadr expr) locals slots)))
       (values `(i32.eqz ,op1) 'i32)))

    ((zero? nonzero? i32->i64 i64->i32)	; and more
     ...)

    ((clz ctz popcnt)
     (check-list expr 2 "Bad unary operator" expr)
     (let-values (((op1 t1) (expand-expr cx (cadr expr) locals slots)))
       (values `(,(operatorize t1 (car expr)) ,op1) t1)))

    ((+ - * div divu mod modu < <u <= <=u > >u >= >=u = != bitand bitor bitxor shl shr shru rotl rotr)
     (check-list expr 3 "Bad binary operator" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals slots))
		   ((op2 t2) (expand-expr cx (caddr expr) locals slots)))
       (check-same-type t1 t2)
       (values `(,(operatorize t1 (car expr)) ,op1 ,op2) t1)))

    (else
     (let* ((name  (car expr))
	    (probe (assq name (cx.funcs cx))))
       (if (not probe)
	   (fail "Not a function" name))
       (values `(call ,(func.slot (cdr probe))
		      ,@(map (lambda (e)
			       (let-values (((new-e ty) (expand-expr cx e locals slots)))
				 new-e))
			     (cdr expr)))
	       (func.result (cdr probe)))))))

(define (operatorize t op)
  (string->symbol (string-append (symbol->string t) (op-name op))))

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
    (else ???)))

;;; Sundry

(define (form? expr)
  (and (list? expr) (not (null? expr))))

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

(define *leave* #f)

(define (fail msg . irritants)
  (display "FAIL: ")
  (display msg)
  (for-each (lambda (x)
	      (display " ")
	      (display x))
	    irritants)
  (newline)
  (*leave*))
