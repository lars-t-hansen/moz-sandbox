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
;;;   - Loop, Break, Continue
;;;   - Global variables (imports, local, exports)
;;;   - Need either a 'drop' we can use to ignore results of function calls or
;;;     some inference of drops when values are not needed in statement blocks
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
;;;   - return statement?  Not very scheme-y...
;;;   - Memories + flat memory??
;;;   - Better syntax checking + errors
;;;   - Produce wabt-compatible output
;;;   - Allow imports from arbitrary module names by adopting eg (func- mod:fn ...) syntax
;;;   - Make sure this works in eg SCM and other r4rs-ish implementations (guile?) (petite chez?),
;;;     anything we can reasonably apt-get or install from an rpm or something, i guess larceny qualifies
;;;     - Note SCM does not have let-values... maybe srfi support?  it does have values + call-with-values
;;;     - can 'require cond-expand
;;;   - Make runnable at shell prompt (swat filename.swat produces filename.wast or maybe even a JS equivalent)
;;;   - ditto as stdin -> stdout processor

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
;;;
;;; During translation all these lists are in reverse order, newest
;;; element first.

(define (make-cx)
  (vector #f				; Slots storage (during body expansion)
	  '()  				; Globals  ((name . global) ...)
	  '()				; Structs  ((name . struct) ...)
	  '()				; Funcs:   ((name . func) ...)
	  0				; Next function ID
	  '()))				; Names    (name ...)

(define (cx.slots cx)          (vector-ref cx 0))
(define (cx.slots-set! cx v)   (vector-set! cx 0 v))
(define (cx.globals cx)        (vector-ref cx 1))
(define (cx.structs cx)        (vector-ref cx 2))
(define (cx.funcs cx)          (vector-ref cx 3))
(define (cx.funcs-set! cx v)   (vector-set! cx 3 v))
(define (cx.func-id cx)        (vector-ref cx 4))
(define (cx.func-id-set! cx v) (vector-set! cx 4 v))
(define (cx.names cx)          (vector-ref cx 5))
(define (cx.names-set! cx v)   (vector-set! cx 5 v))

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

(define (define-function cx name import? export? params result-type slots locals)
  (let* ((id   (cx.func-id cx))
	 (func (make-func name import? export? id params result-type slots locals)))
    (cx.func-id-set! cx (+ id 1))
    (cx.funcs-set!   cx (cons (cons name func) (cx.funcs cx)))
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
	 (body      (cddr f))
	 (slots     (make-slots)))
    (if (and import? (not (null? body)))
	(fail "Import function can't have a body" f))
    (define-name! cx name)
    (cx.slots-set! cx #f)
    (let loop ((xs     (cdr signature))
	       (locals '())
	       (params '()))
      (cond ((null? xs)
	     (define-function cx name import? export? (reverse params) 'void slots locals))

	    ((eq? (car xs) '->)
	     (check-list xs 2 "Bad signature" signature)
	     (let ((t (parse-result-type (cadr xs))))
	       (define-function cx name import? export? (reverse params) t slots locals)))

	    (else
	     (let ((first (car xs)))
	       (if (not (and (list? first) (= 2 (length first)) (symbol? (car first))))
		   (fail "Bad parameter" first))
	       (let ((t    (parse-type (cadr first)))
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
	 (export?   (eq? (car f) 'func+))
	 (import?   (eq? (car f) 'func-))
	 (body      (cddr f)))
    (let* ((func   (cdr (assq name (cx.funcs cx))))
	   (locals (func.locals func))
	   (slots  (func.slots func)))
      (cx.slots-set! cx slots)
      (assemble-function func (expand-body cx body (func.result func) locals)))))

(define (expand-body cx body expected-type locals)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) locals)))
    (let ((drop? (and (eq? expected-type 'void)
		      (not (eq? result-type 'void)))))
      `(,@(get-slot-decls (cx.slots cx)) ,expanded ,@(if drop? '(drop) '())))))

;;; Local and global bindings

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

(define (expand-expr cx expr locals)
  (cond ((symbol? expr)
	 (expand-symbol cx expr locals))
	((number? expr)
	 (expand-number cx expr locals))
	((form? expr)
	 (expand-form cx expr locals))
	(else
	 (fail "Unknown expression" expr))))

(define (expand-symbol cx expr locals)
  (let ((name (symbol->string expr)))
    (cond ((and (> (string-length name) 2)
		(char=? #\. (string-ref name 1))
		(memv (string-ref name 0) '(#\I #\L #\F #\D)))
	   (let ((val (string->number (substring name 2 (string-length name)))))
	     (case (string-ref name 0)
	       ((#\I) (values `(i32.const ,val) 'i32))
	       ((#\L) (values `(i64.const ,val) 'i64))
	       ((#\F) (values `(f32.const ,val) 'f32))
	       ((#\D) (values `(f64.const ,val) 'f64)))))
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

(define (expand-number cx expr locals)
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
    ((begin)
     (check-list-atleast expr 2 "Bad 'begin'" expr)
     (let-values (((e0 t0) (expand-expr cx (cadr expr) locals)))
       (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
	 (if (null? exprs)
	     (if (= (length body) 1)
		 (values (car body) ty)
		 (values `(block ,ty ,@(reverse body)) ty))
	     (let-values (((e1 t1) (expand-expr cx (car exprs) locals)))
	       (loop (cdr exprs) (cons e1 body) t1))))))

    ((if)
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

    ((let)
     ;; FIXME: free locals when unused
     (check-list-atleast expr 3 "Bad 'let'" expr)
     (let* ((bindings (cadr expr))
	    (body     (cddr expr)))
       (let loop ((bindings bindings) (locals locals) (code '()) (undos '()))
	 (if (null? bindings)
	     (let-values (((e0 t0) (expand-expr cx `(begin ,@body) locals)))
	       (unclaim-locals (cx.slots cx) undos)
	       (if (not (null? code))
		   (if (and (pair? e0) (eq? (car e0) 'begin))
		       (values `(begin ,@(reverse code) ,@(cdr e0)) t0)
		       (values `(begin ,@(reverse code) ,e0) t0))
		   (values e0 t0)))
	     (let ((binding (car bindings)))
	       (check-list-oneof binding '(1 2) "Bad binding" binding)
	       (let ((decl (car binding))
		     (init (if (null? (cdr binding)) #f (cadr binding))))
		 (check-list decl 2 "Bad decl" decl)
		 (let* ((name (car decl))
			(_    (check-symbol name "Bad local name" name))
			(type (parse-type (cadr decl))))
		   ;; Note, can't avoid initializing a slot without an
		   ;; initializer because of slot reuse.
		   (let-values (((slot undo) (claim-local (cx.slots cx) type)))
		     (loop (cdr bindings)
			   (cons (cons name (make-binding name slot type)) locals)
			   (cons `(set_local ,slot
					     ,(if init 
						  (expand-expr cx init locals)
						  (typed-zero type)))
				 code)
			   (cons undo undos))))))))))

    ((loop)
     ...)

    ((break)
     ...)

    ((continue)
     ...)

    ((and)
     (check-list expr 3 "Bad 'and'" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		   ((op2 t2) (expand-expr cx (caddr expr) locals)))
       (values `(if i32 ,op1 ,op2 (i32.const 0)) 'i32)))

    ((or)
     (check-list expr 3 "Bad 'or'" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		   ((op2 t2) (expand-expr cx (caddr expr) locals)))
       (values `(if i32 (i32.eqz ,op1) (i32.const 0) ,op2) 'i32)))

    ((while)
     (check-list-atleast expr 2 "Bad 'while'" expr)
     (let* ((block-id (new-block-id cx))
	    (loop-id (new-block-id cx)))
       (values `(block ,block-id
		       (loop ,loop-id
			     (br_if ,block-id ,(expand-expr cx (cadr expr) locals))
			     ,@(map (lambda (e)
				      (expand-expr cx e locals))
				    (cddr expr))
			     (br ,loop-id)))
	       'void)))

    ((not)
     (check-list expr 2 "Bad 'not'" expr)
     (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
       (values `(i32.eqz ,op1) 'i32)))

    ((clz ctz popcnt)
     (check-list expr 2 "Bad unary operator" expr)
     (let-values (((op1 t1) (expand-expr cx (cadr expr) locals)))
       (values `(,(operatorize t1 (car expr)) ,op1) t1)))

    ((+ - * div divu mod modu < <u <= <=u > >u >= >=u = != bitand bitor bitxor shl shr shru rotl rotr)
     (check-list expr 3 "Bad binary operator" expr)
     (let*-values (((op1 t1) (expand-expr cx (cadr expr) locals))
		   ((op2 t2) (expand-expr cx (caddr expr) locals)))
       (check-same-type t1 t2)
       (values `(,(operatorize t1 (car expr)) ,op1 ,op2) t1)))

    (else
     (let* ((name  (car expr))
	    (probe (assq name (cx.funcs cx))))
       (if (not probe)
	   (fail "Not a function" name))
       (values `(call ,(func.id (cdr probe))
		      ,@(map (lambda (e)
			       (let-values (((new-e ty) (expand-expr cx e locals)))
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

(define (typed-zero type)
  `(,(string->symbol (string-append (symbol->string type) ".const")) 0))

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
