;;; -*- fill-column: 80 -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public License,
;;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;;; obtain one at <http://mozilla.org/MPL/2.0/>.
;;;
;;; This is r5rs-ish Scheme, it works with Larceny (http://larcenists.org).

;;; Working on:
;;;
;;;  - cleaning up environment so that we can represent getters, structs
;;;  - search for FIXME
;;;  - locals -> env
;;;  - env also holds globals and all expanders
;;;  - env can hold loops
;;;  - need denotation data types for all the things
;;;  - need to fix several places where we previously created / extended local env:
;;;    params, let, maybe more

;;; Swat is an evolving Scheme/Lisp-syntaxed WebAssembly superstructure.
;;;
;;; The goal is to offer a reasonable superstructure, but not to be able to
;;; express everything Wasm can express.  Notably Swat has an expression
;;; discipline where Wasm has a less structured stack discipline.
;;;
;;; See the .swat programs for examples.  See MANUAL.md for a reference.
;;;
;;; This program translates Swat programs to WebAssembly text format (the format
;;; accepted by Firefox's wasmTextToBinary, not wabt at this point).
;;;
;;; See end for misc ways to run this, and see the shell script "swat" for a
;;; command line interface.
;;;
;;; See end for TODO lists as well as inline in the code and the manual.

;;; Environments map names to denotations.  There is a single lexically
;;; lexically scoped namespace for everything, including loops.

(define (make-env locals globals-cell)
  (cons locals globals-cell))

(define env.locals car)
(define env.globals-cell cdr)

(define (make-globals-cell)
  (vector '()))

(define (env.globals env)
  (vector-ref (env.globals-cell env) 0))

(define (env.globals-set! env x)
  (vector-set! (env.globals-cell env) 0 x))

(define (define-env-global! env name denotation)
  (env.globals-set! env (cons (cons name denotation) (env.globals env))))

(define (extend-env env assocs)
  (make-env (append assocs (env.locals env))
	    (env.globals-cell env)))

(define (lookup env name)
  (cond ((assq name (env.locals env)) => cdr)
	((assq name (env.globals env)) => cdr)
	(else #f)))

(define (lookup-variable env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
	   (fail "No binding for" name))
	  ((or (local? probe) (global? probe))
	   probe)
	  (else
	   (fail "Binding does not denote a variable" name probe)))))

(define (lookup-func env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
	   (fail "No binding for" name))
	  ((func? probe)
	   probe)
	  (else
	   (fail "Binding does not denote a function" name probe)))))

(define (lookup-global env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
	   (fail "No binding for" name))
	  ((global? probe)
	   probe)
	  (else
	   (fail "Binding does not denote a global" name probe)))))

(define (lookup-loop env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
	   (fail "No binding for loop" name))
	  ((loop? probe)
	   probe)
	  (else
	   (fail "Binding does not denote a loop" name probe)))))

(define (funcs env)
  (reverse (filter func? (map cdr (env.globals env)))))

(define (globals env)
  (reverse (filter global? (map cdr (env.globals env)))))

(define (make-standard-env)
  (let ((env (make-env '() (make-globals-cell))))
    (define-keywords! env)
    (define-types! env)
    (define-syntax! env)
    (define-builtins! env)
    env))

;;; Translation context
;;;
;;; During translation all these lists are in reverse order, newest
;;; element first.

(define (make-cx)
  (vector #f				; Slots storage (during body expansion)
	  0				; Next function ID
	  0                             ; Next global ID
	  0)) 				; Gensym ID

(define (cx.slots cx)            (vector-ref cx 0))
(define (cx.slots-set! cx v)     (vector-set! cx 0 v))
(define (cx.func-id cx)          (vector-ref cx 1))
(define (cx.func-id-set! cx v)   (vector-set! cx 1 v))
(define (cx.global-id cx)        (vector-ref cx 2))
(define (cx.global-id-set! cx v) (vector-set! cx 2 v))
(define (cx.gensym-id cx)        (vector-ref cx 3))
(define (cx.gensym-id-set! cx v) (vector-set! cx 3 v))

(define (new-name cx tag)
  (let ((n (cx.gensym-id cx)))
    (cx.gensym-id-set! cx (+ n 1))
    (string->symbol (string-append "$" tag "_" (number->string n)))))

;;; Modules

;;; Special forms that appear at the top level of the module are not handled by
;;; standard expanders but mustn't be redefined at that level, so must be
;;; defined as keywords.

(define (define-keywords! env)
  (for-each (lambda (name)
	      (define-env-global! env name '*keyword*))
	    '(defun defun+ defun- defvar defvar+ defvar- defconst defconst+ defconst-)))
  
;;; To handle imports and mutual references we must perform several passes over
;;; the module to define functions in the table.  The first pass handles
;;; imports, which are given the lowest indices.  The second pass defines other
;;; functions and globals.  Then the third phase expands function bodies.

(define (expand-module m)
  (check-list-atleast m 2 "Bad module" m)
  (check-head m 'defmodule)
  (check-symbol (cadr m) "Bad module name" m)
  (let ((env  (make-standard-env))
	(cx   (make-cx))
	(name (symbol->string (cadr m)))
	(body (cddr m)))
    (for-each (lambda (d)
		(check-list-atleast d 1 "Bad top-level phrase" d)
		(case (car d)
		  ((defun-)
		   (expand-func-phase1 cx env d))
		  ((defconst- defvar-)
		   (expand-global-phase1 cx env d))
		  ((defun defun+ defconst defconst+ defvar defvar+)
		   #t)
		  (else
		   (fail "Unknown top-level phrase" d))))
	      body)
    (for-each (lambda (d)
		(case (car d)
		  ((defun defun+)
		   (expand-func-phase1 cx env d))
		  ((defconst defconst+ defvar defvar+)
		   (expand-global-phase1 cx env d))))
	      body)
    (for-each (lambda (d)
		(case (car d)
		  ((defun defun+)
		   (expand-func-phase2 cx env d))
		  ((defconst defconst+ defvar defvar+)
		   (expand-global-phase2 cx env d))))
	      body)
    (values name
	    (cons 'module
		  (append
		   (map (lambda (g)
			  (let ((t (if (global.mut? g) `(mut ,(global.type g)) (global.type g))))
			    (if (global.import? g)
				`(import "" ,(symbol->string (global.name g)) (global ,t))
				`(global ,@(if (global.export? g) `((export ,(symbol->string (global.name g)))) '())
					 ,t
					 ,(global.init g)))))
			(globals env))
		   (map (lambda (f)
			  (if (func.import? f)
			      `(import "" ,(symbol->string (func.name f)) ,(assemble-function f '()))
			      (func.defn f)))
			(funcs env)))))))

;;; Functions

(define (make-func name import? export? id params result slots env)
  (vector 'func name import? export? id params result slots env #f))

(define (func? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'func)))

(define (func.name f) (vector-ref f 1))
(define (func.import? f) (vector-ref f 2))
(define (func.export? f) (vector-ref f 3))
(define (func.id f) (vector-ref f 4))
(define (func.params f) (vector-ref f 5))
(define (func.result f) (vector-ref f 6))
(define (func.slots f) (vector-ref f 7))
(define (func.env f) (vector-ref f 8))
(define (func.defn f) (vector-ref f 9))
(define (func.defn-set! f v) (vector-set! f 9 v))

(define (define-function! cx env name import? export? params result-type slots)
  (let* ((id   (cx.func-id cx))
	 (func (make-func name import? export? id params result-type slots env)))
    (cx.func-id-set! cx (+ id 1))
    (define-env-global! env name func)
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

(define (expand-func-phase1 cx env f)
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
    (check-unbound env name "already defined at global level")
    (cx.slots-set! cx #f)
    (let loop ((xs       (cdr signature))
	       (bindings '())
	       (params   '()))
      (cond ((null? xs)
	     (define-function! cx (extend-env env bindings) name import? export? (reverse params) 'void slots))

	    ((eq? (car xs) '->)
	     (check-list xs 2 "Bad signature" signature)
	     (let ((t (parse-return-type cx env (cadr xs))))
	       (define-function! cx (extend-env env bindings) name import? export? (reverse params) t slots)))

	    (else
	     (let ((first (car xs)))
	       (check-list first 2 "Bad parameter" first)
	       (check-symbol (car first) "Bad parameter name" first)
	       (let ((t    (parse-type cx env (cadr first)))
		     (name (car first)))
		 (if (assq name bindings)
		     (fail "Duplicate parameter" name))
		 (let-values (((slot _) (claim-param slots t)))
		   (loop (cdr xs)
			 (cons (cons name (make-local name slot t)) bindings)
			 (cons `(param ,t) params))))))))))

(define (expand-func-phase2 cx env f)
  (let* ((signature (cadr f))
	 (name      (car signature))
	 (export?   (eq? (car f) 'defun+))
	 (import?   (eq? (car f) 'defun-))
	 (body      (cddr f)))
    (let* ((func  (lookup-func env name))
	   (env   (func.env func))
	   (slots (func.slots func)))
      (cx.slots-set! cx slots)
      (assemble-function func (expand-body cx body (func.result func) env)))))

(define (expand-body cx body expected-type env)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) env)))
    (let ((drop? (and (eq? expected-type 'void)
		      (not (eq? result-type 'void)))))
      `(,@(get-slot-decls (cx.slots cx)) ,expanded ,@(if drop? '(drop) '())))))

;;; Globals

(define (make-global name import? export? mut? id type)
  (vector 'global name id type import? export? mut? #f))

(define (global? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'global)))

(define (global.name x) (vector-ref x 1))
(define (global.id x) (vector-ref x 2))
(define (global.type x) (vector-ref x 3))
(define (global.import? x) (vector-ref x 4))
(define (global.export? x) (vector-ref x 5))
(define (global.mut? x) (vector-ref x 6))
(define (global.init x) (vector-ref x 7))
(define (global.init-set! x v) (vector-set! x 7 v))

(define (define-global! cx env name import? export? mut? type)
  (let* ((id   (cx.global-id cx))
	 (glob (make-global name import? export? mut? id type)))
    (cx.global-id-set! cx (+ id 1))
    (define-env-global! env name glob)
    glob))

(define (expand-global-phase1 cx env g)
  (check-list-oneof g '(3 4) "Bad global" g)
  (let* ((name    (cadr g))
	 (_       (check-symbol name "Bad global name" name))
	 (export? (memq (car g) '(defconst+ defvar+)))
	 (import? (memq (car g) '(defconst- defvar-)))
	 (mut?    (memq (car g) '(defvar defvar+ defvar-)))
	 (type    (parse-type cx env (caddr g)))
	 (init    (if (null? (cdddr g)) #f (cadddr g)))
	 (_       (if init (check-constant init))))
    (if (and import? init)
	(fail "Import global can't have an initializer"))
    (check-unbound env name "already defined at global level")
    (define-global! cx env name import? export? mut? type)))

;; We could expand the init during phase 1 but we'll want to broaden
;; inits to encompass global imports soon.

(define (expand-global-phase2 cx env g)
  (let* ((name    (cadr g))
	 (init    (if (null? (cdddr g)) #f (cadddr g))))
    (if init
	(let ((defn (lookup-global env name)))
	  (global.init-set! defn (expand-constant-expr cx init))))))

;;; Locals

(define (make-local name slot type)
  (vector 'local name slot type))

(define (local? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'local)))

(define (local.name x) (vector-ref x 1))
(define (local.slot x) (vector-ref x 2))
(define (local.type x) (vector-ref x 3))

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

;;; primitive is #f or the name of a primitive type: i32, i64, f32, f64

(define (make-type name primitive)
  (vector 'type name primitive))

(define (type? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'type)))

(define (type.name x) (vector-ref x 1))
(define (type.primitive x) (vector-ref x 2))

(define (define-types! env)
  (for-each (lambda (name)
	      (define-env-global! env name (make-type name name)))
	    '(i32 i64 f32 f64))
  (define-env-global! env 'bool (make-type 'bool 'i32)))
		
(define (parse-type cx env t)
  (cond ((symbol? t)
	 (let ((probe (lookup env t)))
	   (or (and (type? probe) (type.primitive probe))
	       (fail "Only primitive types supported" t probe))))
	(else
	 (fail "Invalid type" t))))

(define (parse-return-type cx env t)
  (if (eq? t 'void)
      t
      (parse-type cx env t)))

;;; Expressions

(define (expand-expr cx expr env)
  (cond ((symbol? expr)
	 (expand-symbol cx expr env))
	((number? expr)
	 (expand-number expr))
	((and (list? expr) (not (null? expr)))
	 (if (symbol? (car expr))
	     (let ((probe (lookup env (car expr))))
	       (cond ((not probe)
		      (fail "Unbound name in form" expr))
		     ((func? probe)
		      (expand-call cx expr env))
		     ((expander? probe)
		      ((expander.expander probe) cx expr env))
		     (else
		      (fail "Attempting to call non-function" expr))))
	     (expand-call cx expr env)))
	(else
	 (fail "Unknown expression" expr))))

(define (expand-symbol cx expr env)
  (cond ((numbery-symbol? expr)
	 (expand-numbery-symbol expr))
	((lookup-variable env expr) =>
	 (lambda (x)
	   (cond ((local? x)
		  (values `(get_local ,(local.slot x)) (local.type x)))
		 ((global? x)
		  (values `(get_global ,(global.id x)) (global.type x)))
		 (else
		  ???))))
	(else
	 (fail "Symbol does not denote variable or number" expr))))

(define min-i32 (- (expt 2 31)))
(define max-i32 (- (expt 2 31) 1))

(define (expand-constant-expr cx expr)
  (cond ((numbery-symbol? expr)
	 (expand-numbery-symbol expr))
	((number? expr)
	 (expand-number expr))
	(else ???)))

(define (expand-numbery-symbol expr)
  (let* ((name (symbol->string expr))
	 (val  (string->number (substring name 2 (string-length name)))))
    (case (string-ref name 0)
      ((#\I) (values `(i32.const ,val) 'i32))
      ((#\L) (values `(i64.const ,val) 'i64))
      ((#\F) (values `(f32.const ,val) 'f32))
      ((#\D) (values `(f64.const ,val) 'f64))
      (else ???))))

(define (expand-number expr)
  (cond ((and (integer? expr) (exact? expr))
	 (if (<= min-i32 expr max-i32)
	     (values `(i32.const ,expr) 'i32)
	     (values `(i64.const ,expr) 'i64)))
	((number? expr)
	 (values `(f64.const ,expr) 'f64))
	(else
	 (fail "Bad syntax" expr))))

;;;

(define (make-expander name expander)
  (vector 'expander name expander))

(define (expander? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'expander)))

(define (expander.name x) (vector-ref x 1))
(define (expander.expander x) (vector-ref x 2))

(define (define-syntax! env)
  (define-env-global! env 'begin    (make-expander 'begin expand-begin))
  (define-env-global! env 'if       (make-expander 'if expand-if))
  (define-env-global! env 'set!     (make-expander 'set! expand-set!))
  (define-env-global! env 'inc!     (make-expander 'inc! expand-inc!dec!))
  (define-env-global! env 'dec!     (make-expander 'dec! expand-inc!dec!))
  (define-env-global! env 'let      (make-expander 'let expand-let))
  (define-env-global! env 'loop     (make-expander 'loop expand-loop))
  (define-env-global! env 'break    (make-expander 'break expand-break))
  (define-env-global! env 'continue (make-expander 'continue expand-continue))
  (define-env-global! env 'while    (make-expander 'while expand-while))
  (define-env-global! env 'case     (make-expander 'case expand-case))
  (define-env-global! env '%case    (make-expander '%case expand-%case))
  (define-env-global! env 'and      (make-expander 'and expand-and))
  (define-env-global! env 'or       (make-expander 'and expand-or))
  (define-env-global! env 'trap     (make-expander 'and expand-trap)))

(define (define-builtins! env)
  (define-env-global! env 'not      (make-expander 'not expand-not))
  (define-env-global! env 'select   (make-expander 'select expand-select))
  (define-env-global! env 'zero?    (make-expander 'zero? expand-zero?))
  (define-env-global! env 'nonzero? (make-expander 'zero? expand-nonzero?))
  (define-env-global! env 'bitnot   (make-expander 'bitnot expand-bitnot))
  (for-each (lambda (name)
	      (define-env-global! env name (make-expander name expand-unop)))
	    '(clz ctz popcnt neg abs sqrt ceil floor nearest trunc extend8 extend16 extend32))
  (for-each (lambda (name)
	      (define-env-global! env name (make-expander name expand-binop)))
	    '(+ - * div divu rem remu bitand bitor bitxor shl shr shru rotl rotr max min copysign))
  (for-each (lambda (name)
	      (define-env-global! env name (make-expander name expand-relop)))
	    '(< <u <= <=u > >u >= >=u = !=))
  (for-each (lambda (name)
	      (define-env-global! env name (make-expander name expand-conversion)))
	    '(i32->i64 u32->i64 i64->i32 f32->f64 f64->f32 f64->i32 f64->i64 i32->f64 i64->f64
	      f32->i32 f32->i64 i32->f32 i64->f32 f32->bits bits->f32 f64->bits bits->f64)))
  
(define (expand-begin cx expr env)
  (check-list-atleast expr 2 "Bad 'begin'" expr)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
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
	     (let-values (((e1 t1) (expand-expr cx (car exprs) env)))
	       (loop (cdr exprs) (cons e1 body) t1)))))))

(define (expand-if cx expr env)
  (check-list-oneof expr '(3 4) "Bad 'if'" expr)
  (let-values (((test t0) (expand-expr cx (cadr expr) env)))
    (case (length expr)
      ((3)
       (let-values (((consequent t1) (expand-expr cx (caddr expr) env)))
	 (values `(if ,test ,consequent) 'void)))
      ((4)
       (let*-values (((consequent t1) (expand-expr cx (caddr expr) env))
		     ((alternate  t2) (expand-expr cx (cadddr expr) env)))
	 (check-same-type t1 t2 "if arms")
	 (values `(if ,t1 ,test ,consequent ,alternate) t1)))
      (else
       (fail "Bad 'if'" expr)))))

(define (expand-set! cx expr env)
  (check-list expr 3 "Bad 'set!'" expr)
  (let* ((name (cadr expr))
	 (_    (check-lvalue cx name))
	 (val  (caddr expr)))
    (let-values (((e0 t0) (expand-expr cx val env)))
      (let ((binding (lookup-variable env name)))
	(cond ((local? binding)
	       (values `(set_local ,(local.slot binding) ,e0) 'void))
	      ((global? binding)
	       (values `(set_global ,(global.id binding) ,e0) 'void))
	      (else
	       ???))))))

(define (expand-inc!dec! cx expr env)
  (check-list expr 2 "Bad inc/dec" expr)
  (let* ((op   (car expr))
	 (name (cadr expr))
	 (_    (check-lvalue cx name)))
    (let ((binding (lookup-variable env name)))
      (cond ((local? binding)
	     (let* ((type    (local.type binding))
		    (slot    (local.slot binding))
		    (one     (typed-constant type 1))
		    (op      (operatorize type (if (eq? op 'inc!) '+ '-))))
	       (values `(set_local ,slot (,op (get_local ,slot) ,one))
		       'void)))
	    ((global? binding)
	     (let* ((type   (global.type global))
		    (id     (global.id global))
		    (one    (typed-constant type 1))
		    (op     (operatorize type (if (eq? op 'inc!) '+ '-))))
	       (values `(set_global ,id (,op (get_global ,id) ,one))
		       'void)))
	    (else
	     ???)))))

(define (expand-let cx expr env)

  (define (process-bindings bindings)
    (let loop ((bindings bindings) (new-locals '()) (code '()) (undos '()))
      (if (null? bindings)
	  (values (reverse new-locals) (reverse code) undos)
	  (let ((binding (car bindings)))
	    (check-list binding 2 "Bad binding" binding)
	    (let* ((name (car binding))
		   (_    (check-symbol name "Bad local name" name))
		   (_    (if (assq name new-locals)
			     (fail "Duplicate let binding" name)))
		   (init (cadr binding)))
	      (let*-values (((e0 t0)     (expand-expr cx init env))
			    ((slot undo) (claim-local (cx.slots cx) t0)))
		(loop (cdr bindings)
		      (cons (cons name (make-local name slot t0)) new-locals)
		      (cons `(set_local ,slot ,e0) code)
		      (cons undo undos))))))))

  (check-list-atleast expr 3 "Bad 'let'" expr)
  (let* ((bindings (cadr expr))
	 (body     (cddr expr)))
    (let*-values (((new-locals code undos) (process-bindings bindings))
		  ((e0 t0)                 (expand-expr cx `(begin ,@body) (extend-env env new-locals))))
      (unclaim-locals (cx.slots cx) undos)
      (let ((type (if (not (eq? t0 'void)) (list t0) '())))
	(if (not (null? code))
	    (if (and (pair? e0) (eq? (car e0) 'begin))
		(values `(block ,@type ,@(reverse code) ,@(cdr e0)) t0)
		(values `(block ,@type ,@(reverse code) ,e0) t0))
	    (values e0 t0))))))

(define (expand-loop cx expr env)
  (check-list-atleast expr 3 "Bad 'loop'" expr)
  (let* ((id   (cadr expr))
	 (_    (check-symbol id "Bad loop id" id))
	 (body (cddr expr))
	 (loop (make-loop id (new-name cx "brk") (new-name cx "cnt") #f))
	 (env  (extend-env env (list (cons id loop)))))
    (let-values (((e0 t0) (expand-expr cx `(begin ,@body) env)))
      (values `(block ,(loop.break loop) ,@(if (eq? (loop.type loop) 'void) '() (list (loop.type loop)))
		      (loop ,(loop.continue loop)
			    ,e0
			    (br ,(loop.continue loop))))
	      (loop.type loop)))))

(define (expand-break cx expr env)
  (check-list-oneof expr '(2 3) "Bad 'break'" expr)
  (let* ((id   (cadr expr))
	 (_    (check-symbol id "Bad loop id" id))
	 (e    (if (null? (cddr expr)) #f (caddr expr)))
	 (loop (lookup-loop env id)))
    (if e
	(let-values (((e0 t0) (expand-expr cx e env)))
	  (loop.set-type! loop t0)
	  (values `(br ,(loop.break loop) ,e0) 'void))
	(begin
	  (loop.set-type! loop 'void)
	  (values `(br ,(loop.break loop)) 'void)))))

(define (expand-continue cx expr env)
  (check-list expr 2 "Bad 'continue'" expr)
  (let* ((id   (cadr expr))
	 (_    (check-symbol id "Bad loop id" id))
	 (loop (lookup-loop env id)))
    (values `(br ,(loop.continue loop)) 'void)))

(define (expand-while cx expr env)
  (check-list-atleast expr 2 "Bad 'while'" expr)
  (let* ((block-name (new-name cx "brk"))
	 (loop-name  (new-name cx "cnt")))
    (values `(block ,block-name
		    (loop ,loop-name
			  (br_if ,block-name (i32.eqz ,(expand-expr cx (cadr expr) env)))
			  ,(let-values (((e0 t0) (expand-expr cx `(begin ,@(cddr expr)) env)))
			     e0)
			  (br ,loop-name)))
	    'void)))

(define (expand-case cx expr env)
  (check-list-atleast expr 2 "Bad 'case'" expr)
  (let ((temp (new-name cx "local")))
    (expand-expr cx
		 `(let ((,temp ,(cadr expr)))
		    (%case ,temp ,@(cddr expr)))
		 env)))

;; The dispatch expr is always effect-free, and we make use of this.

(define (expand-%case cx expr env)
  
  (define (check-case-types cases default-type)
    (for-each (lambda (c)
		(check-same-type (caddr c) default-type "Case arm type"))
	      cases))

  ;; found-values is a list of the discriminant integer values.
  ;;
  ;; e0 is the dispatch expression, known to be side-effect free and unaffected
  ;; by side effects in the cases.
  ;;
  ;; cases is a list of lists (((integer ...) code type) ...) where each code is
  ;; typically a block and all the integers are known to be distinct across the
  ;; full list.
  ;;
  ;; default is just an expr, default-type is its type.

  (define (finish-case found-values e0 cases default default-type)
    (if (null? cases)
	(begin
	  (assert default)
	  (values default default-type))
	(let* ((_             (check-case-types cases default-type))
	       (ty            (caddr (car cases)))
	       (bty           (if (eq? ty 'void) '() (list ty)))
	       (cases         (map (lambda (c) (cons (new-name cx "case") c)) cases))
	       (default-label (new-name cx "default"))
	       (outer-label   (new-name cx "outer"))
	       (default       (if (not default) '() (list default)))
	       (found-values  (sort found-values <))
	       (smallest      (car found-values))
	       (largest       (car (last-pair found-values)))
	       (size          (+ (- largest smallest) 1))
	       (v             (make-vector size (list default-label))))
	  (if (> size 10000)
	      (fail "Range of cases too large" (list smallest largest)))
	  (for-each (lambda (c)
		      (for-each (lambda (val)
				  (vector-set! v (- val smallest) c))
				(cadr c)))
		    cases)
	  (values
	   `(block ,outer-label ,@bty
		   (block ,default-label
			  ,(letrec ((loop (lambda (cases)
					    (if (null? cases)
						`(br_table ,@(map car (vector->list v))
							   ,default-label
							   ,(if (zero? smallest)
								e0
								`(i32.sub ,e0 (i32.const ,smallest))))
						(let* ((c     (car cases))
						       (label (car c))
						       (code  (caddr c)))
						  `(block
							  (block ,label
								 ,(loop (cdr cases)))
							  (br ,outer-label ,code)))))))
			     (loop cases)))
		   (br ,outer-label ,@default))
	   ty))))
  
  ;; This returns a list of integers, not expressions

  (define (resolve-case-constants cs)
    (let ((constants
	   (map (lambda (c) 
		  (cond ((numbery-symbol? c)
			 (let-values (((v t) (expand-numbery-symbol c)))
			   (check-same-type t 'i32 "Case constant")
			   v))
			((number? c)
			 (let-values (((v t) (expand-number c)))
			   (check-same-type t 'i32 "Case constant")
			   v))
			((and (symbol? c) (lookup-global env c)) =>
			 (lambda (g)
			   (if (and (not (global.import? g))
				    (not (global.mut? g))
				    (eq? (global.type g) 'i32))
			       (global.init g)
			       (fail "Reference to global that is not immutable with a known constant initializer" c))))
			(else
			 (fail "Invalid case constant " c))))
		cs)))
      (map (lambda (c)
	     (assert (and (pair? c) (eq? (car c) 'i32.const)))
	     (cadr c))
	   constants)))
  
  (define (incorporate-constants constants known)
    (cond ((null? constants)
	   known)
	  ((memv (car constants) known)
	   (fail "Duplicate constant in case" (car constants)))
	  (else
	   (incorporate-constants (cdr constants)
				  (cons (car constants) known)))))

  (check-list-atleast expr 2 "Bad 'case'" expr)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-same-type t0 'i32 "Case expression")
    (let loop ((cases (cddr expr)) (case-info '()) (found-values '()))
      (cond ((null? cases)
	     (finish-case found-values c0 case-info #f 'void))

	    ((and (pair? (car cases)) (eq? (caar cases) 'else))
	     (let ((c (car cases)))
	       (check-list-atleast c 2 "Bad else in case" c)
	       (if (not (null? (cdr cases)))
		   (fail "Else clause in case must be last" expr))
	       (let-values (((de te) (expand-expr cx (cons 'begin (cdr c)) env)))
		 (finish-case found-values e0 case-info de te))))

	    (else
	     (let ((c (car cases)))
	       (check-list-atleast c 2 "Bad case in case" c)
	       (check-list-atleast (car c) 1 "Bad constant list in case" c)
	       (let* ((constants    (resolve-case-constants (car c)))
		      (found-values (incorporate-constants constants found-values)))
		 (let-values (((be bt) (expand-expr cx (cons 'begin (cdr c)) env)))
		   (loop (cdr cases)
			 (cons (list constants be bt) case-info)
			 found-values)))))))))

(define (expand-and cx expr env)
  (check-list expr 3 "Bad 'and'" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
		((op2 t2) (expand-expr cx (caddr expr) env)))
    (values `(if i32 ,op1 ,op2 (i32.const 0)) 'i32)))

(define (expand-or cx expr env)
  (check-list expr 3 "Bad 'or'" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
		((op2 t2) (expand-expr cx (caddr expr) env)))
    (values `(if i32 ,op1 (i32.const 1) ,op2) 'i32)))

(define (expand-trap cx expr env)
  (check-list expr 2 "Bad 'trap'" expr)
  (let ((t (parse-return-type cx env (cadr expr))))
    (values '(unreachable) t)))

(define (expand-zero? cx expr env)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(,(operatorize t1 '=) ,op1 ,(typed-constant t1 0)) 'i32)))

(define (expand-nonzero? cx expr env)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(,(operatorize t1 '!=) ,op1 ,(typed-constant t1 0)) 'i32)))

(define (expand-bitnot cx expr env)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(,(operatorize t1 'bitxor) ,op1 ,(typed-constant t1 -1)) t1)))

(define (expand-select cx expr env)
  (check-list expr 4 "Bad select operator" expr)
  (let*-values (((op t) (expand-expr cx (cadr expr) env))
		((op1 t1) (expand-expr cx (caddr expr) env))
		((op2 t2) (expand-expr cx (cadddr expr) env)))
    (check-same-type t1 t2 "select arms")
    (check-same-type t 'i32 "select condition")
    (values `(select ,op2 ,op1 ,op) t1)))

(define (expand-not cx expr env)
  (check-list expr 2 "Bad 'not'" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(i32.eqz ,op1) 'i32)))

(define (expand-unop cx expr env)
  (check-list expr 2 "Bad unary operator" expr)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(,(operatorize t1 (car expr)) ,op1) t1)))

(define (expand-binop cx expr env)
  (check-list expr 3 "Bad binary operator" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
		((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-same-type t1 t2 "binop")
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) t1)))

(define (expand-relop cx expr env)
  (check-list expr 3 "Bad binary operator" expr)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
		((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-same-type t1 t2 "relop")
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) 'i32)))

(define (expand-conversion cx expr env)
  (check-list expr 2 "Bad conversion" expr)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (case (car expr)
      ((i32->i64)  (values `(i64.extend_s/i32 ,e0) 'i64))
      ((u32->i64)  (values `(i64.extend_u/i32 ,e0) 'i64))
      ((i64->i32)  (values `(i32.wrap/i64 ,e0) 'i32))
      ((f32->f64)  (values `(f64.promote/f32 ,e0) 'f64))
      ((f64->f32)  (values `(f32.demote/f64 ,e0) 'f32))
      ((f64->i32)  (values `(i32.trunc_s/f64 ,e0) 'i32))
      ((f64->i64)  (values `(i64.trunc_s/f64 ,e0) 'i64))
      ((i32->f64)  (values `(f64.convert_s/i32 ,e0) 'f64))
      ((i64->f64)  (values `(f64.convert_s/i64 ,e0) 'f64))
      ((f32->i32)  (values `(i32.trunc_s/f32 ,e0) 'f32))
      ((f32->i64)  (values `(i64.trunc_s/f32 ,e0) 'f32))
      ((i32->f32)  (values `(f32.convert_s/i32 ,e0) 'f32))
      ((i64->f32)  (values `(f32.convert_s/i64 ,e0) 'f32))
      ((f32->bits) (values `(i32.reinterpret/f32 ,e0) 'i32))
      ((bits->f32) (values `(f32.reinterpret/i32 ,e0) 'f32))
      ((f64->bits) (values `(i64.reinterpret/f64 ,e0) 'i64))
      ((bits->f64) (values `(f64.reinterpret/i64 ,e0) 'f64))
      (else        ???))))

(define (expand-call cx expr env)
  (let* ((name  (car expr))
	 (args  (cdr expr))
	 (func  (lookup-func env name)))
    (values `(call ,(func.id func)
		   ,@(map (lambda (e)
			    (let-values (((new-e ty) (expand-expr cx e env)))
			      new-e))
			  args))
	    (func.result func))))

(define (check-lvalue cx name)
  (symbol? name))

(define (make-loop id break-name continue-name type)
  (vector 'loop id break-name continue-name type))

(define (loop? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'loop)))

(define (loop.id x) (vector-ref x 1))
(define (loop.break x) (vector-ref x 2))
(define (loop.continue x) (vector-ref x 3))
(define (loop.type x) (vector-ref x 4))
(define (loop.type-set! x v) (vector-set! x 4 v))

(define (loop.set-type! x t)
  (let ((current (loop.type x)))
    (cond ((not current)
	   (loop.type-set! x t))
	  ((eq? t current))
	  (else
	   (fail "Type mismatch for loop" (loop.id x))))))

(define (call-with-loop cx env id fn)
  (let ((loop (make-loop id (new-name cx "brk") (new-name cx "cnt") #f)))
    (fn (extend-env env (list (cons id loop))) loop)))

(define (operatorize t op)
  (let ((name (case t
		((i32 i64) (int-op-name op))
		((f32 f64) (float-op-name op))
		(else ???))))
    (string->symbol (string-append (symbol->string t) name))))

(define (int-op-name x)
  (case x
    ((clz) ".clz")
    ((ctz) ".ctz")
    ((popcnt) ".popcnt")
    ((div) ".div_s")
    ((divu) ".div_u")
    ((rem) ".rem_s")
    ((remu) ".rem_u")
    ((<) ".lt_s")
    ((<u) ".lt_u")
    ((<=) ".le_s")
    ((<=u) ".le_u")
    ((>) ".gt_s")
    ((>u) ".gt_u")
    ((>=) ".ge_s")
    ((>=u) ".ge_u")
    ((bitand) ".and")
    ((bitor) ".or")
    ((bitxor) ".xor")
    ((shl) ".shl")
    ((shr) ".shr_s")
    ((shru) ".shr_u")
    ((rotl) ".rotl")
    ((rotr) ".rotr")
    ((extend8) ".extend8_s")
    ((extend16) ".extend16_s")
    ((extend32) ".extend32_s")
    (else (common-op-name x))))

(define (float-op-name x)
  (case x
    ((div) ".div")
    ((<) ".lt")
    ((<=) ".le")
    ((>) ".gt")
    ((>=) ".ge")
    ((neg) ".neg")
    ((abs) ".abs")
    ((min) ".min")
    ((max) ".max")
    ((sqrt) ".sqrt")
    ((ceil) ".ceil")
    ((floor) ".floor")
    ((nearest) ".nearest")
    ((trunc) ".trunc")
    (else (common-op-name x))))

(define (common-op-name x)
  (case x
    ((+) ".add")
    ((-) ".sub")
    ((*) ".mul")
    ((=) ".eq")
    ((!=) ".ne")
    (else ???)))

(define (typed-constant type value)
  `(,(string->symbol (string-append (symbol->string type) ".const")) ,value))

;;; Sundry

(define (numbery-symbol? x)
  (and (symbol? x)
       (let ((name (symbol->string x)))
	 (and (> (string-length name) 2)
	      (char=? #\. (string-ref name 1))
	      (memv (string-ref name 0) '(#\I #\L #\F #\D))))))

(define (check-unbound env name reason)
  (if (lookup env name)
      (fail "Name cannot be bound because" reason name)))

(define (check-same-type t1 t2 . rest)
  (if (not (eq? t1 t2))
      (let ((msg "Not same type"))
	(if (not (null? rest))
	    (set! msg (string-append msg " in " (car rest))))
	(fail msg t1 t2))))

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

;;; Also see TODOs in MANUAL.md.

;;; TODO for v2
;;;   - Object system, see FUTURE.md
;;;
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
;;;   - Better syntax checking + errors
;;;   - Produce wabt-compatible output
;;;   - Allow imports from arbitrary module names by adopting eg (func- mod:fn ...) syntax
;;;   - Multiple-value return:  -> (t0 t1 ...) return type annotation; LET-VALUES or
;;;     destructuring in LET; VALUES; maybe some way of applying / splatting a captured
;;;     list of values (long tail).  Until we actually have these in wasm we can simulate
;;;     using objects or globals or flat memory (though not for references)
;;;   - deftype
;;;   - poor man's enums:
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
