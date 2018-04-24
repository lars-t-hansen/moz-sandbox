;;; -*- fill-column: 80; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public License,
;;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;;; obtain one at <http://mozilla.org/MPL/2.0/>.
;;;
;;; This is r5rs-ish Scheme, it works with Larceny 1.3 (http://larcenists.org).

;;; Working on: Reference types
;;;
;;;  - We have defclass, null?, null, new, and field refs, field updates.
;;;  - We have runtime support and accessors and all that
;;;  - We can pass things in and out
;;;
;;;  - We don't have:
;;;    - enough test code, including type checks that should fail
;;;    - type checks at boundaries from JS to Wasm (so in class.swat, we
;;;      can pass an Ipso to something that takes a Box, and it will
;;;      not throw)
;;;    - automatic upcasts / proper subtyping support
;;;    - downcast operator
;;;    - virtual functions
;;;    - Object or Root (except in documentation...)
;;;    - inc! and dec! support
;;;
;;;  - We also don't have:
;;;    - arrays
;;;    - strings
;;;    - imported and exported types (imports are important for DOM)
;;;
;;;  - Also:
;;;    - some of the JS support code is pretty messy now, need to clean up

;;; Swat is an evolving Scheme/Lisp-syntaxed WebAssembly superstructure.
;;;
;;; See the .swat programs for examples.  See MANUAL.md for a reference and
;;; other help.  See MANUAL.md and FUTURE.md for some TODO lists.
;;;
;;; This program translates Swat programs to the WebAssembly text format
;;; accepted by Firefox's wasmTextToBinary, plus supporting JS code.
;;;
;;; See the functions "swat" and "swat-noninteractive" for sundry ways to run
;;; this program, and see the shell script "swat" for a command line interface.

;; Environments map names to the entities they denote in the program.  There is
;; a single lexically scoped namespace for everything, including loop labels.

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

(define (lookup-class env name)
  (let ((probe (lookup-type env name)))
    (if (not (type.class probe))
        (fail "Not a class type" name))
    (type.class probe)))

(define (lookup-type env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
           (fail "No binding for" name))
          ((type? probe)
           probe)
          (else
           (fail "Binding does not denote a class" name probe)))))

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

(define (lookup-variable env name)
  (let ((probe (lookup env name)))
    (cond ((not probe)
           (fail "No binding for" name))
          ((or (local? probe) (global? probe))
           probe)
          (else
           (fail "Binding does not denote a variable" name probe)))))

(define (funcs env)
  (reverse (filter func? (map cdr (env.globals env)))))

(define (globals env)
  (reverse (filter global? (map cdr (env.globals env)))))

(define (classes env)
  (map type.class (reverse (filter (lambda (x)
                                     (and (type? x) (type.class x)))
                                   (map cdr (env.globals env))))))

(define (make-standard-env)
  (let ((env (make-env '() (make-globals-cell))))
    (define-keywords! env)
    (define-types! env)
    (define-syntax! env)
    (define-builtins! env)
    env))

;; Translation context
;;
;; During translation all these lists are in reverse order, newest
;; element first.

(define (make-cx name support)
  (vector #f                            ; Slots storage (during body expansion)
          0                             ; Next function ID
          0                             ; Next global ID
          0                             ; Gensym ID
          support                       ; host support code (opaque structure)
          name))                        ; module name

(define (cx.slots cx)            (vector-ref cx 0))
(define (cx.slots-set! cx v)     (vector-set! cx 0 v))
(define (cx.func-id cx)          (vector-ref cx 1))
(define (cx.func-id-set! cx v)   (vector-set! cx 1 v))
(define (cx.global-id cx)        (vector-ref cx 2))
(define (cx.global-id-set! cx v) (vector-set! cx 2 v))
(define (cx.gensym-id cx)        (vector-ref cx 3))
(define (cx.gensym-id-set! cx v) (vector-set! cx 3 v))
(define (cx.support cx)          (vector-ref cx 4))
(define (cx.name cx)             (vector-ref cx 5))

(define (new-name cx tag)
  (let ((n (cx.gensym-id cx)))
    (cx.gensym-id-set! cx (+ n 1))
    (string->symbol (string-append "$" tag "_" (number->string n)))))

;; Modules

;; Special forms that appear at the top level of the module are not handled by
;; standard expanders but mustn't be redefined at that level, so must be
;; defined as keywords.

(define (define-keywords! env)
  (for-each (lambda (name)
              (define-env-global! env name '*keyword*))
            '(defun defun+ defun- defvar defvar+ defvar- defconst defconst+ defconst- defclass defclass+ defclass-)))

;; To handle imports and mutual references we must perform several passes over
;; the module to define functions in the table.  The first pass handles
;; imports, which are given the lowest indices.  The second pass defines other
;; functions and globals.  Then the third phase expands function bodies.

(define (expand-module m support)
  (check-list-atleast m 2 "Bad module" m)
  (check-symbol (cadr m) "Bad module name" m)
  (let* ((env  (make-standard-env))
         (name (symbol->string (cadr m)))
         (cx   (make-cx name support))
         (body (cddr m)))
    (for-each (lambda (d)
                (check-list-atleast d 1 "Bad top-level phrase" d)
                (case (car d)
                  ((defun-)
                   (expand-func-phase1 cx env d))
                  ((defconst- defvar-)
                   (expand-global-phase1 cx env d))
                  ((defclass)
                   (expand-class-phase1 cx env d))
                  ((defun defun+ defconst defconst+ defvar defvar+)
                   #t)
                  (else
                   (fail "Unknown top-level phrase" d))))
              body)
    (resolve-classes cx env)
    (synthesize-class-ops cx env)
    (for-each (lambda (d)
                (case (car d)
                  ((defun defun+)
                   (expand-func-phase1 cx env d))
                  ((defconst defconst+ defvar defvar+)
                   (expand-global-phase1 cx env d))
                  ((defclass)
                   #t)))
              body)
    (for-each (lambda (d)
                (case (car d)
                  ((defun defun+)
                   (expand-func-phase2 cx env d))
                  ((defconst defconst+ defvar defvar+)
                   (expand-global-phase2 cx env d))
                  ((defclass)
                   #t)))
              body)
    (values name
            (cons 'module
                  (append
                   (map (lambda (g)
                          (let* ((t (render-type (global.type g)))
                                 (t (if (global.mut? g) `(mut ,t) t)))
                            (if (global.import g)
                                `(import ,(global.import g) ,(symbol->string (global.name g)) (global ,t))
                                `(global ,@(if (global.export? g) `((export ,(symbol->string (global.name g)))) '())
                                         ,t
                                         ,(global.init g)))))
                        (globals env))
                   (map (lambda (f)
                          (if (func.import f)
                              `(import ,(func.import f) ,(symbol->string (func.name f)) ,(assemble-function f '()))
                              (func.defn f)))
                        (funcs env)))))))

;; Classes

(define (make-class name base fields)
  (vector 'class name base fields #f #f))

(define (class? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'class)))

(define (class.name c) (vector-ref c 1))
(define (class.base c) (vector-ref c 2))
(define (class.base-set! c v) (vector-set! c 2 v))
(define (class.fields c) (vector-ref c 3))
(define (class.fields-set! c v) (vector-set! c 3 v))
(define (class.resolved? c) (vector-ref c 4))
(define (class.resolved-set! c) (vector-set! c 4 #t))
(define (class.type c) (vector-ref c 5))
(define (class.type-set! c v) (vector-set! c 5 v))

(define (define-class! cx env name base fields)
  (let ((cls (make-class name base fields)))
    (define-env-global! env name (make-class-type cls))
    cls))

(define (make-accessor name field-name)
  (vector 'accessor name field-name))

(define (accessor? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'accessor)))

(define (accessor.name x) (vector-ref x 1))
(define (accessor.field-name x) (vector-ref x 2))

(define (define-accessor! cx env field-name)
  (let* ((name  (string->symbol (string-append "*" (symbol->string field-name))))
         (probe (lookup env name)))
    (cond ((not probe)
           (define-env-global! env name (make-accessor name field-name)))
          ((accessor? probe) #t)
          (else
           (fail "Conflict between accessor and other global" name)))))

(define (accessor-expression? env x)
  (and (list? x)
       (= (length x) 2)
       (symbol? (car x))
       (accessor? (lookup env (car x)))))

;; Phase 1 records the names of base and field types, checks syntax, and records
;; the type; the types are resolved and checked by the resolution phase.
;;
;; After phase1, "base" is #f or a symbol and "fields" only has the local
;; fields, with unresolved types.  After the resolution phase, "base" is #f or a
;; class, and "fields" has the fields for the class and its base classes, with
;; the base classes' fields first.

(define (expand-class-phase1 cx env d)
  (check-list-atleast d 2 "Bad defclass" d)
  (let ((name (cadr d)))
    (check-symbol name "class name" name)
    (check-unbound env name "already defined at global level")
    (let-values (((base body)
                  (let ((rest (cddr d)))
                    (if (and (not (null? rest))
                             (let ((x (car rest)))
                               (and (list? x)
                                    (= (length x) 2)
                                    (eq? (car x) 'extends))))
                        (let ((base-name (cadr (car rest))))
                          (check-symbol base-name "base class name" base-name)
                          (values base-name (cdr rest)))
                        (values #f rest)))))
      (let loop ((body body) (fields '()))
        (if (null? body)
            (define-class! cx env name base (reverse fields))
            (let ((c (car body)))
              (cond ((not (and (list? c) (not (null? c))))
                     (fail "Invalid clause in defclass" c d))
                    ((eq? (car c) 'extends)
                     (fail "Invalid extends clause in defclass" c d))
                    ((eq? (car c) 'virtual)
                     (fail "No support for virtuals yet"))
                    ((symbol? (car c))
                     (check-list c 2 "Bad field" c d)
                     (let* ((name (car c))
                            (_    (check-symbol name "Bad field name" name d))
                            (type (cadr c)))
                       (loop (cdr body) (cons (list name type) fields))))
                    (else
                     (fail "Bad clause in defclass" c d)))))))))

(define (resolve-classes cx env)
  (for-each (lambda (cls)
              (resolve-class cx env cls '()))
            (classes env)))

(define (resolve-class cx env cls forbidden)
  (if (not (class.resolved? cls))
      (begin
        (if (memq cls forbidden)
            (fail "Recursive class hierarchy"))
        (if (class.base cls)
            (let ((base (lookup-class env (class.base cls))))
              (resolve-class cx env base (cons cls forbidden))
              (class.base-set! cls base)))
        (let ((base-fields (if (class.base cls)
                               (class.fields (class.base cls))
                               '())))
          (let ((fields (map (lambda (f)
                               (let ((name (car f))
                                     (ty   (lookup-type env (cadr f))))
                                 (if (class? ty)
                                     (resolve-class cx env base (cons cls forbidden)))
                                 (if (assq name base-fields)
                                     (fail "Duplicated field name" name))
                                 (list name ty)))
                             (class.fields cls))))
            (for-each (lambda (f)
                        (define-accessor! cx env (car f)))
                      fields)
            (class.fields-set! cls (append base-fields fields))))
        (class.resolved-set! cls))))

;; Functions

(define (make-func name import export? id rendered-params formals result slots env)
  (vector 'func name import export? id rendered-params formals result slots env #f))

(define (func? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'func)))

(define (func.name f) (vector-ref f 1))
(define (func.import f) (vector-ref f 2)) ; Either #f or a string naming the module
(define (func.export? f) (vector-ref f 3))
(define (func.id f) (vector-ref f 4))
(define (func.rendered-params f) (vector-ref f 5))
(define (func.formals f) (vector-ref f 6))
(define (func.result f) (vector-ref f 7))
(define (func.slots f) (vector-ref f 8))
(define (func.env f) (vector-ref f 9))
(define (func.defn f) (vector-ref f 10))
(define (func.defn-set! f v) (vector-set! f 10 v))

;; formals is ((name type) ...)

(define (define-function! cx env name import export? params formals result-type slots)
  (let* ((id   (cx.func-id cx))
         (func (make-func name import export? id params formals result-type slots env)))
    (cx.func-id-set! cx (+ id 1))
    (define-env-global! env name func)
    func))

(define (assemble-function func body)
  (let* ((f body)
         (f (if (void-type? (func.result func))
                f
                (cons `(result ,(render-type (func.result func))) f)))
         (f (append (func.rendered-params func) f))
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
         (import    (if (eq? (car f) 'defun-) "" #f))
         (body      (cddr f))
         (slots     (make-slots)))
    (if (and import (not (null? body)))
        (fail "Import function can't have a body" f))
    (check-unbound env name "already defined at global level")
    (cx.slots-set! cx #f)
    (let loop ((xs       (cdr signature))
               (bindings '())
               (formals  '())
               (params   '()))
      (cond ((null? xs)
             (define-function! cx (extend-env env bindings) name import export?
                               (reverse params) (reverse formals) *void-type* slots))

            ((eq? (car xs) '->)
             (check-list xs 2 "Bad signature" signature)
             (let ((t (parse-type cx env (cadr xs))))
               (define-function! cx (extend-env env bindings) name import export?
                                 (reverse params) (reverse formals) t slots)))

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
                         (cons (list name t) formals)
                         (cons `(param ,(render-type t)) params))))))))))

(define (expand-func-phase2 cx env f)
  (let* ((signature (cadr f))
         (name      (car signature))
         (body      (cddr f)))
    (let* ((func  (lookup-func env name))
           (env   (func.env func))
           (slots (func.slots func)))
      (cx.slots-set! cx slots)
      (assemble-function func (expand-body cx body (func.result func) env)))))

(define (expand-body cx body expected-type env)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) env)))
    (let ((drop? (and (void-type? expected-type)
                      (not (void-type? result-type)))))
      `(,@(get-slot-decls (cx.slots cx)) ,expanded ,@(if drop? '(drop) '())))))

;; Globals

(define (make-global name import export? mut? id type)
  (vector 'global name id type import export? mut? #f))

(define (global? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'global)))

(define (global.name x) (vector-ref x 1))
(define (global.id x) (vector-ref x 2))
(define (global.type x) (vector-ref x 3))
(define (global.import x) (vector-ref x 4)) ; Either #f or a string naming the module
(define (global.export? x) (vector-ref x 5))
(define (global.mut? x) (vector-ref x 6))
(define (global.init x) (vector-ref x 7))
(define (global.init-set! x v) (vector-set! x 7 v))

(define (define-global! cx env name import export? mut? type)
  (let* ((id   (cx.global-id cx))
         (glob (make-global name import export? mut? id type)))
    (cx.global-id-set! cx (+ id 1))
    (define-env-global! env name glob)
    glob))

(define (expand-global-phase1 cx env g)
  (check-list-oneof g '(3 4) "Bad global" g)
  (let* ((name    (cadr g))
         (_       (check-symbol name "Bad global name" name))
         (export? (memq (car g) '(defconst+ defvar+)))
         (import  (if (memq (car g) '(defconst- defvar-)) "" #f))
         (mut?    (memq (car g) '(defvar defvar+ defvar-)))
         (type    (parse-type cx env (caddr g)))
         (init    (if (null? (cdddr g)) #f (cadddr g)))
         (_       (if init (check-constant init))))
    (if (and import init)
        (fail "Import global can't have an initializer"))
    (check-unbound env name "already defined at global level")
    (define-global! cx env name import export? mut? type)))

;; We could expand the init during phase 1 but we'll want to broaden
;; inits to encompass global imports soon.

(define (expand-global-phase2 cx env g)
  (let* ((name    (cadr g))
         (init    (if (null? (cdddr g)) #f (cadddr g))))
    (if init
        (let ((defn (lookup-global env name)))
          (global.init-set! defn (expand-constant-expr cx init))))))

;; Locals

(define (make-local name slot type)
  (vector 'local name slot type))

(define (local? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'local)))

(define (local.name x) (vector-ref x 1))
(define (local.slot x) (vector-ref x 2))
(define (local.type x) (vector-ref x 3))

;; Local slots storage

(define (make-slots)
  (vector (list 0 '()) '() '() '() '() '()))

(define (slots.tracker x) (vector-ref x 0))

(define *slots-i32* 1)
(define *slots-i64* 2)
(define *slots-f32* 3)
(define *slots-f64* 4)
(define *slots-anyref* 5)

(define (make-tracker)
  (list 0 '()))

(define (tracker.next x) (car x))
(define (tracker.next-set! x v) (set-car! x v))
(define (tracker.defined x) (cadr x))
(define (tracker.defined-set! x v) (set-car! (cdr x) v))

(define (slot-index-for-type t)
  (cond ((i32-type? t) *slots-i32*)
        ((i64-type? t) *slots-i64*)
        ((f32-type? t) *slots-f32*)
        ((f64-type? t) *slots-f64*)
        ((class-type? t) *slots-anyref*)
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
         `(local ,(render-type t)))
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

;; Types

(define (make-type name primitive ref array class)
  (vector 'type name primitive ref array class))

(define (make-primitive-type name)
  (vector 'type name name #f #f #f))

(define (make-ref-type base)
  (make-type 'ref #f base #f #f))

(define (make-array-type element)
  (make-type 'array #f #f element #f))

(define (make-class-type cls)
  (let ((t (make-type 'class #f #f #f cls)))
    (class.type-set! cls t)
    t))

(define (type? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'type)))

(define (type.name x) (vector-ref x 1))
(define (type.primitive x) (vector-ref x 2))
(define (type.ref-base x) (vector-ref x 3))
(define (type.array-element x) (vector-ref x 4))
(define (type.class x) (vector-ref x 5))

(define *void-type* (make-type 'void #f #f #f #f))

(define *i32-type* (make-primitive-type 'i32))
(define *i64-type* (make-primitive-type 'i64))
(define *f32-type* (make-primitive-type 'f32))
(define *f64-type* (make-primitive-type 'f64))

(define (same-type? a b)
  (or (eq? a b)
      (case (type.name a)
        ((ref)   (same-type? (type.ref-base a) (type.ref-base b)))
        ((array) (same-type? (type.array-element a) (type.array-element b)))
        ((class) (eq? (type.class a) (type.class b)))
        (else    #f))))

(define (void-type? x) (eq? x *void-type*))

(define (i32-type? x)  (eq? x *i32-type*))
(define (i64-type? x)  (eq? x *i64-type*))
(define (f32-type? x)  (eq? x *f32-type*))
(define (f64-type? x)  (eq? x *f64-type*))

(define (integer-type? x)
  (case (type.primitive x)
    ((i32 i64) #t)
    (else #f)))

(define (floating-type? x)
  (case (type.primitive x)
    ((f32 f64) #t)
    (else #f)))

(define (number-type? x)
  (case (type.primitive x)
    ((i32 i64 f32 f64) #t)
    (else #f)))

(define (class-type? x)
  (type.class x))

(define (define-types! env)
  (define-env-global! env 'i32 *i32-type*)
  (define-env-global! env 'i64 *i64-type*)
  (define-env-global! env 'f32 *f32-type*)
  (define-env-global! env 'f64 *f64-type*))

(define (parse-type cx env t)
  (cond ((and (symbol? t) (lookup env t)) =>
         (lambda (probe)
           (if (type? probe)
               probe
               (fail "Does not denote a type" t probe))))
        (else
         (fail "Invalid type" t))))

;; Various aspects of rendering reference types and operations on them, subject
;; to change as we grow better support.

(define (render-type t)
  (if (eq? (type.name t) 'class)
      'anyref
      (type.name t)))

(define (typed-object-name t)
  (string-append "TO."
                 (case (type.name t)
                   ((class) "Object")
                   ((i32)   "int32")
                   ((i64)   "int64")
                   ((f32)   "float32")
                   ((f64)   "float64")
                   (else ???))))

(define (synthesize-class-ops cx env)
  (let ((clss (classes env)))
    (if (not (null? clss))
        (let ((s (support.lib-output (cx.support cx)))
              (t (support.type-output (cx.support cx))))

          (format s "_isnull: function (x) { return x === null },\n")
          (synthesize-func-import cx env '_isnull `((p ,(class.type (car clss)))) *i32-type*)

          (format s "_null: function () { return null },\n")
          (synthesize-func-import cx env '_null '() (class.type (car clss)))

          (for-each (lambda (cls)
                      (let ((fields (class.fields cls))
                            (name   (symbol->string (class.name cls))))

                        (format t "~a: new TO.StructType({~a}),\n"
                                name
                                (string-join (map (lambda (f)
                                                    (let ((name (symbol->string (car f)))
                                                          (type (typed-object-name (cadr f))))
                                                      (string-append name ":" type)))
                                                  fields)
                                             ", "))

                        (let ((new-name (string-append "_new_" name))
                              (formals  (string-join (map (lambda (f) (symbol->string (car f))) fields) ",")))
                          (format s "~a: function (~a) { return new self.types.~a(~a) },\n"
                                  new-name
                                  formals
                                  name
                                  formals)
                          (synthesize-func-import
                           cx env
                           (string->symbol new-name)
                           fields
                           (class.type cls)))
                        (for-each (lambda (f)
                                    (let ((field-name (symbol->string (car f)))
                                          (field-type (cadr f)))
                                      (let ((getter-name (string-append "_get_" name "_" field-name)))
                                        (format s "~a: function (p) { return p.~a },\n"
                                                getter-name field-name)
                                        (synthesize-func-import
                                         cx env
                                         (string->symbol getter-name)
                                         `((p ,(class.type cls)))
                                         field-type))

                                      (let ((setter-name (string-append "_set_" name "_" field-name)))
                                        (format s "~a: function (p, v) { p.~a = v },\n"
                                                setter-name field-name)
                                        (synthesize-func-import
                                         cx env
                                         (string->symbol setter-name)
                                         `((p ,(class.type cls)) (v ,field-type))
                                         *void-type*))))
                                  fields)))
                    clss)))))

(define (synthesize-func-import cx env name formals result)
  (let ((rendered-params (map (lambda (f)
                                `(param ,(render-type (cadr f))))
                              formals)))
    (define-function! cx env name "lib" #f rendered-params formals result #f)))

(define (render-field-access env cls field-name base-expr)
  (let* ((name (string->symbol
                (string-append "_get_"
                               (symbol->string (class.name cls))
                               "_"
                               (symbol->string field-name))))
         (func (lookup-func env name)))
    `(call ,(func.id func) ,base-expr)))

(define (render-field-update env cls field-name base-expr val-expr)
  (let* ((name (string->symbol
                (string-append "_set_"
                               (symbol->string (class.name cls))
                               "_"
                               (symbol->string field-name))))
         (func (lookup-func env name)))
    `(call ,(func.id func) ,base-expr ,val-expr)))

(define (render-null env cls)
  (let ((func (lookup-func env '_null)))
    `(call ,(func.id func))))

(define (render-null? env base-expr)
  (let ((func (lookup-func env '_isnull)))
    `(call ,(func.id func) ,base-expr)))

(define (render-new env cls args)
  (let* ((name (string->symbol (string-append "_new_" (symbol->string (class.name cls)))))
         (func (lookup-func env name)))
    `(call ,(func.id func) ,@(map car args))))

;; Expressions

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
                     ((accessor? probe)
                      (expand-accessor cx expr env))
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
      ((#\I) (values `(i32.const ,val) *i32-type*))
      ((#\L) (values `(i64.const ,val) *i64-type*))
      ((#\F) (values `(f32.const ,val) *f32-type*))
      ((#\D) (values `(f64.const ,val) *f64-type*))
      (else ???))))

(define (expand-number expr)
  (cond ((and (integer? expr) (exact? expr))
         (if (<= min-i32 expr max-i32)
             (values `(i32.const ,expr) *i32-type*)
             (values `(i64.const ,expr) *i64-type*)))
        ((number? expr)
         (values `(f64.const ,expr) *f64-type*))
        (else
         (fail "Bad syntax" expr))))

(define (make-expander name expander len)
  (let ((expander (case (car len)
                    ((atleast)
                     (lambda (cx expr env)
                       (if (< (length expr) (cadr len))
                           (fail "Bad" name "expected more operands" expr)
                           (expander cx expr env))))
                    ((oneof)
                     (lambda (cx expr env)
                       (if (not (memv (length expr) (cdr len)))
                           (fail "Bad" name "expected more operands" expr)
                           (expander cx expr env))))
                    ((precisely)
                     (lambda (cx expr env)
                       (if (not (= (length expr) (cadr len)))
                           (fail "Bad" name "expected more operands" expr)
                           (expander cx expr env))))
                    (else
                     ???))))
    (vector 'expander name expander len)))

(define (expander? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'expander)))

(define (expander.name x) (vector-ref x 1))
(define (expander.expander x) (vector-ref x 2))

(define (define-syntax! env)
  (define-env-global! env 'begin    (make-expander 'begin expand-begin '(atleast 2)))
  (define-env-global! env 'if       (make-expander 'if expand-if '(oneof 3 4)))
  (define-env-global! env 'set!     (make-expander 'set! expand-set! '(precisely 3)))
  (define-env-global! env 'inc!     (make-expander 'inc! expand-inc!dec! '(precisely 2)))
  (define-env-global! env 'dec!     (make-expander 'dec! expand-inc!dec! '(precisely 2)))
  (define-env-global! env 'let      (make-expander 'let expand-let '(atleast 3)))
  (define-env-global! env 'loop     (make-expander 'loop expand-loop '(atleast 3)))
  (define-env-global! env 'break    (make-expander 'break expand-break '(oneof 2 3)))
  (define-env-global! env 'continue (make-expander 'continue expand-continue '(precisely 2)))
  (define-env-global! env 'while    (make-expander 'while expand-while '(atleast 2)))
  (define-env-global! env 'case     (make-expander 'case expand-case '(atleast 2)))
  (define-env-global! env '%case    (make-expander '%case expand-%case '(atleast 2)))
  (define-env-global! env 'and      (make-expander 'and expand-and '(precisely 3)))
  (define-env-global! env 'or       (make-expander 'and expand-or '(precisely 3)))
  (define-env-global! env 'trap     (make-expander 'and expand-trap '(oneof 1 2)))
  (define-env-global! env 'new      (make-expander 'new expand-new '(atleast 2)))
  (define-env-global! env 'null     (make-expander 'null expand-null '(precisely 2))))

(define (define-builtins! env)
  (define-env-global! env 'not      (make-expander 'not expand-not '(precisely 2)))
  (define-env-global! env 'select   (make-expander 'select expand-select '(precisely 4)))
  (define-env-global! env 'zero?    (make-expander 'zero? expand-zero? '(precisely 2)))
  (define-env-global! env 'nonzero? (make-expander 'zero? expand-nonzero? '(precisely 2)))
  (define-env-global! env 'null?    (make-expander 'null? expand-null? '(precisely 2)))
  (define-env-global! env 'bitnot   (make-expander 'bitnot expand-bitnot '(precisely 2)))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-unop '(precisely 2))))
            '(clz ctz popcnt neg abs sqrt ceil floor nearest trunc extend8 extend16 extend32))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-binop '(precisely 3))))
            '(+ - * div divu rem remu bitand bitor bitxor shl shr shru rotl rotr max min copysign))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-relop '(precisely 3))))
            '(< <u <= <=u > >u >= >=u = !=))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-conversion '(precisely 2))))
            '(i32->i64 u32->i64 i64->i32 f32->f64 f64->f32 f64->i32 f64->i64 i32->f64 i64->f64
              f32->i32 f32->i64 i32->f32 i64->f32 f32->bits bits->f32 f64->bits bits->f64)))

(define (expand-begin cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
      (cond ((null? exprs)
             (cond ((= (length body) 1)
                    (values (car body) ty))
                   ((void-type? ty)
                    (values `(block ,@(reverse body)) *void-type*))
                   (else
                    (values `(block ,(render-type ty) ,@(reverse body)) ty))))
            ((not (void-type? ty))
             (loop exprs (cons 'drop body) *void-type*))
            (else
             (let-values (((e1 t1) (expand-expr cx (car exprs) env)))
               (loop (cdr exprs) (cons e1 body) t1)))))))

(define (expand-if cx expr env)
  (let-values (((test t0) (expand-expr cx (cadr expr) env)))
    (check-i32-type t0 "'if' condition" expr)
    (case (length expr)
      ((3)
       (let-values (((consequent t1) (expand-expr cx (caddr expr) env)))
         (values `(if ,test ,consequent) *void-type*)))
      ((4)
       (let*-values (((consequent t1) (expand-expr cx (caddr expr) env))
                     ((alternate  t2) (expand-expr cx (cadddr expr) env)))
         (check-same-type t1 t2 "'if' arms" expr)
         (values `(if ,(render-type t1) ,test ,consequent ,alternate) t1)))
      (else
       (fail "Bad 'if'" expr)))))

(define (expand-set! cx expr env)
  (let* ((name (cadr expr))
         (val  (caddr expr)))
    (let-values (((e0 t0) (expand-expr cx val env)))
      (cond ((symbol? name)
             (let ((binding (lookup-variable env name)))
               (cond ((local? binding)
                      (check-same-type t0 (local.type binding) "'set!'" expr)
                      (values `(set_local ,(local.slot binding) ,e0) *void-type*))
                     ((global? binding)
                      (check-same-type t0 (global.type binding) "'set!'" expr)
                      (values `(set_global ,(global.id binding) ,e0) *void-type*))
                     (else
                      ???))))
            ((accessor-expression? env name)
             (let-values (((base-expr cls field-name field-type)
                           (process-accessor-expression cx name env))
                          ((ev tv)
                           (expand-expr cx val env)))
               (check-same-type field-type tv "'set!' object field" expr)
               (values (render-field-update env cls field-name base-expr ev)
                       *void-type*)))
            (else
             (fail "Illegal lvalue" expr))))))

(define (expand-inc!dec! cx expr env)
  (let* ((op   (car expr))
         (name (cadr expr)))
    (cond ((symbol? name)
           (let ((binding (lookup-variable env name)))
             (cond ((local? binding)
                    (let* ((type    (local.type binding))
                           (slot    (local.slot binding))
                           (one     (typed-constant type 1))
                           (op      (operatorize type (if (eq? op 'inc!) '+ '-))))
                      (check-number-type type op expr)
                      (values `(set_local ,slot (,op (get_local ,slot) ,one))
                              *void-type*)))
                   ((global? binding)
                    (let* ((type   (global.type global))
                           (id     (global.id global))
                           (one    (typed-constant type 1))
                           (op     (operatorize type (if (eq? op 'inc!) '+ '-))))
                      (check-number-type type op expr)
                      (values `(set_global ,id (,op (get_global ,id) ,one))
                              *void-type*)))
                   (else
                    ???))))
          ((accessor-expression? env name)
           ;; FIXME: Here we need an extra local to hold the base pointer so
           ;; that we don't evaluate it twice.  So probably pull the same trick
           ;; as for CASE.
           (fail "Not yet implemented: inc! and dec! on field references"))
          (else
           (fail "Illegal lvalue" expr)))))

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

  (let* ((bindings (cadr expr))
         (body     (cddr expr)))
    (let*-values (((new-locals code undos) (process-bindings bindings))
                  ((e0 t0)                 (expand-expr cx `(begin ,@body) (extend-env env new-locals))))
      (unclaim-locals (cx.slots cx) undos)
      (let ((type (if (not (void-type? t0)) (list (render-type t0)) '())))
        (if (not (null? code))
            (if (and (pair? e0) (eq? (car e0) 'begin))
                (values `(block ,@type ,@code ,@(cdr e0)) t0)
                (values `(block ,@type ,@code ,e0) t0))
            (values e0 t0))))))

(define (expand-loop cx expr env)
  (let* ((id   (cadr expr))
         (_    (check-symbol id "Bad loop id" id))
         (body (cddr expr))
         (loop (make-loop id (new-name cx "brk") (new-name cx "cnt") #f))
         (env  (extend-env env (list (cons id loop)))))
    (let-values (((e0 t0) (expand-expr cx `(begin ,@body) env)))
      (values `(block ,(loop.break loop)
                      ,@(if (void-type? (loop.type loop))
                            '()
                            (list (render-type (loop.type loop))))
                      (loop ,(loop.continue loop)
                            ,e0
                            (br ,(loop.continue loop))))
              (loop.type loop)))))

(define (expand-break cx expr env)
  (let* ((id   (cadr expr))
         (_    (check-symbol id "Bad loop id" id))
         (e    (if (null? (cddr expr)) #f (caddr expr)))
         (loop (lookup-loop env id)))
    (if e
        (let-values (((e0 t0) (expand-expr cx e env)))
          (loop.set-type! loop t0)
          (values `(br ,(loop.break loop) ,e0) *void-type*))
        (begin
          (loop.set-type! loop *void-type*)
          (values `(br ,(loop.break loop)) *void-type*)))))

(define (expand-continue cx expr env)
  (let* ((id   (cadr expr))
         (_    (check-symbol id "Bad loop id" id))
         (loop (lookup-loop env id)))
    (values `(br ,(loop.continue loop)) *void-type*)))

(define (expand-while cx expr env)
  (let* ((block-name (new-name cx "brk"))
         (loop-name  (new-name cx "cnt")))
    (values `(block ,block-name
                    (loop ,loop-name
                          ,(let-values (((ec tc) (expand-expr cx (cadr expr) env)))
                             (check-i32-type tc "'while' condition" expr)
                             `(br_if ,block-name (i32.eqz ,ec)))
                          ,(let-values (((e0 t0) (expand-expr cx `(begin ,@(cddr expr)) env)))
                             e0)
                          (br ,loop-name)))
            *void-type*)))

(define (expand-case cx expr env)
  (let ((temp (new-name cx "local")))
    (expand-expr cx
                 `(let ((,temp ,(cadr expr)))
                    (%case ,temp ,@(cddr expr)))
                 env)))

;; The dispatch expr is always effect-free, and we make use of this.

(define (expand-%case cx expr env)

  (define (check-case-types cases default-type)
    (for-each (lambda (c)
                (check-same-type (caddr c) default-type "'case' arm"))
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
               (bty           (if (void-type? ty) '() (list (render-type ty))))
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
                           (check-i32-type t "'case' constant")
                           v))
                        ((number? c)
                         (let-values (((v t) (expand-number c)))
                           (check-i32-type t "'case' constant")
                           v))
                        ((and (symbol? c) (lookup-global env c)) =>
                         (lambda (g)
                           (if (and (not (global.import g))
                                    (not (global.mut? g))
                                    (i32-type? (global.type g)))
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

  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-i32-type t0 "'case' expression")
    (let loop ((cases (cddr expr)) (case-info '()) (found-values '()))
      (cond ((null? cases)
             (finish-case found-values c0 case-info #f *void-type*))

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
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
                ((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-i32-type t1 "'and'" expr)
    (check-i32-type t2 "'and'" expr)
    (values `(if i32 ,op1 ,op2 (i32.const 0)) *i32-type*)))

(define (expand-or cx expr env)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
                ((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-i32-type t1 "'or'" expr)
    (check-i32-type t2 "'or'" expr)
    (values `(if i32 ,op1 (i32.const 1) ,op2) *i32-type*)))

(define (expand-trap cx expr env)
  (let ((t (if (null? (cdr expr))
               *void-type*
               (parse-type cx env (cadr expr)))))
    (values '(unreachable) t)))

;; Returns ((val type) ...)

(define (expand-expressions cx env exprs)
  (map (lambda (e)
         (let-values (((e0 t0) (expand-expr cx e env)))
           (list e0 t0)))
       exprs))

;; formals is ((name type) ...)
;; actuals is ((val type) ...)

(define (check-arguments formals actuals context)
  (if (not (= (length formals) (length actuals)))
      (fail "wrong number of arguments" context))
  (for-each (lambda (formal actual)
              (let ((want (cadr formal))
                    (have (cadr actual)))
                (check-same-type have want "argument" context)))
            formals actuals))

(define (expand-new cx expr env)
  (let ((name (cadr expr)))
    (check-symbol name "Class name required" expr)
    (let* ((cls     (lookup-class env name))
           (fields  (class.fields cls))
           (actuals (expand-expressions cx env (cddr expr))))
      (check-arguments fields actuals expr)
      (values `(render-new env cls actuals) (class.type cls)))))

(define (expand-null cx expr env)
  (let ((name (cadr expr)))
    (check-symbol name "Class name required" expr)
    (let ((cls (lookup-class env name)))
      (values (render-null env cls) (class.type cls)))))

(define (expand-null? cx expr env)
  (let-values (((e t) (expand-expr cx (cadr expr) env)))
    (check-class-type t "'null?'" expr)
    (values (render-null? env e) *i32-type*)))

(define (expand-zero? cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-number-type t1 "'zero?'" expr)
    (values `(,(operatorize t1 '=) ,op1 ,(typed-constant t1 0)) *i32-type*)))

(define (expand-nonzero? cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-number-type t1 "'nonzero?'" expr)
    (values `(,(operatorize t1 '!=) ,op1 ,(typed-constant t1 0)) *i32-type*)))

(define (expand-bitnot cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-integer-type t1 "'bitnot'" expr)
    (values `(,(operatorize t1 'bitxor) ,op1 ,(typed-constant t1 -1)) t1)))

(define (expand-select cx expr env)
  (let*-values (((op t)   (expand-expr cx (cadr expr) env))
                ((op1 t1) (expand-expr cx (caddr expr) env))
                ((op2 t2) (expand-expr cx (cadddr expr) env)))
    (check-i32-type t "'select' condition" expr)
    (check-same-type t1 t2 "'select' arms" expr)
    (values `(select ,op2 ,op1 ,op) t1)))

(define (expand-not cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-i32-type t1 "'not'")
    (values `(i32.eqz ,op1) *i32-type*)))

(define (expand-unop cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (values `(,(operatorize t1 (car expr)) ,op1) t1)))

(define (expand-binop cx expr env)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
                ((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-same-type t1 t2 "binary operator" (car expr))
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) t1)))

(define (expand-relop cx expr env)
  (let*-values (((op1 t1) (expand-expr cx (cadr expr) env))
                ((op2 t2) (expand-expr cx (caddr expr) env)))
    (check-same-type t1 t2 "relational operator" (car expr))
    (values `(,(operatorize t1 (car expr)) ,op1 ,op2) *i32-type*)))

(define (expand-conversion cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (let ((probe (assq (car expr) *conv-op*)))
      (if probe
          (begin (check-same-type t0 (cadr probe) (car expr) expr)
                 (values `(,(cadddr probe) ,e0) (caddr probe)))
          ???))))

(define *conv-op*
  (list (list 'i32->i64 *i32-type* *i64-type* 'i64.extend_s/i32)
        (list 'u32->i64 *i32-type* *i64-type* 'i64.extend_u/i32)
        (list 'i64->i32 *i64-type* *i32-type* 'i32.wrap/i64)
        (list 'f32->f64 *f32-type* *f64-type* 'f64.promote/f32)
        (list 'f64->f32 *f64-type* *f32-type* 'f32.demote/f64)
        (list 'f64->i32 *f64-type* *i32-type* 'i32.trunc_s/f64)
        (list 'f64->i64 *f64-type* *i64-type* 'i64.trunc_s/f64)
        (list 'i32->f64 *i32-type* *f64-type* 'f64.convert_s/i32)
        (list 'i64->f64 *i64-type* *f64-type* 'f64.convert_s/i64)
        (list 'f32->i32 *f32-type* *i32-type* 'i32.trunc_s/f32)
        (list 'f32->i64 *f32-type* *i64-type* 'i64.trunc_s/f32)
        (list 'i32->f32 *i32-type* *f32-type* 'f32.convert_s/i32)
        (list 'i64->f32 *i64-type* *f32-type* 'f32.convert_s/i64)
        (list 'f32->bits *f32-type* *i32-type* 'i32.reinterpret/f32)
        (list 'bits->f32 *i32-type* *f32-type* 'f32.reinterpret/i32)
        (list 'f64->bits *f64-type* *i64-type* 'i64.reinterpret/f64)
        (list 'bits->f64 *i64-type* *f64-type* 'f64.reinterpret/i64)))

;; For this to work, every class defn must introduce a number of imports,
;; corresponding to the operations.  These must be processed early enough
;; for things to "work out".

(define (expand-accessor cx expr env)
  (check-list expr 2 "Bad accessor" expr)
  (let-values (((base-expr cls field-name field-type)
                (process-accessor-expression cx expr env)))
    (values (render-field-access env cls field-name base-expr)
            field-type)))

;; Returns rendered-base-expr class field-name field-type

(define (process-accessor-expression cx expr env)
  (let* ((name       (car expr))
         (accessor   (lookup env name))
         (field-name (accessor.field-name accessor))
         (base       (cadr expr)))
    (let-values (((e0 t0) (expand-expr cx base env)))
      (if (not (type.class t0))
          (fail "Not a class type" expr t0))
      (let* ((cls    (type.class t0))
             (fields (class.fields cls))
             (probe  (assq field-name fields)))
        (if (not probe)
            (fail "Class does not have field" field-name cls))
        (values e0 cls field-name (cadr probe))))))

(define (expand-call cx expr env)
  (let* ((name    (car expr))
         (args    (cdr expr))
         (func    (lookup-func env name))
         (formals (func.formals func))
         (actuals (expand-expressions cx env args)))
    (check-arguments formals actuals expr)
    (values `(call ,(func.id func) ,@(map car actuals))
            (func.result func))))

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

(define (operatorize t op . context)
  (let ((name (cond ((i32-type? t)
                     (let ((probe (or (assq op *int-ops*) (assq op *common-ops*))))
                       (and probe (memq 'i32 (cddr probe)) (cadr probe))))
                    ((i64-type? t)
                     (let ((probe (or (assq op *int-ops*) (assq op *common-ops*))))
                       (and probe (memq 'i64 (cddr probe)) (cadr probe))))
                    ((f32-type? t)
                     (let ((probe (or (assq op *float-ops*) (assq op *common-ops*))))
                       (and probe (memq 'f32 (cddr probe)) (cadr probe))))
                    ((f64-type? t)
                     (let ((probe (or (assq op *float-ops*) (assq op *common-ops*))))
                       (and probe (memq 'f64 (cddr probe)) (cadr probe))))
                    (else
                     ???))))
    (if (not name)
        (apply fail `("Unexpected type for" ,op ,(pretty-type t))))
    (string->symbol (string-append (symbol->string (type.name t)) name))))

(define *int-ops*
  '((clz ".clz" i32 i64)
    (ctz ".ctz" i32 i64)
    (popcnt ".popcnt" i32 i64)
    (div ".div_s" i32 i64)
    (divu ".div_u" i32 i64)
    (rem ".rem_s" i32 i64)
    (remu ".rem_u" i32 i64)
    (< ".lt_s" i32 i64)
    (<u ".lt_u" i32 i64)
    (<= ".le_s" i32 i64)
    (<=u ".le_u" i32 i64)
    (> ".gt_s" i32 i64)
    (>u ".gt_u" i32 i64)
    (>= ".ge_s" i32 i64)
    (>=u ".ge_u" i32 i64)
    (bitand ".and" i32 i64)
    (bitor ".or" i32 i64)
    (bitxor ".xor" i32 i64)
    (shl ".shl" i32 i64)
    (shr ".shr_s" i32 i64)
    (shru ".shr_u" i32 i64)
    (rotl ".rotl" i32 i64)
    (rotr ".rotr" i32 i64)
    (extend8 ".extend8_s" i32 i64)
    (extend16 ".extend16_s" i32 i64)
    (extend32 ".extend32_s" i64)))

(define *float-ops*
  '((div ".div" f32 f64)
    (< ".lt" f32 f64)
    (<= ".le" f32 f64)
    (> ".gt" f32 f64)
    (>= ".ge" f32 f64)
    (neg ".neg" f32 f64)
    (abs ".abs" f32 f64)
    (min ".min" f32 f64)
    (max ".max" f32 f64)
    (sqrt ".sqrt" f32 f64)
    (ceil ".ceil" f32 f64)
    (floor ".floor" f32 f64)
    (nearest ".nearest" f32 f64)
    (trunc ".trunc" f32 f64)))

(define *common-ops*
  '((+ ".add" i32 i64 f32 f64)
    (- ".sub" i32 i64 f32 f64)
    (* ".mul" i32 i64 f32 f64)
    (= ".eq" i32 i64 f32 f64)
    (!= ".ne" i32 i64 f32 f64)))

(define (typed-constant t value)
  `(,(string->symbol (string-append (symbol->string (type.name t)) ".const"))
    ,value))

;; Sundry

(define (numbery-symbol? x)
  (and (symbol? x)
       (let ((name (symbol->string x)))
         (and (> (string-length name) 2)
              (char=? #\. (string-ref name 1))
              (memv (string-ref name 0) '(#\I #\L #\F #\D))))))

(define (pretty-type x)
  (case (type.name x)
    ((i32 i64 f32 f64 void) (type.name x))
    ((class) `(class ,(class.name (type.class x))))
    (else x)))

(define (check-unbound env name reason)
  (if (lookup env name)
      (fail "Name cannot be bound because" reason name)))

(define (check-number-type t context . rest)
  (if (not (number-type? t))
      (apply fail `("Not a number type in" ,context ,@rest "\n" ,(pretty-type t)))))

(define (check-i32-type t context . rest)
  (if (not (i32-type? t))
      (apply fail `("Not an i32 type in" ,context ,@rest "\n" ,(pretty-type t)))))

(define (check-integer-type t context . rest)
  (if (not (integer-type? t))
      (apply fail `("Not an integer type in" ,context ,@rest "\n" ,(pretty-type t)))))

(define (check-same-type t1 t2 context . rest)
  (if (not (same-type? t1 t2))
      (apply fail `("Not same type in" ,context ,@rest "\n" ,(pretty-type t1) ,(pretty-type t2)))))

(define (check-class-type t context . rest)
  (if (not (class-type? t))
      (apply fail `("Not class type in" ,context ,@rest "\n" ,(pretty-type t)))))

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
      (*leave* #f)
      (error "FAILED!")))

(define (string-join ss sep)
  (if (null? ss)
      ""
      (let loop ((tt (list (car ss))) (ss (cdr ss)))
        (if (null? ss)
            (apply string-append (reverse tt))
            (loop (cons (car ss) (cons sep tt)) (cdr ss))))))

;; Host support

(define (make-support)
  (vector 'support
          (open-output-string)          ; for type constructors
          (open-output-string)))        ; for library code

(define (support.type-output x) (vector-ref x 1))
(define (support.lib-output x) (vector-ref x 2))

;; Driver for scripts

(define (swat-noninteractive)
  (define js-mode #f)
  (define stdout-mode #f)
  (define files '())

  (let ((result
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
                           (exit 1))
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
                      files)
            #t))))
    (if (not result)
        (exit 1))))

(define (process-file in out js-mode)
  (do ((phrase (read in) (read in)))
      ((eof-object? phrase))
    (cond ((and (pair? phrase) (eq? (car phrase) 'defmodule))
           (let ((support (make-support)))
             (let-values (((name code) (expand-module phrase support)))
               (write-module out name support code js-mode))))
          ((and (pair? phrase) (eq? (car phrase) 'js))
           (write-js out (cadr phrase) js-mode))
          (else
           (fail "Bad toplevel phrase" phrase)))))

(define (input-name->output-name filename js-mode)
  (if js-mode
      (string-append (substring filename 0 (- (string-length filename) 5)) ".wast.js")
      (string-append (substring filename 0 (- (string-length filename) 5)) ".wast")))

(define (write-module out name support code js-mode)
  (if js-mode
      (begin
        (format out "var ~a =\n(function () {\nvar TO=TypedObject;\nvar self = {\n" name)
        (format out "module:\nnew WebAssembly.Module(wasmTextToBinary(`")
        (pretty-print code out)
        (format out "`)),\n")
        (format out "lib:\n{\n")
        (format out "~a\n" (get-output-string (support.lib-output support)))
        (format out "},\n")
        (format out "types:\n{\n")
        (format out "~a\n" (get-output-string (support.type-output support)))
        (format out "}};\nreturn self })();"))
      (begin
        (display (string-append ";; " name) out)
        (newline out)
        (pretty-print code out))))

(define (write-js out code js-mode)
  (if js-mode
      (begin
        (display code out)
        (newline out))))

;; Driver for testing and interactive use

(define (swat filename)
  (handle-failure
   (lambda ()
     (call-with-input-file filename
       (lambda (f)
         (let loop ((phrase (read f)))
           (if (not (eof-object? phrase))
               (begin
                 (if (and (list? phrase) (not (null? phrase)) (eq? (car phrase) 'defmodule))
                     (let-values (((name code) (expand-module phrase (make-support))))
                       (display (string-append ";; " name))
                       (newline)
                       (pretty-print code)))
                 (loop (read f))))))))))
