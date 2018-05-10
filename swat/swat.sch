;;; -*- fill-column: 80; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public License,
;;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;;; obtain one at <http://mozilla.org/MPL/2.0/>.
;;;
;;; This is r5rs-ish Scheme, it works with Larceny 1.3 (http://larcenists.org).

;;; Swat is a mostly Scheme-syntaxed statically typed language that targets
;;; WebAssembly.  See MANUAL.md for more information.
;;;
;;; This program translates Swat programs to the WebAssembly text format
;;; accepted by Firefox's wasmTextToBinary, plus supporting JS code.
;;;
;;; See the functions "swat" and "swat-noninteractive" for sundry ways to run
;;; this program, and see the shell script "swat" for a command line interface.

;; Environments.
;;
;; These map names to the entities they denote in the program.  There is a
;; single lexically scoped namespace for everything, including loop labels.

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

(define (lookup-predicated env name match? tag)
  (let ((probe (lookup env name)))
    (cond ((not probe)
           (fail "No binding for" name))
          ((match? probe)
           probe)
          (else
           (fail "Binding does not denote a" tag name probe)))))

(define (lookup-type env name)
  (lookup-predicated env name type? "type"))

(define (lookup-func env name)
  (lookup-predicated env name func? "function"))

(define (lookup-func-or-virtual env name)
  (lookup-predicated env name (lambda (x) (or (func? x) (virtual? x))) "function"))

(define (lookup-virtual env name)
  (lookup-predicated env name virtual? "virtual"))

(define (lookup-global env name)
  (lookup-predicated env name global? "global"))

(define (lookup-loop env name)
  (lookup-predicated env name loop? "loop"))

(define (lookup-variable env name)
  (lookup-predicated env name (lambda (x)
                                (or (local? x) (global? x)))
                     "variable"))

(define (lookup-class env name)
  (let ((probe (lookup-type env name)))
    (if (not (type.class probe))
        (fail "Not a class type" name))
    (type.class probe)))

(define (funcs env)
  (reverse (filter func? (map cdr (env.globals env)))))

(define (virtuals env)
  (reverse (filter virtual? (map cdr (env.globals env)))))

(define (funcs-and-virtuals env)
  (reverse (filter (lambda (x) (or (func? x) (virtual? x))) (map cdr (env.globals env)))))

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

;; Translation contexts.

(define (make-cx name support)
  (vector #f                            ; Slots storage (during body expansion)
          0                             ; Next function ID
          0                             ; Next global ID
          0                             ; Gensym ID
          0                             ; Next virtual function ID (for dispatch)
          support                       ; host support code (opaque structure)
          name                          ; module name
          0                             ; Next table index
          '()                           ; List of table entries in reverse order
          '()                           ; Function types ((type . id) ...) where
                                        ; id is some gensym name and type is a
                                        ; rendered func type, we use ids to refer
                                        ; to the type because wasmTextToBinary inserts
                                        ; additional types.  We can search the list with assoc.
          '()                           ; String literals ((string . id) ...)
          0))                           ; Next string literal id

(define (cx.slots cx)            (vector-ref cx 0))
(define (cx.slots-set! cx v)     (vector-set! cx 0 v))
(define (cx.func-id cx)          (vector-ref cx 1))
(define (cx.func-id-set! cx v)   (vector-set! cx 1 v))
(define (cx.global-id cx)        (vector-ref cx 2))
(define (cx.global-id-set! cx v) (vector-set! cx 2 v))
(define (cx.gensym-id cx)        (vector-ref cx 3))
(define (cx.gensym-id-set! cx v) (vector-set! cx 3 v))
(define (cx.vid cx)              (vector-ref cx 4))
(define (cx.vid-set! cx v)       (vector-set! cx 4 v))
(define (cx.support cx)          (vector-ref cx 5))
(define (cx.name cx)             (vector-ref cx 6))
(define (cx.table-index cx)      (vector-ref cx 7))
(define (cx.table-index-set! cx v) (vector-set! cx 7 v))
(define (cx.table-elements cx)   (vector-ref cx 8))
(define (cx.table-elements-set! cx v) (vector-set! cx 8 v))
(define (cx.types cx)            (vector-ref cx 9))
(define (cx.types-set! cx v)     (vector-set! cx 9 v))
(define (cx.strings cx)          (vector-ref cx 10))
(define (cx.strings-set! cx v)   (vector-set! cx 10 v))
(define (cx.string-id cx)        (vector-ref cx 11))
(define (cx.string-id-set! cx v) (vector-set! cx 11 v))

;; Gensym.

(define (new-name cx tag)
  (let ((n (cx.gensym-id cx)))
    (cx.gensym-id-set! cx (+ n 1))
    (splice "$" tag "_" n)))

;; Modules

;; Special forms that appear at the top level of the module are not handled by
;; standard expanders but mustn't be redefined at that level, so must be
;; defined as keywords.
;;
;; defvirtual- actually makes no sense - a virtual would always be imported as a
;; func - but we reserve it anyway.

(define (define-keywords! env)
  (for-each (lambda (name)
              (define-env-global! env name '*keyword*))
            '(defun defun+ defun- defvar defvar+ defvar- defconst defconst+ defconst-
              defclass defclass+ defclass- defvirtual defvirtual+ defvirtual-)))

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
                  ((defun defun+ defconst defconst+ defvar defvar+ defvirtual defvirtual+)
                   #t)
                  (else
                   (fail "Unknown top-level phrase" d))))
              body)
    (resolve-classes cx env)
    (synthesize-class-ops cx env)
    (synthesize-string-ops cx env)
    (for-each (lambda (d)
                (case (car d)
                  ((defun defun+)
                   (expand-func-phase1 cx env d))
                  ((defvirtual defvirtual+)
                   (expand-virtual-phase1 cx env d))
                  ((defconst defconst+ defvar defvar+)
                   (expand-global-phase1 cx env d))))
              body)
    (for-each (lambda (d)
                (case (car d)
                  ((defun defun+)
                   (expand-func-phase2 cx env d))
                  ((defvirtual defvirtual+)
                   (expand-virtual-phase2 cx env d))
                  ((defconst defconst+ defvar defvar+)
                   (expand-global-phase2 cx env d))))
              body)
    (compute-dispatch-maps cx env)
    (synthesize-class-descs cx env)
    (synthesize-string-literals cx env)
    (values name
            (cons 'module
                  (append
                   (generate-types cx env)
                   (generate-tables cx env)
                   (generate-globals cx env)
                   (generate-functions cx env))))))

(define (generate-types cx env)
  (reverse (map (lambda (x)
                  `(type ,(cdr x) ,(car x)))
                (cx.types cx))))

(define (generate-tables cx env)
  (if (not (null? (cx.table-elements cx)))
      `((table anyfunc (elem ,@(reverse (cx.table-elements cx)))))
      '()))

(define (generate-globals cx env)
  (map (lambda (g)
         (let* ((t (render-type (global.type g)))
                (t (if (global.mut? g) `(mut ,t) t)))
           (if (global.module g)
               `(import ,(global.module g) ,(symbol->string (global.name g)) (global ,t))
               `(global ,@(if (global.export? g) `((export ,(symbol->string (global.name g)))) '())
                        ,t
                        ,(global.init g)))))
       (globals env)))

(define (generate-functions cx env)
  (map (lambda (f)
         (if (func.module f)
             `(import ,(func.module f) ,(symbol->string (func.name f)) ,(assemble-function f '()))
             (func.defn f)))
       (funcs-and-virtuals env)))

;; Classes

(define (make-class name base fields)
  (vector 'class
          name                          ; Class name as symbol
          base                          ; Base class object, or #f in Object
          fields                        ; ((name type-name) ...)
          #f                            ; #t iff class has been resolved
          #f                            ; Type object referencing this class object
          #f                            ; Host system information
          '()                           ; Virtuals map: Map from virtual-function-id to function
                                        ;   when function is called on instance of this class
                                        ;   as list ((vid function) ...), the function stores
                                        ;   its own table index
          '()))                         ; List of direct subclasses, unordered

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
(define (class.host c) (vector-ref c 6))
(define (class.host-set! c v) (vector-set! c 6 v))
(define (class.virtuals c) (vector-ref c 7))
(define (class.virtuals-set! c v) (vector-set! c 7 v))
(define (class.subclasses c) (vector-ref c 8))
(define (class.subclasses-set! c v) (vector-set! c 8 v))

(define (class=? a b)
  (eq? a b))

(define (format-class cls)
  (class.name cls))

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
  (let* ((name  (splice "*" field-name))
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
    (if (not (char-upper-case? (string-ref (symbol->string name) 0)))
        (fail "Class names should have an initial upper case letter"))
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
                        (values 'Object rest)))))
      (let loop ((body body) (fields '()))
        (if (null? body)
            (define-class! cx env name base (reverse fields))
            (let ((c (car body)))
              (cond ((not (and (list? c) (not (null? c))))
                     (fail "Invalid clause in defclass" c d))
                    ((eq? (car c) 'extends)
                     (fail "Invalid extends clause in defclass" c d))
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
              (class.base-set! cls base)
              (class.subclasses-set! base (cons cls (class.subclasses base)))))
        (let ((base-fields (if (class.base cls)
                               (class.fields (class.base cls))
                               '())))
          (let ((fields (map (lambda (f)
                               (let ((name (car f))
                                     (ty   (parse-type cx env (cadr f))))
                                 (if (class? ty)
                                     (resolve-class cx env (type.class ty) (cons cls forbidden)))
                                 (if (and (vector? ty) (class? (type.vector-element ty)))
                                     (resolve-class cx env (type.class (type.vector-element ty)) (cons (type.vector-element ty) forbidden)))
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

(define (make-basefunc name module export? id rendered-params formals result slots env)
  (let ((defn        #f)
        (table-index #f))
    (vector 'basefunc
            name                        ; Function name as a symbol
            module                      ; Module name as a string, or #f if not imported
            export?                     ; #t iff exported, otherwise #f
            id                          ; Function index in Wasm module
            rendered-params             ; ((name type-name) ...)
            formals                     ; ((name type) ...)n
            result                      ; type
            slots                       ; as returned by make-slots
            env                         ; Environment extended by parameters
            defn                        ; Generated wasm code as s-expression
            table-index)))              ; Index in the default table, or #f

(define (func.name f) (vector-ref f 1))
(define (func.module f) (vector-ref f 2)) ; Either #f or a string naming the module
(define (func.export? f) (vector-ref f 3))
(define (func.id f) (vector-ref f 4))
(define (func.rendered-params f) (vector-ref f 5))
(define (func.formals f) (vector-ref f 6))
(define (func.result f) (vector-ref f 7))
(define (func.slots f) (vector-ref f 8))
(define (func.env f) (vector-ref f 9))
(define (func.defn f) (vector-ref f 10))
(define (func.defn-set! f v) (vector-set! f 10 v))
(define (func.table-index f) (vector-ref f 11))
(define (func.table-index-set! f v) (vector-set! f 11 v))

(define (format-func fn)
  (func.name fn))

;; func and virtual are subclasses of basefunc, so the vectors created here
;; *must* be laid out exactly as basefunc, but can have additional fields.

(define (make-func name module export? id rendered-params formals result slots env)
  (let ((defn        #f)
        (table-index #f))
    (vector 'func
            name module export? id rendered-params formals result slots env defn table-index)))

(define (func? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'func)))

(define (make-virtual name module export? id rendered-params formals result slots env vid)
  (let ((defn               #f)
        (table-index        #f)
        (uber-discriminator #f)
        (discriminators     #f))
    (vector 'virtual
            name module export? id rendered-params formals result slots env defn table-index
            vid                         ; Virtual function ID (a number)
            uber-discriminator          ; The class obj named in the virtual's signature
            discriminators)))           ; ((class func) ...) computed from body, unsorted

(define (virtual? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'virtual)))

(define (virtual.vid x) (vector-ref x 12))
(define (virtual.uber-discriminator x) (vector-ref x 13))
(define (virtual.uber-discriminator-set! x v) (vector-set! x 13 v))
(define (virtual.discriminators x) (vector-ref x 14))
(define (virtual.discriminators-set! x v) (vector-set! x 14 v))

;; formals is ((name type) ...)

(define (define-function! cx env name module export? params formals result-type slots)
  (let* ((id   (cx.func-id cx))
         (func (make-func name module export? id params formals result-type slots env)))
    (cx.func-id-set! cx (+ id 1))
    (define-env-global! env name func)
    func))

(define (assemble-function func body)
  (let ((f (prepend-signature (func.name func)
                              (func.rendered-params func)
                              (func.result func)
                              (func.export? func)
                              body)))
    (func.defn-set! func f)
    f))

(define (expand-func-phase1 cx env f)
  (expand-func-or-virtual-phase1
   cx env f "function"
   (lambda (cx env name module export? params formals result slots)
     (define-function! cx env name module export? params formals result slots))))

(define (expand-func-phase2 cx env f)
  (let* ((signature (cadr f))
         (body      (cddr f)))
    (let-values (((_ name) (parse-toplevel-name (car signature) #t "")))
      (let* ((func  (lookup-func env name))
             (env   (func.env func))
             (slots (func.slots func)))
        (cx.slots-set! cx slots)
        (assemble-function func (expand-func-body cx body (func.result func) env))))))

(define (expand-func-body cx body expected-type env)
  (let-values (((expanded result-type) (expand-expr cx (cons 'begin body) env)))
    (let ((drop? (and (void-type? expected-type)
                      (not (void-type? result-type)))))
      (if (not drop?)
          (let ((val+ty (widen-value env expanded result-type expected-type)))
            (if val+ty
                `(,@(get-slot-decls (cx.slots cx)) ,(car val+ty))
                (fail "Return type mismatch" (pretty-type expected-type) (pretty-type result-type))))
          `(,@(get-slot-decls (cx.slots cx)) ,expanded (drop))))))

(define (prepend-signature name rendered-params result-type export? body)
  (let* ((f body)
         (f (if (void-type? result-type)
                f
                (cons `(result ,(render-type result-type)) f)))
         (f (append rendered-params f))
         (f (if export?
                (cons `(export ,(symbol->string name)) f)
                f))
         (f (cons 'func f)))
    f))

(define (expand-func-or-virtual-phase1 cx env f tag k)
  (check-list-atleast f 2 (string-append "Bad " tag) f)
  (let* ((import?   (memq (car f) '(defun- defvirtual-)))
         (signature (cadr f))
         (_         (check-list-atleast signature 1 "Bad signature" signature))
         (export?   (memq (car f) '(defun+ defvirtual+)))
         (body      (cddr f))
         (slots     (make-slots)))
    (if (and import? (not (null? body)))
        (fail "Import function can't have a body" f))
    (let-values (((module name) (parse-toplevel-name (car signature) import? tag)))
      (check-unbound env name "already defined at global level")
      (cx.slots-set! cx #f)
      (let loop ((xs       (cdr signature))
                 (bindings '())
                 (formals  '())
                 (params   '()))
        (cond ((null? xs)
               (k cx (extend-env env bindings) name module export?
                  (reverse params) (reverse formals) *void-type* slots))

              ((eq? (car xs) '->)
               (check-list xs 2 "Bad signature" signature)
               (let ((t (parse-type cx env (cadr xs))))
                 (k cx (extend-env env bindings) name module export?
                    (reverse params) (reverse formals) t slots)))

              (else
               (let ((first (car xs)))
                 (check-list first 2 "Bad parameter" first f)
                 (check-symbol (car first) "Bad parameter name" first)
                 (let ((t    (parse-type cx env (cadr first)))
                       (name (car first)))
                   (if (assq name bindings)
                       (fail "Duplicate parameter" name))
                   (let-values (((slot _) (claim-param slots t)))
                     (loop (cdr xs)
                           (cons (cons name (make-local name slot t)) bindings)
                           (cons (list name t) formals)
                           (cons `(param ,(render-type t)) params)))))))))))

;; Virtuals

;; See above for make-virtual and virtual? and additional accessors

(define (expand-virtual-phase1 cx env f)
  (expand-func-or-virtual-phase1
   cx env f "virtual"
   (lambda (cx env name module export? params formals result-type slots)
     (if (not (> (length formals) 0))
         (fail "Virtual function requires at least one argument" f))
     (let* ((first (car formals))
            (name  (car first))
            (type  (cadr first)))
       (if (not (eq? name 'self))
           (fail "Name of first argument to virtual must be 'self'" f))
       (if (not (class-type? type))
           (fail "Type of first argument to virtual must be a class" f)))
     (let* ((id   (cx.func-id cx))
            (vid  (cx.vid cx))
            (virt (make-virtual name module export? id params formals result-type slots env vid)))
       (cx.func-id-set! cx (+ id 1))
       (cx.vid-set! cx (+ vid 1))
       (define-env-global! env name virt)))))

(define (expand-virtual-phase2 cx env f)
  (let* ((signature (cadr f))
         (name      (car signature))
         (disc-name (cadr (cadr signature)))
         (body      (cddr f))
         (virt      (lookup-virtual env name))
         (v-formals (func.formals virt))
         (v-result  (func.result virt))
         (disc-cls  (lookup-class env disc-name))
         (discs     '()))

    ;; Check syntax and type relationships

    (for-each
     (lambda (clause)
       (check-list clause 2 "Virtual dispatch clause" clause f)
       (let ((clause-name (car clause))
             (clause-fn   (cadr clause)))
         (check-symbol clause-name "Virtual dispatch clause" clause-name f)
         (check-symbol clause-fn "Virtual dispatch clause" clause-fn f)
         (let ((clause-cls (lookup-class env clause-name)))
           (if (not (subclass? clause-cls disc-cls))
               (fail "Virtual dispatch clause" clause-cls clause))
           (let ((meth (lookup-func-or-virtual env clause-fn)))
             (let ((fn-formals (func.formals meth))
                   (fn-result  (func.result meth)))
               (if (not (= (length fn-formals) (length v-formals)))
                   (fail "Virtual method mismatch: arguments" f meth))
               (for-each (lambda (fn-arg v-arg)
                           (if (not (type=? (cadr fn-arg) (cadr v-arg)))
                               (fail "Virtual method mismatch: arguments" f meth)))
                         (cdr fn-formals) (cdr v-formals))
               (if (not (subtype? fn-result v-result))
                   (fail "Virtual method mismatch: result" f clause))
               (let* ((fn-first (car fn-formals))
                      (fn-first-ty (cadr fn-first)))
                 (if (not (class-type? fn-first-ty))
                     (fail "Method discriminator" clause))
                 (let ((fn-first-cls (type.class fn-first-ty)))
                   (if (not (subclass? clause-cls fn-first-cls))
                       (fail "Method discriminator" clause "\n" clause-cls "\n" fn-first-cls))
                   (set! discs (cons (list clause-cls meth) discs)))))))))
     body)

    ;; Check for duplicated type discriminators

    (do ((body body (cdr body)))
        ((null? body))
      (let ((b (car body)))
        (if (assq (car b) (cdr body))
            (fail "Duplicate name in virtual dispatch" f))))

    ;; Save information about the types and functions

    (virtual.uber-discriminator-set! virt disc-cls)
    (virtual.discriminators-set! virt (reverse discs))

    ;; Assign table IDs to the functions

    (for-each (lambda (b)
                (let ((func (lookup-func-or-virtual env (cadr b))))
                  (if (not (func.table-index func))
                      (let ((index (cx.table-index cx)))
                        (cx.table-index-set! cx (+ index 1))
                        (func.table-index-set! func index)
                        (cx.table-elements-set! cx (cons (func.id func) (cx.table-elements cx)))))))
              body)

    ;; Hash-cons the signature

    (let ((typeref
           (let ((t `(func ,@(func.rendered-params virt)
                           ,@(if (not (void-type? (func.result virt)))
                                 `((result ,(render-type (func.result virt))))
                                 '()))))
             (let ((probe (assoc t (cx.types cx))))
               (if probe
                   (cdr probe)
                   (let ((name (new-name cx "ty")))
                     (cx.types-set! cx (cons (cons t name) (cx.types cx)))
                     name))))))

      ;; Create the body

      (assemble-virtual
       virt
       `((call_indirect ,typeref
                        ,@(do ((i  0         (+ i 1))
                               (fs v-formals (cdr fs))
                               (xs '()       (cons `(get_local ,i) xs)))
                              ((null? fs) (reverse xs)))
                        ,(render-resolve-virtual env '(get_local 0) `(i32.const ,(virtual.vid virt)))))))))

(define (assemble-virtual virtual body)
  (let ((f (prepend-signature (func.name virtual)
                              (func.rendered-params virtual)
                              (func.result virtual)
                              (func.export? virtual)
                              body)))
    (func.defn-set! virtual f)
    f))

(define (transitive-subclasses cls)
  (cons cls (apply append (map transitive-subclasses (class.subclasses cls)))))

(define (closest-discriminator cls discs)
  (let loop ((discs discs) (best #f))
    (cond ((null? discs)
           (assert best)
           best)
          ((not (subclass? cls (caar discs)))
           (loop (cdr discs) best))
          ((not best)
           (loop (cdr discs) (car discs)))
          ((subclass? (car best) (caar discs))
           (loop (cdr discs) best))
          (else
           (loop (cdr discs) (car discs))))))

(define (compute-dispatch-maps cx env)
  (for-each (lambda (v)
              (let ((uber  (virtual.uber-discriminator v))
                    (discs (virtual.discriminators v))
                    (vid   (virtual.vid v)))

                ;; Do we need an error function at the top discriminator?
                ;; If so, cons an entry onto discs with (uber fn)
                ;;
                ;; FIXME: not right, we need a true error function here.
                (if (not (assq uber discs))
                    (let ((err (make-func 'error #f #f 0 '() '() *void-type* #f #f)))
                      (func.table-index-set! err 0)
                      (set! discs (cons (list uber err) discs))))

                ;; Add entries for v to the affected classes.
                ;;
                ;; TODO: This is slow, presumably we can precompute stuff to
                ;; make it faster.
                (for-each (lambda (disc)
                            (for-each (lambda (cls)
                                        (let* ((d  (closest-discriminator cls discs))
                                               (fn (cadr d)))
                                          (class.virtuals-set! cls (cons (list vid fn)
                                                                         (class.virtuals cls)))))
                                      (transitive-subclasses (car disc))))
                          discs)))
            (virtuals env)))

;; Globals

(define (make-global name module export? mut? id type)
  (vector 'global name id type module export? mut? #f))

(define (global? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'global)))

(define (global.name x) (vector-ref x 1))
(define (global.id x) (vector-ref x 2))
(define (global.type x) (vector-ref x 3))
(define (global.module x) (vector-ref x 4)) ; Either #f or a string naming the module
(define (global.export? x) (vector-ref x 5))
(define (global.mut? x) (vector-ref x 6))
(define (global.init x) (vector-ref x 7))
(define (global.init-set! x v) (vector-set! x 7 v))

(define (define-global! cx env name module export? mut? type)
  (let* ((id   (cx.global-id cx))
         (glob (make-global name module export? mut? id type)))
    (cx.global-id-set! cx (+ id 1))
    (define-env-global! env name glob)
    glob))

(define (expand-global-phase1 cx env g)
  (check-list-oneof g '(3 4) "Bad global" g)
  (let* ((import? (memq (car g) '(defconst- defvar-)))
         (export? (memq (car g) '(defconst+ defvar+)))
         (module  (if import? "" #f))
         (mut?    (memq (car g) '(defvar defvar+ defvar-)))
         (type    (parse-type cx env (caddr g)))
         (init    (if (null? (cdddr g)) #f (cadddr g))))
    (if (and import? init)
        (fail "Import global can't have an initializer"))
    (let-values (((module name) (parse-toplevel-name (cadr g) import? "global")))
      (check-unbound env name "already defined at global level")
      (define-global! cx env name module export? mut? type))))

;; We could expand the init during phase 1 but we'll want to broaden
;; inits to encompass global imports soon.

(define (expand-global-phase2 cx env g)
  (if (not (null? (cdddr g)))
      (let ((init (cadddr g)))
        (let-values (((_ name) (parse-toplevel-name (cadr g) #t "")))
          (let ((defn (lookup-global env name)))
            (let-values (((v t) (expand-constant-expr cx init)))
              (global.init-set! defn v)))))))

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
        ((reference-type? t) *slots-anyref*)
        (else
         (error "Bad type" t))))

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

(define (make-type name primitive ref vector_ class vec)
  (vector 'type name primitive ref vector_ class vec))

(define (make-primitive-type name)
  (vector 'type name name #f #f #f #f))

(define (make-ref-type base)
  (make-type 'ref #f base #f #f #f))

(define (make-vector-type cx env element)
  (let ((probe (type.vector element)))
    (if probe
        probe
        (let ((t (make-type 'vector #f #f element #f #f)))
          (type.vector-set! element t)
          (synthesize-vector-ops cx env element)
          t))))

(define (make-class-type cls)
  (let ((t (make-type 'class #f #f #f cls #f)))
    (class.type-set! cls t)
    t))

(define (type? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'type)))

(define (type.name x) (vector-ref x 1))
(define (type.primitive x) (vector-ref x 2))
(define (type.ref-base x) (vector-ref x 3))
(define (type.vector-element x) (vector-ref x 4))
(define (type.class x) (vector-ref x 5))
(define (type.vector x) (vector-ref x 6)) ; The vector type that has this type as a base
(define (type.vector-set! x v) (vector-set! x 6 v))

(define *void-type* (make-type 'void #f #f #f #f #f))

(define *i32-type* (make-primitive-type 'i32))
(define *i64-type* (make-primitive-type 'i64))
(define *f32-type* (make-primitive-type 'f32))
(define *f64-type* (make-primitive-type 'f64))
(define *string-type* (make-primitive-type 'String))
(define *anyref-type* (make-primitive-type 'anyref))

(define *object-type* (make-class-type (make-class 'Object #f '())))

(define (type=? a b)
  (or (eq? a b)
      (case (type.name a)
        ((ref)    (type=? (type.ref-base a) (type.ref-base b)))
        ((vector) (type=? (type.vector-element a) (type.vector-element b)))
        ((class)  (class=? (type.class a) (type.class b)))
        (else     #f))))

(define (void-type? x) (eq? x *void-type*))

(define (i32-type? x)  (eq? x *i32-type*))
(define (i64-type? x)  (eq? x *i64-type*))
(define (f32-type? x)  (eq? x *f32-type*))
(define (f64-type? x)  (eq? x *f64-type*))
(define (string-type? x) (eq? x *string-type*))
(define (anyref-type? x) (eq? x *anyref-type*))

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

(define (subtype? a b)
  (cond ((and (class-type? a) (class-type? b))
         (subclass? (type.class a) (type.class b)))
        (else
         (type=? a b))))

(define (class-type? x)
  (not (not (type.class x))))

(define (vector-type? x)
  (not (not (type.vector-element x))))

(define (reference-type? x)
  (or (class-type? x) (string-type? x) (anyref-type? x) (vector-type? x)))

(define (subclass? a b)
  (or (eq? a b)
      (let ((parent (class.base a)))
        (and parent
             (subclass? parent b)))))

(define (proper-subclass? a b)
  (and (not (eq? a b))
       (let ((parent (class.base a)))
         (and parent
              (subclass? parent b)))))

(define (define-types! env)
  (define-env-global! env 'i32 *i32-type*)
  (define-env-global! env 'i64 *i64-type*)
  (define-env-global! env 'f32 *f32-type*)
  (define-env-global! env 'f64 *f64-type*)
  (define-env-global! env 'String *string-type*)
  (define-env-global! env 'anyref *anyref-type*)
  (define-env-global! env 'Object *object-type*))

(define (parse-type cx env t)
  (cond ((and (symbol? t) (lookup env t)) =>
         (lambda (probe)
           (if (type? probe)
               probe
               (fail "Does not denote a type" t probe))))
        ((and (list? t) (= (length t) 2) (eq? 'Vector (car t)))
         ;; FIXME Hacky, should look Vector up in the previous line, but that
         ;; gets us into denoting type constructors.
         (let ((base (parse-type cx env (cadr t))))
           (make-vector-type cx env base)))
        (else
         (fail "Invalid type" t))))

(define (widen-value env value value-type target-type)
  (cond ((type=? value-type target-type)
         (list value value-type))
        ((and (class-type? value-type)
              (class-type? target-type)
              (proper-subclass? (type.class value-type) (type.class target-type)))
         (list (render-upcast-class-to-class env (type.class target-type) value)
               target-type))
        ((and (class-type? value-type)
              (anyref-type? target-type))
         (list (render-upcast-class-to-anyref env value)
               target-type))
        ((and (vector-type? value-type)
              (anyref-type? target-type))
         (list (render-upcast-vector-to-anyref env (type.element value-type) value)
               target-type))
        ((and (string-type? value-type)
              (anyref-type? target-type))
         (list (render-upcast-string-to-anyref env value)
               target-type))
        (else
         #f)))

;; Loop labels

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

;; Expressions

(define (expand-expr cx expr env)
  (cond ((symbol? expr)
         (expand-symbol cx expr env))
        ((number? expr)
         (expand-number expr))
        ((char? expr)
         (expand-char expr))
        ((boolean? expr)
         (expand-boolean expr))
        ((string? expr)
         (expand-string-literal cx expr env))
        ((and (list? expr) (not (null? expr)))
         (if (symbol? (car expr))
             (let ((probe (lookup env (car expr))))
               (cond ((not probe)
                      (fail "Unbound name in form" (car expr) "\n" expr))
                     ((or (func? probe) (virtual? probe))
                      (expand-func-call cx expr env probe))
                     ((accessor? probe)
                      (expand-accessor cx expr env))
                     ((expander? probe)
                      ((expander.expander probe) cx expr env))
                     (else
                      (fail "Attempting to call non-function" expr))))
             (fail "Not a call" expr)))
        (else
         (fail "Unknown expression" expr))))

;; Returns ((val type) ...)

(define (expand-expressions cx env exprs)
  (map (lambda (e)
         (let-values (((e0 t0) (expand-expr cx e env)))
           (list e0 t0)))
       exprs))

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

(define min-i64 (- (expt 2 63)))
(define max-i64 (- (expt 2 63) 1))

(define (expand-constant-expr cx expr)
  (cond ((numbery-symbol? expr)
         (expand-numbery-symbol expr))
        ((or (number? expr) (char? expr) (boolean? expr))
         (expand-number expr))
        (else ???)))

(define (expand-numbery-symbol expr)
  (let* ((name (symbol->string expr))
         (val  (string->number (substring name 2 (string-length name)))))
    (case (string-ref name 0)
      ((#\I)
       (check-i32-value val expr)
       (values (render-number val *i32-type*) *i32-type*))
      ((#\L)
       (check-i64-value val expr)
       (values (render-number val *i64-type*) *i64-type*))
      ((#\F)
       (check-f32-value val expr)
       (values (render-number val *f32-type*) *f32-type*))
      ((#\D)
       (check-f64-value val expr)
       (values (render-number val *f64-type*) *f64-type*))
      (else ???))))

(define (expand-number expr)
  (cond ((and (integer? expr) (exact? expr))
         (cond ((<= min-i32 expr max-i32)
                (values (render-number expr *i32-type*) *i32-type*))
               (else
                (check-i64-value ,expr)
                (values (render-number expr *i64-type*) *i64-type*))))
        ((number? expr)
         (check-f64-value expr)
         (values (render-number expr *f64-type*) *f64-type*))
        ((char? expr)
         (expand-char expr))
        ((boolean? expr)
         (expand-boolean expr))
        (else
         (fail "Bad syntax" expr))))

(define (expand-char expr)
  (values (render-number (char->integer expr) *i32-type*) *i32-type*))

(define (expand-boolean expr)
  (values (render-number (if expr 1 0) *i32-type*) *i32-type*))

(define (string-literal->id cx lit)
  (let ((probe (assoc lit (cx.strings cx))))
    (if probe
        (cdr probe)
        (let ((id (cx.string-id cx)))
          (cx.string-id-set! cx (+ id 1))
          (cx.strings-set! cx (cons (cons lit id) (cx.strings cx)))
          id))))

(define (expand-string-literal cx expr env)
  (values (render-string-literal env (string-literal->id cx expr))
          *string-type*))

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
  (define-env-global! env 'begin    (make-expander 'begin expand-begin '(atleast 1)))
  (define-env-global! env '%begin%  (make-expander '%begin% expand-begin '(atleast 1)))
  (define-env-global! env 'if       (make-expander 'if expand-if '(oneof 3 4)))
  (define-env-global! env '%if%     (make-expander '%if% expand-if '(oneof 3 4)))
  (define-env-global! env 'cond     (make-expander 'cond expand-cond '(atleast 1)))
  (define-env-global! env 'set!     (make-expander 'set! expand-set! '(precisely 3)))
  (define-env-global! env '%set!%   (make-expander '%set!% expand-set! '(precisely 3)))
  (define-env-global! env 'inc!     (make-expander 'inc! expand-inc!+dec! '(precisely 2)))
  (define-env-global! env 'dec!     (make-expander 'dec! expand-inc!+dec! '(precisely 2)))
  (define-env-global! env 'let      (make-expander 'let expand-let+let* '(atleast 3)))
  (define-env-global! env '%let%    (make-expander '%let% expand-let+let* '(atleast 3)))
  (define-env-global! env 'let*     (make-expander 'let* expand-let+let* '(atleast 3)))
  (define-env-global! env 'loop     (make-expander 'loop expand-loop '(atleast 3)))
  (define-env-global! env '%loop%   (make-expander '%loop% expand-loop '(atleast 3)))
  (define-env-global! env 'break    (make-expander 'break expand-break '(oneof 2 3)))
  (define-env-global! env '%break%  (make-expander '%break% expand-break '(oneof 2 3)))
  (define-env-global! env 'continue (make-expander 'continue expand-continue '(precisely 2)))
  (define-env-global! env 'while    (make-expander 'while expand-while '(atleast 2)))
  (define-env-global! env 'do       (make-expander 'do expand-do '(atleast 3)))
  (define-env-global! env 'case     (make-expander 'case expand-case '(atleast 2)))
  (define-env-global! env '%case%   (make-expander '%case% expand-%case% '(atleast 2)))
  (define-env-global! env 'and      (make-expander 'and expand-and '(atleast 1)))
  (define-env-global! env '%and%    (make-expander '%and% expand-and '(atleast 1)))
  (define-env-global! env 'or       (make-expander 'or expand-or '(atleast 1)))
  (define-env-global! env '%or%     (make-expander '%or% expand-or '(atleast 1)))
  (define-env-global! env 'trap     (make-expander 'trap expand-trap '(oneof 1 2)))
  (define-env-global! env 'new      (make-expander 'new expand-new '(atleast 2)))
  (define-env-global! env 'null     (make-expander 'null expand-null '(precisely 2)))
  (define-env-global! env 'is       (make-expander 'is expand-is '(precisely 3)))
  (define-env-global! env 'as       (make-expander 'as expand-as '(precisely 3))))

(define (define-builtins! env)
  (define-env-global! env 'not      (make-expander 'not expand-not '(precisely 2)))
  (define-env-global! env 'select   (make-expander 'select expand-select '(precisely 4)))
  (define-env-global! env 'zero?    (make-expander 'zero? expand-zero? '(precisely 2)))
  (define-env-global! env 'nonzero? (make-expander 'zero? expand-nonzero? '(precisely 2)))
  (define-env-global! env 'null?    (make-expander 'null? expand-null? '(precisely 2)))
  (define-env-global! env 'nonnull? (make-expander 'nonnull? expand-nonnull? '(precisely 2)))
  (define-env-global! env 'bitnot   (make-expander 'bitnot expand-bitnot '(precisely 2)))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-unop '(precisely 2))))
            '(clz ctz popcnt neg abs sqrt ceil floor nearest trunc extend8 extend16 extend32))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-binop '(precisely 3))))
            '(+ %+% - %-% * div divu rem remu bitand bitor bitxor shl shr shru rotl rotr max min copysign))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-relop '(precisely 3))))
            '(< <u <= <=u > >u >= >=u =))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-conversion '(precisely 2))))
            '(i32->i64 u32->i64 i64->i32 f32->f64 f64->f32 f64->i32 f64->i64 i32->f64 i64->f64
              f32->i32 f32->i64 i32->f32 i64->f32 f32->bits bits->f32 f64->bits bits->f64))
  (define-env-global! env 'string (make-expander 'string expand-string '(atleast 1)))
  (define-env-global! env 'string-length (make-expander 'string-length expand-string-length '(precisely 2)))
  (define-env-global! env 'string-ref (make-expander 'string-ref expand-string-ref '(precisely 3)))
  (define-env-global! env 'string-append (make-expander 'string-append expand-string-append '(atleast 1)))
  (define-env-global! env '%string-append% (make-expander '%string-append% expand-string-append '(atleast 1)))
  (define-env-global! env 'substring (make-expander 'substring expand-substring '(precisely 4)))
  (for-each (lambda (name)
              (define-env-global! env name (make-expander name expand-string-relop '(precisely 3))))
            '(string=? string<? string<=? string>? string>=?))
  (define-env-global! env 'vector-length (make-expander 'vector-length expand-vector-length '(precisely 2)))
  (define-env-global! env 'vector-ref (make-expander 'vector-ref expand-vector-ref '(precisely 3)))
  (define-env-global! env 'vector-set! (make-expander 'vector-set! expand-vector-set! '(precisely 4)))
  (define-env-global! env 'vector->string (make-expander 'vector->string expand-vector->string '(precisely 2)))
  (define-env-global! env 'string->vector (make-expander 'string->vector expand-string->vector '(precisely 2))))

;; Primitive syntax

(define (expand-begin cx expr env)
  (if (null? (cdr expr))
      (values (void-expr) *void-type*)
      (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
        (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
          (cond ((null? exprs)
                 (cond ((= (length body) 1)
                        (values (car body) ty))
                       (else
                        (values `(block ,@(render-type-spliceable ty) ,@(reverse body)) ty))))
                ((not (void-type? ty))
                 (loop exprs (cons 'drop body) *void-type*))
                (else
                 (let-values (((e1 t1) (expand-expr cx (car exprs) env)))
                   (loop (cdr exprs) (cons e1 body) t1))))))))

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
         (values `(if ,@(render-type-spliceable t1) ,test ,consequent ,alternate) t1)))
      (else
       (fail "Bad 'if'" expr)))))

(define (expand-set! cx expr env)
  (let* ((name (cadr expr))
         (val  (caddr expr)))
    (let-values (((e0 t0) (expand-expr cx val env)))
      (cond ((symbol? name)
             (let ((binding (lookup-variable env name)))
               (cond ((local? binding)
                      (let ((val+ty (widen-value env e0 t0 (local.type binding))))
                        (if (not val+ty)
                            (check-same-type t0 (local.type binding) "'set!'" expr))
                        (values `(set_local ,(local.slot binding) ,(car val+ty)) *void-type*)))
                     ((global? binding)
                      (let ((val+ty (widen-value env e0 t0 (local.type binding))))
                        (if (not val+ty)
                            (check-same-type t0 (global.type binding) "'set!'" expr))
                        (values `(set_global ,(global.id binding) ,(car val+ty)) *void-type*)))
                     (else
                      ???))))
            ((accessor-expression? env name)
             (let-values (((base-expr cls field-name field-type)
                           (process-accessor-expression cx name env))
                          ((ev tv)
                           (expand-expr cx val env)))
               (let ((val+ty (widen-value env ev tv field-type)))
                 (if (not val+ty)
                     (check-same-type field-type tv "'set!' object field" expr))
                 (values (render-field-update env cls field-name base-expr (car val+ty))
                         *void-type*))))
            (else
             (fail "Illegal lvalue" expr))))))

(define (expand-let+let* cx expr env)

  (define is-let* (eq? (car expr) 'let*))

  (define (process-bindings bindings env)
    (let loop ((bindings bindings) (new-locals '()) (code '()) (undos '()) (env env))
      (if (null? bindings)
          (values (reverse new-locals)
                  (reverse code)
                  undos
                  (if is-let* env (extend-env env new-locals)))
          (let ((binding (car bindings)))
            (check-list binding 2 "Bad binding" binding)
            (let* ((name (car binding))
                   (_    (check-symbol name "Bad local name" name))
                   (_    (if (and (not is-let*) (assq name new-locals))
                             (fail "Duplicate let binding" name)))
                   (init (cadr binding)))
              (let*-values (((e0 t0)     (expand-expr cx init env))
                            ((slot undo) (claim-local (cx.slots cx) t0)))
                (let ((new-binding (cons name (make-local name slot t0))))
                  (loop (cdr bindings)
                        (cons new-binding new-locals)
                        (cons `(set_local ,slot ,e0) code)
                        (cons undo undos)
                        (if is-let* (extend-env env (list new-binding)) env)))))))))

  (let* ((bindings (cadr expr))
         (body     (cddr expr)))
    (let*-values (((new-locals code undos new-env) (process-bindings bindings env))
                  ((e0 t0)                         (expand-expr cx `(begin ,@body) new-env)))
      (unclaim-locals (cx.slots cx) undos)
      (let ((type (render-type-spliceable t0)))
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
                      ,@(render-type-spliceable (loop.type loop))
                      (loop ,(loop.continue loop)
                            ,e0
                            (br ,(loop.continue loop)))
                      ,@(if (void-type? (loop.type loop))
                            '()
                            (list (typed-constant (loop.type loop) 0))))
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

(define (expand-new cx expr env)
  (let* ((tyexpr (cadr expr))
         (type   (parse-type cx env tyexpr)))
    (cond ((class-type? type)
           (let* ((cls     (type.class type))
                  (fields  (class.fields cls))
                  (actuals (expand-expressions cx env (cddr expr)))
                  (actuals (check-and-widen-arguments env fields actuals expr)))
             (values (render-new-class env cls actuals) type)))
          ((vector-type? type)
           (check-list expr 4 "Bad arguments to 'new'" expr)
           (let*-values (((el tl) (expand-expr cx (caddr expr) env))
                         ((ei ti) (expand-expr cx (cadddr expr) env)))
             (check-i32-type tl "'new' vector length" expr)
             (let ((base (type.vector-element type)))
               (check-same-type base ti "'new' initial value" expr)
               (values (render-new-vector env base el ei) type))))
          (else
           (fail "Invalid type to 'new'" tyexpr expr)))))

(define (expand-null cx expr env)
  (let* ((tyexpr (cadr expr))
         (type   (parse-type cx env tyexpr)))
    (cond ((class-type? type)
           (values (render-class-null env (type.class type)) type))
          ((vector-type? type)
           (values (render-vector-null env (type.vector-element type)) type))
          ((anyref-type? type)
           (values (render-anyref-null env) *anyref-type*))
          (else
           (fail "Not a valid reference type for 'null'" tyexpr)))))

(define (expand-is cx expr env)
  (let* ((tyexpr      (cadr expr))
         (target-type (parse-type cx env tyexpr)))
    (let-values (((e t) (expand-expr cx (caddr expr) env)))
      (cond ((anyref-type? target-type)
             (values `(block i32 ,e drop (i32.const 1)) *i32-type*))
            ((string-type? target-type)
             (cond ((string-type? t)
                    `(block i32 ,e drop (i32.const 1)))
                   ((anyref-type? t)
                    (values (render-anyref-is-string env e) *i32-type*))
                   (else
                    (fail "Bad source type in 'is'" t expr))))
            ((class-type? target-type)
             (let ((target-cls (type.class target-type)))
               (cond ((class-type? t)
                      (let ((value-cls  (type.class t)))
                        (cond ((subclass? value-cls target-cls)
                               (values `(block i32 ,e drop (i32.const 1)) *i32-type*))
                              ((subclass? target-cls value-cls)
                               (values (render-class-is-class env target-cls e) *i32-type*))
                              (else
                               (fail "Types in 'is' are unrelated" expr)))))
                     ((anyref-type? t)
                      (values (render-anyref-is-class env target-cls e) *i32-type*))
                     (else
                      (fail "Expression in 'is' is not of class type" expr)))))
            ((vector-type? target-type)
             (cond ((and (vector-type? t) (type=? target-type t))
                    `(block i32 ,e dro (i32.const 1)))
                   ((anyref-type? t)
                    (values (render-anyref-is-vector env e (type.vector-element target-type)) *i32-type*))
                   (else
                    (fail "Bad source type in 'is'" t expr))))
            (else
             (fail "Bad target type in 'is'" target-type expr))))))

(define (expand-as cx expr env)
  (let* ((tyexpr      (cadr expr))
         (target-type (parse-type cx env tyexpr)))
    (let-values (((e t) (expand-expr cx (caddr expr) env)))
      (cond ((anyref-type? target-type)
             (cond ((anyref-type? t)
                    (values e target-type))
                   ((string-type? t)
                    (values (render-upcast-string-to-anyref env e) target-type))
                   ((class-type? t)
                    (values (render-upcast-class-to-anyref env e) target-type))
                   ((vector-type? t)
                    (values (render-upcast-vector-to-anyref env (type.vector-element t) e) target-type))
                   (else
                    ???)))
            ((string-type? target-type)
             (cond ((string-type? t)
                    (values e target-type))
                   ((anyref-type? t)
                    (values (render-downcast-anyref-to-string env e) target-type))
                   (else
                    (fail "Bad source type in 'as'" t expr))))
            ((class-type? target-type)
             (let ((target-cls (type.class target-type)))
               (cond ((class-type? t)
                      (let ((value-cls  (type.class t)))
                        (cond ((class=? value-cls target-cls)
                               (values e t))
                              ((subclass? value-cls target-cls)
                               (values (render-upcast-class-to-class env target-cls e)
                                       (class.type target-cls)))
                              ((subclass? target-cls value-cls)
                               (values (render-downcast-class-to-class env target-cls e)
                                       (class.type target-cls)))
                              (else
                               (fail "Types in 'as' are unrelated" expr)))))
                     ((anyref-type? t)
                      (values (render-downcast-anyref-to-class env target-cls e)
                              (class.type target-cls)))
                     (else
                      (fail "Expression in 'as' is not of class type" expr)))))
            ((vector-type? target-type)
             (cond ((and (vector-type? t) (type=? target-type t))
                    (values e target-type))
                   ((anyref-type? t)
                    (values (render-downcast-anyref-to-vector env e (type.vector-element target-type))
                            target-type))
                   (else
                    (fail "Bad source type in 'as'" t expr))))
            (else
             (fail "Bad target type in 'is'" target-type expr))))))

(define (expand-trap cx expr env)
  (let ((t (if (null? (cdr expr))
               *void-type*
               (parse-type cx env (cadr expr)))))
    (values '(unreachable) t)))

;; Derived syntax

(define (expand-cond cx expr env)

  (define (collect-clauses-reverse clauses)
    (let loop ((clauses clauses) (exprs '()))
      (if (null? clauses)
          exprs
          (let ((c (car clauses)))
            (check-list-atleast c 1 "'cond' clause" expr)
            (if (eq? (car c) 'else)
                (begin
                  (if (not (null? (cdr clauses)))
                      (fail "'else' clause must be last" expr))
                  (let-values (((e0 t0) (expand-expr cx `(begin ,@(cdr c)) env)))
                    (loop (cdr clauses)
                          (cons (list #t e0 t0) exprs))))
                (let-values (((ec tc) (expand-expr cx (car c) env)))
                  (check-i32-type tc "'cond' condition" expr)
                  (let-values (((e0 t0) (expand-expr cx `(begin ,@(cdr c)) env)))
                    (loop (cdr clauses)
                          (cons (list ec e0 t0) exprs)))))))))

  (define (else-clause? c)
    (eq? (car c) #t))

  (define (wrap-clauses clauses base t)
    (if (null? clauses)
        base
        (let ((c (car clauses)))
          (wrap-clauses (cdr clauses) `(if ,@t ,(car c) ,(cadr c) ,base) t))))

  (define (expand-clauses-reverse clauses t)
    (let ((last (car clauses)))
      (if (else-clause? last)
          (wrap-clauses (cdr clauses)
                        (cadr last)
                        (render-type-spliceable t))
          (wrap-clauses (cdr clauses)
                        `(if ,(car last) ,(cadr last))
                        '()))))

  (if (null? (cdr expr))
      (values (void-expr) *void-type*)
      (let ((clauses (collect-clauses-reverse (cdr expr))))
        (begin
          (check-same-types (map caddr clauses) "'cond' arms" expr)
          (let* ((last (car clauses))
                 (t    (caddr last)))
            (if (not (else-clause? last))
                (check-same-type t *void-type* "'cond' type" expr))
            (values (expand-clauses-reverse clauses t)
                    t))))))

(define (expand-inc!+dec! cx expr env)
  (let* ((the-op (if (eq? (car expr) 'inc!) '%+% '%-%))
         (name   (cadr expr)))
    (cond ((symbol? name)
           (expand-expr cx `(%set!% ,name (,the-op ,name 1)) env))
          ((accessor-expression? env name)
           (let ((temp     (new-name cx "local"))
                 (accessor (car name))
                 (ptr      (cadr name)))
             (expand-expr cx
                          `(%let% ((,temp ,ptr))
                              (%set!% (,accessor ,temp) (,the-op (,accessor ,temp) 1)))
                          env)))
          (else
           (fail "Illegal lvalue" expr)))))

(define (expand-while cx expr env)
  (let ((loop-name  (new-name cx "while"))
        (test       (cadr expr))
        (body       (cddr expr)))
    (expand-expr cx
                 `(%loop% ,loop-name
                          (%if% (not ,test) (%break% ,loop-name))
                          ,@body)
                 env)))

(define (expand-do cx expr env)

  (define (collect-variables clauses)
    (check-list-atleast clauses 0 "Variable clauses in 'do'" expr)
    (for-each (lambda (c)
                (check-list c 3 "Variable clause in 'do'" c "\n" expr)
                (check-symbol (car c) "Binding in 'do'" c "\n" expr))
              clauses)
    (values (map car clauses)
            (map cadr clauses)
            (map caddr clauses)))

  (define (collect-exit clause)
    (check-list-atleast clause 1 "Test clause in 'do'" expr)
    (values (car clause)
            (cdr clause)))

  (let*-values (((ids inits updates) (collect-variables (cadr expr)))
                ((test results)      (collect-exit (caddr expr))))
    (let ((body      (cdddr expr))
          (loop-name (new-name cx "do"))
          (temps     (map (lambda (id) (new-name cx "tmp")) ids)))
      (expand-expr cx
                   `(%let% ,(map list ids inits)
                       (%loop% ,loop-name
                               (%if% ,test (%break% ,loop-name (%begin% ,@results)))
                               ,@body
                               (%let% ,(map list temps updates)
                                  ,@(map (lambda (id temp) `(set! ,id ,temp)) ids temps))))
                   env))))

(define (expand-case cx expr env)
  (let ((temp (new-name cx "local")))
    (expand-expr cx
                 `(%let% ((,temp ,(cadr expr)))
                    (%case% ,temp ,@(cddr expr)))
                 env)))

;; The dispatch expr is always effect-free, and we make use of this.

(define (expand-%case% cx expr env)

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
  ;; full list.  Note the block may not yield a value; it may be void, or it may
  ;; break or continue.
  ;;
  ;; default is just an expr, default-type is its type.

  (define (finish-case found-values e0 cases default default-type)
    (if (null? cases)
        (begin
          (assert default)
          (values default default-type))
        (let* ((_             (check-case-types cases default-type))
               (ty            (caddr (car cases)))
               (bty           (render-type-spliceable ty))
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
                                                          ,@(if (void-type? ty)
                                                                `(,code (br ,outer-label))
                                                                `((br ,outer-label ,code)))))))))
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
                        ((or (number? c) (char? c) (boolean? c))
                         (let-values (((v t) (expand-number c)))
                           (check-i32-type t "'case' constant")
                           v))
                        ((and (symbol? c) (lookup-global env c)) =>
                         (lambda (g)
                           (if (and (not (global.module g))
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
             (finish-case found-values e0 case-info #f *void-type*))

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
  (cond ((null? (cdr expr))
         (expand-expr cx #t env))
        ((null? (cddr expr))
         (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
           (check-i32-type t0 "'and'" (cadr expr) expr)
           (values e0 t0)))
        (else
         (expand-expr cx `(%if% ,(cadr expr) (%and% ,@(cddr expr)) #f) env))))

(define (expand-or cx expr env)
  (cond ((null? (cdr expr))
         (expand-expr cx #f env))
        ((null? (cddr expr))
         (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
           (check-i32-type t0 "'or'" (cadr expr) expr)
           (values e0 t0)))
        (else
         (let ((temp (new-name cx "local")))
           (expand-expr cx
                        `(%let% ((,temp ,(cadr expr)))
                            (%if% ,temp ,temp (%or% ,@(cddr expr))))
                        env)))))

;; Null is the same for string, anyref, class.

(define (expand-null? cx expr env)
  (let-values (((e t) (expand-expr cx (cadr expr) env)))
    (check-ref-type t "'null?'" expr)
    (values (render-null? env e) *i32-type*)))

(define (expand-nonnull? cx expr env)
  (let-values (((e t) (expand-expr cx (cadr expr) env)))
    (check-ref-type t "'nonnull?'" expr)
    (values (render-nonnull? env e) *i32-type*)))

(define (expand-zero? cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-number-type t1 "'zero?'" expr)
    (values `(,(operatorize t1 '=) ,op1 ,(typed-constant t1 0)) *i32-type*)))

(define (expand-nonzero? cx expr env)
  (let-values (((op1 t1) (expand-expr cx (cadr expr) env)))
    (check-number-type t1 "'nonzero?'" expr)
    (values `(,(operatorize t1 '%!=%) ,op1 ,(typed-constant t1 0)) *i32-type*)))

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

(define (expand-func-call cx expr env func)
  (let* ((args    (cdr expr))
         (formals (func.formals func))
         (actuals (expand-expressions cx env args))
         (actuals (check-and-widen-arguments env formals actuals expr)))
    (values `(call ,(func.id func) ,@(map car actuals))
            (func.result func))))

;; formals is ((name type) ...)
;; actuals is ((val type) ...)

(define (check-and-widen-arguments env formals actuals context)
  (if (not (= (length formals) (length actuals)))
      (fail "wrong number of arguments" context))
  (map (lambda (formal actual)
         (let ((want (cadr formal))
               (have (cadr actual)))
           (or (widen-value env (car actual) have want)
               (fail "Not same type in" context "and widening not possible.\n"
                     (pretty-type have) (pretty-type want)))))
       formals actuals))

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
    (splice (type.name t) name)))

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
    (%+% ".add" i32 i64 f32 f64)
    (- ".sub" i32 i64 f32 f64)
    (%-% ".sub" i32 i64 f32 f64)
    (* ".mul" i32 i64 f32 f64)
    (= ".eq" i32 i64 f32 f64)
    (%!=% ".ne" i32 i64 f32 f64)))

(define (typed-constant t value)
  `(,(splice (type.name t) ".const") ,value))

(define (expand-string cx expr env)
  (let ((actuals (map (lambda (x)
                        (check-i32-type (cadr x) "Argument to 'string'" expr)
                        (car x))
                      (expand-expressions cx env (cdr expr)))))
    (values (render-new-string env actuals) *string-type*)))

(define (expand-string-length cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-string-type t0 "'string-length'" expr)
    (values (render-string-length env e0) *i32-type*)))

(define (expand-string-ref cx expr env)
  (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                ((e1 t1) (expand-expr cx (caddr expr) env)))
    (check-string-type t0 "'string-ref'" expr)
    (check-i32-type t1 "'string-ref'" expr)
    (values (render-string-ref env e0 e1) *i32-type*)))

(define (expand-string-append cx expr env)
  (let ((l (length expr)))
    (values (case l
              ((1)
               (render-string-literal env (string-literal->id cx "")))
              ((2)
               (let*-values (((e0 t0) (expand-expr cx (cadr expr) env)))
                 (check-string-type t0 "'string-append'" expr)
                 e0))
              ((3)
               (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                             ((e1 t1) (expand-expr cx (caddr expr) env)))
                 (check-string-type t0 "'string-append'" expr)
                 (check-string-type t1 "'string-append'" expr)
                 (render-string-append env e0 e1)))
              (else
               (let-values (((e0 t0)
                             (expand-expr cx
                                          `(%string-append% (%string-append% ,(cadr expr) ,(caddr expr))
                                                            ,@(cdddr expr))
                                          env)))
                 e0)))
            *string-type*)))

(define (expand-substring cx expr env)
  (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                ((e1 t1) (expand-expr cx (caddr expr) env))
                ((e2 t2) (expand-expr cx (cadddr expr) env)))
    (check-string-type t0 "'substring'" expr)
    (check-i32-type t1 "'substring'" expr)
    (check-i32-type t2 "'substring'" expr)
    (values (render-substring env e0 e1 e2) *string-type*)))

(define (expand-string-relop cx expr env)
  (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                ((e1 t1) (expand-expr cx (caddr expr) env)))
    (let ((name (string-append "'" (symbol->string (car expr)) "'")))
      (check-string-type t0 name expr)
      (check-string-type t1 name expr))
    (values `(,(case (car expr)
                 ((string=?)  'i32.eq)
                 ((string<?)  'i32.lt_s)
                 ((string<=?) 'i32.le_s)
                 ((string>?)  'i32.gt_s)
                 ((string>=?) 'i32.ge_s)
                 (else        ???))
              ,(render-string-compare env e0 e1)
              (i32.const 0))
            *i32-type*)))

(define (expand-vector-length cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-vector-type t0 "'vector-length'" expr)
    (values (render-vector-length env e0 (type.vector-element t0)) *i32-type*)))

(define (expand-vector-ref cx expr env)
  (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                ((e1 t1) (expand-expr cx (caddr expr) env)))
    (check-vector-type t0 "'vector-ref'" expr)
    (check-i32-type t1 "'vector-ref'" expr)
    (let ((element-type (type.vector-element t0)))
      (values (render-vector-ref env e0 e1 element-type) element-type))))

(define (expand-vector-set! cx expr env)
  (let*-values (((e0 t0) (expand-expr cx (cadr expr) env))
                ((e1 t1) (expand-expr cx (caddr expr) env))
                ((e2 t2) (expand-expr cx (cadddr expr) env)))
    (check-vector-type t0 "'vector-set!'" expr)
    (check-i32-type t1 "'vector-set!'" expr)
    (check-same-type (type.vector-element t0) t2 "'vector-set!'" expr)
    (let ((element-type (type.vector-element t0)))
      (values (render-vector-set! env e0 e1 e2 element-type) *void-type*))))

(define (expand-vector->string cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-vector-type t0 "'vector->string'" expr)
    (check-i32-type (type.vector-element t0) "'vector->string'" expr)
    (values (render-vector->string env e0) *string-type*)))

(define (expand-string->vector cx expr env)
  (let-values (((e0 t0) (expand-expr cx (cadr expr) env)))
    (check-string-type t0 "'string->vector'" expr)
    (values (render-string->vector env e0) (make-vector-type cx env *i32-type*))))

;; Sundry

(define (void-expr)
  '(block))

(define (parse-toplevel-name n import? tag)
  (check-symbol n (string-append "Bad " tag " name") n)
  (let* ((name (symbol->string n))
         (len  (string-length name)))
    (let loop ((i 0))
      (cond ((= i len)
             (values (if import? "" #f) n))
            ((char=? (string-ref name i) #\:)
             (if (not import?)
                 (fail "Import name not allowed for " tag n))
             (if (= i (- len 1))
                 (fail "Import name can't have empty name part" n))
             (values (substring name 0 i)
                     (string->symbol (substring name (+ i 1) len))))
            (else
             (loop (+ i 1)))))))

;; TODO: really want to check that the syntax won't blow up string->number
;; later.

(define (numbery-symbol? x)
  (and (symbol? x)
       (let ((name (symbol->string x)))
         (and (> (string-length name) 2)
              (char=? #\. (string-ref name 1))
              (memv (string-ref name 0) '(#\I #\L #\F #\D))))))

(define (pretty-type x)
  (case (type.name x)
    ((i32 i64 f32 f64 void)
     (type.name x))
    ((class)
     `(class ,(class.name (type.class x))))
    (else
     x)))

(define (iota n)
  (let loop ((n n) (l '()))
    (if (zero? n)
        l
        (loop (- n 1) (cons (- n 1) l)))))

(define (splice . xs)
  (string->symbol
   (apply string-append (map (lambda (x)
                               (cond ((string? x) x)
                                     ((symbol? x) (symbol->string x))
                                     ((number? x) (number->string x))
                                     (else ???)))
                             xs))))

(define (check-i32-value val . context)
  (if (not (and (integer? val) (exact? val) (<= min-i32 val max-i32)))
      (apply fail "Value outside i32 range" val context)))

(define (check-i64-value val . context)
  (if (not (and (integer? val) (exact? val) (<= min-i64 val max-i64)))
      (apply fail "Value outside i64 range" val context)))

(define (check-f32-value val . context)
  (if (not (number? val))
      (apply fail "Value outside f32 range" val context)))

(define (check-f64-value val . context)
  (if (not (number? val))
      (apply fail "Value outside f64 range" val context)))

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

(define (check-string-type t context . rest)
  (if (not (string-type? t))
      (apply fail `("Not a string type in" ,context ,@rest "\n" ,(pretty-type t)))))

(define (check-vector-type t context . rest)
  (if (not (vector-type? t))
      (apply fail `("Not a vector type in" ,context ,@rest "\n" ,(pretty-type t)))))

(define (check-same-types types . context)
  (if (not (null? types))
      (let ((t (car types)))
        (for-each (lambda (e)
                    (apply check-same-type e t context))
                  (cdr types)))))

(define (check-same-type t1 t2 context . rest)
  (if (not (type=? t1 t2))
      (apply fail `("Not same type in" ,context ,@rest "\nlhs = " ,(pretty-type t1) "\nrhs = " ,(pretty-type t2)))))

(define (check-class-type t context . rest)
  (if (not (class-type? t))
      (apply fail `("Not class type in" ,context ,@rest "\ntype = " ,(pretty-type t)))))

(define (check-ref-type t context . rest)
  (if (not (reference-type? t))
      (apply fail `("Not reference type in" ,context ,@rest "\ntype = " ,(pretty-type t)))))

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
  (if (not (or (number? x) (numbery-symbol? x) (char? x) (boolean? x)))
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

(define (comma-separate ss)
  (string-join ss ","))

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
          (open-output-string)          ; for library code
          (open-output-string)          ; for descriptor code
          (open-output-string)          ; for strings
          1))                           ; next class ID

(define (support.type x) (vector-ref x 1))
(define (support.lib x) (vector-ref x 2))
(define (support.desc x) (vector-ref x 3))
(define (support.strings x) (vector-ref x 4))
(define (support.class-id x) (vector-ref x 5))
(define (support.class-id-set! x v) (vector-set! x 5 v))

;; JavaScript support.
;;
;; Various aspects of rendering reference types and operations on them, subject
;; to change as we grow better support.

(define (format-lib cx name fmt . args)
  (apply format (support.lib (cx.support cx)) (string-append "'~a': " fmt ",\n") name args))

(define (format-type cx name fmt . args)
  (apply format (support.type (cx.support cx)) (string-append "'~a': " fmt ",\n") name args))

(define (format-desc cx name fmt . args)
  (apply format (support.desc (cx.support cx)) (string-append "'~a': " fmt ",\n") name args))

(define (js-lib cx env name formals result code . args)
  (assert (symbol? name))
  (assert (every? type? formals))
  (assert (type? result))
  (apply format-lib cx name code args)
  (synthesize-func-import cx env name formals result))

;; Types

(define (render-type t)
  (if (reference-type? t)
      'anyref
      (type.name t)))

(define (render-type-spliceable t)
  (cond ((void-type? t)
         '())
        ((reference-type? t)
         '(anyref))
        (else
         (list (type.name t)))))

(define (typed-object-name t)
  (string-append "TO."
                 (case (type.name t)
                   ((class)  "Object")
                   ((anyref) "Object")
                   ((vector) "Object")
                   ((String) "string")
                   ((i32)    "int32")
                   ((i64)    "int64")
                   ((f32)    "float32")
                   ((f64)    "float64")
                   (else ???))))

;; Numbers

(define (render-number n type)
  (let ((v (cond ((= n +inf.0) '+infinity)
                 ((= n -inf.0) '-infinity)
                 ((not (= n n)) '+nan)
                 (else n))))
    (cond ((i32-type? type) `(i32.const ,v))
          ((i64-type? type) `(i64.const ,v))
          ((f32-type? type) `(f32.const ,v))
          ((f64-type? type) `(f64.const ,v))
          (else ???))))

(define (synthesize-class-ops cx env)
  (let ((clss (classes env)))
    (if (not (null? clss))
        (begin
          (for-each (lambda (cls)
                      (create-class-descriptor cx env cls))
                    clss)
          (synthesize-isnull cx env)
          (synthesize-null cx env)
          (synthesize-generic-upcast cx env)
          (synthesize-resolve-virtual cx env)
          (for-each (lambda (cls)
                      (synthesize-type-and-new cx env cls)
                      (synthesize-upcast cx env cls)
                      (synthesize-testing cx env cls)
                      (synthesize-downcast cx env cls)
                      (for-each (lambda (f)
                                  (synthesize-field-ops cx env cls f))
                                (class.fields cls)))
                    clss)))))

(define (create-class-descriptor cx env cls)
  (let* ((support (cx.support cx))
         (id      (support.class-id support)))
    (support.class-id-set! support (+ id 1))
    (class.host-set! cls id)))

(define (class-ids cls)
  (if (not cls)
      '()
      (cons (class.host cls) (class-ids (class.base cls)))))

(define (class-dispatch-map cls)
  (let ((vs (class.virtuals cls)))
    (if (null? vs)
        '()
        (let* ((largest (apply max (map car vs)))
               (vtbl    (make-vector (+ largest 1) -1)))
          (for-each (lambda (entry)
                      (vector-set! vtbl (car entry) (func.table-index (cadr entry))))
                    vs)
          (vector->list vtbl)))))

;; The use of *object-type* must change once we have more than anyref in these
;; places:
;;
;;  - synthesize-isnull
;;  - synthesize-null
;;  - synthesize-resolve-virtual (but how?)
;;  - narrowing, widening, and checking

(define (synthesize-isnull cx env)
  (js-lib cx env '_isnull `(,*object-type*) *i32-type* "function (x) { return x === null }"))

(define (synthesize-null cx env)
  (js-lib cx env '_null '() *object-type* "function () { return null }"))

(define (synthesize-resolve-virtual cx env)
  (js-lib cx env '_resolve_virtual `(,*object-type* ,*i32-type*) *i32-type*
          "function(obj,vid) { return obj._desc_.vtbl[vid] }"))

(define (synthesize-type-and-new cx env cls)
  (let ((name   (symbol->string (class.name cls)))
        (fields (class.fields cls)))

    (format-type cx name
                 "new TO.StructType({~a})"
                 (comma-separate (cons "_desc_:TO.Object"
                                       (map (lambda (f)
                                              (let ((name (symbol->string (car f)))
                                                    (type (typed-object-name (cadr f))))
                                                (string-append "'" name "':" type)))
                                            fields))))

    (let* ((new-name (splice "_new_" name))
           (fs       (map (lambda (f) (symbol->string (car f))) fields))
           (formals  (comma-separate fs))
           (as       (cons (string-append "_desc_:self.desc." name) fs))
           (actuals  (comma-separate as)))

      (js-lib cx env new-name (map cadr fields) (class.type cls)
              "function (~a) { return new self.types.~a({~a}) }" formals name actuals))))

(define (synthesize-class-descs cx env)
  (for-each (lambda (cls)
              (let ((name   (symbol->string (class.name cls)))
                    (desc   (support.desc (cx.support cx))))

                (format-desc cx name
                             "{id:~a, ids:[~a], vtbl:[~a]}"
                             (class.host cls)
                             (comma-separate (map number->string (class-ids cls)))
                             (comma-separate (map number->string (class-dispatch-map cls))))))
            (classes env)))

;; The compiler shall only insert an upcast operation if the expression being
;; widened is known to be of a proper subtype of the target type.  In this case
;; the operation is a no-op but once the wasm type system goes beyond having
;; just anyref we may need an explicit upcast here.

(define (synthesize-generic-upcast cx env)
  (js-lib cx env '_upcast_class_to_anyref `(,*object-type*) *anyref-type* "function (p) { return p }"))

(define (synthesize-upcast cx env cls)
  (js-lib cx env (splice "_upcast_class_to_" (class.name cls)) `(,*object-type*) (class.type cls)
          "function (p) { return p }"))

(define (synthesize-testing cx env cls)
  (js-lib cx env (splice "_class_is_" (class.name cls)) `(,*object-type*) *i32-type*
          "function (p) { return self.lib._test(~a, p._desc_.ids) }"
          (class.host cls))

  (js-lib cx env (splice "_anyref_is_" (class.name cls)) `(,*anyref-type*) *i32-type*
          "function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(~a, p._desc_.ids) }"
          (class.host cls)))

;; And again.

(define (synthesize-downcast cx env cls)
  (js-lib cx env (splice "_downcast_class_to_" (class.name cls))
          `(,*object-type*) (class.type cls)
          "
function (p) {
  if (!self.lib._test(~a, p._desc_.ids))
    throw new Error('Failed to narrow to ~a' + p);
  return p;
}"
          (class.host cls)
          (class.name cls))

  (js-lib cx env (splice "_downcast_anyref_to_" (class.name cls))
          `(,*anyref-type*) (class.type cls)
          "
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(~a, p._desc_.ids)))
    throw new Error('Failed to narrow to ~a' + p);
  return p;
}"
          (class.host cls)
          (class.name cls)))

(define (synthesize-field-ops cx env cls f)
  (let ((field-name (car f))
        (field-type (cadr f)))

    ;; TODO: here we assume JS syntax for the field-name.  We can work around it
    ;; for the field access but not for the function name.  We need some kind of
    ;; mangling.

    (js-lib cx env (splice "_get_" (class.name cls) "_" field-name)
            `(,(class.type cls)) field-type
            "function (p) { return p.~a }" field-name)

    (js-lib cx env (splice "_set_" (class.name cls) "_" field-name)
            `(,(class.type cls) ,field-type) *void-type*
            "function (p, v) { p.~a = v }" field-name)))

(define (synthesize-func-import cx env name formal-types result)
  (let ((rendered-params (map (lambda (f)
                                `(param ,(render-type f)))
                              formal-types))
        (formals         (map (lambda (k f)
                                (list (splice "p" k) f))
                              (iota (length formal-types)) formal-types)))
    (define-function! cx env name "lib" #f rendered-params formals result #f)))

(define (render-field-access env cls field-name base-expr)
  (let ((name (splice "_get_" (class.name cls) "_" field-name)))
    `(call ,(func.id (lookup-func env name)) ,base-expr)))

(define (render-field-update env cls field-name base-expr val-expr)
  (let ((name (splice "_set_" (class.name cls) "_" field-name)))
    `(call ,(func.id (lookup-func env name)) ,base-expr ,val-expr)))

(define (render-class-null env cls)
  `(call ,(func.id (lookup-func env '_null))))

(define (render-anyref-null env)
  `(call ,(func.id (lookup-func env '_null))))

(define (render-class-is-class env cls val)
  (let ((name (splice "_class_is_" (class.name cls))))
    `(call ,(func.id (lookup-func env name)) ,val)))

(define (render-anyref-is-class env cls val)
  (let ((name (splice "_anyref_is_" (class.name cls))))
    `(call ,(func.id (lookup-func env name)) ,val)))

(define (render-upcast-class-to-class env desired expr)
  (let ((name (splice "_upcast_class_to_" (class.name desired))))
    `(call ,(func.id (lookup-func env name)) ,expr)))

(define (render-upcast-class-to-anyref env expr)
  `(call ,(func.id (lookup-func env '_upcast_class_to_anyref)) ,expr))

(define (render-downcast-class-to-class env cls val)
  (let ((name (splice "_downcast_class_to_" (class.name cls))))
    `(call ,(func.id (lookup-func env name)) ,val)))

(define (render-downcast-anyref-to-class env cls val)
  (let ((name (splice "_downcast_anyref_to_" (class.name cls))))
    `(call ,(func.id (lookup-func env name)) ,val)))

(define (render-null? env base-expr)
  `(call ,(func.id (lookup-func env '_isnull)) ,base-expr))

(define (render-nonnull? env base-expr)
  `(i32.eqz (call ,(func.id (lookup-func env '_isnull)) ,base-expr)))

(define (render-new-class env cls args)
  (let ((name (splice "_new_" (class.name cls))))
    `(call ,(func.id (lookup-func env name)) ,@(map car args))))

(define (render-resolve-virtual env receiver-expr vid)
  `(call ,(func.id (lookup-func env '_resolve_virtual)) ,receiver-expr ,vid))

;; String literals

(define (synthesize-string-literals cx env)
  (let ((out (support.strings (cx.support cx))))
    (for-each (lambda (s)
                (write s out)
                (display ",\n" out))
            (map car (reverse (cx.strings cx))))))

(define (synthesize-string-literal cx env)
  (js-lib cx env '_string_literal `(,*i32-type*) *string-type*
          "function (n) { return self.strings[n] }"))

(define (render-string-literal env n)
  `(call ,(func.id (lookup-func env '_string_literal)) (i32.const ,n)))

;; Strings

(define (synthesize-string-ops cx env)
  (synthesize-new-string cx env)
  (synthesize-string-literal cx env)
  (synthesize-string-length cx env)
  (synthesize-string-ref cx env)
  (synthesize-string-append cx env)
  (synthesize-substring cx env)
  (synthesize-string-compare cx env)
  (synthesize-vector->string cx env)
  (synthesize-string->vector cx env)
  (synthesize-anyref-is-string cx env)
  (synthesize-upcast-string-to-anyref cx env)
  (synthesize-downcast-anyref-to-string cx env))

(define (synthesize-new-string cx env)
  (js-lib cx env '_new_string (make-list 11 *i32-type*) *string-type*
          "
function (n,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) {
  self.buffer.push(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10);
  let s = String.fromCharCode.apply(null, self.buffer.slice(0,self.buffer.length-10+n));
  self.buffer.length = 0;
  return s;
}")

  (js-lib cx env '_string_10chars (make-list 10 *i32-type*) *void-type*
              "
function (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) {
  self.buffer.push(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10);
}"))

(define (render-new-string env args)
  (let ((new_string    (lookup-func env '_new_string))
        (string_10chars (lookup-func env '_string_10chars)))
    (let loop ((n (length args)) (args args) (code '()))
      (if (<= n 10)
          (let ((args (append args (make-list (- 10 n) '(i32.const 0)))))
            `(block ,(render-type *string-type*)
                    ,@(reverse code)
                    (call ,(func.id new_string) (i32.const ,n) ,@args)))
          (loop (- n 10)
                (list-tail args 10)
                (cons `(call ,(func.id string_10chars) ,@(list-head args 10))
                      code))))))

(define (synthesize-string-length cx env)
  (js-lib cx env '_string_length `(,*string-type*) *i32-type*
          "function (p) { return p.length }"))

(define (render-string-length env expr)
  `(call ,(func.id (lookup-func env '_string_length)) ,expr))

(define (synthesize-string-ref cx env)
  (js-lib cx env '_string_ref `(,*string-type* ,*i32-type*) *i32-type*
          "function (p,n) { return p.charCodeAt(n) }"))

(define (render-string-ref env e0 e1)
  `(call ,(func.id (lookup-func env '_string_ref)) ,e0 ,e1))

(define (synthesize-string-append cx env)
  (js-lib cx env '_string_append `(,*string-type* ,*string-type*) *string-type*
          "function (p,q) { return p + q }"))

(define (render-string-append env e0 e1)
  `(call ,(func.id (lookup-func env '_string_append)) ,e0 ,e1))

(define (synthesize-substring cx env)
  (js-lib cx env '_substring `(,*string-type* ,*i32-type* ,*i32-type*) *string-type*
          "function (p,n,m) { return p.substring(m,n) }"))

(define (render-substring env e0 e1 e2)
  `(call ,(func.id (lookup-func env '_substring)) ,e0 ,e1 ,e2))

(define (synthesize-string-compare cx env)
  (js-lib cx env '_string_compare `(,*string-type* ,*string-type*) *i32-type*
          "
function (p,q) {
  let a = p.length;
  let b = q.length;
  let l = a < b ? a : b;
  for ( let i=0; i < l; i++ ) {
    let x = p.charCodeAt(i);
    let y = q.charCodeAt(i);
    if (x != y) return x - y;
  }
  return a - b;
}"))

(define (render-string-compare env e0 e1)
  `(call ,(func.id (lookup-func env '_string_compare)) ,e0 ,e1))

(define (synthesize-vector->string cx env)
  (js-lib cx env '_vector_to_string `(,(make-vector-type cx env *i32-type*)) *string-type*
          "function (x) { return String.fromCharCode.apply(null, x) }"))

(define (render-vector->string env e)
  `(call ,(func.id (lookup-func env '_vector_to_string)) ,e))

(define (synthesize-string->vector cx env)
  (js-lib cx env '_string_to_vector `(,*string-type*) (make-vector-type cx env *i32-type*)
          "function (x) { let a=[]; for(let i=0; i<x.length; i++) a.push(x.charCodeAt(i)); return a }"))

(define (render-string->vector env e)
  `(call ,(func.id (lookup-func env '_string_to_vector)) ,e))

(define (synthesize-upcast-string-to-anyref cx env)
  (js-lib cx env '_upcast_string_to_anyref `(,*string-type*) *anyref-type* "function (p) { return p }"))

(define (render-upcast-string-to-anyref env expr)
  `(call ,(func.id (lookup-func env '_upcast_string_to_anyref)) ,expr))

(define (synthesize-anyref-is-string cx env)
  (js-lib cx env '_anyref_is_string `(,*anyref-type*) *i32-type* "function (p) { return p instanceof String }"))

(define (render-anyref-is-string env val)
  `(call ,(func.id (lookup-func env '_anyref_is_string)) ,val))

(define (synthesize-downcast-anyref-to-string cx env)
  (js-lib cx env '_downcast_anyref_to_string `(,*anyref-type*) *string-type*
          "
function (p) {
  if (!(p instanceof String))
    throw new Error('Failed to narrow to string' + p);
  return p;
}"))

(define (render-downcast-anyref-to-string env val)
  `(call ,(func.id (lookup-func env '_downcast_anyref_to_string)) ,val))

;; Vectors
;;
;; Some of these are a little dodgy because they use anyref as the parameter,
;; yet that implies some kind of upcast.

(define (synthesize-vector-ops cx env element-type)
  (synthesize-new-vector cx env element-type)
  (synthesize-vector-length cx env element-type)
  (synthesize-vector-ref cx env element-type)
  (synthesize-vector-set! cx env element-type)
  (synthesize-upcast-vector-to-anyref cx env element-type))

(define (synthesize-new-vector cx env element-type)
  (let ((name (splice "_new_vector_" (render-element-type element-type))))
    (js-lib cx env name `(,*i32-type* ,element-type) (type.vector element-type)
            "function (n,init) { let a=new Array(n); for (let i=0; i < n; i++) a[i]=init; return a; }")))

(define (render-new-vector env element-type len init)
  (let ((name (splice "_new_vector_" (render-element-type element-type))))
    `(call ,(func.id (lookup-func env name)) ,len ,init)))

(define (synthesize-vector-length cx env element-type)
  (let ((name (splice "_vector_length_" (render-element-type element-type))))
    (js-lib cx env name `(,*anyref-type*) *i32-type*
            "function (p) { return p.length }")))

(define (render-vector-length env expr element-type)
  (let ((name (splice "_vector_length_" (render-element-type element-type))))
    `(call ,(func.id (lookup-func env name)) ,expr)))

(define (synthesize-vector-ref cx env element-type)
  (let ((name (splice "_vector_ref_" (render-element-type element-type))))
    (js-lib cx env name `(,*anyref-type* ,*i32-type*) element-type
            "function (p,i) { return p[i] }")))

(define (render-vector-ref env e0 e1 element-type)
  (let ((name (splice "_vector_ref_" (render-element-type element-type))))
    `(call ,(func.id (lookup-func env name)) ,e0 ,e1)))

(define (synthesize-vector-set! cx env element-type)
  (let ((name (splice "_vector_set_" (render-element-type element-type))))
    (js-lib cx env name `(,*anyref-type* ,*i32-type* ,element-type) *void-type*
            "function (p,i,v) { p[i] = v }")))

(define (render-vector-set! env e0 e1 e2 element-type)
  (let ((name (splice "_vector_set_" (render-element-type element-type))))
    `(call ,(func.id (lookup-func env name)) ,e0 ,e1 ,e2)))

(define (synthesize-upcast-vector-to-anyref cx env element-type)
  (let ((name (splice "_upcast_vector_" (render-element-type element-type) "_to_anyref")))
    (js-lib cx env name `(,*anyref-type*) *anyref-type*
            "function (p) { return p }")))

(define (render-upcast-vector-to-anyref env element-type expr)
  (let ((name (splice "_upcast_vector_" (render-element-type element-type) "_to_anyref")))
    `(call ,(func.id (lookup-func env name)) ,expr)))

(define (render-anyref-is-vector env e element-type)
  ;; FIXME
  '(i32.const 1))

(define (render-downcast-anyref-to-vector env e element-type)
  ;; FIXME
  `(ref.null anyref))

(define (render-element-type element-type)
  (cond ((vector-type? element-type)
         (string-append "@" (render-element-type (type.vector-element element-type))))
        ((class-type? element-type)
         (symbol->string (class.name (type.class element-type))))
        (else
         (symbol->string (type.name element-type)))))

;; Driver for scripts

(define (swat-noninteractive)
  (define js-mode #f)
  (define js-binary-mode #f)
  (define stdout-mode #f)
  (define expect-success #t)
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
                          ((string=? arg "--js-binary")
                           (set! js-binary-mode #t)
                           (loop (cdr args)))
                          ((or (string=? arg "--stdout") (string=? arg "-s"))
                           (set! stdout-mode #t)
                           (loop (cdr args)))
                          ((or (string=? arg "--fail") (string=? arg "-f"))
                           (set! expect-success #f)
                           (loop (cdr args)))
                          ((or (string=? arg "--help") (string=? arg "-h"))
                           (display "Usage: swat options file ...") (newline)
                           (display "Options:") (newline)
                           (display "  -j  --js      Generate .wast.js") (newline)
                           (display "  --js-binary   Generate .metawast.js and .wasm.js") (newline)
                           (display "  -s  --stdout  Print output to stdout") (newline)
                           (display "  -f  --fail    Expect failure, reverse exit codes") (newline)
                           (exit 1))
                          ((and (> len 0) (char=? (string-ref arg 0) #\-))
                           (fail "Bad option" arg "  Try --help"))
                          ((and (> len 5) (string=? (substring arg (- len 5) len) ".swat"))
                           (set! files (cons arg files))
                           (loop (cdr args)))
                          (else
                           (fail "Bad file name" arg))))))
            (for-each
             (lambda (input-filename)
               (call-with-input-file input-filename
                 (lambda (in)
                   (cond (stdout-mode
                          (process-file input-filename in "<stdout>" (current-output-port) js-mode)
                          (if js-binary-mode
                              (fail "--js-binary-mode not compatible with --stdout")))
                         ((not expect-success)
                          (if js-binary-mode
                              (fail "--js-binary-mode not compatible with --fail"))
                          (process-file input-filename in "<str>" (open-output-string) js-mode))
                         (else
                          (let ((root (substring input-filename 0 (- (string-length input-filename) 5))))
                            (cond (js-mode
                                   (let ((output-filename (string-append root ".wast.js")))
                                     (call-with-output-file output-filename
                                       (lambda (out)
                                         (process-file input-filename in 'js out)))))
                                  (js-binary-mode
                                   (let ((output-filename1 (string-append root ".wast.js"))
                                         (output-filename2 (string-append root ".metawast.js")))
                                     (call-with-output-file output-filename1
                                       (lambda (out1)
                                         (call-with-output-file output-filename2
                                           (lambda (out2)
                                             (process-file input-filename in 'js-binary out1 out2)))))))
                                  (else
                                   (let ((output-filename (string-append root ".wast")))
                                     (call-with-output-file output-filename
                                       (lambda (out)
                                         (process-file input-filename in 'wast out))))))))))))
             files)
            #t))))
    (eq? result expect-success)))

(define (process-file input-filename in mode out1 . options)
  (do ((phrase (read-source input-filename in) (read-source input-filename in)))
      ((eof-object? phrase))
    (cond ((and (pair? phrase) (eq? (car phrase) 'defmodule))
           (let ((support (make-support)))
             (let-values (((name code) (expand-module phrase support)))
               (write-module out1 name support code mode)
               (if (eq? mode 'js-binary)
                   (let ((out2 (car options)))
                     (write-metawast out2 name code))))))
          ((and (pair? phrase) (eq? (car phrase) 'js))
           (write-js out1 (cadr phrase) mode))
          (else
           (fail "Bad toplevel phrase" phrase)))))

(define (read-source filename in)
  (with-exception-handler
   (lambda (x)
     (if (read-error? x)
         (fail "Malformed input on" filename "\n" (error-object-message x))
         (fail "Unknown input error on" filename "\n" x)))
   (lambda ()
     (read in))))

(define (write-module out name support code mode)
  (case mode
    ((js js-binary)
     (format out "var ~a =\n(function () {\nvar TO=TypedObject;\nvar self = {\n" name)
     (if (eq? mode 'js)
         (begin
           (format out "module:\nnew WebAssembly.Module(wasmTextToBinary(`")
           (format-module out code)
           (format out "`)),\n"))
         (format out "module:\nnew WebAssembly.Module(~a_bytes),\n" name))
     (format out "desc:\n{\n")
     (format out "~a\n" (get-output-string (support.desc support)))
     (format out "},\n")
     (format out "types:\n{\n")
     (format out "~a\n" (get-output-string (support.type support)))
     (format out "},\n")
     (format out "strings:\n[\n")
     (format out "~a\n" (get-output-string (support.strings support)))
     (format out "],\n")
     (format out "buffer:[],\n")
     (format out "lib:\n{\n")
     (format out "_test: function(x,ys) {
let i=ys.length;
while (i > 0) {
  --i;
  if (ys[i] == x)
    return true;
}
return false;
},\n")
     (format out "~a\n" (get-output-string (support.lib support)))
     (format out "}};\nreturn self })();"))
    ((wast)
     (display (string-append ";; " name) out)
     (newline out)
     (format-module out code))
    (else
     ???)))

(define (write-metawast out name code)
  (format out "var ~a_bytes = wasmTextToBinary(`\n" name)
  (format-module out code)
  (format out "`);\n")
  (format out "new WebAssembly.Module(~a_bytes);\n" name)
  (format out "print('var ~a_bytes = new Uint8Array([' + new Uint8Array(~a_bytes).join(',') + ']).buffer;\\n');\n" name name))

(define (write-js out code mode)
  (case mode
    ((js js-binary)
     (display code out)
     (newline out))
    ((wast) #t)
    (else ???)))

(define (format-module out x)
  (newline out)
  (pretty-print x out))

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
