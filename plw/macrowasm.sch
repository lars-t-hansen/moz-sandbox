;;; -*- fill-column: 80 -*-
;;;
;;; Copyright 2017 Lars T Hansen
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License.  You may obtain a copy
;;; of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
;;; License for the specific language governing permissions and limitations
;;; under the License.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MacroWasm - a macro-assembler for WebAssembly's s-expression form.
;;;
;;; Usage:
;;;  (macrowasm [-o output-filename] [-S] [-v] input-filename)
;;;
;;; All arguments are strings, to facilitate running this as a shell command.
;;;
;;; input-filename
;;;        The file to process.  Conventionally this has extension .mwas.
;;;        Absent any -S or -o option, the output is written to a file
;;;        with the same name but with extension .wasm.
;;;
;;; -o output-filename
;;;        Override the computed output filename, whatever it is.
;;;
;;; -S     Output source text, not source binary.  Computed output name has
;;;        extension .wast.
;;;
;;; -v     Verbose.  Print more diagnostics.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FEATURES

;;; NAMED LOCALS
;;;
;;; Functions may be declared with named short-hands for parameters and locals,
;;; offsets will be assigned for you:
;;;
;;;  (func _name_ ((_pname_ _ptype_) ... -> _rtype_)
;;;     (local (_vname_ _vtype_) ...)
;;;     ...)
;;;
;;; All local and param names must be distinct.
;;;
;;; If a function is declared with named short-hands for parameters and locals
;;; then they can be referenced by names.  A name in an rvalue position will
;;; expand to a (get_local k) for appropriate k; a name in an assignment will
;;; expand to a (set_local k v) or (tee_local k v):
;;;
;;;     (op n m)  => (op (get_local k) (get_local k'))  ; k and k' for n and m
;;;     (set n e) => (set_local k e)                    ; value dropped
;;;     (tee n e) => (tee_local k e)                    ; value used
;;;
;;;
;;; TYPED CONSTANTS
;;;
;;;   1 will expand to (i32.const 1)
;;;   3.0 will expand to (f64.const 3.0)
;;;
;;; Constants can be prefixed to indicate the type explicitly and express
;;; Float32 and Int64 values:
;;;
;;;   I/1 will expand to (i32.const 1)
;;;   L/1 will expand to (i64.const 1)
;;;   D/3.0 will expand to (f64.const 3.0)
;;;   F/3.0 will expand to (f32.const 3.0)
;;;
;;; For the prefixed constants, the fraction is optional, and one may also add
;;; an exponent part.  (The syntax is a consequence of Scheme not allowing
;;; suffixes on numbers; we could have used another infix character but / is
;;; pretty distinctive.)
;;;
;;;
;;; OPERATOR SHORTHANDS WITH TYPE INFERENCE
;;;
;;; Types will be inferred for operators from their arguments so that shorter
;;; operand names can be used.  For example, `(+ 1 2)` will expand to
;;; `(i32.add (i32.const 1) (i32.const 2))`, as will `(add 1 2)` and
;;; `(add (i32.const 1) 2)`, for that matter.  Inference is strictly bottom-up,
;;; and the types of called functions will also be used (imports not yet supported).
;;;
;;; Currently supported binary operator aliases:
;;;
;;;   + - * / % add sub mul div mod
;;;   < <= > >= == != lt le gt ge eq ne
;;;   <u <=u >u >=u lt_u le_u gt_u ge_u eq_u ne_u
;;;
;;;
;;; OTHER TYPE INFERENCE
;;;
;;; Type annotations are inferred for BLOCK from the last expression.
;;;
;;;
;;; COMMON MACROS
;;;
;;; We expand standard cliches into standard sequences, more will come here:
;;; 
;;;   (while e body ...) => (block (loop (br_if 1 (eqz e)) body ... (br 0)))
;;;
;;;
;;; EXAMPLE
;;;
;;; This is valid code:
;;;
;;; (func "fib" ((n i32) -> i32)
;;;       (if (< n 2)
;;;           n
;;;	      (+ (fib (- n 1)) (fib (- n 2)))))

;;; TODO
;;;
;;; - much more testing - write some substantial programs?
;;; - support get_global, set_global (just syntax, for now), others?
;;; - support 'switch'.
;;; - support 'label', 'break', 'continue'.  Various ways to do that.  We don't
;;;   need a label form because a symbol following 'while' or 'block' or 'loop'
;;;   is not ambiguous and can serve as the label.
;;; - more inferred type support (many unops and some binops)
;;; - a few more macros (and, or, %asm)
;;; - we must be able to produce a binary form, not just the textual form
;;; - we have type info, so we can perform better type checking, which will benefit
;;;   producing binary
;;; - support 'traditional' functions partially: infer return type, pass empty
;;;   locals structure, check that there are no locals, limited inference in body,
;;;   might not bother to infer local types
;;; - add simple defstruct facility a la flatjs / old-timey C, with inference:
;;;    (ld fieldname E)    ; type and offset inferred from fieldname, which is unique globally
;;;    (st fieldname E V)

;;; Driver

(define opt-input-file #f)
(define opt-output-file #f)
(define opt-to-source? #f)
(define opt-verbose? #f)

(define (macrowasm . arguments)
  (let loop ((arguments arguments))
    (match arguments
	   '()                 accept0

	   '("-o" _fn . _rest) (lambda (fn rest)
				 (check-string fn "Output file name must be a string")
				 (if opt-output-file
				     (fail "Output file already provided"))
				 (set! opt-output-file fn)
				 (loop rest))

	   '("-S" . _rest)     (lambda (rest)
				 (set! opt-to-source? #t)
				 (loop rest))

	   '("-v" . _rest)     (lambda (rest)
				 (set! opt-verbose? #t)
				 (loop rest))

	   '(_fn . _rest)      (lambda (fn rest)
				 (if opt-input-file
				     (fail "Input file already provided"))
				 (set! opt-input-file fn)
				 (loop rest))

	   '_args              (reject "Unknown compiler arguments")))

  (if (not opt-input-file)
      (fail "No input file"))

  (if (not opt-output-file)
      (set! opt-output-file (string-append (strip-extension opt-input-file)
					   (if opt-to-source? ".wast" ".wasm"))))

  (if opt-verbose?
      (begin (display "Input file: ") (display opt-input-file) (newline)
	     (display "Output file: ") (display opt-output-file) (newline)
	     (display "Source? ") (display opt-to-source?) (newline)))

  (let ((module (call-with-input-file opt-input-file read)))
    (let ((expanded (expand-module module)))
      (if opt-to-source?
	  (call-with-output-file opt-output-file
	    (lambda (out)
	      (pretty-print expanded out)))
	  (fail "Assembly not yet implemented, use -S and rely on the engine's text-to-wasm translator")))))


;;; Expander

(define table #f)
(define memory #f)
(define imports '())
(define exports '())
(define funcs '())
(define functions #f)			; Signatures, for type inference

(define (expand-module m)

  (set! table #f)
  (set! memory #f)
  (set! imports '())
  (set! exports '())
  (set! funcs '())
  (set! functions (make-functions))
  
  (match m
	 '(module . _forms)
	 (lambda (forms)
	   (for-each add-function-if-function forms)
	   (for-each expand-module-body-form forms)))

  `(module
    ,@(if memory (list memory) '())
    ,@(if table (list table) '())
    ,@(reverse imports)
    ,@(reverse funcs)
    ,@(reverse exports)))

(define (add-function-if-function form)
  (match form
	 '(func _name () . _whatever)
	 (lambda (name whatever)
	   (add-function! functions name 'void))

	 '(func _name (-> _retn) . _whatever)
	 (lambda (name retn whatever)
	   (add-function! functions name retn))

	 '(func _name ((_pname _ptype) . _sign) . _body)
	 (lambda (name pname ptype sign body)
	   (let ((type (let loop ((sign sign))
			 (match sign
				'()                accept0
				'(-> _retn)        accept1
				'((_a _b) . _rest) (lambda (a b rest)
						     (loop rest))))))
	     (add-function! functions name type)))

	 '_
	 accept1))

(define (expand-module-body-form f)
  (match f
	 '(import _name)
	 expand-import

	 '(export _name)
	 expand-export

	 ;; New-style function, no named parameters, no return
	 '(func _name () . _body)
	 expand-macro-func

	 ;; New-style function, no named parameters, but return
	 '(func _name (-> _retn) . _body)
	 (lambda (name retn body)
	   (expand-macro-func name `(-> ,retn) ,@body))

	 ;; New-style function, at least one named parameter, don't know the rest
	 '(func _name ((_pname _ptype) . _sign) . _body)
	 (lambda (name pname ptype sign body)
	   (expand-macro-func name (cons (list pname ptype) sign) body))

	 ;; Old-style function
	 '(func _name . _whatever)
	 expand-func))

(define (expand-import name)
  (set! imports (cons `(import ,name) imports)))

(define (expand-export name)
  (set! exports (cons `(export ,name) exports)))

(define (expand-macro-func name sig body)
  (let ((locals (make-locals)))
    (let-values (((param-names ret-type) (expand-signature sig locals))
		 ((local-names body) (expand-locals-and-body body locals)))
      (set! funcs (cons `(func ,name
			       ,@(map (lambda (name)
					(let ((l (get-local locals name)))
					  `(param ,(local.type l) ,(local.slot l))))
				      param-names)
			       ,@(if ret-type (list `(result ,ret-type)) '())
			       ,@(map (lambda (name)
					(let ((l (get-local locals name)))
					  `(local ,(local.type l))))
				      local-names)
			       ,@body)
			funcs)))))

;; Returns return type or #f, and parameter names in original order.

(define (expand-signature sig locals)
  (let ((ret-type #f)
	(param-names '()))
    (let loop ((sig sig))
      (match sig
	     '((_name _type) . _rest)
	     (lambda (name type rest)
	       (add-local! locals name type)
	       (set! param-names (cons name param-names))
	       (loop rest))

	     '(-> _type)
	     (lambda (type)
	       (set! ret-type type))

	     '()
	     accept0))
    (values (reverse param-names) ret-type)))

(define (expand-locals-and-body body locals)
  (let* ((local-names '())
	 (body
	  (match body
		 '((local . _ldefs) . _body)
		 (lambda (ldefs body)
		   (check-list ldefs "Bad locals list")
		   (for-each (lambda (ldef)
			       (match ldef
				      '(_name _type)
				      (lambda (name type)
					(set! local-names (cons name local-names))
					(add-local! locals name type))))
			     ldefs)
		   body)

		 '_body
		 accept1)))
    (values (reverse local-names)
	    (expand-body body locals))))
	 

;; Old-style function.  Different rules here, we can still expand
;; macros but we will have no local scope for names.  There can be no
;; "locals" form and no identifiers, code must use get_local and
;; set_local.  We can still have useful type inference, if we want.

(define (expand-func name whatever)
  (set! funcs (cons `(func ,name ,@whatever) funcs)))

(define (expand-body body locals)

  ;; Obvious syntax to add:
  ;;
  ;; (and a b) => (if a b 0)
  ;; (or a b) => (if a 1 b)
  ;; (%asm token ...) => token ...
  ;;    with form (%local name) to yields the local index for the named local
  ;;    maybe also form (%block L) to get block index, (%loop L) ditto for loops,
  ;;    from a label form name L
  ;;
  ;; Memory references:
  ;;  Maybe (ld i32 p), (st i32 p v) to match the field reference syntax for structures, which
  ;;  would have a fieldname in that slot designating an offset.  Here we would use eg u8, u16
  ;;  as types designating not offset but size.   Operator would then be inferred from that type,
  ;;  and from the operand type for store, to eg i64.store/u8 if the field says u8 and the
  ;;  expression type is int64.
  
  ;; Returns two values, rewritten-expr and type

  (define (expand-expr e)
    (display e) (newline)
    (cond ((symbol? e)
	   (let ((l (probe-local locals e)))
	     (if l
		 (values `(get_local ,(local.slot l))
			 (local.type l))
		 (let ((name (symbol->string e)))
		   (if (and (> (string-length name) 2)
			    (char=? (string-ref name 1) #\/))
		       (let ((rest (substring name 2 (string-length name))))
			 (case (string-ref name 0)
			   ((#\I) (values `(i32.const ,(check-int32 (string->number rest)))
					  'i32))
			   ((#\L) (values `(i64.const ,(check-int64 (string->number rest)))
					  'i64))
			   ((#\D) (values `(f64.const ,(check-float64 (string->number rest)))
					  'f64))
			   ((#\F) (values `(f32.const ,(check-float32 (string->number rest)))
					  'f32))
			   (else (fail "Illegal tagged number" name))))
		       (fail "Reference to unbound variable" e))))))
	  ((number? e)
	   (if (exact? e)
	       (values `(i32.const ,(check-int32 e))
		       'i32)
	       (values `(f64.const ,(check-float64 e))
		       'f64)))
	  ((and (list? e) (not (null? e)))
	   (if (symbol? (car e))
	       (match e
		      '(set _v _expr)
		      (lambda (v expr)
			(let-values (((value-expr value-type) ,(expand-expr expr)))
			  ;; Could check that value-type equals local.type
			  (values `(set_local ,(local.slot (get-local locals v)) ,value-expr)
				  'void)))

		      '(tee _v _expr)
		      (lambda (v expr)
			(let-values (((value-expr value-type) ,(expand-expr expr)))
			  (let ((l (get-local locals v)))
			    ;; Could check that value-type equals local.type
			    (values `(tee_local ,(local.slot l) ,value-expr)
				    (local.type l)))))

		      '(while _expr . _body)
		      (lambda (expr body)
			(let-values (((test-expr test-type) (expand-expr expr))
				     ((body-exprs body-types) (map-expand body)))
			  ;; Could check that test-type is i32
			  ;; Could check that last body expr is void
			  (values `(block
				    (loop
				     (br_if 1 (i32.eqz ,test-expr))
				     ,@body-exprs
				     (br 0)))
				  'void)))

		      '(break)
		      (lambda ()
			;; Note, break can be nested inside blocks inside the loop,
			;; so we must track depth
			(fail "Break NYI"))

		      '(continue)
		      (lambda ()
			;; Note, as for break
			(fail "Continue NYI"))

		      '(drop _e)
		      (lambda (e)
			(let-values (((expr type) (expand-expr e)))
			  (values `(drop ,expr) 'void)))

		      '(if _test _consequent)
		      (lambda (test consequent)
			(let-values (((test-expr test-type) (expand-expr test))
				     ((consequent-expr consequent-type) (expand-expr consequent)))
			  (values `(if ,test-expr ,consequent-expr)
				  'void)))

		      '(if _test _consequent _alternate)
		      (lambda (test consequent alternate)
			(let-values (((test-expr test-type) (expand-expr test))
				     ((consequent-expr consequent-type) (expand-expr consequent))
				     ((alternate-expr alternate-type) (expand-expr alternate)))
			  (let ((result (infer-type "if" consequent-type alternate-type)))
			    (values `(if ,test-expr ,consequent-expr ,alternate-expr)
				    result))))
		      
		      '(select _test _consequent _alternate)
		      (lambda (test consequent alternate)
			(let-values (((test-expr test-type) (expand-expr test))
				     ((consequent-expr consequent-type) (expand-expr consequent))
				     ((alternate-expr alternate-type) (expand-expr alternate)))
			  (let ((result (infer-type "select" consequent-type alternate-type)))
			    (values `(select ,test-expr ,consequent-expr ,alternate-expr)
				    result))))

		      '(block . _rest)
		      (lambda (rest)
			(let-values (((exprs types) (map-expand rest)))
			  (let ((last-type (if (null? types) (car (last-pair types)) 'void)))
			    (values `(block ,@(maybe-type last-type) ,@exprs)
				    last-type))))

		      '(unreachable)
		      (lambda ()
			(values '(unreachable) 'void))

		      '(switch _expr . _cases)
		      (lambda (expr cases)
			;; for now, each case is (case _value _expr ...)
			;; where the _value must be a constant, we'll lift that later
			;; we'll generate a dense switch for sure
			;; missing values will branch to default
			;; there is no fallthrough here
			(fail "Switch NYI"))

		      '(_op . _operands)
		      (lambda (op operands)
			(display (list 'call op operands)) (newline)
			;; TODO: Type inference for unary operators as well!
			;; TODO: We can infer eg i32.wrap/i64 from just "wrap"
			;;       and the operand type.
			(let-values (((exprs types) (map-expand operands)))
			  (cond ((and (= (length operands) 2) (assq op binops))
				 => (lambda (probe)
				      (expand-binop op (cdr probe) exprs types)))
				((and (> (string-length (symbol->string op)) 4)
				      (assoc (substring (symbol->string op) 0 4) type-prefixes))
				 => (lambda (probe)
				      (values `(,op ,exprs) (cdr probe))))
				(else
				 (expand-call op exprs types))))))
	       (fail "Unknown syntax " e)))
	  (else
	   (fail "Unknown syntax " e))))

  (define binops
    '((+ . "add") (- . "sub") (* . "mul") (/ . "div") (% . "mod")
      (add . "add") (sub . "sub") (mul . "mul") (div . "div") (mod . "mod")
      (< . "lt") (<= . "le") (> . "gt") (>= . "ge") (== . "eq") (!= . "ne")
      (le . "le") (gt . "gt") (ge . "ge") (eq . "eq") (ne . "ne")
      (<u . "lt_u") (<=u . "le_u") (>u . "gt_u") (>=u . "ge_u") (lt . "lt")
      (lt_u . "lt_u") (le_u . "le_u") (gt_u . "gt_u") (ge_u . "ge_u")))

  (define type-prefixes
    '(("i32" . i32) ("i64" . i64) ("f32" . f32) ("f64" . f64)))

  (define (expand-binop op new-name exprs types)
    (display (list op new-name exprs types)) (newline)
    (let ((t (infer-type (symbol->string op) (car types) (cadr types))))
      (if (eq? t 'void)
	  (fail "Can't infer operator type" op))
      (values `(,(string->symbol (string-append (symbol->string t) "." new-name))
		,@exprs)
	      t)))

  (define (expand-call op exprs types)
    (let ((f (probe-function functions (symbol->string op))))
      (if (not f)
	  (fail "Function not found" op))
      (values `(,op ,@exprs)
	      (function.return-type f))))

  (define (map-expand es)
    (let ((exprs '())
	  (types '()))
      (for-each (lambda (e)
		  (let-values (((expr type) (expand-expr e)))
		    (set! exprs (cons expr exprs))
		    (set! types (cons type types))))
		es)
      (values (reverse exprs) (reverse types))))

  (define (infer-type where t1 t2)
    (cond ((eq? t1 t2) t1)
	  (else (fail (string-append "Unequal types in " where)))))

  (define (maybe-type t)
    (if (eq? t 'void)
	'()
	(list t)))
  
  (let-values (((exprs types) (map-expand body)))
    exprs))


;;; Store for functions

(define (make-functions)
  (vector 'functions '()))

(define (functions.store f) (vector-ref f 1))
(define (functions.store-set! f v) (vector-set! f 1 v))

(define (make-function name return-type)
  (list name return-type))

(define function.name car)
(define function.return-type cadr)

(define (add-function! f name return-type)
  (display (list f name return-type)) (newline)
  (let ((probe (assoc name (functions.store f))))
    (if probe
	(fail "Function already defined" name))
    (functions.store-set! f (cons (make-function name return-type) (functions.store f)))))

(define (probe-function f name)
  (assoc name (functions.store f)))

  
;;; Store for local variables

(define (make-locals)
  (vector 'locals 0 '()))

(define (locals.num l) (vector-ref l 1))
(define (locals.num-set! l v) (vector-set! l 1 v))
(define (locals.store l) (vector-ref l 2))
(define (locals.store-set! l v) (vector-set! l 2 v))

(define (make-local name type slot)
  (list name type slot))

(define local.name car)
(define local.type cadr)
(define local.slot caddr)

(define (add-local! l name type)
  (check-symbol name "Not a local name")
  (check-symbol type "Not a type name")
  (let ((probe (assq name (locals.store l))))
    (if probe
	(fail "Name already defined " name))
    (let ((n (locals.num l)))
      (locals.num-set! l (+ n 1))
      (locals.store-set! l (cons (make-local name type n) (locals.store l))))))

(define (get-local l name)
  (let ((probe (assq name (locals.store l))))
    (if (not probe)
	(fail "Name not defined " name))
    probe))

(define (probe-local l name)
  (assq name (locals.store l)))


;;; Matcher

;; Match an s-expr pattern against a value in the obvious way,
;; invoking a function on the first pattern that matches.  Pattern
;; variables are introduced by symbols whose names start with '_',
;; they should be distinct for now, eventually we may allow references
;; to earlier bindings.
;;
;; Clearly pattern sets can in principle be compiled to more efficient
;; matchers, should it come to that.
;;
;; Clearly we can add syntax for this if we care.

(define (match expr . patterns)

  (define assignments '())

  (define (descend p e)
    (cond ((and (null? p) (null? e)))
	  ((and (number? p) (number? e) (= p e)))
	  ((and (string? p) (string? e) (string=? p e)))
	  ((and (char? p) (char? e) (char=? p e)))
	  ((and (vector? p) (vector? e)
		(= (vector-length p) (vector-length e))
		(let loop ((i 0))
		  (cond ((= i (vector-length p))
			 #t)
			((descend (vector-ref p i) (vector-ref e i))
			 (loop (+ i 1)))
			(else #f)))))
	  ((and (pair? p) (pair? e)
		(descend (car p) (car e))
		(descend (cdr p) (cdr e))))
	  ((symbol? p)
	   (if (variable-name? p)
	       (begin (set! assignments (cons (cons p e) assignments))
		      #t)
	       (eq? p e)))
	  (else #f)))

  (define (variable-name? p)
    (let ((name (symbol->string p)))
      (and (> (string-length name) 0)
	   (char=? (string-ref name 0) #\_))))

  (define (match patterns)
    (if (null? patterns)
	(fail "Value does not match any pattern: " expr))
    (let ((p (car patterns))
	  (f (cadr patterns)))
      (set! assignments '())
      (if (descend p expr)
	  (apply f (reverse (map cdr assignments)))
	  (match (cddr patterns)))))

  (if (not (even? (length patterns)))
      (fail "Matcher error: bad pattern list"))

  (match patterns))

(define (accept0)
  #t)

(define (accept1 v)
  v)

(define (reject what)
  (lambda (bad)
    (fail (string-append "Bad " what) bad)))


;;; Return the file name without the extension (removing also the ".")
;;; but with the path name intact.

(define (strip-extension fn)
  (let ((len (string-length fn)))
    (let loop ((i (- len 1)))
      (cond ((< i 0) fn)
	    ((char=? #\. (string-ref fn i))
	     (if (= i 0)
		 (fail "Degenerate file name " fn))
	     (substring fn 0 i))
	    ((char=? #\/ (string-ref fn i))
	     fn)
	    (else
	     (loop (- i 1)))))))
	    

;;; Signal failure.  Here we may eventually throw to an error handler.

(define (fail . irritants)
  (apply error irritants))


;;; Miscellaneous

(define (check-list v msg)
  (if (not (list? v))
      (fail msg v)
      v))

(define (check-symbol v msg)
  (if (not (symbol? v))
      (fail msg v)
      v))

(define (check-string v msg)
  (if (not (string? v))
      (fail msg v)
      v))

(define (check-int32 v)
  (if (or (not (exact? v)) (not (<= #x-80000000 v #xFFFFFFFF)))
      (fail "Illegal int32 value" v)
      v))

(define (check-int64 v)
  (if (or (not (exact? v)) (not (<= #x-8000000000000000 v #xFFFFFFFFFFFFFFFF)))
      (fail "Illegal int64 value" v)
      v))

(define (check-float64 v)
  (if (number? v)
      (if (exact? v)
	  (exact->inexact v)
	  v)
      (fail "Illegal float64 value")))

(define (check-float32 v)
  (if (number? v)
      (if (exact? v)
	  (exact->inexact v)
	  v)
      (fail "Illegal float32 value")))
