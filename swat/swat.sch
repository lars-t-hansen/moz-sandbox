(define (swat filename)
  (set! names '())
  (set! funcs '())
  (set! func-slot 0)
  (set! globals '())
  (set! structs '())
  (call-with-input-file filename
      (lambda (f)
	(let ((phrase (read f)))
	  (pretty-print (expand-module phrase))))))

(define globals '())			; assoc list (name slot type)
(define structs '())			; assoc list (name ...)

(define (expand-module m)
  (check-form m)
  (check-head m 'module)
  (do ((defs (cdr m) (cdr defs)))
      ((null? defs))
    (let ((d (car defs)))
      (check-form d)
      (case (car d)
	((func func+)
	 (expand-func d))
	(else
	 (error "Unknown toplevel phrase " d)))))
  (map func-binding-defn funcs))

(define names '())			; list of already-taken global names

(define (define-name! name)
  (if (memq name names)
      (error "Duplicate global name: " name))
  (set! names (cons name names)))

(define funcs '())			; assoc list (name slot)
(define func-slot 0)

(define (define-function name export? params result-type)
  (let* ((number func-slot)
	 (cell (make-func-binding name export? number params result-type #f)))
    (set! func-slot (+ func-slot 1))
    (set! funcs (cons cell funcs))
    cell))

(define (assemble-function cell body)
  (let* ((f body)
	 (f (if (eq? (func-binding-result cell) 'void)
		f
		(cons `(result ,(func-binding-result cell)) f)))
	 (f (append (func-binding-params cell) f))
	 (f (if (func-binding-export? cell)
		(cons `(export ,(symbol->string (func-binding-name cell))) f)
		f))
	 (f (cons 'func f)))
    (func-binding-defn-set! cell f)))

(define (expand-func f)
  (let* ((signature (cadr f))
	 (name      (car signature))
	 (export?   (eq? (car f) 'func+))
	 (body      (cddr f)))
    (define-name! name)
    (let loop ((xs     (cdr signature))
	       (locals '())
	       (slots  (make-slots))
	       (params '()))
      (cond ((null? xs)
	     (let ((cell (define-function name export? (reverse params) 'void)))
	       (assemble-function cell (expand-body body locals slots))))

	    ((eq? (car xs) '->)
	     (if (not (= 2 (length xs)))
		 (error "Bad signature " signature))
	     (let* ((t    (parse-result-type (cadr xs)))
		    (cell (define-function name export? (reverse params) t)))
	       (assemble-function cell (expand-body body locals slots))))

	    (else
	     (let ((first (car xs)))
	       (if (not (and (list? first) (= 2 (length first)) (symbol? (car first))))
		   (error "Bad parameter " first))
	       (let ((t    (parse-type (cadr first)))
		     (name (car first)))
		 (if (assq name locals)
		     (error "Duplicate parameter " name))
		 (let-values (((slots slot) (claim-param slots t)))
		   (loop (cdr xs)
			 (cons (make-binding name slot t) locals)
			 slots
			 (cons `(param ,t) params))))))))))

(define (make-func-binding name export? slot params result defn)
  (cons name (vector export? slot params result defn)))
(define func-binding-name car)
(define func-binding-export? (lambda (x) (vector-ref (cdr x) 0)))
(define func-binding-slot (lambda (x) (vector-ref (cdr x) 1)))
(define func-binding-params (lambda (x) (vector-ref (cdr x) 2)))
(define func-binding-result (lambda (x) (vector-ref (cdr x) 3)))
(define func-binding-defn (lambda (x) (vector-ref (cdr x) 4)))
(define func-binding-defn-set! (lambda (x y) (vector-set! (cdr x) 4 y)))

(define (make-binding name slot type) (list name slot type))
(define binding-name car)
(define binding-slot cadr)
(define binding-type caddr)

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
		 (else (error "Bad type " t)))))
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

(define (parse-type t)
  (case t
    ((i32 i64 f32 f64) t)
    (else
     (error "Bad type " t))))

(define (parse-result-type t)
  (if (eq? t 'void)
      t
      (parse-type t)))

(define (expand-body body locals slots)
  (let-values (((expanded result-type) (expand-expr (cons 'begin body) locals slots)))
    (display (get-slot-decls slots)) (newline)
    (append (get-slot-decls slots) (list expanded))))

;;; returns ((expr ...) type locals)
;;; where the expr ... are rewritten, type is the computed type, and locals are extended

(define (expand-expr expr locals slots)
  (cond ((symbol? expr)
	 (expand-symbol expr locals slots))
	((number? expr)
	 (expand-number expr locals slots))
	((form? expr)
	 (expand-form expr locals slots))
	(else
	 ...)))

;;; Number constants
;;; I.33 aka 33
;;; L.33
;;; F.33.5
;;; D.33.5 aka 33.5

(define (expand-symbol expr locals slots)
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
	   (lambda (binding)
	     (values `(get_local ,(binding-slot binding)) (binding-type binding))))
	  ((assq expr globals) =>
	   (lambda (binding)
	     (values `(get_global ,(binding-slot binding)) (binding-type binding))))
	  (else
	   (error "Bad syntax " expr)))))

(define min-i32 (- (expt 2 31)))
(define max-i32 (- (expt 2 31) 1))

(define (expand-number expr locals slots)
  (cond ((and (integer? expr) (exact? expr))
	 (if (<= min-i32 expr max-i32)
	     (values `(i32.const ,expr) 'i32)
	     (values `(i64.const ,expr) 'i64)))
	((number? expr)
	 (values `(f64.const ,expr) 'f64))
	(else
	 (error "Bad syntax " expr))))

(define (expand-form expr locals slots)
  (case (car expr)
    ((begin)
     (if (null? (cdr expr))
	 (error "Empty 'begin'")
	 (let-values (((e0 t0) (expand-expr (cadr expr) locals slots)))
	   (let loop ((exprs (cddr expr)) (body (list e0)) (ty t0))
	     (if (null? exprs)
		 (if (= (length body) 1)
		     (values (car body) ty)
		     (values `(block ,ty ,@(reverse body)) ty))
		 (let-values (((e1 t1) (expand-expr (car exprs) locals slots)))
		   (loop (cdr exprs) (cons e1 body) t1)))))))

    ((if)
     (let-values (((test t0) (expand-expr (cadr expr) locals slots)))
       (case (length expr)
	 ((3)
	  (let-values (((consequent t1) (expand-expr (caddr expr) locals slots)))
	    (values `(if ,test ,consequent) 'void)))
	 ((4)
	  (let*-values (((consequent t1) (expand-expr (caddr expr) locals slots))
			((alternate  t2) (expand-expr (cadddr expr) locals slots)))
	    (check-same-type t1 t2)
	    (values `(if ,t1 ,test ,consequent ,alternate) t1)))
	 (else
	  (error "Bad 'if'" expr)))))

    ((set!)
     (check-length expr 3)
     (let ((name (cadr expr)))
       (let-values (((e0 t0) (expand-expr (caddr expr) locals)))
	 (let ((probe (assq name locals)))
	   (if probe
	       (values `(set_local ,(binding-slot probe) ,e0) 'void))
	   (let ((probe (assq name globals)))
	     (if probe
		 (values `(set_global ,(binding-slot probe) ,e0) 'void)
		 (error "No binding found for " name)))))))

    ((let*)
     ;; (let* (((id type) init) ...) expr ...)
     ;; where init can be omitted and defaults to 0 of the appropriate type
     (...))

    ((loop)
     ...)

    ((+ - * < <= > >= = !=)
     (check-list expr 3)
     (let*-values (((op1 t1) (expand-expr (cadr expr) locals slots))
		   ((op2 t2) (expand-expr (caddr expr) locals slots)))
       (check-same-type t1 t2)
       (let ((name (string->symbol (string-append (symbol->string t1) (binop-name (car expr))))))
	 (values `(,name ,op1 ,op2) t1))))

    (else
     (let* ((name  (car expr))
	    (probe (assq name funcs)))
       (if (not probe)
	   (error "Not a function: " name))
       (values `(call ,(func-binding-slot probe)
		      ,@(map (lambda (e)
			       (let-values (((new-e ty) (expand-expr e locals slots)))
				 new-e))
			     (cdr expr)))
	       (func-binding-result probe))))))

(define (binop-name x)
  (case x
    ((+) ".add")
    ((-) ".sub")
    ((*) ".mul")
    ((<) ".lt_s")
    ((<=) ".le_s")
    ((>) ".gt_s")
    ((>=) ".ge_s")
    ((=) ".eq")
    ((!=) ".ne")
    (else (error "Unknown binop name " x))))

(define (form? expr)
  (and (list? expr) (not (null? expr))))

(define (check-same-type t1 t2)
  (if (not (eq? t1 t2))
      (error "Not same type: " t1 t2)))

(define (check-form x)
  (if (not (form? x))
      (error "Not a nonempty list: " x)))

(define (check-head x k)
  (if (not (eq? (car x) k))
      (error "Expected keyword: " k " but saw " (car x))))

(define (check-list l n)
  (if (or (not (list? l)) (not (= (length l) n)))
      (error "List of wrong length: " l)))
