; Partial evaluation code

(define longlong #t)
(define omit-encryption #f)

(define m:$head #f)
(define m:$body '())
(define m:$decl '())
(define m:$i 0)

(define (m:init)
  (set! m:$head #f)
  (set! m:$body '())
  (set! m:$decl '())
  (set! m:$i 0)
  #t)

(define (m:exit)
  (display m:$head)
  (newline)
  (let ((i 0))
    (display "WORD ")
    (for-each (lambda (d)
		(display d)
		(display ", ")
		(set! i (remainder (+ i 1) 15))
		(if (zero? i)
		    (newline)))
	      (reverse m:$decl))
    (display "dummy;") (newline))
  (for-each (lambda (x)
	      (display x)
	      (newline))
	    (reverse m:$body))
  #t)

(define (emit-head fmt . rest)
  (set! m:$head (apply m:format fmt rest)))

(define (emit fmt . rest)
  (let ((s (apply m:format fmt rest)))
    (if (not m:$head)
	(begin (display s) (newline))
	(set! m:$body (cons s m:$body)))))

(define (m:format fmt . rest)
  (define suffix (if longlong "ULL" "UL"))
  (apply format fmt
	 (map (lambda (x)
		(cond ((string? x) x)
		      ((number? x) (string-append (number->string x) 
						  suffix))
		      ((name? x) (name.id x))
		      ((vector? x)
		       (string-append "0x" (pr (unbits-vec x)) 
				      suffix))
		      (else ???)))
	      rest)))

; Needs to create declarations.
; Note: name can carry type!

(define (m:name n)
  (set! m:$i (+ m:$i 1))
  (let ((id (string-append (symbol->string n) "_"
			   (number->string m:$i 36))))
    (m:declare id)
    id))

(define (m:spclname n)
  n)

(define (m:declare id)
  (set! m:$decl (cons id m:$decl)))

(define name? symbol?)

(define (name.id x) x)

(define (m:make-bitvector length initial)
  (let ((n (m:name 'x)))
    (emit "~a = ~a;" n (if (zero? initial) "0" "~0"))
    n))

(define (m:shr-vec v n)
  (let ((x (m:name 'x)))
    (emit "~a = ~a >> ~a;" x v n)
    x))

(define (m:shl-vec v n)
  (let ((x (m:name 'x)))
    (emit "~a = ~a << ~a;" x v n)
    x))

(define (m:or-vec! v1 v2)
  (emit "~a |= ~a;" v1 v2)
  v1)

(define (m:and-vec a b)
  (let ((x (m:name 'x)))
    (emit "~a = ~a & ~a;" x a b)
    x))

(define (m:adjust-right a n)
  (let ((x (m:name 'x)))
    (emit "~a = ~a & ~a;" x a (- (expt 2 n) 1))
    x))

(define (m:or-vec a . rest)
  (let ((x (m:name 'x)))
    (emit "~a = ~a;" x a)
    (let loop ((rest rest))
      (if (null? rest)
	  x
	  (begin (emit "~a |= ~a;" x (car rest))
		 (loop (cdr rest)))))))

(define (m:unbits-vec v)
  v)

(define (m:xor-vec! a b)
  (emit "~a ^= ~a;" a b)
  a)

(define (m:vector-ref v n)
  (let ((x (m:name 'x)))
    (emit "~a = ~a[~a];" x v n)
    x))

(define (m:xor-vec a b)
  (let ((x (m:name 'x)))
    (emit "~a = ~a ^ ~a;" x a b)
    x))

; OK for now.

(define (m:trunc-vec v n)
  (m:and-vec v (- (expt 2 n) 1)))

; eof
