; DES encryption and decryption 
;  A first attempt at partial evaluation, based on descrypt6.
;
;  - using vectors, but freely allocating.
;  - S-boxes optimized
;  - the permutation P has been removed by storing output bits of the S-boxes
;    directly in their locations.
;  - the s-box outputs are represented as 32-bit vectors with the bits in the
;    right position of the output of P.
;  - implements the expansion function more efficiently.
;  - precomputes keys for all rounds to enable efficient encryption of
;    many messages.
;
; Lars Thomas Hansen.
; November 27, 1996
;
; Load CHEZ.SCH, then VECTORS.SCH and BASIS.SCH, finally this file.

; Input : 64 bits of plaintext, 64 bits of key (with parity)
; Output: 64 bits of ciphertext

(define gen:des-encrypt
  (let ((key-schedule (map (lambda (l)
			     (list->vector (zero-based l)))
			   key-schedule)))
    (lambda (key-bits)
      (gen:des-process key-bits key-schedule))))

; Input : 64 bits of ciphertext, 64 bits of key (with parity)
; Output: 64 bits of plaintext

(define des-decrypt
  (let ((key-schedule (map (lambda (l)
			     (list->vector (zero-based l)))
			   (reverse key-schedule))))
    (lambda (ciphertext-bits key-bits)
      (des-process ciphertext-bits key-bits key-schedule))))

; S-box recomputation: produce a vector of length 64 indexed by the full
; 6 bits output from the xor in f().  Each element of the s-box is a
; bit vector of length 32 with the output bits in their proper places
; for C followed by the permutation P.

(define (compute-s-boxes s-boxes)

  (define p-inverse-m
    (let ((v   (make-vector 32 0))
	  (p-m (list->vector (zero-based p-m))))
      (do ((i 0 (+ i 1)))
	  ((= i 32) v)
	(vector-set! v (vector-ref p-m i) i))))

  (define (make-mask bits round)
    (let ((roffset (* round 4))
	  (v (make-vector 32 0)))
      (do ((i 0 (+ i 1)))
	  ((= i 4) v)
	(vector-set! v 
		     (vector-ref p-inverse-m (+ i roffset))
		     (vector-ref bits i)))))

  (define (recompute-s-box s-box round)
    (let ((box (make-vector 64 0))
	  (tmp (list->vector
		(map (lambda (line)
		       (list->vector 
			(map (lambda (n)
			       (make-mask (list->vector (bits n 4)) round))
			     line)))
		     s-box))))
      (do ((i 0 (+ i 1)))
	  ((= i 64) box)
	(let ((row (+ (* 2 (quotient i 32)) (remainder i 2)))
	      (col (remainder (quotient i 2) 16)))
	  (vector-set! box i
		       (vector-ref (vector-ref tmp row) col))))))

  (list->vector (map recompute-s-box s-boxes (iota 8))))

(define (compute-keys key keysched)
  (let ((v (make-vector 16)))
    (do ((i 0 (+ i 1)))
	((= i 16) v)
      (vector-set! v i (permute-vec (vector-ref keysched i) key)))))


; Computation itself!

(define gen:des-process
  (let ((s-boxes      (compute-s-boxes s-boxes))
	(ip-m         (list->vector (zero-based ip-m)))
	(ip-inverse-m (list->vector (zero-based ip-inverse-m))))

    (lambda (key-bits key-schedule)
      (let ((keys (compute-keys (list->vector key-bits)
				 (list->vector key-schedule))))
	(gen:des-process-v (gen:parameter 'text)
			   (gen:parameter 'keys)
			   (gen:parameter 's-boxes)
			   ip-m
			   ip-inverse-m
			   )))))

(define (gen:parameter v) v)

(define gen:mask0 (gen:mask (list->vector (bits #x7C0000000000 48))))
(define gen:mask1 (gen:mask (list->vector (bits #x03F000000000 48))))
(define gen:mask2 (gen:mask (list->vector (bits #x000FC0000000 48))))
(define gen:mask3 (gen:mask (list->vector (bits #x00003F000000 48))))
(define gen:mask4 (gen:mask (list->vector (bits #x000000FC0000 48))))
(define gen:mask5 (gen:mask (list->vector (bits #x00000003F000 48))))
(define gen:mask6 (gen:mask (list->vector (bits #x000000000FC0 48))))
(define gen:mask7 (gen:mask (list->vector (bits #x00000000003E 48))))

(define (gen:des-process-v text keys s-boxes ip-m ip-inverse-m)

  ; Note the strange semantics of shl and shr on 32-to-48 shifting
  ; as outlined in vectors.sch.

  (define (gen:expand a)
    (gen:or-vec (gen:shl-vec a 47 48)
		(gen:and-vec (gen:shr-vec a 1 48) gen:mask0)
		(gen:and-vec (gen:shr-vec a 3 48) gen:mask1)
		(gen:and-vec (gen:shr-vec a 5 48) gen:mask2)
		(gen:and-vec (gen:shr-vec a 7 48) gen:mask3)
		(gen:and-vec (gen:shr-vec a 9 48) gen:mask4)
		(gen:and-vec (gen:shr-vec a 11 48) gen:mask5)
		(gen:and-vec (gen:shr-vec a 13 48) gen:mask6)
		(gen:and-vec (gen:shl-vec a 1 48) gen:mask7)
		(gen:shr-vec a 47 48)))

  (define (gen:six-bit-number b boffset)
    (let ((n (new-name 'n)))
      (emit "~a = (~a >> ~a) | 0x2F;" n b (- boffset 6))
      n))

  (define (gen:s-box-process b boffset s-box-idx r)
    (gen:xor-vec! r (gen:vector-ref (gen:vector-ref s-boxes s-box-idx)
				    (gen:six-bit-number b boffset))))

  (define (gen:f a round)
    (let ((b (gen:xor-vec (gen:expand a) (gen:vector-ref keys round)))
	  (res (gen:make-bitvector 32 0)))
      (do ((i 0 (+ i 6))
	   (j 0 (+ j 1)))
	  ((= j 8) res)
	(gen:s-box-process b i j res))))

  (define (gen:rounds l0r0)
    (let ((l0 (gen:shr-vec l0r0 32 64))
	  (r0 (gen:and-vec l0r0 (gen:mask
				 (list->vector (bits #xFFFFFFFF 64))))))
      (do ((i 0 (+ i 1))
	   (l l0 r)
	   (r r0 (gen:xor-vec l (gen:f r i))))
	  ((= i 16)
	   (gen:or-vec (gen:adjust-left r 64 32)
		       (gen:adjust-right l 64 32))))))

  (gen:permute-vec ip-inverse-m (gen:rounds (gen:permute-vec ip-m text))))


; Partial evaluation code

(define new-name
  (let ((i 0))
    (lambda (n)
      (set! i (+ i 10))
      (string-append (symbol->string n) "_" (number->string i)))))
  
(define (emit fmt . rest)
  (display (apply format fmt rest))
  (newline))

(define (gen:mask m)
  (string-append "0x" (pr (unbits (vector->list m)))))

(define (gen:xor-vec! a b)
  (emit "~a ^= ~a;" a b)
  a)

(define (gen:xor-vec a b)
  (let ((x (new-name 'x)))
    (emit "~a = ~a ^ ~a;" x a b)
    x))

(define (gen:shr-vec v n f)
  (let ((x (new-name 'x)))
    (emit "~a = ~a >> ~a;" x v n)
    x))

(define (gen:shl-vec v n f)
  (let ((x (new-name 'x)))
    (emit "~a = ~a << ~a;" x v n)
    x))


(define (gen:adjust-left r width source-width)
  (let ((x (new-name 'x)))
    (emit "~a = ~a << ~a;" x r (- width source-width))
    x))

(define (gen:adjust-right r width source-width)
  r)

(define (gen:or-vec a . rest)
  (let ((x (new-name 'x)))
    (emit "~a = ~a;" x a)
    (let loop ((rest rest))
      (if (null? rest)
	  x
	  (begin (emit "~a |= ~a;" x (car rest))
		 (loop (cdr rest)))))))

(define (gen:and-vec a b)
  (let ((x (new-name 'x)))
    (emit "~a = ~a & ~a;" x a b)
    x))

(define (gen:vector-ref v n)
  (let ((x (new-name 'x)))
    (emit "~a = ~a[~a];" x v n)
    x))

(define (gen:make-bitvector length value)
  (let ((x (new-name 'x)))
    (emit "~a = ~a;" x value)
    x))

(define (gen:permute-vec permutation input)
  (let ((x (new-name 'perm)))
    x))


;Test code

; From Stinson, p 79-81.
; Plaintext, key, expected ciphertext.

(define canonical-test-case
  '(#x0123456789abcdef #x133457799bbcdff1 #x85e813540f0ab405))

(define (test0)
  (let ((t (des-encrypt (bits (car canonical-test-case) 64)
			(bits (cadr canonical-test-case) 64))))
    (if (not (equal? (unbits t) (caddr canonical-test-case)))
	(print "Failed test0! t=~a~%" (pr (unbits t))))
    #t))

(define (test1)
  (let ((c (des-encrypt (bits (car canonical-test-case) 64)
			(bits (cadr canonical-test-case) 64))))
    (let ((p (des-decrypt c (bits (cadr canonical-test-case) 64))))
      (if (not (= (unbits p) (car canonical-test-case)))
	  (print "Failed test1! c=~a, p=~a~%"
		 (pr (unbits c))
		 (pr (unbits p))))
      #t)))

; eof
