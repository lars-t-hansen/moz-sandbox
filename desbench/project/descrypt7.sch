; DES encryption and decryption 
;  - using vectors, but freely allocating.
;  - S-boxes optimized
;  - the permutation P has been removed by storing output bits of the S-boxes
;    directly in their locations.
;  - the s-box outputs are represented as 32-bit vectors with the bits in the
;    right position of the output of P.
;  - implements the expansion function more efficiently.
;  - precomputes keys for all rounds to enable efficient encryption of
;    many messages.
;  - more use of bit operations, rational use of ditto.
;  - slightly more abstract initial/final permutations.
;
; Lars Thomas Hansen.
; December 6, 1996
;
; Load CHEZ.SCH, then VECTORS.SCH and BASIS.SCH, finally this file.

; Input : 64 bits of plaintext, 64 bits of key (with parity)
; Output: 64 bits of ciphertext

(define des-encrypt
  (let ((key-schedule (map (lambda (l)
			     (list->vector (zero-based l)))
			   key-schedule)))
    (lambda (plaintext-bits key-bits)
      (des-process plaintext-bits key-bits key-schedule))))

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

(define des-process
  (let ((s-boxes      (compute-s-boxes s-boxes))
	(ip-m         (list->vector (zero-based ip-m)))
	(ip-inverse-m (list->vector (zero-based ip-inverse-m))))

    (lambda (plaintext-bits key-bits key-schedule)
      (let ((keys (compute-keys (list->vector key-bits)
				 (list->vector key-schedule))))
	(vector->list 
	 (des-process-v (list->vector plaintext-bits)
			keys
			s-boxes
			ip-m
			ip-inverse-m
			))))))

(define mask0 (list->vector (bits #x7C0000000000 48)))
(define mask1 (list->vector (bits #x03F000000000 48)))
(define mask2 (list->vector (bits #x000FC0000000 48)))
(define mask3 (list->vector (bits #x00003F000000 48)))
(define mask4 (list->vector (bits #x000000FC0000 48)))
(define mask5 (list->vector (bits #x00000003F000 48)))
(define mask6 (list->vector (bits #x000000000FC0 48)))
(define mask7 (list->vector (bits #x00000000003E 48)))

(define mask31 (list->vector (bits #x3F 48)))

(define (des-process-v text keys s-boxes ip-m ip-inverse-m)

  (define (ip-permute v)
    (permute-vec ip-m text))

  (define (ip-unpermute v)
    (permute-vec ip-inverse-m v))

  (define (expand a)
    (let ((a (adjust-right a 48)))
      (or-vec (shl-vec a 47)
	      (and-vec (shl-vec a (- 48 33)) mask0)
	      (and-vec (shl-vec a (- 42 29)) mask1)
	      (and-vec (shl-vec a (- 36 25)) mask2)
	      (and-vec (shl-vec a (- 30 21)) mask3)
	      (and-vec (shl-vec a (- 24 17)) mask4)
	      (and-vec (shl-vec a (- 18 13)) mask5)
	      (and-vec (shl-vec a (- 12 9))  mask6)
	      (and-vec (shl-vec a (- 6 5))   mask7)
	      (shr-vec a 31))))

  (define (six-bit-number b boffset)
    (unbits-vec (and-vec (shr-vec b (- 48 boffset 6)) mask31)))

  (define (s-box-process b boffset s-box r)
    (xor-vec! r (vector-ref s-box (six-bit-number b boffset))))

  (define (f a round)
    (let ((b   (xor-vec (expand a) (vector-ref keys round)))
	  (res (make-bitvector 32 0)))
      (do ((i 0 (+ i 6))
	   (j 0 (+ j 1)))
	  ((= j 8) res)
	(s-box-process b i (vector-ref s-boxes j) res))))

  (define (rounds l0r0)
    (let ((l0r0 (split-vec l0r0 32)))
      (do ((i 0 (+ i 1))
	   (l (car l0r0) r)
	   (r (cdr l0r0) (xor-vec l (f r i))))
	  ((= i 16)
	   (append-vec r l)))))

  (ip-unpermute (rounds (ip-permute text))))


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
