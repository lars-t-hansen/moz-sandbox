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
;  - initial/final permutations open-coded as table lookups in large table.
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

(define (compute-perm pvec)
  (let ((p (make-vector 8 #f)))
    (do ((i 0 (+ i 8)))
	((= i 64) p)
      (let ((l '()))
	(do ((k 0 (+ k 1)))
	    ((= k 64))
	  (if (<= i (vector-ref pvec k) (+ i 7))
	      (set! l (cons (cons (vector-ref pvec k) k) l))))
	(let ((t (make-vector 256 #f)))
	  (do ((j 0 (+ j 1)))
	      ((= j 256))
	    (let ((b (bits j 8))
		  (v (make-bitvector 64 0)))
	      (do ((k i (+ k 1))
		   (b b (cdr b)))
		  ((= k (+ i 8)))
		(vector-set! v (cdr (assv k l)) (car b)))
	      (vector-set! t j v)))
	  (vector-set! p (quotient i 8) t))))))

(define des-process
  (let ((s-boxes      (compute-s-boxes s-boxes))
	(ip-m         (compute-perm (list->vector (zero-based ip-m))))
	(ip-inverse-m (compute-perm (list->vector (zero-based ip-inverse-m)))))

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

(define (make-mask n b)
  (list->vector (bits n b)))

(define (mask-2^i i)
  (make-mask (expt 2 i) 64))

(define mask0 (make-mask #x7C0000000000 48))
(define mask1 (make-mask #x03F000000000 48))
(define mask2 (make-mask #x000FC0000000 48))
(define mask3 (make-mask #x00003F000000 48))
(define mask4 (make-mask #x000000FC0000 48))
(define mask5 (make-mask #x00000003F000 48))
(define mask6 (make-mask #x000000000FC0 48))
(define mask7 (make-mask #x00000000003E 48))

(define mask63  (make-mask 63 48))
(define mask255 (make-mask 255 64))

(define (des-process-v text keys s-boxes ip-m ip-inverse-m)

  (define (permute-vec permutation s)
    (let ((v (make-bitvector 64 0)))
      (do ((i 0 (+ i 1))
	   (j 8 (+ j 8)))
	  ((= i 8) v)
	(let ((n (unbits-vec (and-vec (shr-vec s (- 64 j)) mask255))))
	  (or-vec! v (vector-ref (vector-ref permutation i) n))))))

  (define (ip-permute v)
    (permute-vec ip-m v))

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
    (unbits-vec (and-vec (shr-vec b (- 48 boffset 6)) mask63)))

  (define (s-box-process b boffset s-box r)
    (xor-vec! r (vector-ref s-box (six-bit-number b boffset))))

  (define (f a round)
    (let ((b   (xor-vec (expand a) (vector-ref keys round)))
	  (res (make-bitvector 32 0)))
      (do ((i 0 (+ i 6))
	   (j 0 (+ j 1)))
	  ((= j 8) res)
	(s-box-process b i (vector-ref s-boxes j) res))))

  (define (rounds-loop i l r)
    (if (< i 16)
	(rounds-loop (+ i 1) r (xor-vec l (f r i)))
	(or-vec (shl-vec (adjust-right r 64) 32) (adjust-right l 64))))

  (define (rounds l0r0)
    (rounds-loop 0 (trunc-vec (shr-vec l0r0 32) 32) (trunc-vec l0r0 32)))

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
