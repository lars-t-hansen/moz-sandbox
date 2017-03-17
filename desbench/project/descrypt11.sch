; NOT FINISHED
;
; Incorrect assumptions here... we _can_ use 6-bit-indexed tables and only
; need to do magic for the first and last entries (very cheap).
;
; To remove the E function, we assume that the key is given and compute 
; S-box tables that are indexed by 4-bit (!) numbers from the input A.  These
; four-bit numbers are the middle four bit of the six-bit numbers that result
; from running all six-bit inputs to the s-boxes baackwards through the xor 
; with a particular key bit selection, and discarding those six-bit outputs 
; that do not have double leading or trailing bits (01 or 10).
;
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
;  - E function removed by assuming a fixed key.
;
; Lars Thomas Hansen.
; December 10, 1996
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

(define (compute-basic-s-boxes s-boxes)

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

(define (compute-s-boxes s-boxes key-bits key-schedule)
  (let ((s-boxes (compute-basic-s-boxes s-boxes))
	(keys    (compute-keys (list->vector key-bits)
			       (list->vector key-schedule)))
	(s       (make-vector 16 #f)))
    ;; Initialize s with vectors of s-boxes
    (do ((i 0 (+ i 1)))
	((= i 16))
      (vector-set! s i (make-vector 8 (make-vector 64 0))))
    ;; Specialize the S-boxes for each round.
    (do ((i 0 (+ i 1)))
	((= i 16) s)
      ;; For each round, 8 groups of 6 bits
      (do ((j 0 (+ j 1))
	   (n 0 (+ n 6)))
	  ((= j 8))
	;; For each group, all bit pattern combinations
	(do ((k 0 (+ k 1)))
	    ((= k 63))
	  (let ((b (list->vector (bits k 6)))
		(g (subvector (vector-ref keys i) n (+ n 6))))
	    (let ((v (xor-vec b g)))
	      (vector-set! (vector-ref (vector-ref s i) j)
			   (unbits-vec v)
			   (vector-ref (vector-ref s-boxes j) k)))))))))

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
  (let ((ip-m         (compute-perm (list->vector (zero-based ip-m))))
	(ip-inverse-m (compute-perm (list->vector (zero-based ip-inverse-m)))))

    (lambda (plaintext-bits key-bits key-schedule)
      (let ((s-boxes (compute-s-boxes s-boxes key-bits key-schedule)))
	(vector->list 
	 (des-process-v (list->vector plaintext-bits)
			s-boxes
			ip-m
			ip-inverse-m
			))))))

(define (make-mask n b)
  (list->vector (bits n b)))

(define mask15 (make-mask 15 32))

(define (des-process-v text s-boxes ip-m ip-inverse-m)

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

  ; this is now rather more complicated. FIXME

  (define (six-bit-number b boffset)
    (unbits-vec (and-vec (shr-vec b (- 32 boffset 4)) mask15)))

  ; FIXME

  (define (s-box-process b boffset j s-box r)
    (let ((s-box (vector-ref (vector-ref s-boxes round) j) res))
      (cond ((= j 0) ...)
	    ((= j 7) ...)
	    (else
	     (or-vec! r (vector-ref s-box (six-bit-number b boffset)))))))

  (define (f a round)
    (let ((res (make-bitvector 32 0)))
      (do ((i 0 (+ i 4))
	   (j 0 (+ j 1)))
	  ((= j 8) res)
	(s-box-process a i j res))))

  (define (rounds-loop round l r)
    (print "l=~a r=~a~%" (pr (unbits-vec l)) (pr (unbits-vec r)))
    (if (< round 16)
	(rounds-loop (+ round 1) r (xor-vec l (f r round)))
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
