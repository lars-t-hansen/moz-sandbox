; DES encryption and decryption 
;  - using vectors, but freely allocating.
;  - S-boxes optimized
;  - the permutation P has been removed by storing output bits of the S-boxes
;    directly in their locations.
;
; Lars Thomas Hansen.
; November 27, 1996
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
; 6 bits output from the xor in f().

(define (compute-s-boxes s-boxes)
  (list->vector (map recompute-s-box s-boxes)))

(define (recompute-s-box s-box)
  (let ((box (make-vector 64 0))
	(tmp (list->vector
	      (map (lambda (line)
		     (list->vector 
		      (map (lambda (n)
			     (list->vector (bits n 4)))
			   line)))
		   s-box))))
    (do ((i 0 (+ i 1)))
	((= i 64) box)
      (let ((row (+ (* 2 (quotient i 32)) (remainder i 2)))
	    (col (remainder (quotient i 2) 16)))
	(vector-set! box i
		     (vector-ref (vector-ref tmp row) col))))))

; Computation itself!

(define des-process
  ; Precompute s-boxes and permutations:
  ; - all permutations are 0-offset.
  ; - all S-boxes contain bitvectors of length 4.
  ; - all S-boxes are length-64 vectors indexed by the full 6 bits
  ; - everything's a vector (no lists anywhere).
  ; - the p-inverse-m is the destination vector for bits out of s-boxes.

  (let* ((s-boxes      (compute-s-boxes s-boxes))
	 (ip-m         (list->vector (zero-based ip-m)))
	 (ip-inverse-m (list->vector (zero-based ip-inverse-m)))
	 (e-m          (list->vector (zero-based e-m)))
	 (p-m          (list->vector (zero-based p-m)))
	 (p-inverse-m  (let ((v (make-vector 32 0)))
			 (do ((i 0 (+ i 1)))
			     ((= i 32) v)
			   (vector-set! v (vector-ref p-m i) i)))))

    (lambda (plaintext-bits key-bits key-schedule)
      (vector->list 
       (des-process-v (list->vector plaintext-bits)
		      (list->vector key-bits)
		      (list->vector key-schedule)
		      s-boxes
		      ip-m
		      ip-inverse-m
		      e-m
		      p-inverse-m
		      )))))

(define (des-process-v text key keysched s-boxes ip-m ip-inverse-m e-m
		       p-inverse-m)

  (define (compute-key i)
    (permute-vec (vector-ref keysched i) key))

  (define (expand a)
    (permute-vec e-m a))

  (define (s-box-process b boffset s-box r roffset)
    (let ((n (+ (* 32 (vector-ref b boffset))
		(* 16 (vector-ref b (+ boffset 1)))
		(* 8 (vector-ref b (+ boffset 2)))
		(* 4 (vector-ref b (+ boffset 3)))
		(* 2 (vector-ref b (+ boffset 4)))
		(vector-ref b (+ boffset 5)))))
      (let ((bits (vector-ref s-box n)))
	; One can get rid of this loop by pre-shifting the s-box output
	; to the right positions!  It then becomes a 32-bit xor.
	(do ((i 0 (+ i 1)))
	    ((= i 4))
	  (vector-set! r
		       (vector-ref p-inverse-m (+ i roffset))
		       (vector-ref bits i))))))

  (define (f a j)
    (let ((b   (xor-vec (expand a) j))
	  (res (make-vector 32 0)))
      (do ((i 0 (+ i 6))
	   (k 0 (+ k 4))
	   (j 0 (+ j 1)))
	  ((= i 48) res)
	(s-box-process b i (vector-ref s-boxes j) res k))))

  (let ((l0r0 (split-vec (permute-vec ip-m text) 32)))
    (do ((i 0 (+ i 1))
	 (l (car l0r0) r)
	 (r (cdr l0r0) (xor-vec l (f r (compute-key i)))))
	((= i 16)
	 (permute-vec ip-inverse-m (append-vec r l))))))  ; [sic!]

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
