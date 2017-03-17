; DES encryption and decryption -- using vectors, but freely allocating.
;
; Lars Thomas Hansen.
; December 9, 1996.
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

(define (compute-keys key keysched)
  (let ((v (make-vector 16)))
    (do ((i 0 (+ i 1)))
	((= i 16) v)
      (vector-set! v i (permute-vec (vector-ref keysched i) key)))))


(define des-process
  ; Precompute s-boxes and permutations:
  ; - all permutations are 0-offset.
  ; - all S-boxes contain bitvectors of length 4.
  ; - everything's a vector (no lists anywhere).
  (let ((s-boxes      (list->vector
		       (map (lambda (s-box)
			      (list->vector
			       (map (lambda (line)
				      (list->vector 
				       (map (lambda (n)
					      (list->vector (bits n 4)))
					    line)))
				    s-box)))
			    s-boxes)))
	(ip-m         (list->vector (zero-based ip-m)))
	(ip-inverse-m (list->vector (zero-based ip-inverse-m)))
	(e-m          (list->vector (zero-based e-m)))
	(p-m          (list->vector (zero-based p-m))))

    (lambda (plaintext-bits key-bits key-schedule)
      (vector->list 
       (des-process-v (list->vector plaintext-bits)
		      (compute-keys (list->vector key-bits)
				    (list->vector key-schedule))
		      s-boxes
		      ip-m
		      ip-inverse-m
		      e-m
		      p-m)))))

(define (make-mask n b)
  (list->vector (bits n b)))

(define (mask-2^i i k)
  (make-mask (expt 2 i) k))


(define (des-process-v text keys s-boxes ip-m ip-inverse-m e-m p-m)

  (define (permute-vec permutation s)
    (let ((v (make-bitvector 64 0))
	  (s (adjust-right s 64)))
      (do ((i (- (vector-length permutation) 1) (- i 1)))
	  ((< i 0) (trunc-vec v (vector-length permutation)))
	(let* ((b (vector-ref permutation i))
	       (t (if (> b i)
		      (shr-vec s (- b i))
		      (shl-vec s (- i b)))))
	  (or-vec! v (and-vec t (mask-2^i i 64)))))))

  (define (ip-permute v)
    (permute-vec ip-m v))

  (define (ip-unpermute v)
    (permute-vec ip-inverse-m v))

  (define (compute-key i)
    (vector-ref keys i))

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

  (define (s-box-process b offset s-box r roffset)
    (let ((ri  (+ (* 2 (vector-ref b offset))
		  (vector-ref b (+ offset 5))))
	  (ci  (+ (* 8 (vector-ref b (+ offset 1)))
		  (* 4 (vector-ref b (+ offset 2)))
		  (* 2 (vector-ref b (+ offset 3)))
		  (vector-ref b (+ offset 4)))))
      (let ((bits (vector-ref (vector-ref s-box ri) ci)))
	(do ((i 0 (+ i 1)))
	    ((= i 4))
	  (vector-set! r (+ i roffset) (vector-ref bits i))))))

  (define (f a j)
    (let ((b   (xor-vec (expand a) (compute-key j)))
	  (res (make-vector 32 0)))
      (print "E(a)=~a, K(j)=~a, b=~a~%" 
	     (pr (unbits-vec (expand a)))
	     (pr (unbits-vec (compute-key j)))
	     (pr (unbits-vec b)))
      (do ((i 0 (+ i 6))
	   (k 0 (+ k 4))
	   (j 0 (+ j 1)))
	  ((= i 48)
	   (permute-vec p-m res))
	(s-box-process b i (vector-ref s-boxes j) res k))))

  (define (rounds-loop i l r)
    (print "l=~a, r=~a~%" (pr (unbits-vec l)) (pr (unbits-vec r)))
    (if (< i 16)
	(let ((x (f r i)))
	  (print "x=~a~%" (pr (unbits-vec x)))
	  (rounds-loop (+ i 1) r (xor-vec l (f r i))))
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
