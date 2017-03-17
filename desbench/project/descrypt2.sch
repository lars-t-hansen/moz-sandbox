; DES encryption and decryption -- using vectors, but freely allocating.
;
; Lars Thomas Hansen.
; November 22, 1996
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
		      (list->vector key-bits)
		      (list->vector key-schedule)
		      s-boxes
		      ip-m
		      ip-inverse-m
		      e-m
		      p-m)))))

(define (des-process-v text key keysched s-boxes ip-m ip-inverse-m e-m p-m)

  (define (compute-key i)
    (permute-vec (vector-ref keysched i) key))

  (define (expand a)
    (permute-vec e-m a))

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
    (let ((b   (xor-vec (expand a) j))
	  (res (make-vector 32 0)))
      (do ((i 0 (+ i 6))
	   (k 0 (+ k 4))
	   (j 0 (+ j 1)))
	  ((= i 48)
	   (permute-vec p-m res))
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
