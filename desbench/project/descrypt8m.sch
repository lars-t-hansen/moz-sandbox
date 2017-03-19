; Meta 8
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
;  - initial/final permutations open-coded as shifts and masks.
;
; Lars Thomas Hansen.
; December 7, 1996
;
; Load CHEZ.SCH or LARCENY.SCH, then VECTORS.SCH and BASIS.SCH, then META.SCH,
; finally this file.

; Input : 64 bits of plaintext, 64 bits of key (with parity)
; Output: 64 bits of ciphertext

(define (m:test-encryption)
  (delete-file "descrypt8m.c")
  (with-output-to-file "descrypt8m.c"
    (lambda ()
      (m:des-encrypt (bits (cadr canonical-test-case) 64)))))

(define m:des-encrypt
  (let ((key-schedule (map (lambda (l)
			     (list->vector (zero-based l)))
			   key-schedule)))
    (lambda (key-bits)
      (m:init)
      (m:des-process "des_encrypt" key-bits key-schedule)
      (m:exit))))

; Input : 64 bits of ciphertext, 64 bits of key (with parity)
; Output: 64 bits of plaintext

(define m:des-decrypt
  (let ((key-schedule (map (lambda (l)
			     (list->vector (zero-based l)))
			   (reverse key-schedule))))
    (lambda (key-bits)
      (m:init)
      (m:des-process "des_decrypt" key-bits key-schedule)
      (m:exit))))

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

(define (m:compute-s-boxes s-boxes)
  (let ((v (make-vector 8))
	(s-boxes (compute-s-boxes s-boxes)))
    (do ((i 0 (+ i 1)))
	((= i 8) v)
      (let ((n (m:spclname (string->symbol
			    (string-append "s_box_" (number->string i))))))
	(vector-set! v i n)
	(emit "static const WORD ~a[64]={" n)
	(do ((j 0 (+ j 1)))
	    ((= j 64) (emit "};"))
	  (emit "~a," (vector-ref (vector-ref s-boxes i) j)))))))

(define (compute-keys key keysched)
  (let ((v (make-vector 16)))
    (do ((i 0 (+ i 1)))
	((= i 16) v)
      (vector-set! v i (permute-vec (vector-ref keysched i) key)))))

(define (m:compute-keys key keysched)
  (let ((v    (make-vector 16))
	(keys (compute-keys key keysched)))
    (do ((i 0 (+ i 1)))
	((= i 16) v)
      (let ((name (m:spclname
		   (string->symbol
		    (string-append
		     "key_"
		     (number->string i))))))
	(vector-set! v i name)
	(emit "static const WORD ~a = ~a;" name (vector-ref keys i))))))

(define (m:des-process procedure-name key-bits key-schedule)
  (if longlong
      (emit "typedef unsigned long long WORD;")
      (emit "typedef unsigned long WORD;"))
  (let ((s-boxes      (m:compute-s-boxes s-boxes))
	(ip-m         (list->vector (zero-based ip-m)))
	(ip-inverse-m (list->vector (zero-based ip-inverse-m))))
    (let ((keys (m:compute-keys (list->vector key-bits)
				(list->vector key-schedule)))
	  (param (m:spclname 'text)))
      (emit-head "WORD ~a( WORD ~a ) {" procedure-name param)
      (let ((n (m:des-process-v param
				keys
				s-boxes
				ip-m
				ip-inverse-m
				)))
	(emit "return ~a;" n)
	(emit "}")))))

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

(define mask63 (make-mask #x3F 48))

(define (m:des-process-v text keys s-boxes ip-m ip-inverse-m)

  (define (permute-vec permutation s)
    (let ((v (m:make-bitvector 64 0)))
      (do ((i 63 (- i 1)))
	  ((< i 0) v)
	(let* ((b (vector-ref permutation i))
	       (t (if (> b i)
		      (m:shr-vec s (- b i))
		      (m:shl-vec s (- i b)))))
	  (m:or-vec! v (m:and-vec t (mask-2^i i)))))))

  (define (ip-permute v)
    (permute-vec ip-m v))

  (define (ip-unpermute v)
    (permute-vec ip-inverse-m v))

  (define (expand a)
    (let ((a (m:adjust-right a 48)))
      (let* ((v1 (m:shl-vec a 47))
	     (v2 (m:and-vec (m:shl-vec a (- 48 33)) mask0))
	     (v3 (m:and-vec (m:shl-vec a (- 42 29)) mask1))
	     (v4 (m:and-vec (m:shl-vec a (- 36 25)) mask2))
	     (v5 (m:and-vec (m:shl-vec a (- 30 21)) mask3))
	     (v6 (m:and-vec (m:shl-vec a (- 24 17)) mask4))
	     (v7 (m:and-vec (m:shl-vec a (- 18 13)) mask5))
	     (v8 (m:and-vec (m:shl-vec a (- 12 9))  mask6))
	     (v9 (m:and-vec (m:shl-vec a (- 6 5))   mask7))
	     (va (m:shr-vec a 31)))
	(m:or-vec v1 v2 v3 v4 v5 v6 v7 v8 v9 va))))

  (define (six-bit-number b boffset)
    (m:unbits-vec (m:and-vec (m:shr-vec b (- 48 boffset 6)) mask63)))

  (define (s-box-process b boffset s-box r)
    (m:xor-vec! r (m:vector-ref s-box (six-bit-number b boffset))))

  (define (f a round)
    (let ((b   (m:xor-vec (expand a) (vector-ref keys round)))
	  (res (m:make-bitvector 32 0)))
      (do ((i 0 (+ i 6))
	   (j 0 (+ j 1)))
	  ((= j 8) res)
	(s-box-process b i (vector-ref s-boxes j) res))))

  (define (rounds-loop i l r)
    (if (< i 16)
	(rounds-loop (+ i 1) r (m:xor-vec l (f r i)))
	(let* ((va (m:adjust-right r 64))
	       (vb (m:adjust-right l 64)))
	  (m:or-vec (m:shl-vec va 32) vb))))

  (define (rounds l0r0)
    (if (not omit-encryption)
	(let* ((va (m:shr-vec l0r0 32))
	       (vb (m:trunc-vec l0r0 32)))
	  (rounds-loop 0 (m:trunc-vec va 32) vb))
	l0r0))

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
