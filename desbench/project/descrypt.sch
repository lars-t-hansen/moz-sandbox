; DES encryption and decryption.
;
; Lars Thomas Hansen.
; November 22, 1996
;
; Load CHEZ.SCH, then LISTS.SCH and BASIS.SCH, finally this file.

; Input : 64 bits of plaintext, 64 bits of key (with parity)
; Output: 64 bits of ciphertext

(define (des-encrypt plaintext-bits key-bits)
  (des-process plaintext-bits key-bits key-schedule))

; Input : 64 bits of ciphertext, 64 bits of key (with parity)
; Output: 64 bits of plaintext

(define (des-decrypt ciphertext-bits key-bits)
  (des-process ciphertext-bits key-bits (reverse key-schedule)))

(define (des-process text key keysched)

  (define (compute-key i)
    (permute (list-ref keysched (- i 1)) key))

  (define (expand a)
    (permute e-m a))

  (define (s-box-process b s-box)
    (let ((ri  (+ (* 2 (car b)) (car (last-pair b))))
	  (ci  (unbits (but-last (cdr b)))))
      (bits (list-ref (list-ref s-box ri) ci) 4)))

  (define (f a j)
    (let ((bs (split-list* (xor (expand a) j) 6)))
      (permute p-m
	       (apply append (map s-box-process bs s-boxes)))))

  (let ((l0r0 (split-list (permute ip-m text) 32)))
    (do ((i 1 (+ i 1))
	 (l (car l0r0) r)
	 (r (cdr l0r0) (xor l (f r (compute-key i)))))
	((= i 17)
	 (permute ip-inverse-m (append r l))))))  ; [sic!]

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
