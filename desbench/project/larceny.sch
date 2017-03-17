; Larceny

(define format
  (let ((format format))
    (lambda (fmt . args)
      (call-with-output-string
       (lambda (p)
	 (apply format p fmt args))))))

(define (print fmt . args)
  (display (apply format fmt args)))

(define (random-integer-in-interval low high)
  (+ low (random (+ 1 (- high low)))))
