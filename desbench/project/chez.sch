; Chez Scheme

(define (print fmt . args)
  (display (apply format fmt args)))

(define (random-integer-in-interval low high)
  (+ low (random (+ 1 (- high low)))))

(print-vector-length #f)
