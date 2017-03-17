; Key schedule computation
;
; November 22, 1996
;
; Load LISTS.SCH, BASIS.SCH, then this file.

; Given an input key (64 bits), compute the key schedule (16 by 48 bits).
; To compute key schedule lookup tables, use (iota1 64) as key.

(define (compute-key-schedule)
  (do-compute-key-schedule (iota1 64)))

(define (do-compute-key-schedule key)

  (define (pc-1 bits-64)
    (let ((v (list->vector bits-64)))
      (map (lambda (pos)
	     (vector-ref v (- pos 1)))
	   pc-1-m)))

  (define (pc-2 bits-56)
    (let ((v (list->vector bits-56)))
      (map (lambda (pos)
	     (vector-ref v (- pos 1)))
	   pc-2-m)))

  (define key-schedule-shifts
    '#(0 1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1))

  (define (shift c i)
    (rotate-list-left c (vector-ref key-schedule-shifts i)))

  (let* ((tmp (split-list (pc-1 key) 28))
	 (c   (car tmp))
	 (d   (cdr tmp)))
    (do ((i 1 (+ i 1))
	 (l '()))
	((> i 16) (reverse l))
      (set! c (shift c i))
      (set! d (shift d i))
      (set! l (cons (pc-2 (append c d)) l)))))

; eof
