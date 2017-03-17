; -*- scheme -*-
;
; General operations on lists that are not part of Standard Scheme.
; Lars Thomas Hansen
;
; October 14, 1996

; Reduce binary operator with left associativity.

(define (fold-left op initial args)
  (if (null? args)
      initial
      (let loop ((n initial) (args args))
	(if (null? args)
	    n
	    (loop (op n (car args)) (cdr args))))))


; Reduce binary operator with right associativity.

(define (fold-right op initial args)
  (if (null? args)
      initial
      (op (car args) (fold-right op initial (cdr args)))))


; Return a list of integers 0..n-1, for n>0.

(define (iota n)
  (define (loop i)
    (if (= i n)
	'()
	(cons i (loop (+ i 1)))))
  (loop 0))


(define (iota1 n)
  (define (loop i)
    (if (= i n)
	'()
	(cons (+ i 1) (loop (+ i 1)))))
  (loop 0))


; Given a list of length at least k, return a pair consisting of two
; lists: elements 0..k-1, and elements k..n.

(define (split-list l k)

  (define (loop l k r)
    (if (zero? k)
	(cons (reverse r) l)
	(loop (cdr l) (- k 1) (cons (car l) r))))

  (loop l k '()))


; Given a list that has a length which is a multiple of n, split the
; list into sublists of length n.

(define (split-list* l n)
  (if (null? l)
      '()
      (let ((s (split-list l n)))
	(cons (car s) (split-list* (cdr s) n)))))

; Return a list of length n containing all a's.

(define (list-of-n a n)
  (map (lambda (i) a) (iota n)))


; Return the first n elements of the list l.

(define (list-head l n)
    (if (zero? n)
	'()
	(cons (car l) (list-head (cdr l) (- n 1)))))

(define (but-last l)
  (reverse! (cdr (reverse l))))

; Given a sorted list, return a new list where all elements are 
; unique according to the passed predicate.

(define (remove-duplicates l same?)
  (cond ((null? l) l)
	((null? (cdr l)) l)
	((same? (car l) (cadr l)) (remove-duplicates (cdr l) same?))
	(else (cons (car l) (remove-duplicates (cdr l) same?)))))

; Given a list, return a new list consisting of those elements for which the
; predicate returns #t.

(define (select l select?)
  (cond ((null? l) l)
	((select? (car l)) (cons (car l) (select (cdr l) select?)))
	(else (select (cdr l) select?))))

; Given a list of numbers, return its average.

(define (average l)
  (/ (apply + l) (length l)))


(define (every? p . lists)
  (define (loop lists)
    (or (null? (car lists))
	(and (apply p (map car lists))
	     (loop (map cdr lists)))))
  (loop lists))


(define (rotate-list-left l bits)
  (let* ((tmp (split-list l bits))
	 (lhs (car tmp))
	 (rhs (cdr tmp)))
    (append rhs lhs)))


; eof
