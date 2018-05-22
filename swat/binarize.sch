;;; -*- fill-column: 80; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public License,
;;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;;; obtain one at <http://mozilla.org/MPL/2.0/>.

;; This runs with Larceny in -r5 mode:
;;
;;   larceny -r5 -- input-file output-file

(define (binarize input-filename output-filename)
  (let ((in  (open-input-file input-filename))
	(out (open-binary-output-file output-filename)))
    (let loop1 ((c (read-char in)))
      (cond ((eof-object? c)
	     (close-input-port in)
	     (close-output-port out))
	    ((char-whitespace? c)
	     (loop1 (read-char in)))
	    ((char-numeric? c)
	     (let loop2 ((cs (list c)) (c (read-char in)))
	       (if (and (char? c) (char-numeric? c))
		   (loop2 (cons c cs) (read-char in))
		   (let ((n (string->number (list->string (reverse cs)))))
		     (if (<= 0 n 255)
			 (write-char (integer->char n) out)
			 (error "Value out of range: " n))
		     (loop1 c)))))
	    (else
	     (error "Garbage character in file: " c))))))

(define (main args)
  (with-exception-handler
   (lambda (x)
     (display "Error\n")
     (display (error-object-message x))
     (newline)
     (exit 1))
   (lambda ()
     (if (not (= (length args) 2))
	 (error "Usage: binarize input-file output-file"))
     (binarize (car args) (cadr args)))))

(main (vector->list (command-line-arguments)))
