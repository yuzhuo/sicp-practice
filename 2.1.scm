#lang scheme

(define (make-rat n d)
  (if (positive? d)
      (cons n d)
      (cons (- n) (- d))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))

(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))