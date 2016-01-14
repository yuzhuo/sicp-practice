#lang scheme

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car x)
  (define (iter a c)
    (if (= 0 (remainder a 2))
        (iter (/ a 2) (+ c 1))
        c))
  (iter x 0))

(define (cdr x)
  (if (= 0 (remainder x 3))
      (+ 1 (cdr (/ x 3)))
      0))

(cons 3 6)
(car (cons 3 6))
(cdr (cons 3 6))