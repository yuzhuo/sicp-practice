#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x cs)
  (accumulate (lambda (a b) (+ a (* b x)))
              0
              cs))

(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 2 (list 2 3 0 5 0 1))
(horner-eval 2 (list 2 3 0 5 0 2)) 