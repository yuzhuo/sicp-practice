#lang scheme

(define (repeated f n)
  (define (r i res)
    (if (> i n)
        res
        (r (+ i 1) (f res))))
  
  (lambda (m) (r 1 m)))

((repeated (lambda (x) (* x x)) 2) 5)