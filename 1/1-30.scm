#lang scheme

(define (n x) (+ x 1))
(define (self x) x)

(define (sum1 b e f n)
  (define (sum-iter b s)
    (if (> b e) s (sum-iter (n b) (+ s (f b)))))
  
  (sum-iter b 0))

(define (sum2 b e f n)
  (if (> b e) 0 (+ (f b) (sum2 (n b) e f n))))