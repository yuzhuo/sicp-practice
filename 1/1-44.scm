#lang scheme

(define (square x) (* x x))

(define (repeated f n)
  (define (r i res)
    (if (> i n)
        res
        (r (+ i 1) (f res))))
  
  (lambda (m) (r 1 m)))

(define dx 0.000001)

(define (smooth f)
  (define (average v1 v2 v3)
    (/ (+ v1 v2 v3) 3))

  (lambda (x) (average
               (f x)
               (f (- x dx))
               (f (+ x dx)))))

(define (smooth-n-times f n)
  (let ((n-times-smooth (repeated smooth n)))
  (n-times-smooth f)))

((smooth square) 5)
((smooth-n-times square 5) 5)
