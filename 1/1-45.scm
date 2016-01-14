#lang scheme

(define (repeated f n)
  (define (rep i res)
    (if (> i n)
        res
        (rep (+ 1 i) (f res))))
  
  (lambda (x) (rep 1 x)))

(define (fixed-point f f-g)
  (define tolerance 0.00000001)
  (define (fp guess)
    (define next (f guess))
    
    (if (< (abs (- guess next)) tolerance)
        next
        (fp next)))
  
  (fp f-g))

(define (f x) (lambda (y) (/ x y)))

(define (average-damp g)
  (lambda (x) (/ (+ (g x) x) 2)))

(define (lg n)
  (define n/2 (/ n 2))
  
  (cond ((> n/2 1) (+ 1 (lg n/2)))
        ((< n/2 1) 0)
        (else 1)))

(define (exp base n)
  (define (e i s)
    (if (> i n)
        s
        (e (+ 1 i) (* base s))))
  
  (e 1 1))

(define (damped-nth-root x n)
  (define (f a) (lambda (y) (/ a (exp y (- n 1)))))
  
  (fixed-point (repeated (average-damp (f x)) (lg n)) 1.0))


(damped-nth-root 27 3)
(damped-nth-root 81 4)
(damped-nth-root 100 2)
