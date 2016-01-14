#lang scheme

(define dx 0.000001)
(define (deriv g x) (/ (- (g (+ x dx)) (g x)) dx))

(define tolerance 0.00001)
(define (fixed-point f f-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (define next (f guess))
    
    (if (close-enough? guess next)
        next
        (try next)))
  
  (try f-guess))

(define (newton-transform g) (lambda (x) (- x (/ (g x) (deriv g x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

(newton-method (cubic 1 1 1) 1.0)
