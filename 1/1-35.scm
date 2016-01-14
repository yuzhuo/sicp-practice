#lang scheme

(define (search f a b)
  (define mid (/ (+ a b) 2))
  
  (if (< (abs (f mid)) 0.0000000001)
      mid
      (cond ((positive? (f mid)) (search f a mid))
            ((negative? (f mid)) (search f mid b))
            (else mid))))



(define (half-interval f a b)
  (cond ((and (negative? (f a)) (positive? (f b))) (search f a b))
        ((and (positive? (f a)) (negative? (f b))) (search f b a))
        (else (error "Values are not opposite signs:" a b))))

(define (fix-point f x)
  (define tolerance 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess) 
    (if (close-enough? guess (f guess))
        guess
        (try (f guess))))
  
  (try x))

(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(half-interval (lambda (x) (- (+ 1 (/ 1 x)) x))
               -1
               4.0)