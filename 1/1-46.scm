#lang scheme

(define (iterative-improve g? improve)
  (lambda (x f-g)
    (let try ((g f-g))
      (define n (improve g))
      (if (g? g n)
          n
          (try n)))))


(define (sqrt x f-g)
  ((iterative-improve (lambda (c n) (< (abs (- c n)) 0.00001))
                      (lambda (g) (/ (+ (/ x g) g) 2))) x f-g))

(sqrt 2 1.0)


(define (fixed-point f f-g)
  ((iterative-improve (lambda (c n) (< (abs (- c n)) 0.00001))
                      (lambda (g) (f g))) f f-g))

(fixed-point (lambda (y) (* (/ 1 2) (+ y (/ 10 y))))
           1.0)