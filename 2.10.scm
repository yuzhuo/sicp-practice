#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (display-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]"))

(define (sub-interval a b)
  (make-interval
   (- (lower-bound a) (lower-bound b))
   (- (upper-bound a) (upper-bound b))))

(define a (make-interval 0 1))
(define b (make-interval 1 2))

(display-interval (sub-interval b a))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< 0 (* (upper-bound y) (lower-bound y)))
      (mul-interval x
                    (make-interval
                     (/ (lower-bound y) 1)
                     (/ (upper-bound y) 1)))
      (error "Division error!(interval spans 0)")))

(newline)
(div-interval a b)