#lang scheme

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
