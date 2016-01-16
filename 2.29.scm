#lang planet neil/sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch x)
  (car x))
(define (right-branch x)
  (cdr x))
(define (branch-lenght x)
  (car x))
(define (branch-structure x)
  (cdr x))

(define (total-weight x)
  (define (t x s)
    (define lbs (branch-structure (left-branch x)))
    (define rbs (branch-structure (right-branch x)))
    (+
     (if
      (number? lbs)
      (+ s lbs)
      (t lbs s))
     (if
      (number? rbs)
      (+ s rbs)
      (t rbs s))))
  (t x 0))

