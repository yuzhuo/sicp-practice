#lang scheme

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define a (lambda (x) (display "*")))
(define b "whatever")

((one a) b)
(newline)
((two a) b)

(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(newline)
(((+ one two) a) b)