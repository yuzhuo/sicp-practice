#lang planet neil/sicp

(define (fringe x)
  (define (f-recr x)
    (cond ((null? x) nil)
          ((not (pair? x)) (list x))
          (else (append (f-recr (car x)) (f-recr (cdr x))))))
  (f-recr x))

(define x '((1 2) (3 4)))
(fringe x)
(fringe (list 3))
