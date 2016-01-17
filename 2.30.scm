#lang planet neil/sicp

(define (square x)
  (* x x))

(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else
         (cons (square-tree (car t)) (square-tree (cdr t))))))

(square-tree (list 1 2 (list 3 (list 4 5)) 6))










