#lang planet neil/sicp

(define (tree-map m t)
  (cond ((null? t) nil)
        ((not (pair? t)) (m t))
        (else (cons (tree-map m (car t)) (tree-map m (cdr t))))))

(define (square x)
  (* x x))

(define (square-tree t) (tree-map square t))

(square-tree (list 1 2 (list 3 4)))