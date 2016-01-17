#lang planet neil/sicp

(define (f x)
  (lambda (y) (cons x y)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (f (car s)) rest)))))

(subsets (list 1 2 3))