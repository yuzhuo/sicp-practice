#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (+ x 1)) (list 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(length (list (list 1 2) (list 3 4) 5))
