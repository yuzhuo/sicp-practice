#lang scheme

(define (square x) (* x x))
(define (add2 x) (+ x 2))

(define (product1 b e f n)
  (define (product-iter b s)
    (if (> b e) s (product-iter (n b) (* s (f b)))))
  
  (product-iter b 1))

(define (product2 b e f n)
  (if (> b e) 1 (* (f b) (product2 (n b) e f n))))

(* 2.0
   (/ (product1 4 1000 square add2)
      (product1 3 999 square add2)
      1000))

(* 2.0
   (/ (product2 4 1000 square add2)
      (product2 3 999 square add2)
      1000))