#lang scheme

(define (sum-next x) (+ 2 x))
(define (sum-term x) (* x x))

(define (product-next x) (+ 2 x))
(define (product-term x) (* x x))

(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate1 combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (accumulate-iter a accum)
    (if (> a b) accum (accumulate-iter (next a) (combiner accum (term a)))))
  
  (accumulate-iter a null-value))

(define (sum1 term a next b)
  (accumulate1 + 0 term a next b))

(define (product1 term a next b)
  (accumulate1 * 1 term a next b))

(begin
  (display "sum-iter: ")
  (newline)
  (sum1 sum-term 1 sum-next 5))
(begin
  (display "product-iter:")
  (newline)
  (product1 product-term 1 product-next 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum2 term a next b)
  (accumulate1 + 0 term a next b))

(define (product2 term a next b)
  (accumulate1 * 1 term a next b))

(begin
  (display "sum-recur: ")
  (newline)
  (sum2 sum-term 1 sum-next 5))
(begin
  (display "product-recur:")
  (newline)
  (product2 product-term 1 product-next 5))