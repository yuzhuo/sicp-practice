#lang scheme

(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate1 combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (accumulate-iter a accum)
    (if (> a b) accum (accumulate-iter (next a) (combiner accum a))))
  
  (accumulate-iter a null-value))

(define (filtered-accumulate1 combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner
       (if (filter (term a)) (term a) null-value)
       (filtered-accumulate1 combiner null-value term (next a) next b filter))))

(define (filtered-accumulate2 combiner null-value term a next b filter)
  (define (filtered-accumulate-iter a accum)
    (if (> a b)
        accum
        (combiner
         (if (filter (term a)) (term a) null-value)
         (filtered-accumulate-iter (next a) accum))))
  
  (filtered-accumulate-iter a null-value))

(define (filtered-sum1 term a next b filter)
  (filtered-accumulate1 + 0 term a next b filter))

(define (filtered-product1 term a next b filter)
  (filtered-accumulate1 * 1 term a next b filter)) 

(define (filtered-sum2 term a next b filter)
  (filtered-accumulate2 + 0 term a next b filter))

(define (filtered-product2 term a next b filter)
  (filtered-accumulate2 * 1 term a next b filter)) 

(define (filter x) (<= x 49))
(define (add2 x) (+ x 2))
(define (square x) (* x x))

(filtered-sum1 square 1 add2 100 filter)
(filtered-product1 square 1 add2 100 filter)

(filtered-sum2 square 1 add2 100 filter)
(filtered-product2 square 1 add2 100 filter)