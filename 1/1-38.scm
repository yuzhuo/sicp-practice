#lang scheme

(define (cont-frac1 d n k)
  (define (cf i)
    (if (>= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cf (+ i 1))))))
  
  (cf 1))

(define (cont-frac2 d n k)
  (define (cf-iter i tmp)
    (if (<= i 0)
        tmp
        (cf-iter (- i 1)
                 (/ (n i)
                    (+ (d i) tmp)))))
  
  (cf-iter (- k 1) (/ (n k) (d k))))

(define (n i) 1.0)
(define (d i)
  (if (= 2 (remainder i 3))
      (* 2 (+ 1 (quotient i 3)))
      1))

(+ 2 (cont-frac1 d n 11))