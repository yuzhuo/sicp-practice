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

(define (nn x) (lambda (i) (if (= i 1) x (- (* x x)))))

(define (tan-cf x)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (- (* 2 i) 1))
  
  (cont-frac1 d (nn x) 100))

(tan-cf (/ 3.1415926 4))