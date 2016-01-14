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

(cont-frac1 (lambda (i) 1.0)
            (lambda (i) 1.0)
            11)

(cont-frac2 (lambda (i) 1.0)
            (lambda (i) 1.0)
            11)