#lang planet neil/sicp

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
           (count-leaves (cdr x))))))

(define (reverse x)
  (define (r-iter x l)
    (if (null? x)
        l
        (r-iter (cdr x) (cons (car x) l))))

  (r-iter x nil))

(define (deep-reverse x)
  (define (d-iter x l)
    (cond ((null? x) l)
          ((not (pair? x)) (cons x l))
          ((pair? x) (d-iter (cdr x) (d-iter (car x) l)))))
  (d-iter x nil))

(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10))
(define x (list (list 1 2) (list 3 (list 4 5)) (list (list 6 7) 8)))
(deep-reverse x)