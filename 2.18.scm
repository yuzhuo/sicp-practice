#lang planet neil/sicp

(define (reverse x)
  (define (r a b)
    (if (null? a)
        b
        (r (cdr a) (cons (car a) b))))

  (r x '()))

(define (r x)
  (let rr ((a x)
        (b '()))
    (if (null? a)
        b
        (rr (cdr a) (cons (car a) b)))))

(reverse (list 1 2 3))
(r (list 1 2 3))