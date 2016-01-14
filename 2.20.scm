#lang planet neil/sicp

(define (same-parity x . y)
  (define b (odd? x))
  (define (iter a c)
    (if (null? c)
        (cons a '())
        (if b
            (if (odd? (car c))
                (iter (cons a (car c)) (cdr c))
                (iter a (cdr c)))
            (if (even? (car c))
                (iter (cons a (car c)) (cdr c))
                (iter a (cdr c))))))
  (iter x y))

(same-parity 1 2 3 4 5 6)
(same-parity 2 4 5 6 7 8 9)

              