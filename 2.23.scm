#lang planet neil/sicp

(define (for-each f x)
  (define (f-iter y)
    (if (null? y)
        true
        (begin (f (car y)) (f-iter (cdr y)))))

  (f-iter x))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 1 2 3 4))