#lang planet neil/sicp

(define x1 (list 1 3 (list 5 7) 9))
(define x2 (list (list 7)))
(define x3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdaddr x1))
(caar x2)
(cadr (cadr (cadr (cadr (cadr (cadr x3))))))
