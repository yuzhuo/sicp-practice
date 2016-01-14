#lang scheme

(define (sum b e f n)
  (define (sum-iter b s)
    (if (> b e) s (sum-iter (n b) (+ s (f b)))))
  
  (sum-iter b 0))

(define (integral a b n)
  (define h (/ (- b a) n))
  (define h/3 (/ h 3.0))
  (define (next x) (+ x (* 2 h)))
  (define (cube x) (* x x x))
  (* h/3 (+ a
            (* 4 (sum (+ a h) (+ a (* n h)) cube next))
            (* 2 (sum (+ a h h) (+ a (* (- n 2) h)) cube next))
            b)))

(integral 0 1 10)