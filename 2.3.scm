#lang scheme

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (midpoint-segment l)
  (make-point
   (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
   (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point
 (midpoint-segment
  (make-segment
   (make-point 0 0)
   (make-point 100 100))))

(define (width r)
  (abs (- (x-point (start-segment r))
          (x-point (end-segment r)))))

(define (height r)
  (abs (- (y-point (start-segment r))
          (y-point (end-segment r)))))

(define (perimeter r)
  (* 2 (+ (width r) (height r))))

(define (area r)
  (* (width r) (height r)))

(define (make-rect p1 p2) (make-segment p1 p2))

(let
    ((r (make-rect (make-point 100 100) (make-point 0 0))))
  
  (area r)
  (perimeter r))
  