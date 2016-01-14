#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (display-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]"))

(define (add-interval a b)
  (+ (lower-bound a) (lower-bound b)
     (upper-bound a) (upper-bound b)))

(define (sub-interval a b)
  (make-interval
   (- (lower-bound a) (lower-bound b))
   (- (upper-bound a) (upper-bound b))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< 0 (* (upper-bound y) (lower-bound y)))
      (mul-interval x
                    (make-interval
                     (/ (lower-bound y) 1)
                     (/ (upper-bound y) 1)))
      (error "Division error!(interval spans 0)")))

(define (new-mul-interval x y) 
  (define (endpoint-sign i)  
    (cond ((and (>= (upper-bound i) 0) 
                (>= (lower-bound i) 0)) 
           1) 
          ((and (< (upper-bound i) 0) 
                (< (lower-bound i) 0)) 
           -1) 
          (else 0))) 
  
  (let ((es-x (endpoint-sign x)) 
        (es-y (endpoint-sign y)) 
        (x-up (upper-bound x)) 
        (x-lo (lower-bound x)) 
        (y-up (upper-bound y)) 
        (y-lo (lower-bound y))) 
    
    (if (and (= es-x 0) (= es-y 0)) 
        ; Take care of the exceptional condition where we have to test 
        (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
                       (max (* x-lo y-lo) (* x-up y-up))) 
        
        ; Otherwise, select which value goes in which "slot". I'm not sure 
        ; whether there is an intuitive way to explain *why* these 
        ; selections work. 
        (let ((a1 (if (and (<= es-y 0) (<= (- es-y es-x) 0)) x-up x-lo)) 
              (a2 (if (and (<= es-x 0) (<= (- es-x es-y) 0)) y-up y-lo)) 
              (b1 (if (and (<= es-y 0) (<= (+ es-y es-x) 0)) x-lo x-up)) 
              (b2 (if (and (<= es-x 0) (<= (+ es-x es-y) 0)) y-lo y-up))) 
          (make-interval (* a1 a2) (* b1 b2))))))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))

(define (center-percent x)
  (define mid-point
    (/ (+ (upper-bound x) (lower-bound x)) 2))
  
  (/ (/ (- (upper-bound x) (lower-bound x)) mid-point)
     2))

(define test (make-center-percent 10 0.5))
(lower-bound test)
(upper-bound test)
(center-percent test)