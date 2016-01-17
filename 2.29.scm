#lang planet neil/sicp

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch x)
  (car x))
(define (right-branch x)
  (cdr x))
(define (branch-length x)
  (car x))
(define (branch-structure x)
  (cdr x))

(define (total-weight s)
  (if
   (number? s)
   s
   (+
    (total-weight (branch-structure (left-branch s)))
    (total-weight (branch-structure (right-branch s))))))

(define (balanced? x)
  (define (torque b)
    (define s (branch-structure b))
    
    (*
     (branch-length b)
     (if
      (number? s)
      s
      (total-weight s))))
  (define lbs (branch-structure (left-branch x)))
  (define rbs (branch-structure (right-branch x)))
  (define lb (left-branch x))
  (define rb (right-branch x))
  
  (cond ((and (number? lbs) (number? rbs)) (= (torque lb) (torque rb)))
        ((number? lbs) (and (= (torque lb) (torque rb)) (balanced? rbs)))
        ((number? rbs) (and (= (torque lb) (torque rb)) (balanced? lbs)))
        (else (and (= (torque lb) (torque rb)) (balanced? lbs) (balanced? lbs)))))


(define m1 (make-mobile (make-branch 1 2) (make-branch 2 1))) 
(define m2 (make-mobile (make-branch 3 1) (make-branch 1 m1))) 
(balanced? m2)    ;#true 


;第四题可以看出抽象带来的可维护性










