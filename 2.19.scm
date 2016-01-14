#lang planet neil/sicp

;; section 1.2.2
(define (count-change amount) (cc amount 5))
(define (cc amount kind-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kind-of-coins 0) 0)
        (else (+ (cc amount (- kind-of-coins 1))
                 (cc (- amount
                        (first-denomination kind-of-coins))
                     kind-of-coins)))))

(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 50)
        ((= kind-of-coins 2) 25)
        ((= kind-of-coins 3) 10)
        ((= kind-of-coins 4) 5)
        ((= kind-of-coins 5) 1)))

(count-change 100)

;; 2.19

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination2 coin-values)
  (car coin-values))

(define (cc2 amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc2 amount
                (except-first-denomination
                 coin-values))
            (cc2 (- amount
                   (first-denomination2
                    coin-values))
                coin-values)))))

(define us-coins (list 25 50 10 5 1))

(display "cc2:")
(newline)
(cc2 100 us-coins)