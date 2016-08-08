#lang planet neil/sicp

(define (fibb-recursive m)
  (cond ((= m 0) 0)
        ((= m 1) 1)
        ((= m 2) 2)
        (else (+ (fibb-recursive (- m 3)) (fibb-recursive (- m 2)) (fibb-recursive (- m 1))))))

(define (fibb m)
  (define (fibb-iter a b c k)
    (if (= k m)
      a
      (fibb-iter b c (+ a b c) (+ k 1))))
  (fibb-iter 0 1 2 0))

(fibb 50)
