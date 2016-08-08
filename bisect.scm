#lang planet neil/sicp

(define (close-enough? x y)
  (< (abs (- x y)) 0.00000001))

(define (average x y)
  (/ (+ x y) 2))

(define (neg-sin x)
  (sin ( * -1 x )))

(define (bisect-increasing f a b)
  (let* ((midpoint (average a b)) 
         (test-value (f midpoint)))
    (cond ((close-enough? a b) 
            midpoint)
          ((negative? test-value)
            (bisect-increasing f midpoint b))
          ((zero? test-value)
            (midpoint))
          ((positive? test-value)
            (bisect-increasing f a midpoint)))))
         
(define (bisect f a b)
  (if (< (f a) (f b))
    (bisect-increasing f a b)
    (bisect-increasing (lambda (x) (* -1 (f x))) a b)))

(bisect sin 2.0 4.0)
