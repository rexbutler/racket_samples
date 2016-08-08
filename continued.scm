#lang planet neil/sicp

; Recursive Form
(define (cont-frac2 num den k)
  (define (go n)
    (if (= n 0)
      (/ (num n) (den n))
      (/ (num n) (+ (den n) (go (- n 1))))))
  (go k))

; Iterative Form
(define (cont-frac num den k)
  (define (cont-frac-iter alpha n)
    (if (= n 0)
       alpha
       (cont-frac-iter (/ (num n) (+ (den n) alpha)) (- n 1))))
  (cont-frac-iter 0.0 k))

(define MY-PI 3.141592)

(define (a k x)
  (if (= k 1)
    x
    (* -1 x x)))

(define (b k x) 
  (+ 1 (* 2 (- k 1))))

(define (aa x)
  (lambda (k) (a k x)))

(define (bb x)
  (lambda (k) (b k x)))

(define (tan-cf x k)
  (cont-frac (aa x)
             (bb x)
             k))

(tan-cf 2.0 500)


