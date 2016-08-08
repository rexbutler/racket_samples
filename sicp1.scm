#lang racket

(define (accumulate initial op xs)
  (if (null? xs)
    initial
    (op (car xs) (accumulate initial op (cdr xs)))))

(define (my-length xs)
  (accumulate 0 (lambda (x y) (+ 1 y)) xs))

(define (my-map f xs)
  (accumulate '() (lambda (x y) (cons (f x) y)) xs))

(define (my-append xs ys)
  (accumulate ys (lambda (x y) (cons x y)) xs))

(my-append (list 1 2 3 4 ) (list 11 12 13 14))
