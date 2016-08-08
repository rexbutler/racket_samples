#lang racket

(define (my-fold-right op init xs)
  (if (null? xs)
    init
    (op (car xs) (my-fold-right op init (cdr xs)))))

(define (my-fold-left op init xs)
  (define (iterate val ys)
    (if (null? ys)
      val 
      (iterate (op val (car ys)) (cdr ys))))
  (iterate init xs))

(define (my-fold-right-n op init xxs)
  (if (null? (car xxs))
    '()
    (cons (my-fold-right op
                         init 
                         (map car xxs))
          (my-fold-right-n op 
                           init 
                           (map cdr xxs)))))

(define (reverse-iterate xs)
  (define (iterate xs rs)
    (if (null? xs)
      rs
      (iterate (cdr xs) (cons (car xs) rs))))
  (iterate xs '()))

(reverse-iterate '(1 2 3 4 5 6 7))

