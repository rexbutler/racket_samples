#lang racket 

(require racket/trace)

(define (my-length xs)
  (define (iterate xs len)
    (if (null? xs)
      len
      (iterate (cdr xs) (+ 1 len))))
  (iterate xs 0))

(define (my-reverse xs)
  (define (iterate xs rs)
    (if (null? xs)
      rs
      (iterate (cdr xs) (cons (car xs) rs))))
  (iterate xs (list)))

(define (my-append xs ys)
  (define (iterate rs ys)
    (if (null? rs)
      ys
      (iterate (cdr rs) (cons (car rs) ys))))
  (iterate (my-reverse xs) ys))

(define (my-last xs)
  (define (iterate xs)
    (if (null? (cdr xs))
      (car xs)
      (iterate (cdr xs))))
  (if (null? xs) 
    (error "No last of empty list")
    (iterate xs)))

(define (my-map f xs)
  (define (iterate xs rs)
    (if (null? xs)
      rs
      (iterate f (cdr xs) (cons (f (car xs)) rs))))
  (iterate f (my-reverse xs) (list)))

(define (my-accumulate op initial xs)
  (define (iterate val xs)
    (if (null? xs)
      val
      (iterate (op val (car xs)) (cdr xs))))
  (iterate initial xs))

(define (my-subsets xs)
  (if (null? xs)
    (list (list))
    (let ((rest (my-subsets (cdr xs)))) 
      (append rest (map (lambda (ys) (cons (car xs) ys)) rest)))))

(define (my-filter pred xs)
  (define (iterate xs rs)
    (cond
      ((null? xs) rs)
      ((pred (car xs)) (iterate (cdr xs) (cons (car xs) rs)))
      (else (iterate (cdr xs) rs))))
  (iterate (my-reverse xs) (list)))

(my-subsets (list 0 1 2 3))
