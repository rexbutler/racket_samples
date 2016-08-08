#lang racket 

; A collection of functions to manipulate sets represented as lists
; These functions assume each element shows up in any given list only once:
; use set-reduce to remove duplicates.

; Test whether an element is within a set
(define (set-has-elem elem xs)
  (if (null? xs) 
    #f
    (or (= elem (car xs))
        (set-has-elem elem (cdr xs)))))

; Add an element to a set
(define (set-add-elem elem xs)
  (if (set-has-elem elem xs)
    xs
    (cons elem xs)))

; Remove an element from a set
(define (set-remove-elem elem xs)
  (define (iterate xs rs)
    (cond ((null? xs)
             rs)
           ((= elem (car xs)) 
             (iterate (cdr xs) rs))
           (else
             (iterate (cdr xs) (cons (car xs) rs)))))
  (iterate xs (list)))

; Remove duplicates from a list to for a set
(define (set-reduce xs)
  (define (iterate xs rs)
    (cond ((null? xs)
              rs)
           ((set-has-elem (car xs) (cdr xs)) 
             (iterate (cdr xs) rs))
           (else
             (iterate (cdr xs) (cons (car xs) rs)))))
  (iterate xs (list)))

; Form the intersection of two sets
(define (set-intersect-set xs ys)
  (define (iterate xs rs)
    (cond ((null? xs)
            rs)
          ((set-has-elem (car xs) ys)
            (iterate (cdr xs) (cons (car xs) rs)))
          (else
            (iterate (cdr xs) rs))))
  (iterate xs (list)))

; Subtract the second set from the first
(define (set-subtract-set xs ys)
  (define (iterate xs rs)
    (cond ((null? xs)
            rs)
          ((set-has-elem (car xs) ys)
            (iterate (cdr xs) rs))
          (else
            (iterate (cdr xs) (cons (car xs) rs)))))
  (iterate xs (list)))

; Form the union of two sets
(define (set-union-set xs ys)
  (set-reduce (append xs ys))) 

; Form the power set of a set, or the set of all subsets
(define (set-subsets xs)
  (if (null? xs)
    (list (list))
    (let ((rest (set-subsets (cdr xs)))) 
      (append rest (map (lambda (ys) (cons (car xs) ys)) rest)))))

; Form a random subset of a set, with each element chosen with probability 1/2
(define (set-random-subset xs) 
   (define (iterate xs rs)
     (cond ((null? xs)
            rs)
          ((= (random 2) 1)
            (iterate (cdr xs) (cons (car xs) rs)))
          (else
            (iterate (cdr xs) rs))))
  (iterate xs (list)))
