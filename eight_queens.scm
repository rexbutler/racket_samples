#lang racket

(define (flatmap f plist)
  (foldr append
         '()
         (map f plist)))

(define (permutations plist)
  (if (null? plist)
    (list '())
     (flatmap (lambda (x) 
       (map (lambda (p) (cons x p))
            (permutations (remove x plist))))
       plist)))

(define all-permutations (list 0 1 2 3 4))

(define queens8-permutations (filter queens8-permutation all-permutations)) 

(count queens8-permutations)


; (list 3 6 2 0 5 7 4 1))
