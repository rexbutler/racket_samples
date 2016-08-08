#lang racket

(define (sorted-set-in x xs)
  (cond ((null? xs) false)
        ((< x (car xs)) false)
        ((= x (car xs)) true)
        (else (sorted-set-in x (cdr xs)))))

(define (sorted-set-union xs ys)
  (define (sorted-set-union-nn xs ys)
    (let ((x (car xs))
          (y (car ys)))
      (cond ((< x y) 
              (cons x (sorted-set-union (cdr xs) ys)))
            ((= x y)
              (sorted-set-union (cdr xs) ys))
            (else (cons y (sorted-set-union xs (cdr ys)))))))
  (cond ((null? xs) ys)
        ((null? ys) xs)
        (else (sorted-set-union-nn xs ys))))

(define (sorted-set-intersection xs ys)
  (if (or (null? xs) (null? ys)) 
    (list)
    (let ((x (car xs))
          (y (car ys))
          (txs (cdr xs))
          (tys (cdr ys)))
      (cond ((< x y) (sorted-set-intersection txs ys))
            ((= x y) (cons x (sorted-set-intersection txs tys)))
            (else (sorted-set-intersection xs tys))))))

(define (sorted-set-difference set1 set2)
  (if (or (null? set1) (null? set2))
    set1
    (let ((el1 (car set1))
          (el2 (car set2)))
      (cond ((= el1 el2) (sorted-set-difference (cdr set1) (cdr set2)))
            ((< el1 el2) (cons el1 (sorted-set-difference (cdr set1) set2)))
            ((> el1 el2) (sorted-set-difference set1 (cdr set2)))))))


(sorted-set-difference (list 1 2 3) (list 2 3 4))

