#lang planet neil/sicp

(define (accumulate1 combiner init-val term a next b)
  (if (> a b)
    init-val
    (combiner (term a) (accumulate1 combiner init-val term (next a) next b))))

(define (accumulate2 combiner init-val term a next b)
  (define (iter a result)
    (if (> a b)
      result 
      (iter (next a) (combiner result (term a)))))
  (iter a init-val))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (accumulate1 + 0 cube a inc b))

(sum-cubes 1 10)

