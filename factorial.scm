#lang planet neil/sicp

(define (mu-compute mu-initialize mu-iterate mu-finished? mu-finalize)
  (define (mu-iterate-repeat x)
    (if (mu-finished? x)
        x
        (mu-iterate-repeat (mu-iterate x))))
  
  (lambda(x) (mu-finalize (mu-iterate-repeat (mu-initialize x)))))

(define (factorial m)
  (let* ((m-init (lambda(x) (cons x 1)))
         (m-iter (lambda(pair) (cons (- (car pair) 1) (* (car pair) (cdr pair)))))
         (m-finish? (lambda(pair) (= (car pair) 1)))
         (m-final cdr))
    ((mu-compute m-init m-iter m-finish? m-final) m)))

(factorial 6.0)
