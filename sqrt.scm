#lang planet neil/sicp

(define (mu_compute mu_initialize mu_iterate mu_finished? mu_finalize)
  (define (mu_iterate_repeat x)
    (if (mu_finished? x)
        x
        (mu_iterate_repeat (mu_iterate x))))
  
  (lambda(x) (mu_finalize (mu_iterate_repeat (mu_initialize x)))))

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (sqrt_new y)
  (let* ((m_init (lambda(x) x))
         (m_iter (lambda(x) (average x (/ y x))))
         (m_finish? (lambda(x) (< (abs (- y (square x))) 0.00001)))
         (m_final (lambda(x) x)))
    ((mu_compute m_final m_iter m_finish? m_init) 1)))

(sqrt_new 9.0)
