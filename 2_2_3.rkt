#lang sicp
;; book example
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
;; (display (enumerate-interval 1 10))
;; filter
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;; accumulate(reduce in Python)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;; (display (accumulate cons nil (list 1 2 3)))

;; Fibonacci
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
;; (display (map fib (enumerate-interval 0 10)))
(define (even-fib n)
  (accumulate cons nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
;; (display (even-fib 10))
;; exercise 2.33
;; ((lambda (x y) (+ ( * 2 x) y))1 10)
(define (map p sequence)
  (accumulate (lambda (x y)(cons (p x) y))
              nil
              sequence))
;; (display (map fib (list 1 3 43 545 6)))
;; (define (appdend ))
