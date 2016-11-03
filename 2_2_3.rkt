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

;; accumulate
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
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
;; (display (append (list 1 2)(list 3 4)))
(define (length sequence)
  (accumulate (lambda (x y)(+ 1 y)) 0 sequence))
;; (display (length (list 1 2 3 4)))

;; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term)(+ this-coeff (* higher-term x)))
              0
              coefficient-sequence))
;; (display (horner-eval 2 (list 1 3 0 5 0 1)))

;; exercise 2.35
;; (define (count-leaves t)
;;   (accumulate + 0 (map () ())))





