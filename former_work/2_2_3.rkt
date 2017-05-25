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
(define (map-new p sequence)
  (accumulate (lambda (x y)(cons (p x) y))
              nil
              sequence))
;; (display (map-new fib (list 1 3 43 545 6)))
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
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))
;; (define tree (list 1 2 (list 3 4) (list 5 (list 6 7))))
;; (display (count-leaves tree))
;; exercise 2.36
(define (accumulate-n op init seqs)
  (if(null? (car seqs))
     nil
     (cons (accumulate op init (map car seqs))
           (accumulate-n op init (map cdr seqs)))))

;; (define s (list (list 1 2 3)(list 4 5 6)(list 7 8 9)))
;; (display(accumulate-n + 0 s))

;; exercise 2.37 basic matrix and vector operations
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;; (define a (list 1 2 3))
;; (define b (list 4 5 6))
;; (define c (list (list 1 1 1)(list 2 2 2)(list 3 3 3)))
;; (dot-product a b)

(define (maxtrix-*-vector m v)
  (map
    (lambda (matrix-row) (accumulate + 0 (map * v matrix-row)))
    m))
;; (display (maxtrix-*-vector c a))

(define (transpose mat)
  (accumulate-n cons nil mat))
;; (display c)
;; (display (transpose c))

(define (maxtrix-*-maxtrix m n)
  (let((cols (transpose n)))
    (map (lambda (mat) (maxtrix-*-vector cols mat)) m)))
;; (define e (list(list 1 2 3)(list 4 5 6)))
;; (define d (list(list 7 8)(list 9 10)(list 11 12)))
;; (display (maxtrix-*-maxtrix c c))
;; (display (maxtrix-*-maxtrix e d))

;; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(display (accumulate / 1 (list 1 2 3)))
(newline)
(display (fold-left / 1 (list 1 2 3)))
(newline)
(display (accumulate list nil (list 1 2 3)))
(newline)
(display (fold-left list  nil (list 1 2 3)))
(newline)
(display (accumulate + 0 (list 1 2 3)))
(newline)
(display (fold-left + 0 (list 1 2 3)))
(newline)

;; exercise 2.39
(define (reverse-r sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))
(display (reverse-r (list 1 2 3)))
(define (reverse-l sequence)
  (fold-left (lambda (x y) (y x)) nil sequence))
;; (display (reverse-l (list 1 2 3)))

;; nested mapping
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (showall n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;; (newline)
;; (display (showall 4))
;; (newline)
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(define (permutations s)
  (if(null? s)
     (list nil)
     (flatmap (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
              s)))
;; (display (permutations (list 1 2 3 )))

;; exercise 2.40
(define (unique-pairs n)
  (flatmap  (lambda (i)
          (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))
;; (display (unique-pairs 4))
