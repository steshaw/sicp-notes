#lang racket

;
; See https://mitpress.mit.edu/sicp/chapter1/node9.html
;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
   (average guess (/ x guess)))

(define (square n)
  (* n n))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt n)
  (sqrt-iter 1.0 n))