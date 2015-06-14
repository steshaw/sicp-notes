#lang racket

;
; Implementation of sqrt using fixed point.
;
; See https://mitpress.mit.edu/sicp/chapter1/node22.html
;

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square n)
  (* n n))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;
; Definition of fixed-point.
;
; See https://mitpress.mit.edu/sicp/chapter1/node21.html
;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))