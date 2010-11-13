#lang racket


(define zero (lambda (f) (lambda (x) x)))

(define (inc n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define um (inc zero))
(define dois (inc um))

(define (mostra numero) ((numero (lambda (x) (+ x 1))) 0))

(mostra zero)
(mostra um)
(mostra dois)

(define (soma a b) (b b))

(mostra (soma dois um))
