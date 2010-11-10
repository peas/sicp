#lang racket

(define T (lambda (a b) a))

(define F (lambda (a b) b))

(define mostra (lambda (boolean) (boolean 'verdadeiro 'falso)))

(mostra T)
(mostra F)

(define and (lambda (b1 b2) (b1 b2 F)))

(mostra (and T T))
(mostra (and T F))
(mostra (and F T))
(mostra (and F F))

(define or (lambda (b1 b2) (b1 T b2)))

(newline)
(mostra (or T T))
(mostra (or T F))
(mostra (or F T))
(mostra (or F F))

(newline)
(define negacao (lambda (b) (b F T)))

(mostra (negacao T))
(mostra (negacao F))

