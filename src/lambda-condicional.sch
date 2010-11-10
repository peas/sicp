#lang racket

(define T (lambda (a b) a))

(define F (lambda (a b) b))

(define mostra (lambda (boolean) (boolean 'verdadeiro 'falso)))

(mostra T)
(mostra F)


; se B1 for verdadeirao, vai devolver B2
; senao devolve F de cara!
(define and (lambda (b1 b2) (b1 b2 F)))

(mostra (and T T))
(mostra (and T F))
(mostra (and F T))
(mostra (and F F))

; se b1 for verdadeiro, devolve T de cara
; senao devolve b2
(define or (lambda (b1 b2) (b1 T b2)))

(newline)
(mostra (or T T))
(mostra (or T F))
(mostra (or F T))
(mostra (or F F))

(newline)

; se for verdadeiro, devolve primeiro elemento (que é F)
; senao devolve o segundo, que é T
(define negacao (lambda (b) (b F T)))

(mostra (negacao T))
(mostra (negacao F))

