#lang racket

(define (-cons a b) (lambda (f) (f a b)))

(define (-car x) (x (lambda (a b) a)))

(define (-cdr x) (x (lambda (a b) b)))

(define lista (-cons 1 (-cons 2 3)))

(-car lista)

(-car (-cdr lista))

(-cdr lista)
	



