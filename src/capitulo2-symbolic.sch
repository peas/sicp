#lang racket
(require rackunit)

(cdr '((x1 x2) (y1 y2)))
(memq 'red '(red shoes blue socks))
(memq 'red '((red shoes) (blue socks)))
(eq? '(a b) '(a b))
		
		
; considerando ambas as listas de mesmo tamanho:		
(define (equal2? a b)
	(cond 	((and (null? a) (null? b)) #t)
			((symbol? a) (eq? a b))
			(else (and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b))))))
		
(check-equal? (equal2? '(a b) '(a b)) #t)
(check-equal? (equal2? '(a b) '(a c)) #f)
