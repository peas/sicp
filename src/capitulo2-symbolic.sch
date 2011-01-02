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

; arvore binaria
(define (entry x) (car x))
(define (left x) (cadr x))
(define (right x) (caddr x))
(define (tree entry left right) (list entry left right))
(define (element? x set) 
	(cond	((null? set) false)
			((= (entry set) x) true)
			((< (entry set) x) (element? x (right x)))
			((> (entry set) x) (element? x (left x)))))
			
(define (add x set)
	(cond	((null? set) (tree x '() '()))
			((= (entry set) x) set)
			((> (entry set) x) (tree (entry set) (add x (left set)) (right set)))
			((< (entry set) x) (tree (entry set) (left set) (add x (right set))))))

(define (set-to-list x)
	(if (null? x) '()
		(append (set-to-list (left x))
		  (cons (entry x) (set-to-list (right x))))))
		
(define x (add 0 (add 3 (add 2 (add 1 '())))))
x
(check-equal? '(0 1 2 3) (set-to-list x))
			
; usando o acumulador para evitar o append	
			
(define (set-to-list2 set)
	(define (copy-to-list x result))
		(if (null? x) '()
			(append (set-to-list (left x))
		  	(cons (entry x) (set-to-list (right x)))))
	(copy-to-list set '())
