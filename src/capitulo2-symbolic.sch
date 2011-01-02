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
(define y (add 9 (add 7 (add 8 '()))))
(define z (add 0 (add 2 (add 8 '()))))
x
y
z
(check-equal? (set-to-list x) '(0 1 2 3))
(check-equal? (set-to-list y) '(7 8 9))
(check-equal? (set-to-list z) '(0 2 8))
			
; usando o acumulador para evitar o append	
			
(define (union s1 s2)
	(if (null? s1) s2
		(union (left s1) (union (right s1) (add (entry s1) s2)))))

(check-equal? (set-to-list (union x y)) '(0 1 2 3 7 8 9))


(define (intersection s1 s2)
	(if (or (null? s1) (null? s2)) '()
		(cond 	((= (entry s1) (entry s2)) 
					(add (entry s1) (union (intersection (left s1) (left s2)) (intersection (right s1) (right s2)))))
				((< (entry s1) (entry s2)) 
					(union (intersection s1 (left s2)) (intersection (right s1) s2)))
				((> (entry s1) (entry s2)) 
					(union (intersection (left s1) s2) (intersection s1 (right s2))))
		)))

(check-equal? (set-to-list (intersection x z)) '(0 2))
(check-equal? (set-to-list (intersection y z)) '(8))
(check-equal? (set-to-list (intersection y x)) '())



