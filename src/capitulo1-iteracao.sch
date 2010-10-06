#lang racket

(define (inc x)
	(+ x 1))

(define (dec x)
	(- x 1))
	
; realmente recursivo:
(define (plus a b)
	(display a)(display " ")(display b)	(newline)
	(if (= a 0)
		b
		(inc (plus (dec a) b)))
)

; iterativo: matem estado toda hora, nao usa a pilha para armazenar nada
; fica na cara pois o resultado da recursão é rapidamente devolvido, sem utiliza-lo
; indicando que a toda invocacao temos tudo o que é necessario para o calculo
(define (plus2 a b)
	(display a)(display " ")(display b)	(newline)
	(if (= a 0)
		b
		(plus2 (dec a) (inc b))
	)
)

(plus 3 4)

(plus2 3 4)


(define (moeda tipo)
	(cond	((= tipo 1) 1)
			((= tipo 2) 5)
			((= tipo 3) 10)
			((= tipo 4) 25)
			((= tipo 5) 50)
			
	)
)

(moeda 5)
(moeda 7)

(define (t quantia tipo_moeda)
	(cond 	((= quantia  0) 1) ; bateu exato, é valido
			((or (< quantia 0) (= tipo_moeda 0)) 0)
			(else 	(+ 
						(t (- quantia (moeda tipo_moeda)) tipo_moeda) ; usando a moeda atual
						(t quantia (dec tipo_moeda)) ; sem usar a moeda atual
					)
			)
		)
)

(define (troco quantia)
	(t quantia 5))

(troco 100)


(define (pascal x y)
	(display x)(display " ")(display y)	(newline)	
	(if (or (= y x) (= y 0)) 1
		(+ (pascal (- x 1) y) (pascal (- x 1) (- y 1)))
	)
)

(pascal 5 3)


