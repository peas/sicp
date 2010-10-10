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

; recursivo, exponencial
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

; basicao recursivo linear
(define (pascal x y)
	(display x)(display " ")(display y)	(newline)	
	(if (or (= y x) (= y 0)) 1
		(+ (pascal (- x 1) y) (pascal (- x 1) (- y 1)))
	)
)

(pascal 5 3)


;; exponeciacao

(define (exponenciacao x n)
	(cond 	[(= n 0) 1]
			[(= 0 (remainder n 2)) (* (exponenciacao x (/ n 2)) (exponenciacao x (/ n 2)))]
			[else (* x (exponenciacao x (- n 1)))]))

(exponenciacao 2 5)
(exponenciacao 2 6)
(exponenciacao 2 7)
(exponenciacao 2 8)

;1.16 ab^n é a invariante
(define (exp b n a)
	(cond 	[(= n 0) a]
			[(= 0 (remainder n 2)) (exp (* b b) (/ n 2) a)]
			[else (exp b (- n 1) (* a b))]))

(define (exp-fast x n) (exp x n 1))

(exp-fast 2 5)
(exp-fast 2 6)
(exp-fast 2 7)
(exp-fast 2 8)
(exp-fast 2 17)

(define (minimo-divisor n x)
	(cond 	((= (remainder n x) 0) x)
			((>= (* x x) n) n)
			(#t (minimo-divisor n (+ x 1)))))

(define (minimod n)
	(minimo-divisor n 2))

(minimod 1)
(minimod 17)
(minimod 21)

(define (even? x) (= 0 (remainder x 2)))

