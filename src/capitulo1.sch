#lang racket

(+ 2 3)
(+ (* 3 2) (+ 1 7))

(define a 10)
(+ a 5)


#| (define (abs x)
	(cond 	[((< x 0) (- x))]
			[((= x 0) 0)]
			[((> x 0) x)]
	)
) |#
		
	
(define (abs x) 
	(cond 	((< x 0) (- x))
			(#t x)
	)
)

		
(abs (- 5))
(abs 5)

(>= 10 5)

(/ 
	(+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
	(* 3 (- 6 2) (- 2 7))	)

(define (two-largests a b c)
	(if (< a b)
		(if (< a c) (list b c) (list b a))
		(if (< b c) (list a c) (list a b))
	) 
)

(two-largests 1 2 3)

(define (square x) (* x x))

(define (sum-of-squares x y)
	 (+ (square x) (square y)))

(define (sum-of-squares-list x)
	(sum-of-squares (car x) (car (cdr x))))

(sum-of-squares 3 4)

(define (square-of-two-largests a b c) 
	(sum-of-squares-list (two-largests a b c)))

(square-of-two-largests 2 1 3)
	

(define (p) (p))

(define (test x y)
	(if (= x 0) 0 y)	
)

; busca em profundidade ferra isso e da loop infinito, se fosse normative (em alrgura) funcionaria
;(test 0 (p))

(list? 'x)

(begin
	(display "Hello")
	(newline))
	
(define (sqrt-newton x)
	(define (improve-guess guess) (/ (+ guess (/ x guess)) 2))
	(define (good? guess) (< (abs (- x (square guess))) 0.001))
	(define (sqrt-iteracao guess)
		(if (good? guess) 
			guess
			(sqrt-iteracao (improve-guess guess))))
	(sqrt-iteracao 1.0))

(sqrt-newton 2)


(define (pontos x)
	(define (soma-lista x) (apply + x))
	(define (strike? x) (= (length x) 1))
	(define (spare? x) (= (soma-lista x) 10))

	(define (pontos2 jogada-anterior jogadas)
		; devolve a proxima jogada, que pode ter um ou dois lances
		(define (proxima-jogada)
			(if (= (first jogadas) 10)
				(list 10)
				(list (first jogadas) (first (rest jogadas)))
			)
		)
		; devolve o restante das jogadas, removendo a proxima
		(define (outras-jogadas)
			(if (= (length (proxima-jogada)) 1) 
				(rest jogadas)
				(rest (rest jogadas))
			)
		)
		(if (empty? jogadas)
			0
			(+
				(soma-lista (proxima-jogada))
				(pontos2 (proxima-jogada) (outras-jogadas))
				; calculando bonus
				(cond
					[(strike? jogada-anterior) (soma-lista (proxima-jogada))]
					[(spare? jogada-anterior) (first (proxima-jogada))]
					[else 0])
			)
		)
	)
	(pontos2 '(0 0) x)
)

(define (assert x y) (begin (display x) (display  " should be ") (display y) (newline)))

(assert 10 (pontos '(1 2 3 4 0 0)))
(assert 22 (pontos '(5 5 3 4 1 1)))
(assert 29 (pontos '(5 5 6 4 1 1)))
(assert 20 (pontos '(10 2 3)))
(assert 40 (pontos '(10 10 2 3)))
(assert 28 (pontos '(5 5 5 5 1 1)))
(assert 38 (pontos '(1 1 10 5 5 1 4)))


