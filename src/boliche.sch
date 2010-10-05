#lang racket
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

