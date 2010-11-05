#lang racket

(define (inc x)
	(+ x 1))

(define (dec x)
	(- x 1))


(define (sigma a term next b)
	(if (> a b) 
		0
		(+ (term a) (sigma (next a) term next b))))
		

(define (identity x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (next-pi x) (+ x 4))
(define (term-pi x) (/ 1.0 (* x (+ x 2))))


(sigma 1 identity inc 100)
(* 8 (sigma 1 term-pi next-pi 1000))
(sigma 1 cube inc 100)



(define (integral f a b dx) 
	(define (add-dx x) (+ dx x))
	(* dx (sigma (+ a (/ dx 2.0)) f add-dx b)))

(integral cube 0 1 0.01)	
(integral cube 0 1 0.001)

;; 1.29 simpsons's rule para aproximacao da integral:

(define (even? x) (= 0 (remainder x 2)))

(define (integral-simpson f a b n) 
	(define h (/ (- b a) n))
	(define (y k) (f (+ a (* k h))))
	(define (term-y k) (* (y k) (if (even? k) 2 4)))
	(* (/ h 3.0) (+ (y 0) (y n) (* 4 (y (- n 1))) (sigma 1 term-y inc (- n 2)))))
	
	(integral-simpson cube 0.0 1.0 100)
	(integral-simpson cube 0.0 1.0 1000)

;; 1.30 versao iterativa da soma

(define (sigma-i a term next b)
	(define (sigma-iter n result)
		(if (> n b) 
			result
			(sigma-iter (next n) (+ result (term n)))))
	(sigma-iter a 0)) 

(sigma-i 1 identity inc 100)
(* 8 (sigma-i 1 term-pi next-pi 1000))
(sigma-i 1 cube inc 100)


(define (accumulate combiner null-value a term next b)
	(if (> a b) 
		null-value
		(combiner (term a) (accumulate combiner null-value (next a) term next b))))


(define (sigma-a a term next b) (accumulate + 0 a term next b))
(define (product a term next b) (accumulate * 1 a term next b))

(sigma-a 1 identity inc 100)
(* 8 (sigma-a 1 term-pi next-pi 1000))
(sigma-a 1 cube inc 100)


; accumulate versao com processamento iterativo
(define (accumulate2 combiner null-value a term next b)
	(define (accumulate-iter n result) 
		(if (> n b) 
			result
			(accumulate-iter (next n) (combiner result (term n)))))
		(accumulate-iter a null-value))

(define (sigma-a2 a term next b) (accumulate2 + 0 a term next b))
(define (product2 a term next b) (accumulate2 * 1 a term next b))


(sigma-a2 1 identity inc 100)
(* 8 (sigma-a2 1 term-pi next-pi 1000))
(sigma-a2 1 cube inc 100)



(define average (lambda (x y) (/ (+ x y) 2.0)))
(average 3 4)

(define tolerance 0.000001)
(define (fixed-point f guess)
	(define (good? v1 v2) (< (abs (- v1 v2)) tolerance))
	(define (try x)
		(let ((next-x (f x)))
			(if (good? x next-x) next-x (try next-x))))
	(try guess))


(fixed-point cos 1)

(define (raiz x) (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(raiz 2.0)
(raiz 4.0)
