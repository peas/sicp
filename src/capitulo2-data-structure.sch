#lang racket

(define (inc x)
	(+ x 1))

(define (dec x)
	(- x 1))


	(define (gcd a b) 
		(cond [(= 0 (remainder a b)) b]
			  [#t (gcd b (remainder a b))]))


	(define (numer x) (car x))
	(define (denom x) (cdr x))

(define (make-rat x y)
	(let ((minimo (gcd x y) ))
	 (cons (/ x minimo) (/ y minimo))))

(define (print-rat x) 
	(display (numer x)) (display "/") (display (denom x)) (newline))

	(define (add-rat x y)
	  (make-rat (+ (* (numer x) (denom y))
	               (* (numer y) (denom x)))
	            (* (denom x) (denom y))))
	(define (sub-rat x y)
	  (make-rat (- (* (numer x) (denom y))
	               (* (numer y) (denom x)))
	            (* (denom x) (denom y))))
	(define (mul-rat x y)
	  (make-rat (* (numer x) (numer y))
	            (* (denom x) (denom y))))
	(define (div-rat x y)
	  (make-rat (* (numer x) (denom y))
	            (* (denom x) (numer y))))
	(define (equal-rat? x y)
	  (= (* (numer x) (denom y))
	     (* (numer y) (denom x))))
	

(gcd 40 32)
(gcd 40 33)

(define half (make-rat  1 2))
(define third (make-rat  1 3))
(print-rat half)

(print-rat (add-rat half half))
(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
	
