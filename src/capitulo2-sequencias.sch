#lang racket
(require rackunit)

(define (accumulate operation initial sequence)
	(if (null? sequence)
		initial
		(operation (car sequence) (accumulate operation initial (cdr sequence)))))
		
(check-equal? (accumulate + 0 '(1 2 3 4)) 10)


(define (length2 seq) (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(check-equal? (length2 '(1 2 3 4)) 4)

(define (map2 f seq) (accumulate (lambda (x y) (cons (f x) y)) '() seq))

(check-equal? (map2 (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))

(define (filter2 f seq) (accumulate (lambda (x y) (if (f x) (cons x y) y)) '() seq))

(check-equal? (filter2 (lambda (x) (odd? x)) '(1 2 3 4)) '(1 3))

(define (append2 a b) (accumulate (lambda (x y) (cons x y)) b a))

(check-equal? (append2 '(1 2) '(3 4)) '(1 2 3 4))

; atenção! quadratico
(define (reverse-r x) (accumulate (lambda (x y) (append y (list x))) '() x))

(check-equal? (reverse-r '( 1 2 3 4)) '(4 3 2 1))


