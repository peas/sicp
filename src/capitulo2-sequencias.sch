#lang racket
(require rackunit)

(define (accumulate operation initial sequence)
	(if (null? sequence)
		initial
		(operation (car sequence) (accumulate operation initial (cdr sequence)))))
		
(check-equal? (accumulate + 0 '(1 2 3 4)) 10)


(define (length seq) (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(check-equal? (length '(1 2 3 4)) 4)
