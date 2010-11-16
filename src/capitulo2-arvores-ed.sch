#lang racket
(require rackunit)

(define (count list) 
  (if (null? list) 0 (+ 1 (count (cdr list)))))

(define (atom? x) (not (pair? x)))

(define (folhas x)
	(cond ((null? x) 0)
			((atom? x) 1)
			(#t (+ (folhas (car x)) 
			   (folhas (cdr x))))))
			
(define x (cons (list 1 2) (list 3 4)))


(check-equal? (count x) 3)
(check-equal? (folhas x) 4)

(check-equal? (count (list x x)) 2)
(check-equal? (folhas (list x x)) 8)
