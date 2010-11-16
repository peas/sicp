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


(define s1 (list 1 3 (list 5 7) 9))

(define s2 (list (list 7)))

(define s3 '( 1 ( 2 ( 3 ( 4 ( 5 ( 6 7)))))))

(check-equal? (car (cdr (car (cddr s1)))) 7)
(check-equal? (caar s2) 7)
(check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr s3)))))))))))) 7)
