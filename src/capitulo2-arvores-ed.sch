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


(define (reverse tree)
	(define (reverse2 rest result)
		(cond 	((null? rest) result)
				(#t (reverse2 (cdr rest) (cons (if (atom? (car rest)) (car rest) (reverse2 (car rest) '())) result)))))
	(reverse2 tree '()))

(check-equal? (reverse '((1 2) (3 4))) '((4 3) (2 1)))

; DFS

(define (fringe x)
	(cond ((null? x) x)
			((atom? x) (list x))
			(#t (append (fringe (car x)) 
			   (fringe (cdr x))))))
			
(check-equal? (fringe '((1 2) (3 4))) '(1 2 3 4))


(define square (lambda (x) (* x x)))

(define (map-tree f tree)
	(map 
		(lambda (x) 
			(if (atom? x) (f x)
		 		(map-tree f x)))
		tree))

(check-equal? (map-tree square '(1 (2 3) (4 5))) '(1 (4 9) (16 25)))

; (map (lambda (x) (display x) (newline)) '( 1 2 (3 4) 5))