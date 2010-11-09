#lang racket


(define q (list 1 4 9 16))

(define i (list 1 3 5 7 9))

(define (count list) 
  (if (null? list) 0 (+ 1 (count (cdr list)))))

(define append
	(lambda (list1 list2) 
		(if (null? list1) 
			list2
	    (cons (car list1) (append (cdr list1) list2)))))
		
(count q)
(append q i)

(define (last list)
	(define (last2 before rest)
		(if (null? rest) before (last2 (car rest) (cdr rest))))
	(last2 (car list) (cdr list)))

(last q)
(last i)


(define (reverse list)
	(define (reverse2 rest result)
		(if (null? rest) 
			result
		(reverse2 (cdr rest) (cons (car rest) result))))
	(reverse2 list '()))

(reverse q)