#lang racket
(require rackunit)

(define q (list 1 4 9 16))

(define i (list 1 3 5 7 9))

(define (count list) 
  (if (null? list) 0 (+ 1 (count (cdr list)))))

(define append
	(lambda (list1 list2) 
		(if (null? list1) 
			list2
	    (cons (car list1) (append (cdr list1) list2)))))
		
(check-equal? (count q) 4)
(check-equal? (append q i) '(1 4 9 16 1 3 5 7 9))

(define (last list)
	(define (last2 before rest)
		(if (null? rest) before (last2 (car rest) (cdr rest))))
	(last2 (car list) (cdr list)))

(check-equal? (last q) 16)
(check-equal? (last i) 9)


(define (reverse list)
	(define (reverse2 rest result)
		(if (null? rest) 
			result
		(reverse2 (cdr rest) (cons (car rest) result))))
	(reverse2 list '()))

(check-equal? (reverse q) '(16 9 4 1))

; nao funciona direito pq faz cons de pair com elemento em vez de elemento com  pair
(define (reverse-nt list)
		(if (null? list) 
			'()
		(cons (reverse-nt (cdr list)) (car list))))

(reverse-nt q)

(define (mesma-paridade . w)
	(define (paridade x lista)
		(if (null? lista) 
			lista
			(if (eq? (remainder (car lista) 2) (remainder x 2))
				(cons (car lista) (paridade x (cdr lista)))
				(paridade x (cdr lista)))))
	(paridade (car w) w))
	
(check-equal? (mesma-paridade 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (mesma-paridade 2 3 4 5 6 7) '(2 4 6))
	
(define (map function list)
	(if (null? list)
		'()
		(cons (function (car list)) (map function (cdr list)))))

(define (square x) (* x x))
(check-equal? (map square '(2 3 4 5)) '(4 9 16 25))

; 2.22 mesmo problema que inverter lista
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list '(1 2 3 4))

; 2.23
(define (for-each function list)
	(if (null? list) #t (begin (function (car list)) (for-each function (cdr list))) ))

(for-each (lambda (x) (display x) (newline)) '( 1 2 4 5))
