#lang racket


(define zero 
	(lambda (f) (lambda (x) x)))

(define um
	(lambda (f) (lambda (x) (f x))))

(define dois
	(lambda (f) (lambda (x) (f (f x)))))

(define tres
	(lambda (f) (lambda (x) (f (f (f x))))))

(define (mostra numero) ((numero (lambda (x) (+ x 1))) 0))

(mostra zero)
(mostra um)
(mostra dois)
(mostra tres)

; devolve uma nova funcao que, antes de fazer f(x), aplica
; n em f e pega esse resultado para aplicar em x (isso é, ((n(f))x) )
; como essa funcao vai ser a identidade do zero (lambda (x) x), vai
; fazer a cadeia de Fs se aplicarem
(define (inc n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
	

(define -um (inc zero))
(define -dois (inc um))
(define -tres (inc dois))

(mostra zero)
(mostra -um)
(mostra -dois)
(mostra -tres)

; n1(f), pega o resultado e 
(define (soma n1 n2)
	(lambda (f) (lambda (x) ((n2 (n1 f)) x))))

(newline)
(mostra (soma -dois -um))
(mostra (soma -um -dois))
(mostra (soma -dois -dois))
(mostra (soma -dois -tres))
(mostra (soma -tres -dois))
(mostra (soma -tres zero))

; facil! n1(n2(f)) aplicado em x
(define (mult n1 n2)
	(lambda (f) (lambda (x) ((n2 (n1 f)) x))))

(newline)
(mostra (mult -dois -um))
(mostra (mult -um -dois))
(mostra (mult -dois -dois))
(mostra (mult -dois -tres))
(mostra (mult -tres -dois))
(mostra (mult -tres zero))
