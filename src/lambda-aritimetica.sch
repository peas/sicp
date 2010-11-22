#lang racket

; numeros de Church expandidos. use isso para perceber o que eh
; pense que é como um inject, vai aplicando o retorno em f

(define zero 
	(lambda (f) (lambda (x) x)))

(define um
	(lambda (f) (lambda (x) (f x))))

(define dois
	(lambda (f) (lambda (x) (f (f x)))))

(define tres
	(lambda (f) (lambda (x) (f (f (f x))))))

; se f é x+1, e o inicial é zero, vira os numeros naturais
(define (mostra numero) ((numero (lambda (x) (+ x 1))) 0))

(define (tostring numero) ((numero (lambda (x) ())) "x"))

(mostra zero)
(mostra um)
(mostra dois)
(mostra tres)
(tostring tres)

; devolve uma nova funcao que, antes de fazer f(x), aplica
; n em f e pega esse resultado para aplicar em x (isso é, ((n(f))x) )
; como essa funcao vai ser a identidade do zero (lambda (x) x), vai
; fazer a cadeia de Fs se aplicarem
(define (inc n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
	

(define -um (inc zero))
(define -dois (inc -um))
(define -tres (inc -dois))
(define -quatro (inc -tres))

(mostra zero)
(mostra -um)
(mostra -dois)
(mostra -tres)

; soma, mais dificl que a multiplicacao
; aplica n1 vezes f em x, e depois, nesse resultado, aplica n2 vezes f
; da para eliminar o x como na mult?
(define (soma n1 n2)
	(lambda (f) (lambda (x) ((n2 f)   ((n1 f) x) ) )))

(newline)
(mostra (soma -dois -um))
(mostra (soma -um -dois))
(mostra (soma -dois -dois))
(mostra (soma -dois -tres))
(mostra (soma -tres -dois))
(mostra (soma -dois -quatro))
(mostra (soma -tres zero)) ; errado!

; facil! n1(n2(f)) aplicado em x
;(define (mult n1 n2)
	;(lambda (f) (lambda (x) ((n2 (n1 f)) x))))
	
; reduzindo depois de ler o paper:
(define (mult n1 n2)
	(lambda (f) (n2 (n1 f))))

(newline)
(mostra (mult -dois -um))
(mostra (mult -um -dois))
(mostra (mult -dois -dois))
(mostra (mult -dois -tres))
(mostra (mult -tres -dois))
(mostra (mult -tres zero))


; mais um sem querer!! preciso entender esse
(define (exp n1 n2)
	(lambda (f) (lambda (x) (((n2 n1) f) x)))  )

(newline)
(mostra (exp -dois -um))
(mostra (exp -um -dois))
(mostra (exp -dois -dois))
(mostra (exp -dois -tres))
(mostra (exp -tres -dois))
(mostra (exp -dois -quatro))
(mostra (exp -tres zero)) 
