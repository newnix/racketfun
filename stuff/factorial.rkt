#lang racket
(provide factorial)
(define (factorial int)
	(if (> int 1)
			(* (factorial (- int 1)) int)
			(* int 1)))
