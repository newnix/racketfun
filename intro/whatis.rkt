#lang racket

(provide whatis)
;; Create a simple filter function to identify the type of a given argument
;; only really useful in the repl
(define (whatis i)
	; define an internal function for printing variables with a given type string
	(define (id v)
		(if (string? v)
				(printf "\"~a\" is a(n) ~a\n" i v)
				(printf "~s is not a string!\n" (quote v))))
	(cond
		[(integer? i) (id "integer")]
		[(boolean? i) (id "boolean")]
		[(string? i) (id "string")]
		[(identifier? i) (id "identifier")]
		[(procedure? i) (id "procedure")]
		[(vector? i) (id "vector")]
		[(byte? i) (id "byte")]
		;; If the argument is a list, it is also a pair
		[(list? i) (id "list")]
		[(pair? i) (id "pair")]
		))
