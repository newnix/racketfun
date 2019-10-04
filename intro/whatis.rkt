#lang racket

(provide whatis)
;; Create a simple filter function to identify the type of a given argument
;; only really useful in the repl
(define (whatis i)
	; define an internal function for printing variables with a given type string
	(define (id v)
		(if (string? v)
				(printf "\"~a\" is ~a\n" i v)
				(printf "~s is not a string!\n" (quote v))))
	(cond
		[(null? i) (id "NULL")]
		[(integer? i) (id "an integer")]
		[(boolean? i) (id "a boolean")]
		[(string? i) (id "a string")]
		[(identifier? i) (id "an identifier")]
		[(procedure? i) (id "a procedure")]
		[(vector? i) (id "a vector")]
		[(byte? i) (id "a byte")]
		;; test for mutability first
		[(mpair? i) (id "a mutable pair")]
		;; If the argument is a list, it is also a pair
		[(list? i) (id "a list")]
		[(pair? i) (id "a pair")]
		[(vector? i) (id "a vector")]
		[(hash? i) (id "a hash table")]
		[(box? i) (id "a box")]
		[(void? i) (id "void")]
		;; Default to reporting unknown types
		(id "not a known type")
		))
