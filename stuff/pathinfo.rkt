#lang racket
(provide getpath)

(define env (current-environment-variables))

; This returns the value of your $PATH as a string
(define (getpath-str env)
		(if (environment-variables? env)
				(bytes->string/utf-8 (environment-variables-ref env #"PATH"))
				'()))

; This returns the value of $PATH as a list of strings
(define (getpath)
	(string-split (getpath-str env) ":"))

; Now, let's build a hash table of our available binares
(define (pathbins)
	(lambda () 
		(map hash (getpath) (pathlsts))))

; Get a list of directory contents
(define (pathlsts)
	(map directory-list (getpath)))
