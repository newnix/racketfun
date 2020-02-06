#lang racket
(provide getpath)
(provide pathbins)

(define env (current-environment-variables))
(define path (getpath))
(define bins (pathlsts))

; This returns the value of your $PATH as a string
(define (getpath-str env)
		(if (environment-variables? env)
				(bytes->string/utf-8 (environment-variables-ref env #"PATH"))
				'()))

; This returns the value of $PATH as a list of strings
(define (getpath)
	(string-split (getpath-str env) ":"))

; Build a list of hashes for the directories in $PATH
(define (pathbins)
	(map hash (getpath) (pathlsts)))

; Get a list of directory contents
(define (pathlsts)
	(map directory-list (getpath)))

; Generate a hash if the lists aren't empty
(define (hash-if-exists pathlst binslst)
	(lambda () 
		(if (empty? pathlst)
				'()
				(hash (first pathlst) (first binslst)))))
