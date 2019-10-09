;; Begin Chapter 4: Expressions and Definitions
#lang racket
(require "whatis.rkt")

;; Begin 4.1
(printf "Starting Chapter 4.1: Notation\n")
(printf "\nA note on nomenclature:\n\tParentheses: ()\n\tBrackets: []\n\tBraces: {}\n\tChevrons: <>\n")
(printf "\nThese names have existed before computing, and can greatly reduce confusion in communication\n\n")
(printf "The new grammar for describing Racket expressions is as follows:\n")
(printf "\t(something [id ...+] an-expr ...)\n")
(printf "The \"an-expr\" portion is called a meta-variable, which has its own naming convention:\n")
;; Work on building a procedure that will print all the strings in the list
;; using the format of (printf "\t-~a\n" str)
;; I have no doubt that this could be written better, but I'm glad to have it working at this time
(define (plst lst)
	(define (lp lst)
		(cond
			[(empty? lst) lst]
			[else
				(if (string? (first lst))
						(printf "\t-~a\n" (first lst))
						(printf "\n"))
				(if (not (empty? (rest lst)))
						(lp (rest lst))
						(printf "\n"))
				]))
	(if (not (empty? lst))
			(lp lst)
			(printf "Given empty list!\n")))
(plst '("id stands in for an identifier such as \'x\' or \'spiderman\'" 
				"keyword stands in for a keyword such as \'#:tag\'"
				"expr stands in for any sub form, and is evaluated as an expression"
				"body stands in for any sub-form; it will be parsed as a local definition or an expression." 
				"body can parse as a definition only if it is not proceeded by an expression and the last -body must be an expression."))

(printf "A term in brackets indicates a parenthized sequence of forms, where brackets are conventionally used.\n")
(printf "Brackets do not indicate optional portions of the syntax!\n")
(printf "A \'...\' indicates zero or more repetitions of the preceeding form, and \'...+\' indicates one or more\n")
(printf "of the preceeding datum. Otherwise, non-italicized identifiers stand for themselves.\n")
(printf "So based on the above grammar, we have a few confarmant uses of (something [id ...+] an-expr ...):\n")
(plst '(" (something [x])"
				 " (something [x] (+ 1 2))"
				 " (something [x my-favorite-martian x] (+ 1 2) #f)"
			 ))
(printf "Some syntactic-form specifications refer to meta variables that are not implicitly\n")
(printf "defined nor previously defined. Such meta variables are defined after the main form,\n")
(printf "using a BNF-like format for alternatives.\ne.g.:\n\t")
(printf "(something-else [thing ..+] an-expr ...)\n\tthing = thing-id\n\t      | thing-keyword\n\n")
(printf "This states that, within (something-else ...), /thing/ is either an identifier or a keyword\n")

;; 4.2 Identifiers and Binding:
;; The context of an expression determines the meaning of identifiers that appear
;; in the expression. 
(printf "\nThe context of an expression determines the meaning of identifiers that appear in that expression.\n")
(printf "In particular, starting a module with the language racket as in: #lang racket\n")
(printf "means that, within the module, the identifiers described in this guide start with the meaning described here:")
(printf "\ncons refers to a function that creates a pair, car refers to the function that\n")
(printf "extracts the first element of a pair, and so on.\n")
(printf "Forms like define, lambda, and let associate a meaning with one or more identifiers;\n")
(printf "that is, they bind identifiers. The part of the program for which the binding applies is the scope of the binding\n")
(printf "The set of bindings in effect for a given expression is the expression's environment.\nFor example:\n\n")
(printf "#lang racket\n\n(define f1\n  (lambda (x)\n    (let ([y 5])\n      (+ x y))))\n\n(f 10)\n\n")
(printf "Running the above example:\n")
(define f1
	(lambda (x)
		(let ([y 5])
			(+ x y))))
(printf "\t(f1 10) -> ~s\n\n" (f1 10))
(printf "the define is a binding of 'f1', the lambda has a binding for 'x', and the 'let'\n")
(printf "has a binding for 'y'. The scope of the binding for 'f1' is the entire module;\n")
(printf "the scope of the 'x' binding is \"(let ([y 5]) (+ x y))\"; and the scope of the 'y'\n")
(printf "binding is just \"(+ x y)\". The environment of \"(+ x y)\" includes binding for\n")
(printf "'y', 'x', and 'f', as well as everything in racket.\n\n")
