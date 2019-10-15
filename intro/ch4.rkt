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
(printf "A module-level define can bind ony identifiers that aren't already defined or required\n")
(printf "into the module. A local define or other binding forms, however, can give a new local\n")
(printf "binding for an identifier that already has a binding; such a binding shadows the existing binding\n")
(printf "EX:\n\n")
;; Define the necessary function for the example printout
(define f2
	(lambda (append)
		(define cons (append "ugly" "confusing"))
		(let ([append 'this-was])
			(list append cons))))
;; Replaced the tab escape characters (\t) with two spaces to better display in output
(printf "(define f2\n  (lambda (append)\n    (define cons (append \"ugly\" \"confusing\"))\n    ")
(printf "(let ([append \'this-was])\n      (list append cons))))\n\n(f2 list) -> ~s\n\n" (f2 list))
(printf "Similarly, a module-level define can shadow a binding from the module's language. For example\n")
(printf "(define cons 1) in a racket module shadows the cons definition that is provided by racket.\n")
(printf "Intentionally shadowing a language binding is rarely a good idea, especially for widely used bindings lke cons\n")
(printf "but shadowing relieves a programmer from having to avoid every binding provided by the language.\n\n")

;; 4.3 Function Calls (Procedure Applications)
(printf "An expression of the form: (proc-expr arg-expr ...)\n")
(printf "is a function call--also known as a procedure application--when proc-expr is not an\n")
(printf "identifier that is bound as a syntax transformer (such as if or define).\n")

;; 4.3.1 Evaluation Order and Arity
(printf "A function call is evaluated by first evaluating the proc-expr and all the arg-exprs in order\n")
(printf "(left to right). Then, if proc-expr produces a function that accepts as many arguments as \n")
(printf "supplied arg-exprs, the function is called. Otherwise, an exception is raised.\n")
(printf "EX:\n\n")
(printf "\t(cons 1 null) -> ~s\n" (cons 1 null))
(printf "\t(+ 1 2 3) -> ~s\n" (+ 1 2 3))
(printf "\nHowever, the following would throw an arity mismatch:\n\t")
(printf "(cons 1 2 3)\n")
(printf "\nWhile this will throw an exception about not being a procedure:\n\t")
(printf "(1 2 3)\n\n")
(printf "Some functions, such as cons, accept a fixed number of arguments. Some, such as + or list,\n")
(printf "accept any number of arguments. Still others accept a range of argument counts;\n")
(printf "for example substring accepts either two or three arguments.\n")
(printf "A functions arity is the number of arguments that it accepts.\n")

;; 4.3.2 Keyword Arguments
(printf "\nSome functions accept keyword arguments in addition to positional arguments. For\n")
(printf "that case, an arg can be an arg-keyword arg-expr sequence instead of just arg-expr:\n")
