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

;; Could probably use keywords to make these both a single function
(define (lprint lst)
	(define (lp lst)
		(cond
			[(empty? lst) lst]
			[else
				(if (string? (first lst))
						(printf "~a" (first lst))
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
(printf "A functions arity is the number of arguments that it accepts.\n\n")

;; 4.3.2 Keyword Arguments
(printf "4.3.2: Keyword Arguments\n")
(printf "Some functions accept keyword arguments in addition to positional arguments. For\n")
(printf "that case, an arg can be an arg-keyword arg-expr sequence instead of just arg-expr:\n")
(printf "\t(go \"super.rkt\" #:mode \'fast)\n\n")
(printf "Calls the function bound to `go` with \"super.rkt\" as a positional argument and \n")
(printf "\'fast as an argument associated with the \"#:mode\" keyword. A keyword is implicitly\n")
(printf "paired with the expression that follows it.\n\n")
(printf "Since a keyword itself is not an expression, the following is a syntax error:\n\t")
(printf "(go \"super.rkt\" #:mode #:fast)\n\n")
(printf "The \"#:mode\" keyword must be followed by an expression to produce an argument value,\n")
(printf "and \"#:fast\" is not an expression, it is another keyword.\n\n")
(printf "The order of keyword args determines the order in which arg-exprs are evaluated, but a\n")
(printf "function accepts keyword arguments independant of their position in the argument list.\n")
(printf "Thus, the first example is equivalent to:\n\t(go #:mode \' \"super.rkt\")\n\n")

;; 4.3.3 The `apply` Function
(printf "4.3.3: The `apply` Function\n")
(printf "The syntax for function calls suppors any number of arguments, but a specific call always\n")
(printf "specifies a fixed number of arguments. As a result, a function that takes a list of arguments\n")
(printf "cannot directly apply a functon like `+` to all the items in a list:\n\t")
(printf "(define (avg lst) ; Won't work\n\t  (/ (+ lst) (length lst)))\n\n")
(printf "The above results in a contract violation as the \"+\" function cannot operate on all the values of the list\n")
(printf "without some other operation to only provide compatible data from the list (i.e. given a list '(1 2 3 4)\n")
(printf "iterate over the values such that you're only ever comparing the numeric values in the list)")
(printf "\n\t(define (avg lst) ; won't always work...\n\t")
(printf "  (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))\n\t    (length lst)))\n\n")
(printf "This solution has an obvious flaw in that it can only fit at most 3 values in the argument list.\n")
(printf "\nThe `apply` function allows us a way around this restriction, it takes a function and a list\n")
(printf "argument, then applies the function to the values in the list:\n")
(lprint
	'("\t(define (avg lst)\n\t  (/ (apply + lst) (length lst)))\n\n"))
(lprint
	'("As a convenience, the `apply` function accepts additional arguments between the function\n"
		"and the list. The additional arguments are effectively `cons`'d onto the argument list:\n"
		"\t(define (anti-sum lst)\n\t  (apply - 0 lst))\n\n"
		"\t(anti-sum '(1 2 3)) -> -6\n"))
(lprint
	'("The `apply` function accepts keyword arguments as well and it passes them along to the called function:\n\t"
		"(apply go #:mode 'fast '(\"super.rkt\"))\n\t(appy go '(\"super.rkt\") #:mode 'fast)\n\n"
		"Keywords that are included in `apply`'s argument list do not count as keyword arguments for the\n"
		"called function; instead all arguments in the list are trteated as positional arguments.\n"
		"To pass a list of keyword arguments to a function, use the `keyword-appy` function which accepts\n"
		"a function to apply and three lists. The fist two lists are in parallel, where the first list\n"
		"contains keywords (as sorted by `keyword<?`), and the second list containes the corresponding argument\n"
		"for each keyword. The third list is the positional function arguments, same as with `apply`.\n\n"))

;; 4.4 Functions (Procedures): lambda
(lprint
	'("4.4: Functions: lamda\n"
		"A lambda expression creates a function. In the simplest case, a lambda expression has the form:\n\t"
		"(lambda (arg-id ...)\n\t  body ...+)\n\n"))
(lprint
	'("A lambda form with n `arg-ids` accepts n arguments:\n"
		"\t((lambda (x) x) 1) -> 1\n\n"
		"\t((lambda (x y) (+ x y)) 1 2) -> 3\n\n"
		"\t((lambda (x y) (+ x y)) 1) -> ERROR: Arity mismatch\n\n"))
(lprint
	'("A lambda expression can also have the form:\n"
		"(lambda rest-id\n  body ...+)\n\n"
		"That is, a lambda expression can have a single rest-id that is not surreounded by parentheses.\n"
		"The resulting function accepts any number of arguments and the arguments are put into a\n"
		"list bound to rest-id.\n"
		"For example:\n\t((lambda x x) 1 2 3) -> '(1 2 3)\n\n\t"
		"((lambda x x)) -> '()\n\n\t"
		"((lambda x (car x)) 1 2 3) -> 1\n\n"))
(lprint
	'("Functionsn with a rest-id often use `apply` to call another function that accepts any number of arguments.\n"
		"Ex:\n\t(define max-mag\n\t  (lambda nums\n\t    (apply max (map magnitude nums))))\n\n"
		"\t(max 1 -2 0) -> 1\n\n\t(max-mag 1 -2 0) -> 2\n\n"))
(lprint
	'("The lambda form also supports required arguments combined with a rest-id:\n\t"
		"(lambda (arg-id ..+ . rest-id)\n\t  body ...+)\n\n"
		"The result of this form is a function that requires at lesat as many arguments as arg-ids.\n"
		"For example:\n\t(define max-mag\n\t  (lambda (num . nums)\n\t    (apply max (map magnitude (cons num nums))))\n\n"
		"\t(max-mag 1 -2 0) -> 2\n\t(max-mag) -> ERROR: Arity mismatch\n\n"
		"The last invokation fails because at least one argument is expected by the function signature.\n"
		"A rest-id variable is sometimes called a \"rest argument\" because it accepts the rest of the function args.\n"))

;; 4.4.2 Declaring Optional Arguments
(lprint
	'("4.4.2: Declaring Optional Arguments:\n"
		"Instead of just an identifier, an argument (other than a rest argument) in a lambda form\n"
		"can be specified with an identifier and default value:\n\t"
		"(lambda gen-formals\n\t  body ...+)\n\n\t"
		"gen-formals = (arg ...)\n\t"
		"            | rest-id\n\t"
		"            | (arg ...+ . rest-id)\n\n\t"
		"        arg = arg-id\n\t"
		"            | [arg-id default-expr]\n\n"
		"An argument of the form [arg-id default-expr] is optional. When the argument is not supplied by the caller\n"
		"the value is taken from `default-expr`. The default-expr can refer to any preceeding arg-id, and\n"
		"every following arg-id must have a default as well.\nFor example:\n\t"
		"(define greet\n\t  (lambda (given [surname \"Smith\"])\n\t    (string-append \"Hello, \" given \" \" surname)))\n\n"
		"\t(greet \"John\") -> \"Hello, John Smith\"\n\t"
		"(greet \"John\" \"Doe\") -> \"Hello, John Doe\n\n"
		"\t(define greet\n\t  (lambda (given [surname (if (equal? given \"John\")\n"
		"\t\t\t\t\"Doe\"\n\t\t\t\t\"Smith\")])\n\t  "
		"(string-append \"Hello, \" given \" \" surname)))\n\n"
		"\t(greet \"John\") - > \"Hello, John Doe\"\n\t"
		"(greet \"Adam\") -> \"Hello, Adam Smith\"\n\n"))

;; 4.4.3 Declaring Keyword Arguments
(lprint
	'("4.4.3 Declaring Keyword Arguments:\n"
		"A lambda form can declare an argument to be passed by keyword, rather than by position.\n"
		"Keyword arguments can be mixed with positional arguments, and default-value expressions can be used with both:\n"
		"\t(lambda gen-formals\n\t  body ...+)\n\n"
		"\tgen-formals = (arg ...)\n"
		"\t            | rest-id\n"
		"\t            | (arg ...+ . rest-id)\n\n"
		"\t        arg = arg-id\n"
		"\t            | [arg-id default-expr]\n"
		"\t            | arg-keyword arg-id\n"
		"\t            | arg-keyword [arg-id default-expr]\n\n"
		"An argument specified as `arg-keyword arg-id` is supplied by an application use thing same `arg-keyword`.\n"
		"The position of the keyword-identifier pair in the argument list does not matter for matching with\n"
		"arguments in an application, because it will be matched to an argument value by keyword rather than position.\n"
		"See:\n\t"
		"(define greet\n\t  (lambda (given #:last surname)\n\t"
		"    (string-appnd \"Hello, \" given \" \" surname)))\n\n"
		"\t(greet \"John\" #:last \"Smith\") -> \"Hello, John Smith\"\n"
		"\t(greet #:last \"Doe\" \"John\") -> \"Hello, John Doe\"\n"))
(define greet
	(lambda (given #:last surname)
		(string-append "Hello, " given " " surname)))
(greet "John" #:last "Doe")
(define greet2
	(lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
		(string-append hi ", " given " " surname)))
(lprint
	'("\nAn arg-keyword [arg-id default-expr] argument specifier as keyword-based argument with a default value.\n"
		"e.g:\n\t(define greet\n\t  (lambda (#:hi [hi \"Hello\"] given #:list [surname \"Smith\"])\n\t"
		"    (string-append hi \", \" given \" \" surname)))\n\t(greet \"John\") -> \"Hello, John Smith\"\n"
		"\t(greet \"Karl\" #:last \"Marx\") -> \"Hello, Karl Marx\"\n"
		"\t(greet \"John\" #:hi \"Howdy\") -> \"Howdy, John Smith\"\n\n"
		"The lambda form does not directly support the creation of a function that accepts \"rest\"\n"
		"keywords. To construct a function that accepts all keyword arguments, use\n"
		"`make-keyword-procedure`. The function supplied to `make-keyword-procedure`\n"
		"recieves keyword arguments through parallel lists in the first two positional arguments,\n"
		"and then all remaining positional arguments are passed as the remaining arguments.\nE.g:\n\t"
		"(define (trace-wrap f)\n\t  (make-keyword-procedure\n\t    (lambda (kws kw-args . rest)\n\t"
		"      (printf \"Called with ~s ~s ~s\\n\" kws kw-args rest)\n\t"
		"      (keyword-apply f kws kw-args rest))))\n\n"
		"\t ((trace-wrap greet) \"John\" #:hi \"Howdy\"):\n"))
(define (trace-wrap f)
	(make-keyword-procedure
		(lambda (kws kw-args . rest)
			(printf "Called with ~s ~s ~s\n" kws kw-args rest)
			(keyword-apply f kws kw-args rest))))
((trace-wrap greet2) "John" #:hi "Howdy")
