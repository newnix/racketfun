#lang racket

; This is simply copying Racket documentation on simple
; definitions and expressions, being used as a means to help solidify 
; the learning process of working with Racket

; This is simple definiton, it bind the given ID to the result of an
; expression. 

(define pie 3) ; So now calling "pie" prints the number 3

; Now call the macro
pie

; This is slightly more complicated, binding the first ID to 
; a function (also referred to as a /procedure/), in this case
; the expressions are the body of the function, which returns 
; the value of the last expression

(define (sub1 str)       ; Defines "sub1" as a function with one argument
	(substring str 0 pie))

; Now we call the function
(sub1 "ayy, lmao")

; This is an example of a somewhat more complicated function definition
; a function can include multiple expressions in its body, but only the 
; value of the last expression is actually returned when called. 
; The other expressions are only evaluated for useful side-effects, 
; such as printing messages to the console

(define (bake flavor)
	(printf "Preheating the oven...\n")
	(printf "Prepping ~s pie ingredients...\n" flavor)
	(string-append flavor " pie"))

; Now call the function to demonstrate its use
(bake "apple")

; Since Racket is functional, it's generally desirable to avoid 
; side-effects. As an example, this "nobake" function will not 
; show the same behaviour as "bake", so any argument provided 
; is essentially ignored as "string-append" is not enclosed in
; parenthesis, making it just another expression in the function definition

(define (nobake flavor)
	string-append flavor "jello")

; Calling it will only print out "jello"

(nobake "blue")

; Note on whitespace: IT DOESN'T FUCKING MATTER FOR COMPILATION!
; However, by convention, the general style used above is considered "correct"

; In Racket, identifiers can be pretty much anything except the following:
; ( ) [ ] { } " , ' ` ; # | \
; or number constants, aside from these, you can use any string of text as identifiers

; Continuing on with Racket usage,
; function calls are also referred to as "procedure applications" 
; some examples of pre-defined functions follow
(printf "Using (string-append \"Fuck\" \" Python\" \" BS\")\n")
(string-append "Fuck" " Python" " BS")

(printf "Using (substring \"exile.digital\" 0 5)\n")
(substring "exile.digital" 0 5)

(printf "Using (string? \"Hello there!\")\n")
(string? "Hello there!")

(printf "Using (sqrt 64)\n")
(sqrt 64)
(printf "And now the negative!\n")
(sqrt -64)

(printf "Now for addition: (+ 8 8)\n")
(+ 8 8)
(printf "And subtraction: (- 16 8)\n")
(- 16 8)

(printf "Value comparison: (< 64 0)\n")
(< 64 0)
(printf "Include equality: (>= 64 0)\n")
(>= 64 0)

(printf "Test for numeric data: (number? \"nine\")\n")
(number? "nine")
(printf "And one that should be true: (number? 8)\n")
(number? 8)

(printf "Equality test with anything! (equal? 6 \"six\")\n")
(equal? 6 "six")

; Onward to conditional statements!
; Apparently a basic `if` statement should look like the following:
;		( if <expr> <expr> <expr>)
; the first expression is always evaluated for truthiness, if 
; it is true (or more accurately non-#f), the second expression is evaluated
; for the result, otherwise the third expression is evaluated
; so it could be viewed as ( if <truthy> <then-expr> <else-expr> )
; some examples follow:

(printf "Running a simple if statement\n")
(if (> 8 16)
		"bigger"
		"smaller")

(printf "Now a string comparison conditional\n")
(define (reply s)
	(if (equal? "hello" (substring s 0 5))
			"Hello, there!"
			"Wha?"))
(printf "Defined function \"(reply s)\"\nNow calling with argument \"howdy\"\n")
(reply "howdy")
(printf "Now calling as (reply \"hello\")\n")
(reply "hello")

; Now to display use of a nested if statement
(printf "Creating nested if statement for function \"reply2\"\n")
(define (reply2 s)
	(if (if (string? s)
					(equal? "hello" (substring s 0 5))
					#f)
			"Why, hello there!"
			"What's that?"))

; Now call the function
(printf "Calling the new \"reply2\" as (relpy2 2), (reply2 \"howdy\"), and (reply2 \"hello\")\n")
(reply2 2)
(reply2 "howdy")
(reply2 "hello")

; Third iteration, now using a short circuit in the "and" operation
(define (reply3 s)
	(if (and (string? s)
					 (>= (string-length s) 5)
					 (equal? "hello" (substring s 0 5)))
			"Hi!"
			"Eh?"))
(printf "A better version, called \"reply3\" has been defined, calling now with the same arguments as before\n")
(reply3 42)
(reply3 "howdy")
(reply3 "hello")

; A similar pattern is a series of nested conditionals, often including a 
; sequence of tests to determine how to proceed, like this function:
(printf "\nNow defining a function, (reply-more s), to use tests in the conditionals\n")
(define (reply-more s)
	(if (and (<= 5 (string-length s))
					 (equal? "hello" (substring s 0 5)))
			(printf "Why, hello there!\n")
			(if (or (equal? "bye" (substring s 0 3))
							(if (<= 7 (string-length s)) 
									(equal? "goodbye" (substring s 0 7))
									#f))
					(printf "Hasta luego!\n")
					(if (equal? "?" (substring s (- (string-length s) 1)))
							(printf "No idea\n")
							(printf "Eh?")))))

; thanks to some liberties in this exercise, I had to add some additional bounds-checking,
; fortunately this was pretty simple, just add another condition to the execution path ensuring that 
; the string is long enough to satisfy the substring comparisons at a given index
(printf "Now trying to call it with arguments \"hello\", \"what?\", and \"bye\"\n")

(reply-more "hello")
(reply-more "what?")
(reply-more "bye")

; The above tests can apparently be done in a shorthand form known as a "cond" expression,
; a nearly identical function using "cond" will be implemented here:
(printf "\nNow a similar function, (reply-cond s) will be implemented using \"cond\"\n")
(define (reply-cond s)
	(cond
		[(and (<= 5 (string-length s))
					(equal? "hello" (substring s 0 5)))
		 (printf "Hello, meatbag!\n")]
		[(and (<= 7 (string-length s))
					(equal? "goodbye" (substring s 0 7)))
		 (printf "So long, greasy fur puppet\n")]
		[(and (<= 3 (string-length s))
					(equal? "bye" (substring s 0 3)))
		 (printf "And good riddance!\n")]
		[(equal? "?" (substring s (- (string-length s) 1)))
		 (printf "Are you on drugs?\n")]
		[else (printf "ERROR\n")]))
; the use of brackets is purely by convention, Racket sees brackets as interchangeable with parenthesis,
; the only caveat being that an expression must have each symbol balanced appropriately. 
; Racket convention would be to use parenthesis under normal situations, and brackets 
; to demand more attention to the function/expresssion
(printf "Now calling (reply-cond s) with the same arguments as (reply-more s) previously\n")
(reply-cond "hello")
(reply-cond "what?")
(reply-cond "bye")

; Revisiting function calls (2.2.6)
; Apparently, the prior explanation of function calls in Racket was oversimplified.
; The actual function syntax can be expressed as "(<expr> <expr>*)" where the first expression
; is often an identifier, such as "string-append", but it can be anything that evaluates to a function, 
; like this conditional expression:
(define (double v)
	;; This is a pretty awesome capability
	((if (string? v) string-append +) v v))

(printf "Calling (double v) with arguments kek and 16\n")
(double "kek")
(double 16)

; Syntactically, the first expression in a function could be a number, but that causes an error
; due to numbers not being functions

; 2.2.7: Anonymous functions with lambda
; Since it'd be tedious to program if all numbers had to be named, and the same is true of functions.
; For example, having a function "twice" that takes a function and an argument can be used conveniently
; if you already have a name for the function, such as "sqrt"
(define (twice f v) 
	(f (f v)))
(printf "Calling (twice sqrt 16)\n")
(twice sqrt 16)

; We can also define a function that does not yet exist, and pass it as an argument to "twice"
(define (louder s)
	(string-append s "!"))
(printf "Calling (twice louder \"ERROR\")\n")
(twice louder "ERROR")

; Now, if we only use the function inside of another function, it may be more beneficial to
; use an anonymous function, or "lambda function". We can use "lambda" which produces a function 
; during its evaluation. For example:
(printf "Calling (twice (lambda (s) \n\t(string-append s \"!\"))\n\t\"HELLO\")\n")
(twice (lambda (s)
				 (string-append s "!"))
			 "HELLO")

; Another use of "lambda" is as the result of a function that generates functions,
; such os the following (make-add-suffix s2)
(define (make-add-suffix s2)
	(lambda (s) (string-append s s2)))
(printf "Calling (twice (make-add-suffix \"!\") \"Hello\")\n")
(twice (make-add-suffix "!") "Hello")
(printf "Calling (twice (make-add-suffix \"?!\") \"Hello\")\n")
(twice (make-add-suffix "?!") "Hello")
(printf "Calling (twice (make-add-suffix \"...\") \"Hello\")\n")
(twice (make-add-suffix "...") "Hello")

; Now some more examples of how we can use the lambda results, 
; and how the lexical scoping of Racket "remembers" the right variables/arguments
(define louder2 (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))
(printf "Calling (twice less-sure \"Really\")\n")
(twice less-sure "Really")
(printf "Calling (twice louder2 \"Really\")\n")
(twice louder2 "Really")

; 2.2.8: Local binding with define, let, and let*
; In the body of a function, definitions can appear before the body expressions:
;		(define (<id> <id>*) <definition> <expr)
;		(lambda (<id>*) <definition> <expr>)
; Definitions at the start of a function body are local to the function body.
; This is exemplified in the following function (converse s)
(define (converse s)
	(define (starts? s2) ; Function locally bound to (converse s)
		(define len2 (string-length s2)) ; Function locally bound to starts?
		(and (>= (string-length s) len2)
				 (equal? s2 (substring s 0 len2))))
	(cond
		[(starts? "hello") "Hi!"]
		[(starts? "goodbye") "Bye!"]
		[else "huh?"]))

; Now call the "converse" function
(printf "Calling (converse \"hello\")\n")
(converse "hello there!")
(printf "Calling (converse \"que\")\n")
(converse "que")
(printf "Calling (converse \"goodbye\")\n")
(converse "goodbye")
; If we attempted to use the "starts?" function itself, we'd have a failure, 
; as there's no such function defined at the global/top scope

; An alternative means of creating local bindings is via "let",
; this has the advantage of being usable in any expression position.
; "let" also binds many identifiers at once, rather than requiring 
; separate "define" statements per each identifier. The general usage
; can be shown as such:
;		(let ({[<id> <expr]}* ) <expr>)
; Each binding clause is an identifier and an expression surrounded by 
; brackets, and the expressions after the clauses are the body of the "let".
; In each clause, the identifier is bound to the result of the 
; expression for use in the body. For example:
(let ([x (random 4)]
			[o (random 4)])
	(cond
		[(> x o) (printf "X wins!\n")]
		[(> o x) (printf "O wins!\n")]
		[else (printf "Cat's game!\n")]))

; The bindings of a "let" form are only available in the body of the "let", 
; so binding clauses cannot refer to each other. However, the "let*" form
; allows later clauses to use earlier bindings:
(let* ([x (random 100)]
			 [o (random 100)]
			 [diff (number->string (abs (- x o)))])
	(cond
		[(> x o) (printf "X wins by ~a!\n" diff)]
		[(> o x) (printf "O wins by ~a!\n" diff)]
		[else (printf "Cat's game!\n")]))

; Begin 2.3: Lists, Iteration, and Recursion
; Since Racket's a LISP dialect, lists are still a prominent feature of the language.
; The list function takes any number of values and returns a list containing the values:
(printf "Running some examples of the `list` function:\n")
(list "red" "green" "blue")
(list 1 2 3 4 5 6 7 8 9 0)
; A list literal is displayed as '("red" "green" "blue")
(printf "Running (length (list \"R\" \"G\" \"B\"))\n")
(length (list "R" "G" "B"))
(define (rgb)
	(list "R" "G" "B"))
(define (cmyk)
	(list "C" "M" "Y" "K"))
(printf "Defined (rgb) as (list \"R\" \"G\" \"B\")\n")
(printf "Running: (length (rgb))\n")
(length (rgb))
(printf "Now to get the value at position 1\n")
(list-ref (rgb) 1)
(printf "Appending CMYK to the list\n")
(append (rgb) (cmyk))
(define (colorschemes)
	(append (rgb) (cmyk)))
(printf "Defined (colorschemes) as the appended list\n")
(colorschemes)
; In addition to the ability to calculate length, append, and reference by index, 
; there's the ability to reverse a list and check for element membership
(printf "Printing the colorschemes in reverse\n")
(reverse (colorschemes))
(printf "Checking for membership of \"Z\" in (colorschemes)\n")
(member "Z" (colorschemes))
; 2.31
; In addition to these simple functions, there are functions to iterate over the elements of a list,
; these are roughly analagous to for loops in other languages
; The first of these is (map), which uses per-element results to populate a new list:
(printf "Running: (map sqrt (list 1 4 9 16))\n")
(map sqrt (list 1 4 9 16))
(printf "Attempting to use (map) on a lamba function as well\n")
(map (lambda (i)
			 (string-append i "!"))
		 (colorschemes))
; There's also the ability to use (andmap) and (ormap) to return results 
; based on logical AND or OR results with the input list(s)
; map functions can accept multiple lists, but they must all have the same number of elements,
; and the given function must accept one argument for each list like so:
(map (lambda (s n)
			 (substring s 0 n))
		 (map (lambda (i) (string-append i "!")) (cmyk))
		 ; It's nice that syntax highlighting will change the color of parens used to enclose a list
		 ; vs other s-exprs
		 '(1 2 1 2))

(printf "Running: (andmap string? (colorschemes)) (expecting #t)\n")
(andmap string? (colorschemes))
(printf "Now creating a literal list: '(\"R\" 255 \"G\" 128 \"B\" 64) as rgbval\n")
(define rgbval '("R" 255 "G" 128 "B" 64))
rgbval
(printf "Running: (ormap number? rgbval)\n")
(ormap number? rgbval)
; The filter function keeps elemnts for which the body result is true and 
; discards the elements for which it's false
(printf "Filtering out the numbers from ~s\n" rgbval)
(filter string? rgbval)
(printf "Filtering out the strings from ~s\n" rgbval)
(filter number? rgbval)

; A less popular function (foldl) generalizes some iteration functions.
; It uses the per-element function to both process an element and combine it with the "current"
; value, so the per-element function takes an extra first argument, and a starting "current"
; value must be provided before the lists:
(printf "Demonstrating foldl with other list functions on ~s\n" rgbval)
(foldl (lambda (elem v)
				 (+ v (* elem elem)))
			 0
			 (filter number? (reverse rgbval)))
; 2.3.2
; Although map and other iteration functions are predefined, they're not primitive in any
; interesting sense, they can be written from scratch using some primitives covered here:
(define numbers '(0 1 2 3 4 5 6 7 8 9))
(printf "Creating a new list, \"numbers\": ~s\n" numbers)
(printf "First element: ~s\n" (first numbers))
(printf "Rest of the elemonts: ~s\n" (rest numbers))

; To create a new node for linked lists, that is to add a node to the front of the 
; list, we use the cons function, which is short for "construct".
; To get an empty list to start with, use the empty constant:
(printf "Creating new ephemeral list with empty and cons\n")
empty
(printf "Since empty is a constant, we can also use it to generate a single node list\n(cons \"head\" empty)\n")
(cons "head" empty)
(printf "And now add tail to the front\n")
(cons "tail" (cons "head" empty))
(printf "Testing for truth with (empty? empty)\n")
(empty? empty)
(printf "Testing both cons? and empty? with ~s\n" numbers)
(cons? numbers)
(empty? numbers)
; Additionally, cons? will detect a non-empty list and empty? will detect an empty list as the truth value
; obviously, using this, we can simply use either to get both states tested and just switch on true/false
(printf "Using these tools, we can implement our own O(n) length calculation!\n")
(define (on-len lst)
	(cond
		[(empty? lst) 0]
		[else (+ 1(on-len (rest lst)))]))
(printf "Using (on-len ~s)\n" numbers)
(on-len numbers)

; We can use the same concept to build our own version of map
(printf "Now constructing on-map\n")
(define (on-map f lst)
	(cond
		[(empty? lst) empty] ; Return empty list if given empty list
		[else (cons (f (first lst)) ; Else copy the 1st value of lst into f and recurse with (rest lst)
								(on-map f (rest lst)))]))
(printf "Now running (on-map string-downcase ~s)\n" (colorschemes))
(on-map string-downcase (colorschemes))

;; 2.3.3
;; Tail recursion:
; Both the on-map and on-list functions run in O(n) space for a list of length n. 
; this is pretty easy to visualize by seeing how on-list would have to evaluate for 
; (on-len (rgb)):
; = (+ 1 (on-len (list "b" "g")))
; = (+ 1 (+ 1 (on-len (list "g"))))
; = (+ 1 (+ 1 (+ 1 (on-len (list)))))
; = (+ 1 (+ 1 (+ 1 0)))
; = (+ 1 (+ 1 1))
; = (+ 1 2)
; = 3
; This pattern continues for as long as the list han entries
; This can be avoided by counting as we encounter entries like so:
(printf "Building c-len definition...\n")
(define (c-len lst)
	; Internal function iter:
	(define (iter lst len)
		(cond
			[(empty? lst) len]
			[else (iter (rest lst) (+ len 1))]))
	; body of c-len calls iter:
	(iter lst 0))
(printf "Calling (c-len ~s)\n" numbers)
(c-len numbers)
; The evaluation for c-len is done in constant space, so if given say, (c-len (rgb))
; the execution would look like so:
; (c-len (rgb))
; (c-len '("R" "G" "B"))
; = (iter (list "R" "G" "B") 0)
; = (iter (list "G" "B") 1)
; = (iter (list "B") 2)
; = (iter (list) 3)
; = 3
; It still takes O(n) time, but not O(n) space
; This kind of optimization is referred to as "tail call optimization" 
; as an expression in the tail position with respect to another expression
; will not take extra computational space over the other expression.

; A similar, though often less worthwhile optimization can be made to on-map
; as demonstrated below, but this will have the resulting list returned in reverse order
(define (rmap f lst) 
	(define (iter lst br)
		(cond
			[(empty? lst) (reverse br)]
			[else (iter (rest lst)
									(cons (f (first lst))
												br))]))
	(iter lst empty))
;; The above function is essentially the same as:
(define (rmap2 f lst)
	(for/list ([i lst])
					 (f i)))

(printf "Now running (rmap string-downcase ~s\n" (colorschemes))
(rmap string-downcase (colorschemes))
(printf "Now running (rmap2 string-downcase ~s\n" (colorschemes))
(rmap2 string-downcase (colorschemes))

;; Iteration in Racket is a special case of recursion, though in other languages
;; iteration can be preferable to reduce your call stack
;; In Racket, tail call recursion is essentially the same as a loop with an exit condition

;; 2.3.4:
;; An example of how to remove consecutive repeated values from a list using recursion:
(define (rdups l) 
	(cond
		[(empty? l) empty] ; If list is empty, return empty list
		[(empty? (rest l)) l] ; Return the list if there's only one node
		[else
			(let ([i (first l)])
				(if (equal? i (first (rest l)))
						(rdups (rest l))
						(cons i (rdups (rest l)))))]))
;; Also define a list of duplicate values
(define dupstr '(0 0 0 1 1 1 2 3 3 4 4 5 5 5 5 5))
(printf "Removing duplicates from ~s\n" dupstr)
(rdups dupstr)

;; 2.4
;; Pairs, Lists, and Racket Syntax
;; The cons function will actually accept any two values, not just a list as the second argument.
;; When the second argument is not empty or created by cons, a pair is created. This
;; has slightly different syntax, as the the elements are separated by a: . 
(printf "Bulding a pair with (cons \"exile\" \"digital\"): ~s\n" (cons "exile" "digital"))

;; This means that the result of cons is not always a list, the more traditional
;; name for cons? is pair? and the fist/rest functions are historically car/cdr
;; some examples of use:
(car (cons 1 2))
(cdr (cons 1 2))
(pair? empty)
(pair? (cons 1 2))
(pair? (list 1 2 3))

;; odds are that a non-list pair will be encountered only when making mistakes, 
;; but can be intentional such as in the make-hash function, which takes a list of 
;; pairs, using (car '(1 . "foo")) for the key and (cdr '(1 . "foo"))  for the value
;; printing pairs however, can have confusing results like so:
(define pair0 '(0 . ( 1 . 2)))
(define pair1 '(0 . (1 . (2 . ()))))
(printf "Displaying pairs can be confusing, so extar care should be taken when operating on pairs/lists:\n")
(printf "\t'(0 . (1 . 2)) -> ~s\n" pair0)
(printf "\t'(0 . (1 . (2 . ()))) -> ~s\n" pair1)

;; Naturally, lists of any sort can be nested
(printf "Creating a nested list:\n")
(printf "\t~s\n" (list (colorschemes) "lol" (list numbers)))
(printf "If you wrap an identifier with \"quote\" you get output that looks like an identifier: ~s\n" (quote (colorschemes)))
