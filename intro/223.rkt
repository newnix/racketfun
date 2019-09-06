#lang racket

; In Racket, identifiers can be pretty much anything except the following:
; ( ) [ ] { } " , ' ` ; # | \
; or number constants, aside from these, you can use any string of text as identifiers

; Continuing on with Racket usage,
; function calls are also referred to as "procedure applications" 
; some examples of pre-defined functions follow
(printf "Using (string-append \"Fuck\" \" Python\" \" BS\"\n")
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
