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
