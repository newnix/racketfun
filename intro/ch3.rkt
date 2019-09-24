#lang racket
;; Begin chapter 3, exploring build-in data types

;; 3.1: Booleans
(printf "One of the primitive types in Racket is booleans (true/false):\n")
(printf "True: ")
(= 2 (+ 1 1))
(printf "False: ")
(= 1 (+ 2 2))
(printf "These are most commonly used in control-flow test expressions like if, cond, and, or\n")

;; 3.2: Numbers
;; In Racket, numbers can be either exact or inexact
(printf "In Racket, exact numbers are integers, rational, or complex numbers with exact real and imaginary parts\n")
(printf "such as 5, 3/4, and 1+2i\n")
(printf "An inexact number is an IEEE floating-point representation or \ncomplex numbers with at least one part being IEEE floating point\n")
(printf "Running #e0.5 -> ~s\n" #e0.5)
(printf "Running (/ 1 2.0) -> ~s\n" (/ 1 2.0))
(printf "The rounding for inexact numbers can be represented with \"0.1\"\n")
(printf "(inexact->exact 0.1) -> ~s\n" (inexact->exact 0.1))

;; Generally, computation is fastest with integers capable of fitting into the machine's native
;; word-size. The effects on more expensive/complex computations on a given program may not
;; be worthwhile depending on the intended use case. 
(printf "Some examples on the difference between computation time with (sigma f a b)\n")
(define (sigma f a b)
	(if (= a b) ; if A == B
			0
			(+ (f a) (sigma f (+ a 1) b))))
;; May not be the best illustration running these back-to back, 
;; but there's still some difference between the two
(printf "Timing for (roud (sigma (lambda (x) (/ 1 x)) 1 2000))\n")
(time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
(printf "Timing for (round (sigma (lambda (x) (/ 1.0 x)) 1 2000))\n")
(time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))

;; the = procedure checks for numerical equality, if given both inexact and exact 
;; numbers, it'll convert both numbers to exact before comparing.
;; the eqv? and equal? functions will consider both exactness and quality
(printf "Running (= 1 1.0) -> ~s\n" (= 1 1.0))
(printf "Running (eqv? 1 1.0) -> ~s\n" (eqv? 1 1.0))

;; 3.3: Characters
;; A Racket character corresponds to a unicode scalar value,
;; this is a 21-bit value that maps to some notion of a "glyph" 
;; or a printable character or portion of a character in the Uincode standard.
;; basically, any single latin character or common Chinese character can fit this definition.
;; A printable character will typically be printed as #\${CHAR}, while unprintable ones will be #\u${VAL}
;; when using the integer->char procedure.
(printf "\nBasic int->char conversions:\n")
(printf "(integer->char 65) -> ~s\n" (integer->char 65))
(printf "(char->integer A) -> ~s\n" (char->integer #\A))
;; This is probably supposed to be lambda, but the codepoint doesn't seems right
(printf "#\\u03BB -> ~s\n" #\u03BB)

;; There are several available procedures for doing tests and transformations
;; on characters, though some will only work as expected when presented as strings
(printf "\nRunning basic character conversion functions:\n")
(printf "(char-alphabetic? #\\A) -> ~s\n" (char-alphabetic? #\A))
(printf "(char-numeric? #\\0) -> ~s\n" (char-numeric? #\0))
(printf "(char-whitespace? #\\newline) -> ~s\n" (char-whitespace? #\newline))
(printf "(char-downcase #\\A) -> ~s\n" (char-downcase #\A))

;; The char=? function compares two or more characters, and char-ci=? 
;; does the same, ignoring case. eqv? and equal? operate identically on characters;
;; char=? should be used when specifically declaring the values should be treated as characters
(printf "\nRunning some character equivalence tests:\n")
(printf "(char=? #\\a #\\A) -> ~s\n" (char=? #\a #\A))
(printf "(char-ci=? #\\a #\\A) -> ~s\n" (char-ci=? #\a #\A))
(printf "(eqv? #\\a (char-downcase #\\A)) -> ~s\n" (eqv? #\a (char-downcase #\A)))
(printf "(eqv? #\\a #\\A) -> ~s\n" (eqv? #\a #\A))
