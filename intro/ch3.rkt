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
