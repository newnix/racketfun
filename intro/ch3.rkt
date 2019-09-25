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
(printf "#\\u03BB should be a lambda, if it's not, then the font or locale isn't doing something correctly\n")
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

;; 3.4 Strinngs (Unicode)
;; A string is a fixed-length array of characters. They're printed using double quotes (")
;; where any double quote and backslash characters are escaped with backslashes.
;; There are some similarities to C escape codes such as \n and \r being linefeed and 
;; carriage return, while octal escapes are \[0-7]{1,3} and hex is \u[0-9A-F]{1,4},
;; unprintable characters are typically printed as their hex values with the \u escape sequence
(printf "\nThe (display s) procedure prints the contents of a string:\n")
(printf "(display \"string\") :\n")
(display "string")

;; A stirng can be either mutable or immutable,
;; a string literal is immutable, but most other strings are mutable
;; the make-string procedure creates a mutable string of a given length with 
;; an optional fill character
(printf "(make-string 5 #\\.) -> ~s\n" (make-string 5 #\.))
;; This is necessary as the new string is not returned from string-set!
(printf "NOTE: the \"->\" in these printouts are for results-in, not return value!\n")
(define s (make-string 5 #\.))
(string-set! s 2 #\0)
(printf "(stirng-set! (make-string 5 #\\.) 2 #\\0) -> ~s\n" s)
(printf "String ordering and case operations are locale-independant, though some, like upcase, can vary with locale\n")
(printf "(string<? \"apple\" \"Banana\") -> ~s\n" (string<? "apple" "Banana"))
(printf "(string-ci<? \"apple\" \"Banana\") -> ~s\n" (string-ci<? "apple" "Banana"))

;; 3.5 Bytes and Byte strings 
;; A byte is an 8-bit integer capable of representing the 256 values between 0 and 255
;; the byte? predicate will recognize numbers that represent valid bytes.
(printf "\nNow on to 3.5: Bytes and Byte strings!\n")
(printf "(byte? 255) -> ~s\n" (byte? 255))
(printf "(bytes-ref #\"Apple\" 0) -> ~s\n" (bytes-ref #"Apple" 0))
(printf "(make-bytes 3 65) -> ~s\n" (make-bytes 3 65))
(define b (make-bytes 2 0))
(printf "Creating mutable byte string 'b' with (make-bytes 2 0) -> ~s\n" b)
(bytes-set! b 0 1)
(printf "(bytes-set! b 0 1) -> ~s\n" b)
(bytes-set! b 1 255)
(printf "(bytes-set! b 1 255) -> ~s\n" b)
(printf "The following should print a lambda as encoded by UTF-8\n")
(printf "(bytes->string/utf-8 #\"\\316\\273\") -> ~s\n" (bytes->string/utf-8 #"\316\273"))
(printf "This is the same byte string encoded using the latin-1 codepage\n")
(printf "(bytes->string/latin-1 #\"\\316\\273\") -> ~s\n" (bytes->string/latin-1 #"\316\273"))
;; This will switch the code page over to Greek temporarily
(printf "The following block will attempt to print lambda using the greek code page\n")
(let ([cvt (bytes-open-converter "cp1253" ; the Greek code page
																 "UTF-8")]
			[dest (make-bytes 2)]) ; creates an empty, mutable, 2-byte string
	(bytes-convert cvt #"\353" 0 1 dest)
	(bytes-close-converter cvt)
	(bytes->string/utf-8 dest))

;; 3.6 Symbols:
;; A symbol is an atomic value that prints like an identifier preceeded with '.
;; an expression that starts with ' and continues with an identifier produces a symbol value
(printf "\nStarting 3.6: Symbols\n")
(printf "(symbol? \'a) -> ~s\n" (symbol? 'a))
(printf "symbols are case-sensitive:\n")
(printf "\t(eq? \'a \'a) -> ~s\n" (eq? 'a 'a))
(printf "\t(eq? \'a \'A) -> ~s\n" (eq? 'a 'A))
(printf "Any string can be supplied to string->symbol to get the corresponding symbol\n")
(printf "(eq? \'a (string->symbol \"a\")) -> ~s\n" (eq? 'a (string->symbol "a")))
(printf "(string->symbol \"one, two\") -> ~s\n" (string->symbol "one, two"))
(printf "The (write s) function prints a symbol without the \' prefix:\n\t")
(write 'Apple)
(write '|6|)
(printf "\nThe (display s) form is the same as the corresponding string\n\t")
(display 'Apple)
(display '|6|)
(printf "And neither include a newline in the output by default!\n")
(printf "\nThe (gensym) and string->uninterned-symbol functions generate new, uninterned symbols\n")
(printf "that are not equal to any previous symbol, interned or uninterned.\n")
(printf "Uninterned symbols are then useful as fresh tags that cannot be confused with any other value\n\n")
(define us (gensym))
(printf "Defining a new symbol \"us\" as ~s: (expecting \'g80)\n\t" us)
(printf "(eq? us \'g80) -> ~s\n" (eq? us 'g80))
(printf "\t(eq? \'a (string->uninterned-symbol \"a\")) -> ~s\n" (eq? 'a (string->uninterned-symbol "a")))
