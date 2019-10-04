#lang racket
;; Begin chapter 3, exploring build-in data types
;; Include the whatis procedure for additional introspection
(require "whatis.rkt")

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

;; 3.7 Keywords:
;; A keyword value is similar to a symbol, but its printed form is prefixed with a #
(printf "\nKeywords:\n")
(printf "(string->keyword \"apple\") -> ~s\n" (string->keyword "apple"))
(printf "(eq? \'#:apple (string->keyword \"apple\")) -> ~s\n" (eq? '#:apple (string->keyword "apple")))
;; A keyword is identical to an identifier, but when quoted produces a value. Unlike an identifier 
;; which when quoted produces a symbol
;; An example of how they're different:
(printf "Defining a path identifier, dir, which exemplifies the difference between symbols and keywords\n")
(define dir (find-system-path 'temp-dir)) ; not '#:temp-dir
(printf "Writing to a file named ~s\n" (build-path dir "stuff.txt"))
(with-output-to-file (build-path dir "stuff.txt")
										 (lambda () (printf "example\n"))
										 ; optional #:mode argument can be 'text or 'binary
										 #:mode 'text
										 ; optional #:exists argument can be 'replace, 'truncate, ...
										 #:exists 'replace)

;; 3.8 Lists and Pairs:
;; A pair joins any two arbitrary values. The cons procedure constructs pairs and the
;; car and cdr procedures extract the first and second elements respectively. The pair? predicate 
;; returns true when provided with a pair
(define tpair (cons (cons 1 2) 3))
(printf "\nStarting 3.8 examples with pairs and lists:\n")
(printf "(cons 1 2) -> ~s\n" (cons 1 2))
(printf "Creating tpair: ~s\n" tpair)
(printf "(cons (cons 1 2) 3) -> ~s\n" (cons (cons 1 2) 3))
(printf "(whatis tpair) -> ") (whatis tpair)
;; A list is a combination of pairs that creates a linked list.
;; More precisely a list is either the empty list null, or a pair whose 
;; first elemenet is a list element and whose second element is a list.
;; The list? predicate will recognize the null or empty list as a list.
(printf "null -> ~s\n" null)
(whatis null)
(define tlist (cons 0 (cons 1 (cons 2 null))))
(printf "Creating tlist: ~s\n" tlist)
(printf "(list? tlist) -> ~s\n" (list? tlist))
(printf "(pair? tlist) -> ~s\n" (pair? tlist))
(whatis tlist)
(printf "(list? (cons 1 2)) -> ~s\n" (list? (cons 1 2)))
(whatis (cons 1 2))
;; A list or pair prints using list or cons when one of its elements cannot be written as a 
;; quoted value. For example, a value constructed with srcloc cannot be written with quote
;; and such prints using srcloc:
(printf "What follows are some examples of constracts that are unable to be written using quote\n")
(srcloc "ch3.rkt" 1 0 1 (+ 4 4))
(list 'here (srcloc "ch3.rkt" #f #f #f #f) 'there)
(cons 1 (srcloc "ch3.rkt" 1 0 1 8))
(cons 1 (cons 2 (srcloc "ch3.rkt" 1 0 1 8)))
(printf "srcloc results: ~s\n" (srcloc->string (srcloc "ch3.rkt" 200 #f #f #f)))
(printf "\nThere are no differences between (display v) and (write v) when it comes to lists and pairs other than printing the elements\n")
(printf "(write (cons 1 2)) -> ~s\n" (write (cons 1 2)))
(printf "(display (cons 1 2)) -> ~s\n" (display (cons 1 2)))
(printf "(write null) -> ~s\n" (write null))
(printf "(display null) -> ~s\n" (display null))
(printf "(write (list 1 2 \"3\")) -> ~s\n" (write (list 1 2 "3")))
(printf "(display (list 1 2 \"3\")) -> ~s\n" (display (list 1 2 "3")))
;; Among the most important predefined procedures that operate on lists 
;; aro those that iterate through a list's elements:
(printf "Some of the most important list procedures are those that iterate over the elements:\n")
(define lst '(1 2 3 4 5))
(printf "Using the list lst (~s) for demonstration:\n" lst)
(whatis lst)
(printf "\n(map (lambda (i) (/ 1 i)) lst) -> ~s\n" (map (lambda (i) (/ 1 i)) lst))
(printf "\nNOTE: The expression (< i 3) is equivalent to the infix notation of (i . < . 3)\n")
(printf "(andmap (lambda (i) (< i 3)) lst) -> ~s\n" (andmap (lambda (i) (< i 3)) lst))
(printf "(ormap (lambda (i) (< i 3)) lst) -> ~s\n" (ormap (lambda (i) (< i 3)) lst))
(printf "(filter (lambda (i) (< i 3)) lst) -> ~s\n" (filter (lambda (i) (< i 3)) lst))
(printf "(foldl (lambda (v i) (+ v i)) 10 lst) -> ~s\n" (foldl (lambda (v i) (+ v i)) 10 lst))
(printf "\nNOTE: Due to how (display i) works, this line will not print as intended\n")
(printf "(for-each (lambda (i) (display i)) lst) -> ~s\n" (for-each (lambda (i) (display i)) lst))
(printf "(member 3 lst) -> ~s\n" (member 3 lst))
(printf "(assoc \'where \'((when \"3:30\") (where \"Florida\") (who \"Mickey\"))) -> ~s\n" 
				(assoc 'where '((when "3:30") (where "Florida") (who "Mickey"))))
;; Pairs are immutable by default, use mcons to construct them with mutability
(printf "\nContrary to Lisp tradition, pairs in Racket are immutable.\n")
(printf "The pair? and list? predicates only recognize these immutable data types,\n")
(printf "to create a mutable pair, use (mcons i v) and test with the mpair? predicate.\n")
(define mp (mcons 1 2))
(printf "(define mp (mcons 1 2)) -> ~s\n" mp)
(whatis mp)
(printf "(pair? mp) -> ~s\n" (pair? mp))
(printf "(mpair? mp) -> ~s\n" (mpair? mp))
(printf "(set-mcar! mp 0) -> ")
(set-mcar! mp 0)
(printf "~s\n" mp)

;; 3.9 Vectors:
;; A vector is a fixed length array of arbitrary values. Unlike a list, a vector
;; supports constant-time access and update of its elements.
;; A vector prints similarly to a list, but with '#(...) rather than '(...)
;; alternatively using vector if an element cannot be directly expressed.
(printf "\nVectors:\n")
(define myvec #("test" "vector" "setup"))
(printf "(define myvec #(\"test\" \"vector\" \"setup\") -> ~s\n" myvec)
(printf "(vector? myvect) -> ~s\n" (vector? myvec))
(printf "Some of the same list functions are available to vectors:\n")
(printf "(vector-ref myvec 1) -> ~s\n" (vector-ref myvec 1))
(printf "(list->vector (map string-titlecase (vector->list myvec))) -> ")
(list->vector (map string-titlecase
									 (vector->list myvec)))
(printf "\n")

;; 3.10 Hash Tables
;; A hash table implements a mapping from keys to values where both 
;; keys and values are arbitrary Racket values, and access/update operations 
;; are typically constant time.
;; Keys are compared using equal? eqv? or eq? depending on if the hash table is created
;; with make-hash, make-hasheqv, or make-hasheq.
(printf "\n3.10: Hash Tables\n")
(define ht (make-hash))
(printf "Created example hash table \"ht\"\n")
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(printf "(hash-ref ht \"apple\") -> ~s\n" (hash-ref ht "apple"))
(printf "(hash-ref ht \"coconut\" \"nonexistent\") -> ~s\n" (hash-ref ht "coconut" "nonexistent"))
;; Hash tables default to being constructed as immutable entities with the provided set of keys and values
;; which can be extended with hash-set, producing a new entry in the table
(whatis ht)
(printf "Mutable hash tables are considered \"weak\" and defined as such:\n")
(printf "\t(define wht (make-weak-hasheq))\n")
(define wht (make-weak-hasheq))
(whatis wht)
(printf "(hash-set! wht (gensym) \"can you see me?\") -> ")
(hash-set! wht (gensym) "can you see me?")
(printf "~s\n" (quote wht))
(printf "(hash-count wht) -> ~s\n" (hash-count wht))
(printf "Running GC pass...\n")
(collect-garbage)
(printf "(hash-count wht) -> ~s\n" (hash-count wht))
;; However, this could still have a strongly bound value in the hash table,
;; so long as the corresponding key is accessible, creating a catch-22 when trying to 
;; clear the entries in a GC pass.
;; This can be worked around by binding the key to an ephemeron
(printf "Now demonstrating a way to use an ephemeron to clean a weak hash table:\n")
(printf "(define eht (make-weak-hasheq))\n")
(define eht (make-weak-hasheq))
(let ([g (gensym)])
	(hash-set! eht g (list g)))
(collect-garbage)
(printf "(let ([g (gensym)])\n\t(hash-set! eht g (list g)))\n\t(collect-garbage)\n(hash-count eht) -> ~s\n"(hash-count eht))
(printf "\nNow using an ephemeron:\n")
(define eht2 (make-weak-hasheq))
(let ([g (gensym)])
	(hash-set! eht2 g (make-ephemeron g (list g))))
(collect-garbage)

(printf "(define eht2 (make-weak-hasheq))\n(let ([g (gensym)])\n\t(hash-set! eht2 g (make-ephemeron g (list g))))\n(collect-garbage)\n(hash-count eht2) -> ~s\n" (hash-count eht2))

;; 3.11 Boxes:
;; A box is like a single element vector. It can print as a quoted #&
;; followed by the printed form of the boxed value, a literal #&
;; can also be used as an expression, but it has little to no use
;; as the resulting box would be a constant.
(define bx (box "apple"))
(printf "\nNow see boxes:\n\n(define bx (box \"apple\"))\n")
(whatis bx)
(printf "(unbox bx) -> ~s\n" (unbox bx))
(printf "(set-box! bx '(banana gram))\n")
(set-box! bx '(banana gram))
(whatis bx)

;; 3.12 Void and Undefined
;; Some procedures and expressions have no real need for a result value. 
;; For example, (display v) is called purely for its side-effect of writing output.
;; In such cases the result value is normally a constant that prints as #<void>.
;; The REPL would not print anything in such a case.
(printf "\nFun with #<void>\n")
(void)
(void 1 2 3)
(whatis (list (void)))
(void? (void))
(whatis (void))
