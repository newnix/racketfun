#lang racket
(provide getpath)
(provide pathbins)
(provide hashpath)

(define env (current-environment-variables))

; This returns the value of your $PATH as a string
(define (getpath-str env)
    (if (environment-variables? env)
        (bytes->string/utf-8 (environment-variables-ref env #"PATH"))
        '()))

; This returns the value of $PATH as a list of strings
(define (getpath)
  (string-split (getpath-str env) ":"))

; Build a list of hashes for the directories in $PATH
(define (pathbins)
  (map hash (getpath) (pathlsts)))

; Get a list of directory contents
(define (pathlsts)
  (map directory-list (getpath)))

;; Make the binary PATH information available as a native hash type
(define (unpath lst)
  (if (empty? lst)
    '()
    (map path->string lst)))

(define hashpath (make-hash (map cons (getpath) (map unpath (pathlsts)))))

;; This is a relatively simple reimplementation of `which(1)`
;; it may or may not work yet, some logic still needs to be worked out
(define (pathscan ht str)
  (for ([i (hash-keys ht)])
    (if (index-of (hash-ref ht i) str)
      (printf "~a\n" (string-append i "/" str))
      #f
    )
  ))
