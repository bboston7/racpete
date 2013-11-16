#lang racket

;(provide )

#|
This module provides utilities relating to morse code.
|#

(define (contains-morse (line) (regexp-match contains-morse-pattern line)))

(define (convert (s)
                 (let ([wordlist (string-split s "  ")]
                       [assocdr (lambda (v lst)
                                  (cond
                                    [(null? lst) #f]
                                    [(equal? (cdar lst) v) (caar lst)]
                                    [else (assocdr v (cdr lst))]))]
                       [convert-char (lambda (s) 

; Regex to match a line if line contains morse code.
(define contains-morse-pattern #px"[-. ]{5}")

; Regex to parse out the morse code from a line.
(define get-morse-pattern #px"[-. ]+")

(define morse-map
  (list
    (cons "a" ".-")
    (cons "b" "-...")
    (cons "c" "-.-.")
    (cons "d" "-..")
    (cons "e" ".")
    (cons "f" "..-.")
    (cons "g" "--.")
    (cons "h" "....")
    (cons "i" "..")
    (cons "j" ".---")
    (cons "k" "-.-")
    (cons "l" ".-..")
    (cons "m" "--")
    (cons "n" "-.")
    (cons "o" "---")
    (cons "p" ".--.")
    (cons "q" "--.-")
    (cons "r" ".-.")
    (cons "s" "...")
    (cons "t" "-")
    (cons "u" "..-")
    (cons "v" "...-")
    (cons "w" ".--")
    (cons "x" "-..-")
    (cons "y" "-.--")
    (cons "z" "--..")
    (cons "1" ".----")
    (cons "2" "..---")
    (cons "3" "...--")
    (cons "4" "....-")
    (cons "5" ".....")
    (cons "6" "-....")
    (cons "7" "--...")
    (cons "8" "---..")
    (cons "9" "----.")
    (cons "0" "-----")))
