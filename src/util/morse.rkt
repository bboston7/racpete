#lang racket

(provide
  parse-morse
  contains-morse?
  convert-morse
  string->morse
)

#|
This module provides utilities relating to morse code.

A note on the semantics of morse code:  For our purposes, morse code is parsed
letter-by-letter, where letters are separated by a single space, and words are
separated by two spaces.
|#

#|
Get longest matching morse string from a line.
|#
(define (parse-morse line)
  (foldl (lambda (x y) (if (> (string-length x) (string-length y)) x y))
         ""
         (regexp-match* get-morse-pattern line)))

(define (contains-morse? line) (regexp-match contains-morse-pattern line))

(define (convert-morse s)
  (let*
     ; Converts morse character to english.  "" on failure.
     ([morse->charstr (lambda (m) (assocdr m morse-map))]

     ; Converts a morse word to an english word.  Malformed characters become "".
     [morseword->str
       (lambda (m)
         (letrec ([helper (lambda (lst acc)
                            (if (null? lst)
                              acc
                              (helper (cdr lst)
                                      (string-append
                                        acc
                                        (morse->charstr (car lst))))))])
         (helper (string-split m) "")))]

     ; Takes a string of morse words and converts to english string.
     [morsewords->str
       (lambda (words)
         (letrec ([helper (lambda (lst acc)
                            (if (null? lst)
                              acc
                              (helper (cdr lst)
                                      (string-append acc " "
                                        (morseword->str (car lst))))))])
           (helper (string-split words "  ") "")))])
  (string-trim (morsewords->str s) #:right? #f)))

#|
Takes an english string and converts it to a morse string.
|#
(define (string->morse s)
  (let
    ([strchar->morsechar
       (lambda (c) (let ([lookup (assoc c morse-map)])
                     (if lookup
                       (cond
                         [(equal? (cdr lookup) " ") "  "]
                         [else (string-append " " (cdr lookup))])
                       (string-append " " c))))])
    (string-append (strchar->morsechar (substring s 0 1))
                   (string->morse (substring s 1)))))

; Like assoc but compares v with each cdr, and returns car.  Failure gives back "".
(define (assocdr v lst)
  (cond
    [(null? lst) ""]
    [(equal? (cdar lst) v) (caar lst)]
    [else (assocdr v (cdr lst))]))

; Regex to match a line if line contains morse code.
(define contains-morse-pattern #px"[-. ]{6}")

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
