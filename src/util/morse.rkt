#lang racket

;(provide )

#|
This module provides utilities relating to morse code.
|#

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
