#lang typed/racket

(require/typed racket
               [string-split (String -> (Listof String))])

(provide strip-tags
         chop-token
         string-contains?
         string-ends-with?
         string-starts-with?)

#|
Strips tags and the string "\n" out of str
|#
(: strip-tags (String -> String))
(define (strip-tags str)
  (regexp-replace* #rx"<.*?>|\\\n" str ""))

#|
Chops out the first token of a string.
Can be used to get the message portion of an irc log line.
|#
(: chop-token (String -> String))
(define (chop-token line)
  (string-join (cdr (string-split line))))

#|
Returns a true value if token is in str
|#
(: string-contains? (String String -> Boolean))
(define (string-contains? str token)
  (regexp-match?
    (regexp (regexp-quote (string-downcase token)))
    (string-downcase str)))

#|
Returns #t if str ends with token
|#
(: string-ends-with? (String String -> Boolean))
(define (string-ends-with? str token)
  (regexp-match?
    (regexp (string-append (regexp-quote (string-downcase token)) "$"))
    (string-downcase str)))

#|
Returns true if str starts with token
|#
(: string-starts-with? (String String -> Boolean))
(define (string-starts-with? str token)
  (regexp-match?
    (regexp (string-append "^" (regexp-quote (string-downcase token))))
    (string-downcase str)))

(module+ test
  (require typed/rackunit)
  (check-true (string-contains? "abcd" "b") "Basic string contains")
  (check-true (string-contains? "abcd" "ab") "Substring at front")
  (check-true (string-contains? "abcd" "cd") "Substring at end")
  (check-true (string-contains? "abcd" "abcd") "str == token")
  (check-true (string-contains? "abcd" "AB") "Unmatched case")
  (check-true (string-contains? "abcd" "") "Empty string token")
  (check-true (string-contains? "" "") "str == token == empty string")
  (check-false (string-contains? "" "abcd") "Empty string str")
  (check-false (string-contains? "abcd" "ef") "token not in str")
  (check-false (string-contains? "abcd" "abcde") "str substring of token")
  (check-false (string-contains? "abcd" "abe") "partial match")

  (check-true (string-starts-with? "abcd" "ab") "Basic string starts with")
  (check-true (string-starts-with? "abcd" "abcd") "str == token")
  (check-true (string-starts-with? "abcd" "") "token empty string")
  (check-true (string-starts-with? "" "") "str == token == empty string")
  (check-true (string-starts-with? "abcd" "AB") "Unmatched case")
  (check-false (string-starts-with? "abcd" "ef") "token not in str")
  (check-false (string-starts-with? "abcd" "abe") "partial match")
  (check-false (string-starts-with? "" "abe") "empty str")
  )

