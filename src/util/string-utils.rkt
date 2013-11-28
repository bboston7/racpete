#lang racket

(provide (contract-out
           [strip-tags (-> string? string?)]
           [string-contains? (-> string? string? boolean?)]
           [string-starts-with? (-> string? string? boolean?)]))

#|
Strips tags and the string "\n" out of str
|#
(define (strip-tags str)
  (regexp-replace* #rx"<.*?>|\\\n" str ""))

#|
Returns a true value if token is in str
|#
(define (string-contains? str token)
  (regexp-match?
    (regexp (regexp-quote (string-downcase token)))
    (string-downcase str)))

#|
Returns true if str starts with token
|#
(define (string-starts-with? str token)
  (regexp-match?
    (regexp (string-append "^" (regexp-quote (string-downcase token))))
    (string-downcase str)))

