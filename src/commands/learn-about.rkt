#lang racket

(provide egrep
         learn-about)

(require "../util/list-utils.rkt"
         "../util/string-utils.rkt")

(define (egrep msg quotes)
  (let ([token (string-join (cdr (string-split msg)))])
    (if (equal? (string-trim token) "")
      #f
      (let ([matches
              (filter (λ (x) (regexp-match (pregexp token) (strip-tags x)))
                      quotes)])
        (if (null? matches)
          (string-append "No matches for " token)
          (chop-token (pick-random matches)))))))

#|
Given a string, returns a quote containing that string
|#
(define (learn-about msg quotes)
  (let ([token (string-append " " (string-join (cdddr (string-split msg))))])
    (if (equal? (string-trim token) "")
      #f
      (let ([matches (filter (lambda (x) (string-contains? x token)) quotes)])
        (if (null? matches)
          (string-append "Cache miss!  Tell me about" token)
          (chop-token (list-ref matches (random (length matches)))))))))
