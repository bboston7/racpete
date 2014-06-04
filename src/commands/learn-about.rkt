#lang racket

(provide egrep
         learn-about)

(require "../util/list-utils.rkt"
         "../util/string-utils.rkt")

(define (egrep msg quotes out)
  (thread (λ ()
    (let ([token (string-join (cdr (string-split msg)))])
      (if (equal? (string-trim token) "")
        #f
        (let ([matches
                (with-handlers ([exn:fail? (λ (_) (list "del Bad regex?  Bad you!"))])
                  (filter (λ (x) (regexp-match (pregexp token) (strip-tags x)))
                          quotes))])
          (if (null? matches)
            (out (string-append "No matches for " token))
            (out (chop-token (pick-random matches))))))))))

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
