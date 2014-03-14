#lang racket

(require
  "../util/list-utils.rkt"
  "../util/string-utils.rkt"
)

(provide
  has-said
  what-would-say
)

#|
Add to the nexts dictionary and the ending token list from a single message
|#
(define (parse-tokens first second tokens nexts ends)
  (if (null? tokens)
    (values nexts (cons second ends))
    (parse-tokens second
                  (car tokens)
                  (cdr tokens)
                  (cons
                    (cons
                      (cons first second)
                      (car tokens))
                    nexts)
                  ends)))

#|
Make the nexts dictionary and the ending token list
|#
(define (form-chain msgs nexts ends)
  (if (null? msgs)
    (values nexts ends)
    (let-values ([(nexts ends)
                   (parse-tokens #f #f
                                 (string-split (car msgs))
                                 nexts
                                 ends)])
      (form-chain (cdr msgs)
                  nexts
                  ends))))

#|
Generate a randomized list of tokens given a nexts dictionary and an ending token list
(Note: the sentence is formed backwards)
|#
(define (build-msg nexts ends rev-acc)
  (let* ([second (car rev-acc)]
        [first (cadr rev-acc)]
        [third (pick-random
                (filter
                  (lambda (x)
                    (equal?
                      (cons first second)
                      (car x)))
                  nexts))])
    (if third
      (let ([rev-acc (cons (cdr third) rev-acc)])
        (if (and (member (cdr third) ends) (< (random 100) 25)) ; 25% chance of ending if end token
            rev-acc
            (build-msg nexts ends rev-acc)))
      rev-acc)))

#|
Turn a (reversed) token list into a string
|#
(define (make-quote msgs)
  (let-values ([(nexts ends)
                 (form-chain msgs '() '())])
              (string-join (cddr (reverse (build-msg nexts ends '(#f #f)))))))

(define (say-quote nick msg)
  (string-append "<" nick "> " msg))

#|
Given a nick and a quote list, forms a quote from that person
|#
(define (what-would-say nick quotes)
  (let* ([rx (pregexp (string-append "^\\<" nick "\\>"))]
        [matches (filter (lambda (x) (regexp-match? rx x)) quotes)])
    (say-quote nick (if (equal? nick NICK)
                      (make-quote (map chop-token quotes))
                      (if (null? matches)
                        "derp derp derp derp"
                        (make-quote (map chop-token matches)))))))

#|
Given a nick, returns a quote from that person
|#
(define (has-said nick quotes)
  (let* ([rx (pregexp (string-append "^\\<" nick "\\>"))]
        [matches (filter (lambda (x) (regexp-match? rx x)) quotes)])
    (if (null? matches)
      (string-append "Unfortunately, " nick " has never spoken.")
      (pick-random matches))))

