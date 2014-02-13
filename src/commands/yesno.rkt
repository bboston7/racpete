#lang racket

(provide
  yesno
)

#|
This module provides the functions for the random fortune teller command.
|#

(define responses
  (list
    "yes"
    "no"
    "maybe"
    "how should I know?"
    "bboston7: ^"
    "john: ^"
    "japacible: ^"
    "sunjayc: ^"
    "ask the danimal"
    "probably"
    "signs point to yes"
    "signs point to no"
    "what did jenpa have to say about it?"
    "yes, but only if cosimo stops quitting all the time"
    "VERY YES"
    "unfortunately."
  ))

(define (pick-response)
  (begin
    (sleep (+ (* (random) 0.75) 0.25))
    (list-ref responses (random (length responses)))))

(define yesno pick-response)

