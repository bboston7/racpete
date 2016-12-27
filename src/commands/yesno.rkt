#lang racket

(require
  "../util/names-manager.rkt"
  "../util/list-utils.rkt"
)

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
    (lambda ()
      (string-append (pick-random (current-nicks)) ": ^"))
    "probably"
    "signs point to yes"
    "signs point to no"
    (lambda ()
      (string-append "what did " (pick-random (current-nicks)) " have to say about it?"))
    "VERY YES"
    "unfortunately."
  ))

(define (yesno)
  (let ([response (pick-random responses)])
    (if (procedure? response)
      (response)
      response)))

