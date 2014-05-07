#lang typed/racket

(require "../util/connection.rkt"
         "../util/list-utils.rkt")

(provide ping-stimulator
         start-stimulator)

; Stimulate conversation after an hour of inactivity
(define STIMULATION_TIME_S (* 60 60))

; Time in seconds of the last message
(define last-message (current-seconds))

(define (ping-stimulator) (set! last-message (current-seconds)))

#|
Starts the conversation stimulator.  After STIMULATION_TIME_S seconds have
passed without a message, a random function from fns will be called and printed
out
|#
(: start-stimulator ((Listof (-> Any)) -> Any))
(define (start-stimulator fns)
  (: stim-loop (-> Any))
  (define (stim-loop)
    (when (> (- (current-seconds) last-message) STIMULATION_TIME_S)
      ((assert (pick-random fns)))
      (ping-stimulator))
    ; Wait a minute before re-checking the condition
    (sleep 60)
    (stim-loop))
  (when (null? fns) (raise-argument-error 'fns "non-empty list" fns))
  (thread stim-loop))
