#lang typed/racket

(require "../util/connection.rkt"
         "../util/list-utils.rkt")

(provide ping-stimulator
         start-stimulator)

; Stimulate conversation after an hour of inactivity (on average)
(define STIMULATION_TIME_MAX_S (* 2 60 60))

; Time in seconds for next stimulation (Add 10 to give bot time to connect)
(define next-stim (make-parameter
                    (+ (current-seconds) (random STIMULATION_TIME_MAX_S) 10)))

; Set the time for the next-stim (Add 2 to avoid negative sleep)
(define (ping-stimulator) (next-stim (+ (current-seconds) (random STIMULATION_TIME_MAX_S))))

#|
Starts the conversation stimulator.  After STIMULATION_TIME_S seconds have
passed without a message, a random function from fns will be called and printed
out
|#
(: start-stimulator ((Listof (-> Any)) -> Any))
(define (start-stimulator fns)
  (: stim-loop (-> Any))
  (define (stim-loop)
    (when (>= (current-seconds) (next-stim))
      ((assert (pick-random fns)))
      (ping-stimulator))
    ; Wait until next stim, then re-check condition
    (sleep (- (next-stim) (current-seconds)))
    (stim-loop))
  (when (null? fns) (raise-argument-error 'fns "non-empty list" fns))
  (thread stim-loop))
