#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt")

#|
Builds a list of quotes from the current channel log
|#
(define (build-quotes)
  (letrec ([log-file (open-input-file (string-append CHAN ".log") #:mode 'text)]
         ; Tail recursive because log files get large!
         ; Unfortunately, that means the list is backwards, this doesn't matter
         ; now, and since we don't store date information in our logs, maybe it
         ; never will
         [tail-fn
           (lambda (acc)
             (let ([line (read-line log-file)])
               (if (eof-object? line)
                 acc
                 (tail-fn (cons line acc)))))])
    (tail-fn null)))

(define quotes (build-quotes))

(define (log nick message)
  (let ([line (string-append "<" nick ">" message)])
    (begin
      (set! quotes (cons line quotes))
      (display-to-file (string-append line "\n")
                       (string-append CHAN ".log") #:exists 'append))

(define (print-private nick msg)
  (display (string-append nick " : " msg "\n")))

(define (command-handler nick msg)
  (cond
    [(regexp-match #rx"^\\.die" msg) (write-to-channel "please don't kill me")]))

(start-pete command-handler)
