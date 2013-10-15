#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt")

(define (log nick message)
  (display-to-file (string-append "<" nick "> " message "\n")
                   (string-append CHAN ".log") #:exists 'append))

(define (print-private nick msg)
  (display (string-append nick " : " msg "\n")))

(define (command-handler nick msg)
  (cond
    [(regexp-match #rx"^\\.die" msg) (write-to-channel "please don't kill me")]))

(start-pete command-handler)
