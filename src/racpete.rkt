#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt")

(define (log nick message)
  (display-to-file (string-append "<" nick ">" message) CHAN #:exists 'append))

(start-pete)
