#!/usr/bin/racket
#lang racket

(require "config.rkt")

(provide start-pete write-to-channel)

#|
Sets input to the input stream from the server and output to the output stream
from our computer
|#
(define-values (input output) (tcp-connect HOST PORT))

#|
Identifies with the IRC Server
|#
(define (identify)
  (begin
    (write-string (string-append "NICK " NICK "\r\n") output)
    (write-string (string-append "USER " IDENT " 0 * :" REALNAME "\r\n") output)
    (flush-output output)))

(define (join)
  (begin
    (sleep 1)
    (write-string (string-append "JOIN " CHAN "\r\n") output)
    (flush-output output)))

(define (clean-up-and-quit)
  (begin
    (display "Cleaning up and quitting....")
    (close-output-port output)
    (close-input-port input)
    (exit)))

(define (write-to-channel msg)
  (send-string (string-append "PRIVMSG " CHAN " :" msg)))

#|
Prints out data returned from the server
|#
(define (read-in)
  (define line (read-line input))
  (begin
    (cond
      [(eof-object? line) (clean-up-and-quit)]
      [(regexp-match #rx"^PING" line) (ping-respond line)])
    (display (string-append line "\n"))
    (read-in)))

#|
Responds to a PING with a proper PONG
|#
(define (ping-respond line)
  (begin
    (send-string (string-replace line "PING" "PONG"))))

(define (send-string str)
  (begin
    (write-string (string-append str "\r\n") output)
    (flush-output output)))

(define (start-pete)
  (begin
    (identify)
    (join)
    (read-in)))

