#!/usr/bin/racket
#lang racket

; TODO: Move these into a config file
(define HOST "seattle.uwirc.com")
(define PORT 6667)
(define NICK "racpete_cool")
(define IDENT "racpete_cool")
(define REALNAME "Rac Pete_cool")
(define CHAN "#cse143")

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

#|
Prints out data returned from the server
|#
(define (read-in)
  (define line (read-line input))
  (begin
    (display (string-append line "\n"))
    (cond
      [(regexp-match #rx"^PING" line) (ping-respond line)])
    (read-in)))

#|
Responds to a PING with a proper PONG
|#
(define (ping-respond line)
  (write-string (string-replace line "PING" "PONG")))

#|
  (begin
    (display (read-line input))
    (read-in)))
|#

; TODO: Move this stuff out into some sort of main file
(identify)
(read-in)

(close-output-port output)
(close-input-port input)
