#!/usr/bin/racket
#lang racket

; TODO: Move these into a config file
(define HOST "seattle.uwirc.com")
(define PORT 6667)
(define NICK "racpete")
(define IDENT "racpete")
(define REALNAME "Rac Pete")
(define CHAN "")

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
    (write-string (string-append "JOIN #cse143\r\n") output)
    (flush-output output)))

(define (clean-up-and-quit) (
  (begin
    (display "Cleaning up and quitting....")
    (close-output-port output)
    (close-input-port input))))

#|
Prints out data returned from the server
|#
(define (read-in)
  (define line (read-line input))
  (cond
    [(eof-object? line) (clean-up-and-quit)]
    [#t (read-in)]))
#|
;  (begin
;    (display (string-append (read-line input) "\n"))
;    (read-in)))
|#

; TODO: Move this stuff out into some sort of main file
(identify)
(join)
(read-in)

