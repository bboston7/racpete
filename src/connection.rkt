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
    (send-string (string-append "NICK " NICK))
    (send-string (string-append "USER " IDENT " 0 * :" REALNAME))))

(define (join)
  (begin
    (sleep 1)
    (send-string (string-append "JOIN " CHAN))))

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
(define (read-in privmsg-func)
  (define line (read-line input))
  (begin
    (cond
      [(eof-object? line) (clean-up-and-quit)]
      [(regexp-match #rx"^PING" line) (ping-respond line)]
      [(regexp-match #rx"^.* PRIVMSG" line) (handle-privmsg privmsg-func line)])
    (display (string-append line "\n"))
    (read-in privmsg-func)))

(define (is-privmsg line)
  (begin
    (display (cadr (string-split line)))
    (equal? (cadr (string-split line)) "PRIVMSG")))

#|
Breaks apart and handles a privmsg

fn - Function to pass nick and message to
|#
(define (handle-privmsg fn line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (define msg (cadr tokens))
  (begin
    (fn nick msg)))

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

(define (print-private nick msg)
  (display (string-append nick " : " msg)))

(define (start-pete callback)
  (begin
    (identify)
    (join)
    (read-in callback)))
