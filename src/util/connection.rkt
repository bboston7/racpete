#!/usr/bin/racket
#lang racket

(require "../config.rkt")

(provide
  (contract-out
    [act-to-channel (-> (or/c boolean? string?) any)]
    [write-to-channel (-> (or/c boolean? string?) (or/c boolean? void?))])
  clean-up-and-quit
  quit
  start-pete)

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

#|
Joins the channel
|#
(define (join)
  (begin
    (sleep 5)
    (send-string (string-append "JOIN " CHAN))))

#|
Exits the program after cleaning up files/sockets
|#
(define (clean-up-and-quit [code 0])
  (begin
    (display "Cleaning up and quitting....")
    (close-output-port output)
    (close-input-port input)
    (exit code)))

#|
Quits with the message msg
|#
(define (quit msg)
  (send-string (string-append "QUIT :" msg)))

#|
Sends msg to the channel

Parameters:
    msg - Message to send to server
|#
(define (write-to-channel msg) (write-to-thing msg CHAN))

#|
Sends msg to thing, which can be a user or the channel

Parameters:
    msg - Message to send to server
    thing - Channel or a nick
|#
(define (write-to-thing msg thing)
  (and msg (not (equal? msg ""))
    (send-string (string-append "PRIVMSG " thing " :" msg))))

#|
Sends action to the channel

Parameters:
    action - Action text to send to server
|#
(define (act-to-channel action) (act-to-thing action CHAN))

#|
Sends action to thing, which can be a user or the channel

Parameters:
    action - Action text to send to server
    thing - Channel or a nick
|#
(define (act-to-thing action thing)
  (and action (not (equal? action ""))
    (send-string (string-append "PRIVMSG " thing " :ACTION " action))))

#|
Reads in, and handles messages from the server

Parameters
    privmsg-func - Function to call on receiving a PRIVMSG command.
|#
(define (read-in privmsg-func)
  (define line (read-line input))
  (begin
    (cond
      [(eof-object? line) (clean-up-and-quit)]
      [(regexp-match #rx"^PING" line) (ping-respond line)]
      [(regexp-match (string-append "^.* PRIVMSG " CHAN) line)
       (handle-privmsg privmsg-func (string-trim line))])
    (display (string-append line "\n"))
    (read-in privmsg-func)))

#|
Breaks apart and handles a privmsg

fn - Function to pass nick and message to
|#
(define (handle-privmsg fn line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (define msg (string-join (cdr tokens) ":"))
  (begin
    (fn nick msg)))

#|
Responds to a PING with a proper PONG
|#
(define (ping-respond line)
  (begin
    (send-string (string-replace line "PING" "PONG"))))

#|
Sends str to the channel

Parameters
    str - string? to send to the server
|#
(define (send-string str)
  (begin
    (write-string (string-append str "\r\n") output)
    (flush-output output)))

#|
Starts the bot

Parameters
    callback - Function to call back to on PRIVMSG from server
|#
(define (start-pete callback)
  (begin
    (identify)
    (join)
    (read-in callback)))
