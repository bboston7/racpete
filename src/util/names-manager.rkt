#!/usr/bin/racket
#lang typed/racket

(require "../config.rkt")

(require/typed racket
               [string-split (String String -> (Listof String))]
               [string-replace (String String String -> String)]
               [string-trim (String -> String)])

(provide current-nicks
         handle-statusmsg
         handle-names
         handle-join
         handle-nick
         handle-part)

(define current-nicks (make-parameter '()))
(define nick-prefixes (make-parameter "@%+"))

#|
Retrieves list of user prefixes from server
|#
(: handle-statusmsg (String -> Any))
(define (handle-statusmsg line)
  (define regmatch (regexp-match #px" STATUSMSG=(\\S*) " line))
  (if regmatch
    (let ([match (cadr regmatch)])
      (if match
        (nick-prefixes match)
        '()))
    '()))

#|
Handle a name list response
|#
(: handle-names (String -> Any))
(define (handle-names line)
  (define tokens (string-split line ":"))
  (define names (string-split (cadr tokens) " "))
  (current-nicks
    (map (lambda: ([name : String])
           (regexp-replace (string-append "^[" (nick-prefixes) "]*")
                           name
                           ""))
         names)))

#|
Handle a user JOIN message
|#
(: handle-join (String -> Any))
(define (handle-join line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (current-nicks (cons nick (current-nicks))))

#|
Handle a user PART or QUIT message
|#
(: handle-part (String -> Any))
(define (handle-part line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (current-nicks (remove nick (current-nicks))))

#|
Handle a user NICK message
|#
(: handle-nick (String -> Any))
(define (handle-nick line)
  (define tokens (string-split line ":"))
  (define oldnick (car (string-split (car tokens) "!")))
  (define newnick (third (string-split (car tokens) " ")))
  (current-nicks (cons newnick (remove oldnick (current-nicks)))))

