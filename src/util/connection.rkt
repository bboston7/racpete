#!/usr/bin/racket
#lang typed/racket

(require "../config.rkt")

(require/typed racket
               [string-split (String String -> (Listof String))]
               [string-replace (String String String -> String)]
               [string-trim (String -> String)])

(provide act-to-channel
         names-from-channel
         clean-up-and-quit
         quit
         start-pete
         current-nicks
         write-to-channel
         write-to-user)

(define nicks '())

#|
Sets input to the input stream from the server and output to the output stream
from our computer
|#
(define-values (input output) (tcp-connect HOST PORT))

#|
Identifies with the IRC Server
|#
(: identify (-> Any))
(define (identify)
  (send-string (string-append "NICK " NICK))
  (send-string (string-append "USER " IDENT " 0 * :" REALNAME)))

#|
Joins the channel
|#
(: join (-> Any))
(define (join)
  (sleep 3)
  (send-string (string-append "JOIN " CHAN)))

#|
Exits the program after cleaning up files/sockets
|#
(: clean-up-and-quit (case->
                       [-> Any]
                       [Positive-Integer -> Any]))
(define (clean-up-and-quit [code 0])
  (displayln "Cleaning up and quitting....")
  (close-output-port output)
  (close-input-port input)
  (exit code))

#|
Quits with the message msg
|#
(: quit (String -> Any))
(define (quit msg)
  (send-string (string-append "QUIT :" msg)))

#|
Sends msg to the channel

Parameters:
    msg - Message to send to server
|#
(: write-to-channel (String -> Any))
(define (write-to-channel msg) (write-to-thing msg CHAN))

#|
Sends msg to the specified nick

Parameters:
    user - Nick of user we want to pm
    msg - Message to send to nick
|#
(: write-to-user (String String -> Any))
(define (write-to-user msg user) (write-to-thing msg user))

#|
Sends msg to thing, which can be a user or the channel

Parameters:
    msg - Message to send to server
    thing - Channel or a nick
|#
(: write-to-thing (String String -> Any))
(define (write-to-thing msg thing)
  (and msg (not (equal? msg ""))
    (send-string (string-append "PRIVMSG " thing " :" msg))))

#|
Sends action to the channel

Parameters:
    action - Action text to send to server
|#
(: act-to-channel (String -> Any))
(define (act-to-channel action) (act-to-thing action CHAN))

#|
Sends action to thing, which can be a user or the channel

Parameters:
    action - Action text to send to server
    thing - Channel or a nick
|#
(: act-to-thing (String String -> Any))
(define (act-to-thing action thing)
  (and action (not (equal? action ""))
    (send-string (string-append "PRIVMSG " thing " :ACTION " action))))

#|
Refresh names from channel
|#
(: names-from-channel ( -> Any))
(define (names-from-channel) (names-from-thing CHAN))

#|
Refresh names from thing, which can be a channel

Parameters:
    thing - Channel or a nick
|#
(: names-from-thing (String -> Any))
(define (names-from-thing thing)
    (send-string (string-append "NAMES " thing)))

#|
Reads in, and handles messages from the server

Parameters
    chanmsg-func - Function to call on receiving a PRIVMSG command at the
    channel level.
    privmsg-func - Function to call on receiving a PRIVMSG command for actual
    private messages.
|#
(: read-in ((String String -> Any) (String String -> Any) -> Any))
(define (read-in chanmsg-func privmsg-func)
  (define line (read-line input))
  (cond
    [(eof-object? line) (reconnect)]
    [(regexp-match #rx"^PING" line) (ping-respond line)]
    [(regexp-match (pregexp (string-append "^\\S* 005 " NICK ".* STATUSMSG=\\S* ")) line)
     (handle-statusmsg (string-trim line))]
    [(regexp-match (pregexp (string-append "^\\S* 353 " NICK)) line)
     (handle-names (string-trim line))]
    [(regexp-match (string-append "^.* JOIN :" CHAN) line)
     (handle-join (string-trim line))]
    [(regexp-match #px"^\\S* (PART|QUIT) " line)
     (handle-part (string-trim line))]
    [(regexp-match #px"^\\S* NICK " line)
     (handle-nick (string-trim line))]
    [(regexp-match (string-append "^.* PRIVMSG " CHAN) line) ; for channel-level
     (handle-privmsg chanmsg-func (string-trim line))]
    [(regexp-match (string-append "^.* PRIVMSG " NICK) line) ; for pm-level
     (handle-privmsg privmsg-func (string-trim line))])
  (displayln line)
  (read-in chanmsg-func privmsg-func))

(define: nick-prefixes : String "@%+")

#|
Retrieves list of user prefixes from server
|#
(: handle-statusmsg (String -> Any))
(define (handle-statusmsg line)
  (define regmatch (regexp-match #px" STATUSMSG=(\\S*) " line))
  (if regmatch
    (let ([match (cadr regmatch)])
      (if match
        (set! nick-prefixes match)
        '()))
    '()))

#|
Handle a name list response
|#
(: handle-names (String -> Any))
(define (handle-names line)
  (define tokens (string-split line ":"))
  (define names (string-split (cadr tokens) " "))
  (set! nicks
    (map (lambda: ([name : String])
           (regexp-replace (string-append "^[" nick-prefixes "]*")
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
  (set! nicks (cons nick nicks)))

#|
Handle a user PART or QUIT message
|#
(: handle-part (String -> Any))
(define (handle-part line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (set! nicks (remove nick nicks)))

#|
Handle a user NICK message
|#
(: handle-nick (String -> Any))
(define (handle-nick line)
  (define tokens (string-split line ":"))
  (define oldnick (car (string-split (car tokens) "!")))
  (define newnick (third (string-split (car tokens) " ")))
  (set! nicks (cons newnick (remove oldnick nicks))))

#|
Return nicks as a list
|#
(define (current-nicks) nicks)

#|
Breaks apart and handles a privmsg

fn - Function to pass nick and message to
|#
(: handle-privmsg ((String String -> Any) String -> Any))
(define (handle-privmsg fn line)
  (define tokens (string-split line ":"))
  (define nick (car (string-split (car tokens) "!")))
  (define msg (string-join (cdr tokens) ":"))
  (fn nick msg))

#|
Responds to a PING with a proper PONG
|#
(: ping-respond (String -> Any))
(define (ping-respond line)
  (send-string (string-replace line "PING" "PONG")))

#|
Sends str to the channel

Parameters
    str - string? to send to the server
|#
(: send-string (String -> Any))
(define (send-string str)
  (write-string (string-append str "\r\n") output)
  (flush-output output))

#|
Reconnect to the server
|#
(: reconnect (-> Any))
(define (reconnect)
  (close-input-port input)
  (close-output-port output)
  (printf "disconnected, reconnecting in ~a seconds\n" RECONNECT_TIMER)
  (sleep RECONNECT_TIMER)
  (displayln "reconnecting...")
  (set!-values (input output) (tcp-connect HOST PORT))
  (identify)
  (join))

#|
Starts the bot

Parameters
    callback - Function to call back to on PRIVMSG from server
|#
(: start-pete ((String String -> Any) (String String -> Any) -> Any))
(define (start-pete chan-callback priv-callback)
  (identify)
  (join)
  (read-in chan-callback priv-callback))
