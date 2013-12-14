#lang racket

(require
  (planet gh/sha:1:1)
  "../util/connection.rkt"
)

(provide
  verify
  die
  update
)

#|
This module provides commands that require the user to authenticate via a
supplied password argument.  The commands provided are only accessible via
private message to the bot, and the password is stored as a sha-512 hash on
disk.
|#

#|
Verify determines if the supplied password, pass, is correct, and if it is, it
runs the callback, cb, with the variable number of arguments, args.
|#
(define (verify pass cb . args)
  (let* ([passfile ".racpete.pass"]
         [cur-hash (if (file-exists? passfile)
                     (read-line (open-input-file passfile #:mode 'text))
                     #f)])
    (if (equal? cur-hash (bytes->hex-string (sha512 (string->bytes/utf-8 pass))))
      (apply cb args)
      #f)))

#|
Quits the server after checking permissions of the caller
|#
(define (die) (quit "told to die") (clean-up-and-quit))

#|
Exits, updates from git, then rejoins
|#
(define (update)
  (define UPDATE-EXIT-CODE 2)
  (quit "I'M GITTIN' AN UPGRADE MA")
  (clean-up-and-quit UPDATE-EXIT-CODE))

