#!/usr/bin/racket
#lang racket

(require
  "commands/ycombinator.rkt"
  "config.rkt"
  "util/connection.rkt"
  "util/string-utils.rkt"
  "util/urltilities.rkt")

(define UPDATE-EXIT-CODE 2)

#|
Builds a pair of lists.  The car is quotes from the current channel log, the cdr
is urls from the current channel log
|#
(define (build-quotes-and-urls)
  (letrec ([file-name (string-append "logs/" CHAN ".log")]
           [log-file
             (if (file-exists? file-name)
               (open-input-file file-name #:mode 'text)
               #f)]
         ; Tail recursive because log files get large!
         ; Unfortunately, that means the list is backwards, this doesn't matter
         ; now, and since we don't store date information in our logs, maybe it
         ; never will
         [tail-fn
           (lambda (quotes-acc url-acc)
             (let ([line (read-line log-file)])
               (if (eof-object? line)
                 (cons quotes-acc url-acc)
                 (let ([url-match (regexp-match urlregex line)])
                   (if url-match
                     (tail-fn (cons line quotes-acc) (cons (car url-match) url-acc))
                     (tail-fn (cons line quotes-acc) url-acc))))))])
    (if log-file
      (begin0
        (tail-fn null null)
        (close-input-port log-file))
      (cons null null))))

(define quotes-and-urls (build-quotes-and-urls))
(define quotes (car quotes-and-urls))
(define links (cdr quotes-and-urls))

#|
Log a line from the chat
|#
(define (log nick message)
  (let ([line (string-append "<" nick "> " message)])
    (begin
      (set! quotes (cons line quotes))
      (let ([url-match (regexp-match urlregex line)])
        (when url-match
          (set! links (cons (car url-match) links))))
      (display-to-file (string-append line "\n")
                       (string-append "logs/" CHAN ".log") #:exists 'append))))

#|
Handles incomming user irc commands
|#
(define (command-handler nick msg)
  (let ([urlres (regexp-match urlregex msg)])
    (cond
      [(equal? ".q" msg) (write-to-channel (get-random-line quotes))]
      [(equal? "(y)" msg) (write-to-channel (string-append "I hate you Lopez"))]
      [(equal? ".boom" msg) (write-to-channel (string-append "BOOM GOES THE DYNAMITE!"))]
      [(equal? ".link me" msg) (let* ([url (get-random-line links)]
                                     [title (get-website-title url)])
                                 (begin
                                   (write-to-channel url)
                                   (write-to-channel title)))]
      [(string-starts-with? msg "tell me about")
       (let ([out (learn-about msg)])
         (and out (write-to-channel out)))]
      [(equal? ".die" msg) (die nick)]
      [(equal? ".ycombinator" msg) (begin (write-to-channel yc1) (write-to-channel yc2)
                                          (write-to-channel yc3) (write-to-channel yc4))]
      [urlres (let ([title (get-website-title (car urlres))])
                (begin (write-to-channel title) (log nick msg)))]
      [(equal? ".update" msg) (update nick)]
      [else (log nick msg)])))

#|
Returns the message portion of an irc log line
|#
(define (get-message line)
  (string-join (cdr (string-split line))))

#|
Returns a random line from the passed list
|#
(define (get-random-line lst)
  (list-ref lst (random (length lst))))


#|
Given a string, returns a quote containing that string
|#
(define (learn-about msg)
  (let ([token (string-append " " (string-join (cdddr (string-split msg))))])
    (if (equal? (string-trim token) "")
      #f
      (let ([matches (filter (lambda (x) (string-contains? x token)) quotes)])
        (if (null? matches)
          (string-append "Cache miss!  Tell me about" token)
          (get-message (list-ref matches (random (length matches)))))))))

#|
Quits the server after checking permissions of the caller
|#
(define (die nick)
  ; TODO: Factor this out into some sort of auth module.
  (and (ormap (lambda (op) (equal? nick op)) OPS)
       (quit "told to die")
       (clean-up-and-quit)))

#|
Exits, updates from git, then rejoins
|#
(define (update nick)
  ; TODO: Factor this out into some sort of auth module.
  (and (ormap (lambda (op) (equal? nick op)) OPS)
       (quit "I'M GITTIN' AN UPGRADE MA")
       (clean-up-and-quit UPDATE-EXIT-CODE)))

(start-pete command-handler)
