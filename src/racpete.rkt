#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt"
  "urltilities.rkt")

#|
Builds a list of quotes from the current channel log
|#
(define (build-quotes)
  (letrec ([log-file (open-input-file (string-append CHAN ".log") #:mode 'text)]
         ; Tail recursive because log files get large!
         ; Unfortunately, that means the list is backwards, this doesn't matter
         ; now, and since we don't store date information in our logs, maybe it
         ; never will
         [tail-fn
           (lambda (acc)
             (let ([line (read-line log-file)])
               (if (eof-object? line)
                 acc
                 (tail-fn (cons line acc)))))])
    (tail-fn null)))

(define quotes (build-quotes))

#|
Log a line from the chat
|#
(define (log nick message)
  (let ([line (string-append "<" nick "> " message)])
    (begin
      (set! quotes (cons line quotes))
      (display-to-file (string-append line "\n")
                       (string-append CHAN ".log") #:exists 'append))))

#|
Handles incomming user irc commands
|#
(define (command-handler nick msg)
  (let ([urlres (regexp-match urlregex msg)])
    (cond
      [(equal? ".q" msg) (write-to-channel (get-random-quote))]
      [(regexp-match #rx"^tell me about" msg)
       (let ([out (learn-about msg)])
         (and out (write-to-channel out)))]
      [(regexp-match #rx"^\\.die" msg) (write-to-channel "please don't kill me")]
      [urlres (let ([title (get-website-title (car urlres))])
                (begin (if (equal? title "") "" (write-to-channel title))
                       (log nick msg)))]
      [else (log nick msg)])))

#|
Returns the message portion of an irc log line
|#
(define (get-message line)
  (string-join (cdr (string-split line))))

#|
Returns a true value if token is in str
|#
(define (string-contains str token)
  (regexp-match (regexp (string-downcase token)) (string-downcase str)))

#|
Returns a random quote from the logs
|#
(define (get-random-quote)
  (list-ref quotes (random (length quotes))))


#|
Given a string, returns a quote containing that string
|#
(define (learn-about msg)
  (let ([token (string-append " " (string-join (cdddr (string-split msg))))])
    (if (equal? (string-trim token) "")
      #f
      (let ([matches (filter (lambda (x) (string-contains x token)) quotes)])
        (if (null? matches)
          (string-append "Cache miss!  Tell me about" token)
          (get-message (list-ref matches (random (length matches)))))))))

(start-pete command-handler)
