#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt"
  "urltilities.rkt"
  "ycombinator.rkt")

#|
Builds a pair of lists.  The car is quotes from the current channel log, the cdr
is urls from the current channel log
|#
(define (build-quotes-and-urls)
  (letrec ([log-file (open-input-file (string-append CHAN ".log") #:mode 'text)]
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
    (tail-fn null null)))

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
                       (string-append CHAN ".log") #:exists 'append))))

#|
Handles incomming user irc commands
|#
(define (command-handler nick msg)
  (let ([urlres (regexp-match urlregex msg)])
    (cond
      [(equal? ".q" msg) (write-to-channel (get-random-line quotes))]
      [(equal? "(y)" msg) (write-to-channel (string-append "I hate you " nick)]
      [(equal? ".link me" msg) (let* ([url (get-random-line links)]
                                     [title (get-website-title url)])
                                 (begin
                                   (write-to-channel url)
                                   (write-to-channel title)))]
      [(regexp-match #rx"^tell me about" msg)
       (let ([out (learn-about msg)])
         (and out (write-to-channel out)))]
      [(regexp-match #rx"^\\.die" msg) (write-to-channel "please don't kill me")]
      [(equal? ".ycombinator" msg) (begin (write-to-channel yc1) (write-to-channel yc2)
                                          (write-to-channel yc3) (write-to-channel yc4))]
      [urlres (let ([title (get-website-title (car urlres))])
                (begin (write-to-channel title) (log nick msg)))]
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
      (let ([matches (filter (lambda (x) (string-contains x token)) quotes)])
        (if (null? matches)
          (string-append "Cache miss!  Tell me about" token)
          (get-message (list-ref matches (random (length matches)))))))))

(start-pete command-handler)
