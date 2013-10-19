#!/usr/bin/racket
#lang racket

(require
  "config.rkt"
  "connection.rkt")
(require
  html
  xml
  net/url)

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
  (let ([line (string-append "<" nick ">" message)])
    (begin
      (set! quotes (cons line quotes))
      (display-to-file (string-append line "\n")
                       (string-append CHAN ".log") #:exists 'append))))

(define (print-private nick msg)
  (display (string-append nick " : " msg "\n")))

(define (get-website-title url-string)
  (letrec ([get-html (lambda (url-string)
                    (read-html (get-pure-port (string->url url-string))))]
         [get-title-tag-text
           (lambda (html-blobs)
             (cond
               ; Case 1: We didn't find a title.
               [(null? html-blobs) "OH FUCK WHERE IS THE TITLE?!!?"]

               ; Case 2: Found title node, return it.
               [(title? (car html-blobs))
                (foldl (lambda (x y)
                         (let ([pcdata-or-string
                                 (lambda (x)
                                   (if (pcdata? x) (pcdata-string x) x))])
                           (string-append (pcdata-or-string x)
                                          (pcdata-or-string y))))
                       ""
                       (html-full-content (car html-blobs)))]

               ; Case 3: No title yet, add DOM nodes to list and recurse.
               [else (get-title-tag-text
                       (append (cdr html-blobs)
                               (filter html-full?
                                       (html-full-content
                                         (car html-blobs)))))]))])
    (get-title-tag-text (list (get-html url-string)))))

;(display (string-append (get-website-title "https://www.google.com") "\n"))
;(display (string-append (get-website-title "http://www.aldkfjaldfkj.com") "\n"))
;(display (string-append (get-website-title "http://www.youtube.com/watch?v=OGzxF_qInPQ") "\n"))

#|
Handles incomming user irc commands
|#
(define (command-handler nick msg)
  (let ([urlres (regexp-match
                  #px"https?://([\\da-zA-Z.]+)\\.([a-zA-Z.]{2,6})[/\\w.-]*/?"
                  msg)])
    (begin (display (string-append msg "\n"))
    (cond
      [(equal? ".q" msg) (write-to-channel (get-random-quote))]
      [(regexp-match #rx"^\\.die" msg) (write-to-channel "please don't kill me")]
      [urlres (begin (display "hi\n") (write-to-channel (get-website-title (car
                                                                             urlres))))]
      [else (log nick msg)]))))

(define (get-random-quote)
  (list-ref quotes (random (length quotes))))

(start-pete command-handler)
