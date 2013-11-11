#lang racket

(require
  html
  xml
  net/url)

(provide
  urlregex
  get-website-title)

#|
This module provides utilities for use in processing urls
|#

#|
This is a regex that matches on urls
|#
(define urlregex #px"https?://([\\da-zA-Z.]+)\\.([a-zA-Z.]{2,6})[/\\w.a-zA-Z?=&-]*/?")

#|
Parses the website at the given url and returns the title node's contents

Parameters
    url-string - string? representation of a url.
|#
(define (get-website-title url-string)
  (letrec ([retrieved-html (with-handlers
                             ([exn:fail? (lambda (e) "")])
                             (get-pure-port (string->url url-string)))]
           [get-title-tag-text
             (lambda (html-blobs)
               (cond
                 ; Case 1: We didn't find a title.
                 [(null? html-blobs) ""]

                 ; Case 2: Found title node, return it.
                 [(title? (car html-blobs))
                  (foldr (lambda (x y)
                           (let*
                             ([symbol->str (lambda (c)
                                             (cond [(equal? c 'nbsp) "  "]
                                                   [else " "]))]
                              [webdata->string
                                (lambda (x)
                                  (cond [(pcdata? x) (pcdata-string x)]
                                        [(entity? x)
                                         (let ([ent (entity-text x)])
                                           (if (valid-char? ent)
                                             (make-string
                                               1
                                               (integer->char ent))
                                             (symbol->str ent)))]
                                        [(string? x) x]
                                        [else ""]))])
                             (string-append (webdata->string x)
                                            (webdata->string y))))
                         ""
                         (html-full-content (car html-blobs)))]

                 ; Case 3: No title yet, add DOM nodes to list and recurse.
                 [else (get-title-tag-text
                         (append (cdr html-blobs)
                                 (filter html-full?
                                         (html-full-content
                                           (car html-blobs)))))]))])
    (if (equal? retrieved-html "")
      ""
      (string-trim (get-title-tag-text (list (read-html retrieved-html)))))))
