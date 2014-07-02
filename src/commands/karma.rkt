#lang racket

(require "../config.rkt")

(provide get-karma
         modify-karma)

(define FILE_NAME (format "logs/~a.karma" CHAN))

(define karma
  (if (file-exists? FILE_NAME)
    (hash-copy (with-input-from-file FILE_NAME read))
    (make-hash)))

(define (key-not-found) 0)

#|
Convert a string item to a key in the karma hash table
|#
(define (string->key str)
    (string-downcase (string-normalize-spaces str #px"\\s+" "-")))

#|
Modify the karma of item

Parameters:
    item      - Item to modify the karma of
    direction - 'incr or 'decr to increment or decrement the karma of item
|#
(define (modify-karma item direction)
  (unless (equal? item "")
    (let* ([delta (match direction
                    ['incr 1]
                    ['decr -1]
                    [_ (raise "direction was not 'incr or 'decr")])]
           [key (string->key item)]
           [current (hash-ref karma key key-not-found)])
      (hash-set! karma key (+ current delta)))
    (write-to-file karma FILE_NAME #:exists 'truncate/replace)))

#|
Returns the karma associated with item
|#
(define (get-karma item)
  (if (equal? item "")
    #f
    (format "~a has ~a karma point(s)"
      item
      (hash-ref karma (string->key item) key-not-found))))
