#lang racket

(require math/statistics
         "../config.rkt"
         "../util/list-utils.rkt"
         "../util/math-utils.rkt")

(provide get-karma
         leaderboard
         modify-karma
         random-karma)

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
    (get-karma-string item (hash-ref karma (string->key item) key-not-found))))

#|
Returns the proper string for an item/karma pair
|#
(define (get-karma-string item karma)
  (format "~a has ~a karma point(s)" item karma))

#|
Prints the leaderboard out, calling out-fn
|#
(define (leaderboard out-fn)
  (if (>= (hash-count karma) 3)
    (let* ([assoc-list (hash->list karma)]
           [sorted (sort assoc-list (λ (x y) (< (cdr x) (cdr y))))]
           [best (take-right sorted 3)]
           [points (map cdr sorted)])
      (out-fn (format
                "Top 3: ~a (~a), ~a (~a), ~a (~a)"
                (caaddr best)
                (cdaddr best)
                (caadr best)
                (cdadr best)
                (caar best)
                (cdar best)))
      (out-fn (format
                "Bottom 3: ~a (~a), ~a (~a), ~a (~a)"
                (caar sorted)
                (cdar sorted)
                (caadr sorted)
                (cdadr sorted)
                (caaddr sorted)
                (cdaddr sorted)))
      (out-fn (format
                "Mean: ~a, Median: ~a, Mode: ~a, StdDev: ~a"
                (real->decimal-string (mean points))
                (real->decimal-string (exact-median points))
                (mode points)
                (real->decimal-string (stddev points)))))
    (out-fn "not enough items for a leaderboard")))

#|
Returns the karma associated with a random item
|#
(define (random-karma)
  (if (zero? (hash-count karma))
    "no items in karma table, use ++ and -- to assign karma to things!"
    (let ([item (pick-random (hash->list karma))])
      (get-karma-string (car item) (cdr item)))))
