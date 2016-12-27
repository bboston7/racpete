#lang racket

(provide
  compute-kwanzaa
  compute-kwanzaa-str
)

#|
This module provides a subroutine for computing the square of the difference
between the current PST-adjusted unixtime in seconds, and the PST-adjusted
unixtime corresponding to 12:00 AM of the first day of Kwanzaa occurring in
2017 in seconds.

In some capacity, this can be thought of as the "distance to Kwanzaa", since it
is of course the square of the Euclidean norm for real numbers.
|#

(define (compute-kwanzaa)
  (let* ([utc-unixtime (current-seconds)]
        [ds (seconds->date utc-unixtime)]
        [offset (if (date-dst? ds) (* 3600 7) (* 3600 8))] ; w. coast best coast
        [when-the-hell-is-kwanzaa? (- 1514246400 offset)]) ; srsly, no one knows
    (expt (- (- utc-unixtime offset) when-the-hell-is-kwanzaa?) 2)))

(define (compute-kwanzaa-str) (number->string (compute-kwanzaa)))
