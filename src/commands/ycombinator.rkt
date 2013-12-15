#lang racket

(provide
  yc1
  yc2
  yc3
  yc4
  ycombo
)

#|
This module provides the lines needed to print an ascii y combinator.  The
lines are numbered in the order that they need to be printed.  I.e.
yc1 is printed first, yc2 is printed second, etc.
|#

(define yc1 "_        _       _  _                 _   _           _  _     _  _                 _   _           _  _")
(define yc2 " \\      (       |    \\               (   |             |  |   |    \\               (   |             |  |")
(define yc3 " /\\    -+-      |    /\\     \\/      -+-  |   \\/   \\/   |  |   |    /\\     \\/      -+-  |   \\/   \\/   |  |")
(define yc4 "/  \\_   |   o   |_  /  \\_   /\\   o   |   |_  /\\   /\\  _| _|   |_  /  \\_   /\\   o   |   |_  /\\   /\\  _| _|")

#|
Writes the Y combinator using write-fn.
|#
(define (ycombo write-fn . args)
  (apply write-fn yc1 args) (apply write-fn yc2 args)
  (apply write-fn yc3 args) (apply write-fn yc4 args))

