#lang typed/racket

(provide pick-random)

#|
Picks a random element from the list
|#
(: pick-random ((Listof Any) -> Any))
(define (pick-random lst)
  (let ([len (length lst)])
    (if (zero? len)
      #f
      (list-ref lst (random (length lst))))))

