#lang typed/racket

(provide pick-random)

#|
Picks a random element from the list
|#
(: pick-random (All (A) ((Listof A) -> (Option A))))
(define (pick-random lst)
  (let ([len (length lst)])
    (if (zero? len)
      #f
      (list-ref lst (random (length lst))))))

