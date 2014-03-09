#lang typed/racket

(provide pick-random)

#|
Picks a random element from the list
|#
(: pick-random ((Listof Any) -> Any))
(define (pick-random ls)
  (list-ref ls (random (length ls))))

