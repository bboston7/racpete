#lang racket

(require
  "util/names-manager.rkt"
)

(provide
  ball-so-hard
)

#|
provides a command that explains to the unwashed masses how much game we really
got
|#

(define (ball-so-hard)
  (let* [(nicks (shuffle (current-nicks)))
         (len (length nicks))
         (index (random len))
         (n1 (list-ref nicks index))
         (n2 (list-ref nicks (modulo (add1 index) len)))]
    (if (zero? (random 2))
      (string-append "according to " n1 ", " n2 " can ball SO HARD")
      (string-append "according to " n1 ", " n2 " has DAT LEAN"))))
