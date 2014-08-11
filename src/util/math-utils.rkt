#lang racket

(provide exact-median
         mode)

#|
Find the median of a list of numbers.
The function provided by math/statistics
doesn't properly take the median of
lists with an even number of elements.
|#
(define (exact-median lst)
  (let ([len (length lst)]
        [lst (sort lst <)])
    (if (even? len)
      (/ (+ (list-ref lst (- (/ len 2) 1))
            (list-ref lst (/ len 2)))
         2)
      (list-ref lst (quotient len 2)))))

#|
Find the mode of a list of numbers.
|#
(define (mode lst)
  (let ([counts (sort
                  (foldl
                    (lambda (new old)
                      (if (equal? new (caar old))
                        (cons (cons new (+ 1 (cdar old))) (cdr old))
                        (cons (cons new 1) old)))
                    '((#f . 0))
                    (sort lst >))
                  >
                  #:key cdr
                  #:cache-keys? #t)])
    (map car (filter (lambda (x)
                       (equal? (cdr x) (cdar counts)))
                     counts))))

(module+ test ;; no spec so there can't be bugs.  thanks ernst!
  (require rackunit)

  (check-equal? (exact-median '(1)) 1)
  (check-equal? (exact-median '(1 2)) (/ 3 2))
  (check-equal? (exact-median '(1 2 3)) 2)
  (check-equal? (exact-median '(1 2 3 4)) (/ 5 2))
  (check-equal? (exact-median '(3 2 4 1)) (/ 5 2))

  (check-equal? (mode '(1 2 2 3)) '(2))
  (check-equal? (mode '(1 1 2 3)) '(1))
  (check-equal? (mode '(1 2 3 3)) '(3))

  (check-equal? (mode '(1 2 2 3 3)) '(2 3))
  (check-equal? (mode '(2 2 1 3 3)) '(2 3))
  (check-equal? (mode '(2 2 3 3 1)) '(2 3))
  (check-equal? (mode '(2 1 2 3 3)) '(2 3))
  (check-equal? (mode '(3 1 2 3 2)) '(2 3))
)
