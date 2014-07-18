#lang racket

(require racket/engine)

(provide convert-base
         convert-base-args)

(define TIMEOUT 2000)
(define MAX-LEN 400)

(define (char->digit c)
  (let ([zero (char->integer #\0)]
        [nine (char->integer #\9)]
        [A    (char->integer #\A)]
        [Z    (char->integer #\Z)]
        [a    (char->integer #\a)]
        [z    (char->integer #\z)]
        [d    (char->integer   c)])
    (cond
      [(and (>= d zero) (<= d nine))
         (- d zero)]
      [(and (>= d A) (<= d Z))
         (+ (- d A) 10)]
      [(and (>= d a) (<= d z))
         (+ (- d a) 10)]
      [else #f])))

(define (digit->char d)
  (let ([zero (char->integer #\0)]
        [a    (char->integer #\a)]
        [c    (integer->char   d)])
    (integer->char
      (cond
        [(and (>= d 0) (<= d 9))
           (+ d zero)]
        [(and (<= d 35))
           (+ (- d 10) a)]))))

; Better than racket's builtin
(define (string->number s base)
  (define (digit c)
    (let ([d (char->digit c)])
      (if (>= d base)
        #f d)))
  (let ([cs (string->list s)])
    (if (and (= base 1) (foldr = #\1 cs))
      (length cs)
      (foldl (lambda (x y) (+ (* base y) (digit x))) 0 cs))))

; Better than racket's builtin
(define (number->string n base)
  (define (encode n base digits)
    (if (= n 0) digits
      (let-values ([(n d) (quotient/remainder n base)])
        (encode n base (cons (digit->char d) digits)))))
  (list->string
    (if (= base 1)
      (make-list n #\1)
      (encode n base '()))))

#|
Convert str from base 'from' to base 'to'
|#
(define (convert-base str from to)
  (with-handlers ([exn:fail? (lambda (v) "No.")])
                 (number->string (string->number str from) to)))

(define (convert-base-args args)
  (let* ([eng (engine
                (lambda (b)
                  (with-handlers ([exn:fail? (lambda (v) "No.")])
                                 (match-let ([(list str from to) args])
                                   (convert-base str (string->number from 10) (string->number to 10)))
                                 )))]
         [ans (if (engine-run TIMEOUT eng) (engine-result eng) "timeout")])
    (if (> (string-length ans) MAX-LEN) (string-append
                                          (substring ans 0 MAX-LEN) "...") ans)))

; Tests
#|
Test that we can properly convert to and from various bases
|#
(module+ test
  (require rackunit)
  (check-equal? (convert-base "10" 10 2) "1010")
  (check-equal? (convert-base "1010" 2 10) "10")
  (check-equal? (convert-base "10432" 5 16) "2e6")
  (check-equal? (convert-base "2e6" 16 5) "10432")
  (check-equal? (convert-base "52345" 7 2) "11001001000111")
  (check-equal? (convert-base "11001001000111" 2 7) "52345")
  (check-equal? (convert-base "52345" 14 4) "300121021")
  (check-equal? (convert-base "300121021" 4 14) "52345")
  (check-equal? (convert-base "11111" 5 5) "11111")
  (check-equal? (convert-base "34657544345" 9 16) "2dcea44a2")
  (check-equal? (convert-base "2dcea44a2" 16 9) "34657544345")
  (check-equal? (convert-base "100" 17 14) "169")
  (check-equal? (convert-base "100" 14 17) "b9")
  (check-equal? (convert-base "10" 10 1) "1111111111")
  (check-equal? (convert-base "35" 10 36) "z")
  (check-equal? (convert-base "36" 10 36) "10")

  (check-equal? (convert-base "aoeu" 14 14) "No.")
  (check-equal? (convert-base "10" #f 14) "No.")
  (check-equal? (convert-base "500" 14 #f) "No.")
  (check-equal? (convert-base "5" 4 14) "No.")
  (check-equal? (convert-base "a" 10 14) "No.")
  (check-equal? (convert-base "344" 2 14) "No.")
  (check-equal? (convert-base "100" 1 14) "No.")
  (check-equal? (convert-base "36" 10 37) "No.")

  ; technically outside of "range" but it's still ok
  (check-equal? (convert-base "ben" 64 10) "45975")
  (check-equal? (convert-base "45975" 10 64) "ben")
  )
