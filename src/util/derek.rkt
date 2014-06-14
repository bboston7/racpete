#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)

(provide try-eval)

#|

This is derek, a language of mathematical expressions that pete recognizes and
can compute.

|#

(struct num-node (n))
(struct plus-node (l r))
(struct minus-node (l r))
(struct times-node (l r))
(struct divide-node (l r))
(struct mod-node (l r))

(define-tokens ts (NUM))
(define-empty-tokens ets (+ - * / % END FAIL))

;; Scanner: returns #f on unmatchable input
(define scanner
  (lexer ["+" (token-+)]
         ["-" (token--)]
         ["*" (token-*)]
         ["/" (token-/)]
         ["%" (token-%)]
         [(repetition 1 +inf.0 numeric) (token-NUM lexeme)]
         [whitespace (scanner input-port)]
         [(eof) (token-END)]
         [any-char (token-FAIL)]))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((NUM) (num-node (string->number $1)))
               ((E + E) (prec +) (plus-node $1 $3))
               ((E - E) (prec -) (minus-node $1 $3))
               ((E * E) (prec *) (times-node $1 $3))
               ((E % E) (prec %) (mod-node $1 $3))
               ((E / E) (prec /) (divide-node $1 $3))))
    (tokens ts ets)
    (start E)
    (end END)
    (error (lambda (a b c) #f))
    (precs (left + -)
           (left * / %))))

;; Interpreter: returns #f if src program is malformed
(define (eval e)
  (match e
         [(struct num-node (n)) n]
         [(struct plus-node (l r)) (+ (eval l) (eval r))]
         [(struct minus-node (l r)) (- (eval l) (eval r))]
         [(struct times-node (l r)) (* (eval l) (eval r))]
         [(struct divide-node (l r)) (/ (eval l) (eval r))]
         [(struct mod-node (l r)) (modulo (eval l) (eval r))]
         [#f #f]))

;; attempts to eval the source program s.  returns #f on failure
(define (try-eval s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (with-handlers
      ([exn:fail:contract:divide-by-zero? (lambda (exn) "divide by zero")])
      (let ([ans (eval ast)])
        ;; if the user happened to just write a number, we do not want to print
        (if ans
          (if (equal? (number->string ans) (car (string-split s)))
            #f
            (number->string ans))
          #f)))))

(module+ test ;; no spec so there can't be bugs.  thanks ernst!
         (require rackunit)
         (check-eq? (try-eval "") #f)
         (check-eq? (try-eval "482") #f)
         (check-eq? (try-eval "  482") #f)
         (check-eq? (try-eval "82    ") #f)
         (check-eq? (try-eval " hi   ") #f)
         (check-eq? (try-eval " hi 3 + 4  ") #f)
         (check-eq? (try-eval "      ") #f)
         (check-equal? (try-eval "1+1") "2")
         (check-equal? (try-eval " 1 + 1 ") "2")
         (check-equal? (try-eval "14 + 15") "29")
         (check-equal? (try-eval "4 * 9") "36")
         (check-equal? (try-eval "4 - 9") "-5")
         (check-equal? (try-eval "100 / 5") "20")
         (check-equal? (try-eval "100 % 5") "0")
         (check-equal? (try-eval "3 + 4 * 5 + 6") "29")
         (check-equal? (try-eval "9 % 3 - 6 + 3 * 6") "12")
         (check-equal? (try-eval "1 / 0") "divide by zero")
         (check-equal? (try-eval "5 % 0") "divide by zero")
         )
