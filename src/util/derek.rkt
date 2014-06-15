#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require racket/engine)

(provide try-eval)

#|

This is derek, a language of mathematical expressions that pete recognizes and
can compute.

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

(struct num-node (n))
(struct minus-num-node (n))
(struct plus-node (l r))
(struct minus-node (l r))
(struct times-node (l r))
(struct divide-node (l r))
(struct mod-node (l r))
(struct power-node (l r))
(struct paren-node (e))

(define-tokens ts (NUM))
(define-empty-tokens ets (+ - * / % ^ LPAREN RPAREN END FAIL))

;; Scanner: returns #f on unmatchable input
(define scanner
  (lexer ["+" (token-+)]
         ["-" (token--)]
         ["*" (token-*)]
         ["/" (token-/)]
         ["%" (token-%)]
         ["^" (token-^)]
         ["(" (token-LPAREN)]
         [")" (token-RPAREN)]
         [(union
            (concatenation (repetition 1 +inf.0 numeric)
                           (repetition 0 1 ".")
                           (repetition 0 +inf.0 numeric))
            (concatenation (repetition 0 +inf.0 numeric)
                           (repetition 0 1 ".")
                           (repetition 1 +inf.0 numeric))) (token-NUM lexeme)]
         [whitespace (scanner input-port)]
         [(eof) (token-END)]
         [any-char (token-FAIL)]))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((NUM) (num-node (string->number $1)))
               ((- E) (prec +) (minus-num-node $2))
               ((E + E) (prec +) (plus-node $1 $3))
               ((E - E) (prec -) (minus-node $1 $3))
               ((E * E) (prec *) (times-node $1 $3))
               ((E % E) (prec %) (mod-node $1 $3))
               ((E / E) (prec /) (divide-node $1 $3))
               ((E ^ E) (prec ^) (power-node $1 $3))
               ((LPAREN E RPAREN) (prec LPAREN) (paren-node $2))))
    (tokens ts ets)
    (start E)
    (end END)
    (error (lambda (a b c) #f))
    (precs (left + -)
           (left * / %)
           (right ^)
           (nonassoc LPAREN RPAREN))))

;; Interpreter: returns #f if src program is malformed
(define (eval e)
  (match e
         [(struct num-node (n)) n]
         [(struct minus-num-node (n)) (- (eval n))]
         [(struct plus-node (l r)) (+ (eval l) (eval r))]
         [(struct minus-node (l r)) (- (eval l) (eval r))]
         [(struct times-node (l r)) (* (eval l) (eval r))]
         [(struct divide-node (l r)) (/ (eval l) (eval r))]
         [(struct mod-node (l r)) (modulo (eval l) (eval r))]
         [(struct power-node (l r)) (expt (eval l) (eval r))]
         [(struct paren-node (e)) (eval e)]
         [#f #f]))

;; attempts to eval the source program s with a timeout.  returns #f on failure
(define (try-eval s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))]
         [eng (engine (lambda (b)
                        (with-handlers
                          ([exn:fail:contract:divide-by-zero?
                             (lambda (exn) "divide by zero")])
                          (eval ast))))]
         [ans (if (engine-run TIMEOUT eng) (engine-result eng) "timeout")])
        ;; if the user happened to just write a number, we do not want to print
        (cond [(string? ans) ans] ;; error message
              [ans (let* ([sans (number->string ans)]
                          [l (string-length sans)])
                     (cond [(equal? sans (car (string-split s))) #f]
                           [(> l MAX-LEN) (string-append
                                            (substring sans 0 MAX-LEN) "...")]
                           [else sans]))]
              [else #f])))

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
         (check-equal? (try-eval "2^4") (number->string (expt 2 4)))
         (check-equal? (try-eval "(1 - 3)") (number->string (- 1 3)))
         (check-equal? (try-eval "4 * (1 + 3)") (number->string (* 4 (+ 1 3))))
         (check-equal? (try-eval "((3 + (4 % 3))^2)")
                       (number->string (expt (+ 3 (modulo 4 3)) 2)))
         (check-equal? (try-eval "99999999^9999999") "timeout")
         (check-equal? (try-eval "9999^9999")
                       (string-append
                         (substring (number->string (expt 9999 9999)) 0 MAX-LEN)
                         "..."))
         (check-equal? (try-eval "3 ^ 3 ^ 3") (number->string (expt 3 (expt 3 3))))
         (check-eq? (try-eval "-5") #f)
         (check-equal? (try-eval "-5 + 4") (number->string (+ -5 4)))
         (check-equal? (try-eval "-5 + 6") (number->string (+ -5 6)))
         (check-equal? (try-eval "-5 * -6") (number->string (* -5 -6)))
         (check-equal? (try-eval "-55 * -69") (number->string (* -55 -69)))
         (check-equal? (try-eval "5 + -55 * -69 + 9")
                       (number->string (+ 5 (* -55 -69) 9)))
         (check-equal? (try-eval "1 + -6") (number->string (+ 1 -6)))
         (check-equal? (try-eval "-71 + 6") (number->string (+ -71 6)))
         (check-equal? (try-eval "-71^6") (number->string (- (expt 71 6))))
         (check-equal? (try-eval "-88 -99") (number->string (+ -88 -99)))
         (check-equal? (try-eval "-88 * 99") (number->string (* -88 99)))
         (check-equal? (try-eval "4 + -88 * 99") (number->string (+ 4 (* -88 99))))
         (check-equal? (try-eval "4.5 + 3.5") (number->string (+ 4.5 3.5)))
         (check-equal? (try-eval "4.5 * 3.5") (number->string (* 4.5 3.5)))
         (check-equal? (try-eval "4.7 / 3.5") (number->string (/ 4.7 3.5)))
         (check-equal? (try-eval "9.222^9.555") (number->string (expt 9.222 9.555)))
         (check-equal? (try-eval "1. + .5") (number->string (+ 1. .5)))
         (check-eq? (try-eval ".") #f)
         (check-equal? (try-eval "-0.") (number->string (- 0.)))
         (check-equal? (try-eval ".12345^9") (number->string (expt .12345 9)))
         )
