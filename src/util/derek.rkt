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
(struct if-node (t e1 e2))
(struct bool-node (b))
(struct lt-node (l r))

(define-tokens ts (NUM BOOL))
(define-empty-tokens ets (+ - * / % ^ < LPAREN RPAREN IF THEN ELSE END FAIL))

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
         ["<" (token-<)]
         [(union
            (concatenation (repetition 1 +inf.0 numeric)
                           (repetition 0 1 ".")
                           (repetition 0 +inf.0 numeric))
            (concatenation (repetition 0 +inf.0 numeric)
                           (repetition 0 1 ".")
                           (repetition 1 +inf.0 numeric))) (token-NUM lexeme)]
         ["if" (token-IF)]
         ["then" (token-THEN)]
         ["else" (token-ELSE)]
         ["true" (token-BOOL #t)]
         ["false" (token-BOOL #f)]
         [whitespace (scanner input-port)]
         [(eof) (token-END)]
         [any-char (token-FAIL)]))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((NUM) (num-node (string->number $1)))
               ((BOOL) (bool-node $1))
               ((- E) (prec +) (minus-num-node $2))
               ((E + E) (prec +) (plus-node $1 $3))
               ((E - E) (prec -) (minus-node $1 $3))
               ((E * E) (prec *) (times-node $1 $3))
               ((E % E) (prec %) (mod-node $1 $3))
               ((E / E) (prec /) (divide-node $1 $3))
               ((E ^ E) (prec ^) (power-node $1 $3))
               ((E < E) (prec <) (lt-node $1 $3))
               ((LPAREN E RPAREN) (prec LPAREN) (paren-node $2))
               ((IF E THEN E ELSE E) (prec IF) (if-node $2 $4 $6))
               ))
    (tokens ts ets)
    (start E)
    (end END)
    (error (lambda (a b c) #f))
    (precs (nonassoc IF THEN ELSE)
           (nonassoc <)
           (left + -)
           (left * / %)
           (right ^)
           (nonassoc LPAREN RPAREN))))

;; typechecker
(define (tc ast)
  (let ([num? (lambda (x) (eq? x 'num))]
        [bool? (lambda (x) (eq? x 'bool))])
    (match ast
           [(struct num-node (_)) 'num]
           [(struct bool-node (_)) 'bool]
           [(struct plus-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct minus-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct times-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct divide-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct mod-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct power-node (l r)) (and (num? (tc l)) (num? (tc r)) 'num)]
           [(struct minus-num-node (n)) (and (num? (tc n)) 'num)]
           [(struct paren-node (e)) (tc e)]
           [(struct lt-node (l r)) (and (num? (tc l)) (num? (tc r)) 'bool)]
           [(struct if-node (t e1 e2)) (let ([tt (tc t)]
                                             [e1t (tc e1)]
                                             [e2t (tc e2)])
                                         (and (bool? tt) (eq? e1t e2t) e1t))]
           [_ (error "unrecognized case in tc")]
           )))

;; holds the result of an evaluation
(struct res (r))
(define (r->s r)
  (let ([v (res-r r)])
    (cond [(boolean? v) (if v "true" "false")]
          [(number? v) (number->string v)]
          [else (error "unrecognized type")])))

;; Interpreter: returns a res struct containing the result of evaluating the
;; program.  If the input expression is well-formed, this will never "get stuck"
(define (eval e)
  (define (eval* e)
    (match e
           [(struct num-node (n)) n]
           [(struct bool-node (b)) b]
           [(struct minus-num-node (n)) (- (eval* n))]
           [(struct plus-node (l r)) (+ (eval* l) (eval* r))]
           [(struct minus-node (l r)) (- (eval* l) (eval* r))]
           [(struct times-node (l r)) (* (eval* l) (eval* r))]
           [(struct divide-node (l r)) (/ (eval* l) (eval* r))]
           [(struct mod-node (l r)) (modulo (eval* l) (eval* r))]
           [(struct power-node (l r)) (expt (eval* l) (eval* r))]
           [(struct paren-node (e)) (eval* e)]
           [(struct lt-node (l r)) (< (eval* l) (eval* r))]
           [(struct if-node (t e1 e2)) (if (eval* t) (eval* e1) (eval* e2))]
           [_ (error "unrecognized case in eval")]
           ))
  (res (eval* e)))

;; attempts to eval the source program s with a timeout.  returns #f on failure
(define (try-eval s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (if (and ast (tc ast))
      (let* ([eng (engine (lambda (b)
                            (with-handlers
                              ([exn:fail:contract:divide-by-zero?
                                 (lambda (exn) "divide by zero")])
                              (eval ast))))]
             [ans (if (engine-run TIMEOUT eng) (engine-result eng) "timeout")])
        (cond [(string? ans) ans] ;; error message
              [ans (let* ([sans (r->s ans)]
                          [l (string-length sans)])
                     (cond [(equal? sans (car (string-split s))) #f]
                           [(> l MAX-LEN) (string-append
                                            (substring sans 0 MAX-LEN) "...")]
                           [else sans]))]
              [else #f]))
      #f)))

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
         (check-eq? (try-eval "3++4") #f)

         ;; if/else and bool tests
         (check-eq? (try-eval "true + false") #f)
         (check-eq? (try-eval "false") #f)
         (check-eq? (try-eval "true") #f)
         (check-eq? (try-eval "3.9 + false") #f)
         (check-eq? (try-eval "if false then 5 (^ 8 else) 2 + 2") #f)
         (check-equal? (try-eval "if true then 1 else 2")
                       (number->string (if #t 1 2)))
         (check-equal? (try-eval "if false then 1 else 2")
                       (number->string (if #f 1 2)))
         (check-equal? (try-eval "if true then 1 + 1 else 2 + 2")
                       (number->string (if #t (+ 1 1) (+ 2 2))))
         (check-equal? (try-eval "if false then 1 + 1 else 2 + 2")
                       (number->string (if #f (+ 1 1) (+ 2 2))))
         (check-equal? (try-eval "if true then 4^3 else 5 * 8")
                       (number->string (if #t (expt 4 3) (* 5 8))))
         (check-equal? (try-eval "if true then 4^3 else 5 * 8 + if false then 3 else 9")
                       (number->string (if #t (expt 4 3) (+ (* 5 8) (if #f 3 9)))))
         (check-equal? (try-eval "if true then false else true") "false")
         (check-equal? (try-eval "if if true then false else true then if false
                                 then true else false else if false then true
                                 else false") "false")
         (check-equal? (try-eval "4 + (if true then 9 else 8)^2")
                       (number->string (+ 4 (expt (if #t 9 8) 2))))

         ;; comparison op tests
         (check-eq? (try-eval "true < 4") #f)
         (check-eq? (try-eval "true < false") #f)
         (check-eq? (try-eval "4 < false") #f)
         (check-equal? (try-eval "10 < 20") "true")
         (check-equal? (try-eval "20 < 20") "false")
         (check-equal? (try-eval "20.5 < 20.2") "false")
         (check-equal? (try-eval "if 2 < 4 then 100 else 2")
                                 (number->string (if (< 2 4) 100 2)))
         )
