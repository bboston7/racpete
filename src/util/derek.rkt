#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require racket/engine)

(provide try-eval)

#|

This is derek, an ML-like language that pete recognizes and can evaluate

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

;; exp nodes
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
(struct let-in-node (i e1 e2))
(struct var-node (i))
(struct lambda-node (x t e))
(struct app-node (e1 e2))
(struct exp-node (e))

;; type nodes
(struct numt-node () #:transparent)
(struct boolt-node () #:transparent)
(struct arrowt-node (l r) #:transparent)

(struct closure (e env arg))

(define-tokens ts (NUM BOOL VAR))
(define-empty-tokens ets (+ - * / % ^ < = : LPAREN RPAREN IF THEN ELSE LET IN
                            LETEND LAMBDA ARROW BOOLT NUMT DOT APP END FAIL))

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
         ["=" (token-=)]
         ["." (token-DOT)]
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
         ["let" (token-LET)]
         ["in" (token-IN)]
         ["end" (token-LETEND)]
         ["\\" (token-LAMBDA)]
         [":" (token-:)]
         ["->" (token-ARROW)]
         ["bool" (token-BOOLT)]
         ["num" (token-NUMT)]
         [(repetition 1 +inf.0 alphabetic) (token-VAR lexeme)]
         [whitespace (scanner input-port)]
         [(eof) (token-END)]
         [any-char (token-FAIL)]))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((IF E THEN E ELSE E) (prec IF) (if-node $2 $4 $6))
               ((LAMBDA VAR : TYPE DOT E) (prec LAMBDA) (lambda-node $2 $4 $6))
               ((- E) (prec +) (minus-num-node $2))
               ((E + E) (prec +) (plus-node $1 $3))
               ((E - E) (prec -) (minus-node $1 $3))
               ((E * E) (prec *) (times-node $1 $3))
               ((E % E) (prec %) (mod-node $1 $3))
               ((E / E) (prec /) (divide-node $1 $3))
               ((E ^ E) (prec ^) (power-node $1 $3))
               ((E < E) (prec <) (lt-node $1 $3))
               ((A) (exp-node $1))
               )
             (A
               ((F A) (app-node $1 $2))
               ((F) (exp-node $1))
               )
             (F
               ((NUM) (num-node (string->number $1)))
               ((BOOL) (bool-node $1))
               ((VAR) (var-node $1))
               ((LPAREN E RPAREN) (prec LPAREN) (paren-node $2))
               ((LET VAR = E IN E LETEND) (prec LET) (let-in-node $2 $4 $6))
               )
             (TYPE
               ((BOOLT) (boolt-node))
               ((NUMT) (numt-node))
               ((TYPE ARROW TYPE) (prec ARROW) (arrowt-node $1 $3))
               ((LPAREN TYPE RPAREN) (prec LPAREN) (paren-node $2))
               )
             )
    (tokens ts ets)
    (start E)
    (end END)
    (error (lambda (a b c) #f))
    (precs (left APP)
           (nonassoc IF)
           (nonassoc LET)
           (nonassoc LAMBDA)
           (right ARROW)
           (nonassoc <)
           (left + -)
           (left * / %)
           (right ^)
           (nonassoc LPAREN RPAREN)
           )))

;; typechecker
(define (tc ast env)
  (let ([num (numt-node)]
        [num? numt-node?]
        [bool (boolt-node)]
        [bool? boolt-node?]
        [arrow? arrowt-node?])
    (match ast
           [(struct num-node (_)) num]
           [(struct bool-node (_)) bool]
           [(struct plus-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct minus-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct times-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct divide-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct mod-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct power-node (l r)) (and (num? (tc l env)) (num? (tc r env)) num)]
           [(struct minus-num-node (n)) (and (num? (tc n env)) num)]
           [(struct paren-node (e)) (tc e env)]
           [(struct lt-node (l r)) (and (num? (tc l env)) (num? (tc r env)) bool)]
           [(struct if-node (t e1 e2)) (let ([tt (tc t env)]
                                             [e1t (tc e1 env)]
                                             [e2t (tc e2 env)])
                                         (and (bool? tt) (equal? e1t e2t) e1t))]
           [(struct let-in-node (i e1 e2)) (tc e2 (cons `(,i . ,(tc e1 env)) env))]
           [(struct var-node (i)) (let ([t (assoc i env)]) (and t (cdr t)))]
           [(struct lambda-node (x t e)) (let ([rt (tc e (cons `(,x . ,t) env))])
                                           (and rt (arrowt-node t rt)))]
           [(struct app-node (e1 e2)) (let ([t2 (tc e2 env)]
                                            [t1 (tc e1 env)])
                                        (and (arrow? t1)
                                             (equal? (arrowt-node-l t1) t2)
                                             (arrowt-node-r t1)))]
           [(struct exp-node (e)) (tc e env)]
           [_ (error "unrecognized case in tc")]
           )))

;; holds the result of an evaluation
(struct res (r))

;; res struct to string
(define (r->s r)
  (let ([v (res-r r)])
    (cond [(boolean? v) (if v "true" "false")]
          [(number? v) (number->string v)]
          [(closure? v) "<fun>"]
          [else (error "unrecognized type")])))

;; Interpreter: returns a res struct containing the result of evaluating the
;; program.  If the input expression is well-formed, this will never "get stuck"
(define (eval e env)
  (define (eval* e env)
    (match e
           [(struct num-node (n)) n]
           [(struct bool-node (b)) b]
           [(struct lambda-node (x t e)) (closure e env x)]
           [(struct var-node (i)) (cdr (assoc i env))]
           [(struct minus-num-node (n)) (- (eval* n env))]
           [(struct plus-node (l r)) (+ (eval* l env) (eval* r env))]
           [(struct minus-node (l r)) (- (eval* l env) (eval* r env))]
           [(struct times-node (l r)) (* (eval* l env) (eval* r env))]
           [(struct divide-node (l r)) (/ (eval* l env) (eval* r env))]
           [(struct mod-node (l r)) (modulo (eval* l env) (eval* r env))]
           [(struct power-node (l r)) (expt (eval* l env) (eval* r env))]
           [(struct paren-node (e)) (eval* e env)]
           [(struct lt-node (l r)) (< (eval* l env) (eval* r env))]
           [(struct if-node (t e1 e2)) (if (eval* t env) (eval* e1 env) (eval* e2 env))]
           [(struct let-in-node (i e1 e2)) (eval* e2 (cons `(,i . ,(eval* e1 env)) env))]
           [(struct app-node (e1 e2)) (let ([arg (eval* e2 env)]
                                            [cl (eval* e1 env)])
                                        (eval* (closure-e cl)
                                               (cons `(,(closure-arg cl) . ,arg) (closure-env cl))))]
           [(struct exp-node (e)) (eval* e env)]
           [_ (error "unrecognized case in eval")]
           ))
  (res (eval* e env)))

;; attempts to eval the source program s with a timeout.  returns #f on failure
(define (try-eval s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (if (and ast (tc ast null))
      (let* ([eng (engine (lambda (b)
                            (with-handlers
                              ([exn:fail:contract:divide-by-zero?
                                 (lambda (exn) "divide by zero")])
                              (eval ast null))))]
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

         ;; let in variable tests
         (check-eq? (try-eval "let x = 4 in y end") #f)
         (check-eq? (try-eval "let x = a in y end") #f)
         (check-eq? (try-eval "let x = a in x") #f)
         (check-eq? (try-eval "let 5 = 4 in 4 end") #f)
         (check-equal? (try-eval "let x = 4 in x end")
                       (number->string (let ([x 4]) x)))
         (check-equal? (try-eval "let x = 4 in x * 4 end")
                       (number->string (let ([x 4]) (* x 4))))
         (check-equal? (try-eval "33 + let x = 4 in x * 4 end")
                       (number->string (+ 33 (let ([x 4]) (* x 4)))))
         (check-equal? (try-eval "let x = 4 in if x < 5 then x else 44 end")
                       (number->string (let ([x 4]) (if (< x 5) x 44))))
         (check-equal? (try-eval "let x = 4 in 2 end")
                       (number->string (let ([x 4]) 2)))
         (check-equal? (try-eval "let x = 4 in x + x end")
                       (number->string (let ([x 4]) (+ x x))))
         (check-equal? (try-eval "let y = 5 in let x = 4 in y + x end end")
                       (number->string (let ([y 5]) (let ([x 4]) (+ y x)))))
         (check-equal? (try-eval "let y = 5 in let y = 4 in y^2 end end")
                       (number->string (let ([y 5]) (let ([y 4]) (expt y 2)))))
         (check-equal? (try-eval "let maxiscool = 5 in maxiscool+1 end")
                       (number->string (let ([maxiscool 5]) (+ maxiscool 1))))

         ;; function tests
         (check-equal? (try-eval "\\ x : num . x + 100") "<fun>")
         (check-eq? (try-eval "let x = \\ x : bool . x + 100 in x end") #f)
         (check-equal? (try-eval "let x = \\ x : num . if true then x + 100
                                 else 4 in x end") "<fun>")
         (check-equal? (try-eval "\\ x : (num -> num -> num) . x") "<fun>")
         (check-equal? (try-eval "(\\ x : num . x + 1) 5")
                       (number->string (add1 5)))
         (check-eq? (try-eval "\\ x : num . x + 1 5") #f)
         (check-equal? (try-eval "let f = \\ x : num . x + 4 in f 5 end")
                       (number->string (let ([f (lambda (x) (+ x 4))]) (f 5))))
         (check-equal? (try-eval "let f = \\ y :bool . if y then 1 else 2 in f true end")
                       (number->string (let ([f (lambda (y) (if y 1 2))]) (f #t))))
         (check-equal? (try-eval "(if 3 < 5 then \\ x : num . x else \\ y : num . y + 1) 5")
                       (number->string ((if (< 3 5) identity add1) 5)))
         (check-eq? (try-eval "(if 3 < 5 then \\ x : bool . x else \\ y : num .
                                 y + 1) 5") #f)
         (check-eq? (try-eval "(if 3 < 5 then \\ x : num . true else \\ y : num .
                                 y + 1) 5") #f)
         (check-eq? (try-eval "(if 3 < 5 then \\ x : num . x else \\ y : num .
                                 x + 1) 5") #f)
         (check-equal? (try-eval "((\\ f : num -> num . \\ x : num . f x)
                                  (\\ x : num . x + 1)) 6")
                       (number->string (apply add1 '(6))))
         )
