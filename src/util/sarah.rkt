#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(require racket/engine)

(provide try-sarah
         write-polanski)

#|

This is sarah an s-expression based language that pete recognizes and can evaluate

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

;; exp nodes
(struct s-node (e as))
(struct var-node (v))
(struct let-node (vs e))
(struct let-arg-node (v e))
(struct lambda-node (a e))
(struct builtin-node (s as))
(struct num-node (n))
(struct exp-node (e))

(struct closure (e env a))

(define-tokens ts (NUM VAR BUILTIN))
(define-empty-tokens ets (LPAREN RPAREN LPAREN! RPAREN! LET LAMBDA
                          EOF FAIL))

(define-lex-abbrev identic
  (:or alphabetic
       numeric
       ))

;; Scanner: returns #f on unmatchable input
(define scanner
  (lexer ["(" (token-LPAREN)]
         [")" (token-RPAREN)]
         ["[" (token-LPAREN!)]
         ["]" (token-RPAREN!)]
         ["let" (token-LET)]
         ["lambda" (token-LAMBDA)]
         ["+" (token-BUILTIN lexeme)]
         ["-" (token-BUILTIN lexeme)]
         ["*" (token-BUILTIN lexeme)]
         ["/" (token-BUILTIN lexeme)]
         ["%" (token-BUILTIN lexeme)]
         ["^" (token-BUILTIN lexeme)]
         [(::
            (:- identic numeric)
            (:* identic)) (token-VAR lexeme)]
         [(:or
            (:: (:+ numeric)
                (:? ".")
                (:* numeric))
            (:: (:* numeric)
                (:? ".")
                (:+ numeric))) (token-NUM lexeme)]
         [whitespace (scanner input-port)]
         [(eof) (token-EOF)]
         [any-char (token-FAIL)]))

;; Scanner for romanpolanski
 ; matches all input
(define scanner-rp
  (compose reverse string->list))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((LPAREN LET LPAREN LETARGS RPAREN E RPAREN) (let-node $4 $6))
               ((LPAREN LAMBDA LPAREN VAR RPAREN E RPAREN) (lambda-node $4 $6))
               ((LPAREN BUILTIN ARGS RPAREN) (builtin-node $2 $3))
               ((LPAREN E ARGS RPAREN) (s-node $2 $3))
               ((NUM) (num-node (string->number $1)))
               ((VAR) (var-node $1))
               )
             (ARGS
               (() null)
               ((E ARGS) (cons $1 $2))
               )
             (LETARGS
               (() null)
               ((LETARG LETARGS) (cons $1 $2))
               )
             (LETARG
               ((LPAREN! VAR E RPAREN!) (let-arg-node $2 $3))
               )
             )
    (tokens ts ets)
    (start E)
    (end EOF)
    (error (lambda (a b c) #f))
    (precs (nonassoc LPAREN RPAREN)
           (nonassoc LPAREN! RPAREN!)
           )))

;; Parser for romanpolanski into sarah's AST
 ; returns #f on parsing failure
(define (parse-rp cs)
  (define (parse-rp* cs)
    (define-syntax-rule (pop x stack body)
      (let ([stack (parse-rp* stack)])
        (if (pair? stack)
          (let ([x (car stack)]
                [stack (cdr stack)])
            body)
          #f)))
    (define-syntax-rule (vpop x stack body)
      (if (pair? stack)
        (let ([x (car stack)]
              [stack (cdr stack)])
          body)
        #f))
    (match cs
      ['() '()]
      [(cons c stack)
       (cond
         [(equal? #\+ c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "+" (list x y)) stack)))]
         [(equal? #\- c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "-" (list x y)) stack)))]
         [(equal? #\* c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "*" (list x y)) stack)))]
         [(equal? #\/ c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "/" (list x y)) stack)))]
         [(equal? #\% c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "%" (list x y)) stack)))]
         [(equal? #\^ c) (pop y stack
                         (pop x stack
                              (cons (builtin-node "^" (list x y)) stack)))]
         [(equal? #\~ c) (pop x stack
                              (cons (builtin-node "-" (list (num-node 0) x)) stack))]
         [(equal? #\, c) (pop y stack
                         (pop x stack
                              (cons x (cons y stack))))]
         [(equal? #\: c) (vpop v stack
                         (pop e stack
                         (pop b stack
                              (cons (let-node (list (let-arg-node v e)) b) stack))))]
         [(equal? #\\ c) (vpop a stack
                         (pop b stack
                              (cons (lambda-node a b) stack)))]
         [(equal? #\$ c) (pop l stack
                         (pop v stack
                              (cons (s-node l (list v)) stack)))]
         [(char? c)
          (cond
            [(char-numeric? c) (cons (num-node (string->number (string c))) stack)]
            [else (cons (var-node c) stack)]
            )]
         [else (cons c stack)]
         )]
      [_ #f]
      ))
  (parse-rp* cs))

;; typechecker
 ; who needs it

;; holds the result of an evaluation
(struct res (r))

;; res struct to string
(define (r->s r)
  (let ([v (res-r r)])
    (cond [(number? v) (number->string v)]
          [(closure? v) "<lambda>"]
          [else (error "unrecognized type")])))

;; Interpreter: returns a res struct containing the result of evaluating the
;; program.  If the input expression is well-formed, this will never "get stuck"
(define (eval e env)
  (define (eval-b s args)
    (match s
           ["+" (apply + args)]
           ["-" (apply - args)]
           ["*" (apply * args)]
           ["/" (apply / args)]
           ["%" (apply modulo args)]
           ["^" (apply expt args)]
           [_ (error "unrecognized b-functor")]
           ))
  (define (eval-s s args env)
    (match s
           [(struct closure (e env a))
            (let ([l (length args)])
              (if (not (equal? l 1))
                (raise-arity-error (string->symbol a) l)
              (eval* e (cons `(,a . ,(car args)) env))))]
           [_ (error "unrecognized s-functor")]
           ))
  (define (add-env vs env)
    (match vs
           ['() env]
           [(cons (struct let-arg-node (v e)) rst)
            (cons `(,v . ,(eval* e env)) (add-env rst env))]
           ))
  (define (eval* e env)
    (match e
           [(struct num-node (n)) n]
           [(struct var-node (v)) (cdr (assoc v env))]
           [(struct lambda-node (a e)) (closure e env a)]
           [(struct builtin-node (s as)) (eval-b s (map (lambda (a) (eval* a env)) as))]
           [(struct s-node (e as)) (eval-s (eval* e env)
                                           (map (lambda (a) (eval* a env)) as)
                                           env)]
           [(struct exp-node (e)) (eval* e env)]
           [(struct let-node (vs e)) (eval* e (add-env vs env))]
           [_ (error "unrecognized case in eval")]
           ))
  (res (eval* e env)))

;; attempts to eval the source program s with a timeout.  returns #f on failure
(define (try-eval-sarah s ast)
  (if ast
    (let* ([eng (engine (lambda (b)
                          (with-handlers
                            ([exn:fail:contract:divide-by-zero?
                               (lambda (exn) "divide by zero")]
                             [exn:fail:contract:arity?
                               (lambda (exn) "arity mismatch")]
                             [exn:fail?
                               (lambda (exn) #f)])
                            (eval ast null))))]
           [ans (if (engine-run TIMEOUT eng) (engine-result eng) "timeout")])
      (cond [(string? ans) ans] ;; error message
            [ans (let* ([sans (r->s ans)]
                        [l (string-length sans)])
                   (cond [(and (string? s) (equal? sans (car (string-split s)))) #f]
                         [(> l MAX-LEN) (string-append
                                          (substring sans 0 MAX-LEN) "...")]
                         [else sans]))]
            [else #f]))
    #f))

;; parses sarah and attempts to run it
(define (try-sarah s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (try-eval-sarah s ast)))

;; parses romanpolanski and attempts to run it
(define (try-polanski s)
  (let* ([cs (scanner-rp s)]
         [ast (with-handlers
                ([exn:fail? (lambda (exn) #f)])
                (parse-rp cs))])
    (if ast
      (try-eval-sarah #f (car ast))
      #f)))

(define (write-polanski s fn)
  (cond
    [(try-polanski s) => fn]
    [else #f]))


(module+ test ;; no spec so there can't be bugs.  thanks ernst!
         (require rackunit)
         (check-eq? (try-sarah "") #f)
         (check-eq? (try-sarah "482") #f)
         (check-eq? (try-sarah "  482") #f)
         (check-eq? (try-sarah "82    ") #f)
         (check-eq? (try-sarah " hi   ") #f)
         (check-eq? (try-sarah " hi 3 + 4  ") #f)
         (check-eq? (try-sarah "      ") #f)
         (check-equal? (try-sarah "(but whatever man)") #f)

         (check-equal? (try-sarah "(+ 1 1)") "2")
         (check-equal? (try-sarah " ( + 1 1 ) ") "2")

         (check-equal? (try-sarah "(++ 1 1)") #f)

         (check-equal? (try-sarah "[+ 1 1]") #f)
         (check-equal? (try-sarah " [ + 1 1 ] ") #f)
         (check-equal? (try-sarah "[+ 1 1)") #f)

         (check-equal? (try-sarah "(* 4 9)") "36")

         (check-equal? (try-sarah "(+ 5 2 6 7)")
                       (number->string (+ 5 2 6 7)))
         (check-equal? (try-sarah "(- 5 2 6 7)")
                       (number->string (- 5 2 6 7)))
         (check-equal? (try-sarah "(* 5 2 6 7)")
                       (number->string (* 5 2 6 7)))
         (check-equal? (try-sarah "(/ 5 2 6 7)")
                       (number->string (/ 5 2 6 7)))
         (check-equal? (try-sarah "(% (+ 5 2) (* 6 7))")
                       (number->string (modulo (+ 5 2) (* 6 7))))
         (check-equal? (try-sarah "(^ (- 5 2) (/ 6 7))")
                       (number->string (expt (- 5 2) (/ 6 7))))

         (check-equal? (try-sarah "(x 4 9)") #f)
         (check-equal? (try-sarah "(x+*-y! 4 9)") #f)
         (check-equal? (try-sarah "(% 5 3 7)") "arity mismatch")
         (check-equal? (try-sarah "(^ 5 3 7)") "arity mismatch")

         (check-equal? (try-sarah "(let ([x 5]) (+ 2 3))") "5")
         (check-equal? (try-sarah "(let ([x 5]) (+ x x))")
                       (number->string (let ([x 5]) (+ x x))))
         (check-equal? (try-sarah "(let ([x 5]) (let ([y 6]) (* x y)))")
                       (number->string (let ([x 5] [y 6]) (* x y))))
         (check-equal? (try-sarah "(let ([x 5] [y 6]) (* x y))")
                       (number->string (let ([x 5] [y 6]) (* x y))))

         (check-equal? (try-sarah
                         "((lambda (x) (+ x 1)) 3)")
                       (number->string
                         ((lambda (x) (+ x 1)) 3)))
         (check-equal? (try-sarah
                         "((lambda (x) (+ x 1)) 3 5)")
                       "arity mismatch")
         (check-equal? (try-sarah "((lambda (x) 4))") "arity mismatch")

         (check-equal? (try-sarah
                         "(let ([fn (lambda (x) (* x 2))] [y 3]) (fn y))")
                       (number->string
                         (let ([fn (lambda (x) (* x 2))] [y 3]) (fn y))))
         (check-equal? (try-sarah
                         "(let ([x (lambda (x) (lambda (y) (+ x y)))]) ((x 3) 5))")
                       (number->string
                         (let ([x (lambda (x) (lambda (y) (+ x y)))]) ((x 3) 5))))
         (check-equal? (try-sarah
                         "(let ([x (lambda (x) (lambda (y) (+ x y)))]) (x 3))")
                       "<lambda>")
         (check-equal? (try-sarah
                         "(let ([x (lambda (x) (lambda (y) (+ x y)))]) (x 3 5))")
                       "arity mismatch")


         (check-equal? (try-polanski "2") "2")
         (check-equal? (try-polanski "2~") "-2")
         (check-equal? (try-polanski "2~~") "2")
         (check-equal? (try-polanski "23") "3")
         (check-equal? (try-polanski "234+") "7")
         (check-equal? (try-polanski "3~4+~") "-1")
         (check-equal? (try-polanski "2 3 +") #f)
         (check-equal? (try-polanski "23,") "2")

         (check-equal? (try-polanski "23+") "5")
         (check-equal? (try-polanski "234++") "9")
         (check-equal? (try-polanski "23+45++")
                       (number->string (+ (+ 2 3) (+ 4 5))))
         (check-equal? (try-polanski "23-") "-1")
         (check-equal? (try-polanski "23,-") "1")
         (check-equal? (try-polanski "432--") "3")
         (check-equal? (try-polanski "32-45-+")
                       (number->string (+ (- 3 2) (- 4 5))))
         (check-equal? (try-polanski "23*") "6")
         (check-equal? (try-polanski "432**") "24")
         (check-equal? (try-polanski "32*45-+")
                       (number->string (+ (* 3 2) (- 4 5))))
         (check-equal? (try-polanski "52/6/7/")
                       (number->string (/ 5 2 6 7)))
         (check-equal? (try-polanski "23^") "8")
         (check-equal? (try-polanski "52+67*%")
                       (number->string (modulo (+ 5 2) (* 6 7))))
         (check-equal? (try-polanski "52-67/^")
                       (number->string (expt (- 5 2) (/ 6 7))))

         (check-equal? (try-polanski "52x67/^") #f)

         (check-equal? (try-polanski "23+6x:") "5")
         (check-equal? (try-polanski "x3+6x:")
                       (number->string (let ([x 6]) (+ x 3))))
         (check-equal? (try-polanski "xx+5x:")
                       (number->string (let ([x 5]) (+ x x))))
         (check-equal? (try-polanski "xy*6y:5x:")
                       (number->string (let ([x 5] [y 6]) (* x y))))

         (check-equal? (try-polanski "3x1+x\\") "<lambda>")
         (check-equal? (try-polanski "3x1+x\\$")
                       (number->string
                         ((lambda (x) (+ x 1)) 3)))
         (check-equal? (try-polanski "yf$3y:x2*x\\f:")
                       (number->string
                         (let ([fn (lambda (x) (* x 2))] [y 3]) (fn y))))
         (check-equal? (try-polanski "53x$$xy+y\\x\\x:")
                       (number->string
                         (let ([x (lambda (x) (lambda (y) (+ x y)))]) ((x 3) 5))))
         (check-equal? (try-polanski "3x$xy+y\\x\\x:")
                       "<lambda>")
)

