#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(require racket/engine)

(provide try-sarah)

#|

This is sarah an s-expression based language that pete recognizes and can evaluate

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

;; exp nodes
(struct s-node (v a))
(struct num-node (n))
(struct exp-node (e))

;; type nodes, need transparency for equal? to work
(struct numt-node () #:transparent)

(define-tokens ts (NUM VAR))
(define-empty-tokens ets (+ LPAREN RPAREN EOF FAIL))

(define-lex-abbrev identic
  (:or alphabetic
       numeric
       #\+
       #\-
       #\*
       #\/
       #\%
       #\^
       #\!
       ))

;; Scanner: returns #f on unmatchable input
(define scanner
  (lexer ["(" (token-LPAREN)]
         [")" (token-RPAREN)]
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

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((LPAREN VAR ARGS RPAREN) (s-node $2 $3))
               ((F) (exp-node $1))
               )
             (ARGS
               (() null)
               ((E ARGS) (cons $1 $2))
               )
             (F
               ((NUM) (num-node (string->number $1)))
               )
             )
    (tokens ts ets)
    (start E)
    (end EOF)
    (error (lambda (a b c) #f))
    (precs (nonassoc +)
           (nonassoc LPAREN RPAREN)
           )))

;; typechecker
 ; who needs it

;; holds the result of an evaluation
(struct res (r))

;; res struct to string
(define (r->s r)
  (let ([v (res-r r)])
    (cond [(number? v) (number->string v)]
          [else (error "unrecognized type")])))

;; Interpreter: returns a res struct containing the result of evaluating the
;; program.  If the input expression is well-formed, this will never "get stuck"
(define (eval e env)
  (define (eval-s s args)
    (match s
           ["+" (apply + args)]
           ["-" (apply - args)]
           ["*" (apply * args)]
           ["/" (apply / args)]
           ["%" (apply modulo args)]
           ["^" (apply expt args)]
           [_ (error "unrecognized s-functor")]
           ))
  (define (eval* e env)
    (match e
           [(struct num-node (n)) n]
           [(struct s-node (v a)) (eval-s v (map (lambda (e) (eval* e env)) a))]
           [(struct exp-node (e)) (eval* e env)]
           [_ (error "unrecognized case in eval")]
           ))
  (res (eval* e env)))

;; attempts to eval the source program s with a timeout.  returns #f on failure
(define (try-sarah s)
  (let* ([in (open-input-string s)]
         [gen (lambda () (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (if ast
      (let* ([eng (engine (lambda (b)
                            (with-handlers
                              ([exn:fail:contract:divide-by-zero?
                                 (lambda (exn) "divide by zero")]
                               [exn:fail?
                                 (lambda (exn) #f)])
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
         (check-equal? (try-sarah "(% 5 3 7)") #f)
         (check-equal? (try-sarah "(^ 5 3 7)") #f)
)

