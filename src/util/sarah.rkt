#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(require racket/engine)

(provide try-sarah
         write-sp)

#|

This is sarah, an s-expression based language that pete recognizes and can evaluate

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

;; exp nodes
(struct s-node (e as))
(struct var-node (v))
(struct let-node (t vs e))
(struct let-arg-node (v e))
(struct lambda-node (as e))
(struct if-node (p t f))
(struct builtin-node (s as))
(struct num-node (n))
(struct bool-node (b))
(struct exp-node (e))
(struct nil-node ())

;; need mutability to implement let rec
(struct closure (e env as) #:mutable)

(define-tokens ts (NUM VAR BUILTIN LET))
(define-empty-tokens ets (LPAREN RPAREN LPAREN! RPAREN! LAMBDA THUNK
                          IF TRUE FALSE NIL EOF FAIL))

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
         ["let" (token-LET 'let)]
         ["let*" (token-LET 'let*)]
         ["letrec" (token-LET 'letrec)]
         ["letrec*" (token-LET 'letrec*)]
         ["lambda" (token-LAMBDA)]
         ["thunk" (token-THUNK)]
         ["if" (token-IF)]
         [(:or
            "+" "-" "*" "/" "%" "^"
            "<" "<=" "=" "!=" ">=" ">" "!" "&" "|"
            "cons" "car" "cdr" "list"
            )
          (token-BUILTIN lexeme)]
         ["#t" (token-TRUE)]
         ["#f" (token-FALSE)]
         ["nil" (token-NIL)]
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

;; Scanner for sarahpolanski
 ; matches all input
(define scanner-sp
  (compose reverse string->list))

;; Parser: returns #f on parsing failure
(define parse
  (parser
    (grammar (E
               ((LPAREN LET LPAREN LETARGS RPAREN E RPAREN) (let-node $2 $4 $6))
               ((LPAREN LAMBDA LPAREN VARS RPAREN E RPAREN) (lambda-node $4 $6))
               ((LPAREN THUNK E RPAREN) (lambda-node '() $3))
               ((LPAREN IF E E E RPAREN) (if-node $3 $4 $5))
               ((LPAREN BUILTIN ARGS RPAREN) (builtin-node $2 $3))
               ((LPAREN E ARGS RPAREN) (s-node $2 $3))
               ((LITERAL) $1)
               ((VAR) (var-node $1))
               )
             (VARS
               (() null)
               ((VAR VARS) (cons $1 $2))
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
             (LITERAL
               ((NUM) (num-node (string->number $1)))
               ((TRUE) (bool-node #t))
               ((FALSE) (bool-node #f))
               ((NIL) (nil-node))
               )
             )
    (tokens ts ets)
    (start E)
    (end EOF)
    (error (lambda (a b c) #f))
    (precs (nonassoc LPAREN RPAREN)
           (nonassoc LPAREN! RPAREN!)
           )))

;; Parser for sarahpolanski into sarah's AST
 ; returns #f on parsing failure
(define (parse-sp cs)
  (define (parse-sp* cs)
    (define-syntax-rule (pop x stack body)
      (let ([stack (parse-sp* stack)])
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
    (define-syntax-rule (bool->num body)
      (if-node body
               (num-node 1)
               (num-node 0)))
    (define-syntax-rule (num->bool body)
      (if-node (builtin-node "=" (list (num-node 0) body))
               (bool-node #f)
               (bool-node #t)))
    (define-syntax-rule (op s stack)
      (pop y stack
      (pop x stack
           (cons (builtin-node s (list x y)) stack))))
    (define-syntax-rule (bool-op s stack)
      (pop y stack
      (pop x stack
           (cons (bool->num (builtin-node s (list x y))) stack))))
    (match cs
      ['() '()]
      [(cons c stack)
       (cond
         [(equal? #\+ c) (op "+" stack)]
         [(equal? #\- c) (op "-" stack)]
         [(equal? #\* c) (op "*" stack)]
         [(equal? #\/ c) (op "/" stack)]
         [(equal? #\% c) (op "%" stack)]
         [(equal? #\^ c) (op "^" stack)]
         [(equal? #\~ c) (pop x stack
                              (cons (builtin-node "-" (list (num-node 0) x)) stack))]
         [(equal? #\< c) (bool-op "<" stack)]
         [(equal? #\= c) (bool-op "=" stack)]
         [(equal? #\> c) (bool-op ">" stack)]
         [(equal? #\! c) (pop x stack
                              (cons (if-node (builtin-node "=" (list (num-node 0) x))
                                             (num-node 1)
                                             (num-node 0)) stack))]
         [(equal? #\& c) (pop x stack
                         (pop y stack
                              (cons (bool->num (builtin-node "&"
                                                 (list (num->bool x)
                                                       (num->bool y)))) stack)))]
         [(equal? #\| c) (pop x stack
                         (pop y stack
                              (cons (bool->num (builtin-node "|"
                                                 (list (num->bool x)
                                                       (num->bool y)))) stack)))]
         [(equal? #\? c) (pop p stack
                         (pop t stack
                         (pop f stack
                              (cons (if-node (num->bool p) t f) stack))))]
         [(equal? #\, c) (pop y stack
                         (pop x stack
                              (cons x (cons y stack))))]
         [(equal? #\: c) (vpop v stack
                         (pop e stack
                         (pop b stack
                              (cons (let-node 'let (list (let-arg-node v e)) b) stack))))]
         [(equal? #\# c) (vpop v stack
                         (pop e stack
                         (pop b stack
                              (cons (let-node 'letrec (list (let-arg-node v e)) b) stack))))]
         [(equal? #\\ c) (vpop a stack
                         (pop b stack
                              (cons (lambda-node (list a) b) stack)))]
         [(equal? #\$ c) (pop f stack
                         (pop x stack
                              (cons (s-node f (list x)) stack)))]
         [(equal? #\. c) (op "cons" stack)]
         [(equal? #\[ c) (pop c stack
                              (cons (builtin-node "car" (list c)) stack))]
         [(equal? #\] c) (pop c stack
                              (cons (builtin-node "cdr" (list c)) stack))]
         [(char? c)
          (cond
            [(char-numeric? c) (cons (num-node (string->number (string c))) stack)]
            [else (cons (var-node c) stack)]
            )]
         [else (cons c stack)]
         )]
      [_ #f]
      ))
  (parse-sp* cs))

;; typechecker
 ; who needs it

;; holds the result of an evaluation
(struct res (r))

;; res struct to string
(define (r->s r)
  (define (paren s)
    (string-append "(" s ")"))
  (letrec ([v (res-r r)]
           [cons?->s (lambda (v)
                       (let ([s (->s v)])
                         (if (cons? v)
                           (paren (->s v))
                           (->s v))))]
           [->s (lambda (v)
                  (cond [(number? v) (number->string v)]
                        [(boolean? v) (if v "#t" "#f")]
                        [(null? v) "()"]
                        [(closure? v) "<lambda>"]
                        [(cons? v)
                         (string-append
                           (cons?->s (car v))
                           (let ([s (->s (cdr v))])
                             (match (cdr v)
                               [(cons _ _)
                                (string-append " " s)]
                               ['() ""]
                               [_ (string-append " . " s)])))]
                        [else (error "unrecognized type")]))])
    (cons?->s v)))

;; Interpreter: returns a res struct containing the result of evaluating the
;; program.  If the input expression is well-formed, this will never "get stuck"
(define (eval e env)
  (define (and- preds)
    (if (null? preds)
      #t
      (and (car preds) (and- (cdr preds)))))
  (define (or- preds)
    (if (null? preds)
      #f
      (or (car preds) (or- (cdr preds)))))
  (define (=- preds)
    (cond
      [(and- (map null? preds)) #t]
      [(and- (map number? preds)) (apply = preds)]
      [else #f]))
  (define (eval-b s args)
    (match s
           ["+" (apply + args)]
           ["-" (apply - args)]
           ["*" (apply * args)]
           ["/" (apply / args)]
           ["%" (apply modulo args)]
           ["^" (apply expt args)]
           ["<" (apply < args)]
           ["<=" (apply <= args)]
           ["=" (=- args)]
           ["!=" (not (=- args))]
           [">=" (apply >= args)]
           [">" (apply > args)]
           ["!" (apply not args)]
           ["&" (and- args)]
           ["|" (or- args)]
           ["cons" (apply cons args)]
           ["car" (apply car args)]
           ["cdr" (apply cdr args)]
           ["list" args]
           [_ (error "unrecognized b-functor")]
           ))
  (define (eval-s s args env)
    (match s
           [(struct closure (e env as))
            (let ([argslen (length args)]
                  [aslen (length as)])
              (if (not (equal? argslen aslen))
                (apply raise-arity-error 'lambda aslen args)
              (eval* e (append (map cons as args) env))))]
           [_ (error "unrecognized s-functor")]
           ))
  (define (add-env vs env)
    (match vs
      ['() env]
      [(cons (struct let-arg-node (v e)) rst)
       (cons `(,v . ,(eval* e env)) (add-env rst env))]
      ))
  (define (add-env* vs env)
    (match vs
      ['() env]
      [(cons (struct let-arg-node (v e)) rst)
       (let ([a `(,v . ,(eval* e env))])
         (add-env* rst (cons a env)))]
      ))
  (define (add-env-rec add-env vs env)
    (let ([env* (add-env vs env)])
      (map (lambda (x)
             (match x
               [(cons v (struct closure (e env as)))
                (begin
                  (set-closure-env! (cdr x) env*)
                  x)]
               [_ x]))
           env*)))
  (define (add-env-case t vs env)
    (match t
      ['let (add-env vs env)]
      ['let* (add-env* vs env)]
      ['letrec (add-env-rec add-env vs env)]
      ['letrec* (add-env-rec add-env* vs env)]
      ))
  (define (eval* e env)
    (match e
           [(struct num-node (n)) n]
           [(struct bool-node (b)) b]
           [(struct nil-node ()) '()]
           [(struct var-node (v)) (cdr (assoc v env))]
           [(struct lambda-node (as e)) (closure e env as)]
           [(struct builtin-node (s as)) (eval-b s (map (lambda (a) (eval* a env)) as))]
           [(struct s-node (e as)) (eval-s (eval* e env)
                                           (map (lambda (a) (eval* a env)) as)
                                           env)]
           [(struct exp-node (e)) (eval* e env)]
           [(struct let-node (t vs e)) (eval* e (add-env-case t vs env))]
           [(struct if-node (p t f)) (if (eval* p env)
                                       (eval* t env)
                                       (eval* f env))]
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
         [gen (thunk (scanner in))]
         [ast (with-handlers
                ([exn:fail:read? (lambda (exn) #f)])
                (parse gen))])
    (try-eval-sarah s ast)))

;; parses sarahpolanski and attempts to run it
(define (try-sp s)
  (let* ([cs (scanner-sp s)]
         [ast (with-handlers
                ([exn:fail? (lambda (exn) #f)])
                (parse-sp cs))])
    (if ast
      (try-eval-sarah #f (car ast))
      #f)))

(define (write-sp s fn)
  (cond
    [(try-sp s) => fn]
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

         (check-equal? (try-sarah "(= 4 4)") "#t")
         (check-equal? (try-sarah "(!= 4 4)") "#f")
         (check-equal? (try-sarah "(<= 4 4)") "#t")
         (check-equal? (try-sarah "(>= 4 4)") "#t")
         (check-equal? (try-sarah "(< 3 4)") "#t")
         (check-equal? (try-sarah "(> 3 4)") "#f")
         (check-equal? (try-sarah "(< 2 5 7)") "#t")
         (check-equal? (try-sarah "(< 6 5 7)") "#f")
         (check-equal? (try-sarah "(! (< 6 5 7))") "#t")
         (check-equal? (try-sarah "(+ #t #f)") #f)

         (check-equal? (try-sarah "(& #t #f)") "#f")
         (check-equal? (try-sarah "(& #t #f #t)") "#f")
         (check-equal? (try-sarah "(& #t #t #t)") "#t")
         (check-equal? (try-sarah "(| #t #f)") "#t")
         (check-equal? (try-sarah "(| #t #f #f)") "#t")
         (check-equal? (try-sarah "(| #f #f #f)") "#f")

         (check-equal? (try-sarah "(if #t 64 23)") "64")
         (check-equal? (try-sarah "(if #f 64 23)") "23")
         (check-equal? (try-sarah "(if (> 3 4) (/ 5 0) 23)") "23")
         (check-equal? (try-sarah "(+ (if (> 3 4) (/ 5 0) 23) 11)") "34")

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

         (check-equal? (try-sarah "(let ([x 2] [y x]) y)") #f)
         (check-equal? (try-sarah "(let* ([x 2] [y x]) y)") "2")
         (check-equal? (try-sarah "(letrec ([x 2] [y x]) y)") #f)
         (check-equal? (try-sarah "(letrec* ([x 2] [y x]) y)") "2")
         (check-equal? (try-sarah "(let ([x y] [y 2]) x)") #f)
         (check-equal? (try-sarah "(let* ([x y] [y 2]) x)") #f)
         (check-equal? (try-sarah "(letrec ([x y] [y 2]) x)") #f)
         (check-equal? (try-sarah "(letrec* ([x y] [y 2]) x)") #f)

         (check-equal? (try-sarah "(let ([x 2] [y (thunk x)]) (y))") #f)
         (check-equal? (try-sarah "(let* ([x 2] [y (thunk x)]) (y))") "2")
         (check-equal? (try-sarah "(letrec ([x 2] [y (thunk x)]) (y))") "2")
         (check-equal? (try-sarah "(letrec* ([x 2] [y (thunk x)]) (y))") "2")
         (check-equal? (try-sarah "(let ([x (thunk y)] [y 2]) (x))") #f)
         (check-equal? (try-sarah "(let* ([x (thunk y)] [y 2]) (x))") #f)
         (check-equal? (try-sarah "(letrec ([x (thunk y)] [y 2]) (x))") "2")
         (check-equal? (try-sarah "(letrec* ([x (thunk y)] [y 2]) (x))") "2")

         (check-equal? (try-sarah "(let ([z 5]) (let ([z 1] [y z]) y))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (let* ([z 1] [y z]) y))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (letrec ([z 1] [y z]) y))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (letrec* ([z 1] [y z]) y))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (let ([y z] [z 1]) y))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (let* ([y z] [z 1]) y))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (letrec ([y z] [z 1]) y))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (letrec* ([y z] [z 1]) y))") "5")

         (check-equal? (try-sarah "(let ([z 5]) (let ([z 1] [y (thunk z)]) (y)))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (let* ([z 1] [y (thunk z)]) (y)))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (letrec ([z 1] [y (thunk z)]) (y)))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (letrec* ([z 1] [y (thunk z)]) (y)))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (let ([y (thunk z)] [z 1]) (y)))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (let* ([y (thunk z)] [z 1]) (y)))") "5")
         (check-equal? (try-sarah "(let ([z 5]) (letrec ([y (thunk z)] [z 1]) (y)))") "1")
         (check-equal? (try-sarah "(let ([z 5]) (letrec* ([y (thunk z)] [z 1]) (y)))") "1")

         (check-equal? (try-sarah "((lambda () 4))") "4")
         (check-equal? (try-sarah "((thunk 4))") "4")
         (check-equal? (try-sarah "((thunk 4) 5)") "arity mismatch")
         (check-equal? (try-sarah "(lambda (x) (+ x y))") "<lambda>")
         (check-equal? (try-sarah
                         "((lambda (x) (+ x 1)) 3)")
                       (number->string
                         ((lambda (x) (+ x 1)) 3)))
         (check-equal? (try-sarah
                         "((lambda (x y) (+ x y)) 3 5)")
                       (number->string
                         ((lambda (x y) (+ x y)) 3 5)))
         (check-equal? (try-sarah
                         "((lambda (x) (+ x 1)) 3 5)")
                       "arity mismatch")
         (check-equal? (try-sarah
                         "((lambda (x y) (+ x y)) 3)")
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
         (check-equal? (try-sarah
                         "(let ([x 3]) ((lambda (y) (+ x y)) 5))")
                       (number->string
                         (let ([x 3]) ((lambda (y) (+ x y)) 5))))

         (check-equal? (try-sarah "(cons 2)") "arity mismatch")
         (check-equal? (try-sarah "(car 5)") #f)

         (check-equal? (try-sarah "(= (cons 1 2) 2)") "#f")
         (check-equal? (try-sarah "(= (cons 1 2) 1)") "#f")
         (check-equal? (try-sarah "(= (cons 1 2) (cons 1 2))") "#f")
         (check-equal? (try-sarah "(= 4 2 (cons #f 1) 1)") "#f")
         (check-equal? (try-sarah "(!= (cons 1 2) 2)") "#t")
         (check-equal? (try-sarah "(!= (cons 1 2) 1)") "#t")
         (check-equal? (try-sarah "(!= (cons 1 2) (cons 1 2))") "#t")
         (check-equal? (try-sarah "(!= 4 2 (cons #t 1) 1)") "#t")

         (check-equal? (try-sarah "(cons 3 4)") "(3 . 4)")
         (check-equal? (try-sarah "(car (cons 3 4))") "3")
         (check-equal? (try-sarah "(cdr (cons 3 4))") "4")
         (check-equal? (try-sarah "(cons 4 (cons 3 (cons 2 (cons 1 #f))))")
                       "(4 3 2 1 . #f)")
         (check-equal? (try-sarah "(cons 4 (cons (cons 3 6) (cons 2 (cons 1 #f))))")
                       "(4 (3 . 6) 2 1 . #f)")
         (check-equal? (try-sarah "(cons (cons 5 (thunk 2)) (cons #t 1))")
                       "((5 . <lambda>) #t . 1)")

         (check-equal? (try-sarah "(+ 2 nil)") #f)
         (check-equal? (try-sarah "(nil)") #f)
         (check-equal? (try-sarah "(cons 4 nil)") "(4)")
         (check-equal? (try-sarah "(cons 2 (cons 4 nil))") "(2 4)")
         (check-equal? (try-sarah "(cons nil (cons 4 nil))") "(() 4)")
         (check-equal? (try-sarah "(= nil 2)") "#f")
         (check-equal? (try-sarah "(= nil nil)") "#t")

         (check-equal? (try-sarah "(letrec
                                     ([map (lambda (fn xs)
                                             (if (= xs nil)
                                               nil
                                               (cons (fn (car xs))
                                                     (map fn (cdr xs)))))])
                                     (map (lambda (x) (+ x 1)) (list 1 2 3 4)))")
                       "(2 3 4 5)")

         (check-equal? (try-sarah "(list 4 3 2 1)") "(4 3 2 1)")
         (check-equal? (try-sarah "(car (list 4 3 2 1))") "4")
         (check-equal? (try-sarah "(cdr (list 4 3 2 1))") "(3 2 1)")
         (check-equal? (try-sarah "(cdr (list 2))") "()")

         (check-equal? (try-sarah
                         "(letrec ([succ (lambda (n)
                                           (thunk
                                             (cons n (succ (+ n 1)))))])
                            (car ((cdr ((cdr ((cdr ((succ 0))))))))))")
                       (number->string
                         (letrec ([succ (lambda (n)
                                          (thunk
                                            (cons n (succ (+ n 1)))))])
                           (car ((cdr ((cdr ((cdr ((succ 0))))))))))))


         (check-equal? (try-sp "2") "2")
         (check-equal? (try-sp "2~") "-2")
         (check-equal? (try-sp "2~~") "2")
         (check-equal? (try-sp "23") "3")
         (check-equal? (try-sp "234+") "7")
         (check-equal? (try-sp "3~4+~") "-1")
         (check-equal? (try-sp "2 3 +") #f)
         (check-equal? (try-sp "23,") "2")

         (check-equal? (try-sp "23+") "5")
         (check-equal? (try-sp "234++") "9")
         (check-equal? (try-sp "23+45++")
                       (number->string (+ (+ 2 3) (+ 4 5))))
         (check-equal? (try-sp "23-") "-1")
         (check-equal? (try-sp "23,-") "1")
         (check-equal? (try-sp "432--") "3")
         (check-equal? (try-sp "32-45-+")
                       (number->string (+ (- 3 2) (- 4 5))))
         (check-equal? (try-sp "23*") "6")
         (check-equal? (try-sp "432**") "24")
         (check-equal? (try-sp "32*45-+")
                       (number->string (+ (* 3 2) (- 4 5))))
         (check-equal? (try-sp "52/6/7/")
                       (number->string (/ 5 2 6 7)))
         (check-equal? (try-sp "23^") "8")
         (check-equal? (try-sp "52+67*%")
                       (number->string (modulo (+ 5 2) (* 6 7))))
         (check-equal? (try-sp "52-67/^")
                       (number->string (expt (- 5 2) (/ 6 7))))

         (check-equal? (try-sp "52x67/^") #f)

         (check-equal? (try-sp "34<") "1")
         (check-equal? (try-sp "43<") "0")
         (check-equal? (try-sp "34=") "0")
         (check-equal? (try-sp "44=") "1")
         (check-equal? (try-sp "43>") "1")
         (check-equal? (try-sp "44>") "0")
         (check-equal? (try-sp "44>!") "1")
         (check-equal? (try-sp "4!") "0")
         (check-equal? (try-sp "4!!") "1")
         (check-equal? (try-sp "0!") "1")
         (check-equal? (try-sp "0!!") "0")

         (check-equal? (try-sp "00&") "0")
         (check-equal? (try-sp "01&") "0")
         (check-equal? (try-sp "11&") "1")
         (check-equal? (try-sp "00|") "0")
         (check-equal? (try-sp "10|") "1")
         (check-equal? (try-sp "11|") "1")

         (check-equal? (try-sp "231?") "3")
         (check-equal? (try-sp "230?") "2")
         (check-equal? (try-sp "250/34>?") "2")
         (check-equal? (try-sp "250/34>!?") "divide by zero")
         (check-equal? (try-sp "750/34>?1+") "8")

         (check-equal? (try-sp "23+6x:") "5")
         (check-equal? (try-sp "x3+6x:")
                       (number->string (let ([x 6]) (+ x 3))))
         (check-equal? (try-sp "xx+5x:")
                       (number->string (let ([x 5]) (+ x x))))
         (check-equal? (try-sp "xy*6y:5x:")
                       (number->string (let ([x 5] [y 6]) (* x y))))

         (check-equal? (try-sp "3x1+x\\") "<lambda>")
         (check-equal? (try-sp "xy+x\\") "<lambda>")
         (check-equal? (try-sp "3x1+x\\$")
                       (number->string
                         ((lambda (x) (+ x 1)) 3)))
         (check-equal? (try-sp "yf$3y:x2*x\\f:")
                       (number->string
                         (let ([fn (lambda (x) (* x 2))] [y 3]) (fn y))))
         (check-equal? (try-sp "53x$$xy+y\\x\\x:")
                       (number->string
                         (let ([x (lambda (x) (lambda (y) (+ x y)))]) ((x 3) 5))))
         (check-equal? (try-sp "3x$xy+y\\x\\x:")
                       "<lambda>")
         (check-equal? (try-sp "3xy+y\\$5x:")
                       (number->string
                         (let ([x 5]) ((lambda (y) (+ x y)) 3))))

         (check-equal? (try-sp "4f$xx1-f$+0x0=?x\\f#")
                       (number->string
                         (letrec ([f (lambda (x)
                                       (if (= x 0)
                                         0
                                         (+ x (f (- x 1)))))])
                           (f 4))))

         (check-equal? (try-sp "2.") #f)
         (check-equal? (try-sp "5[") #f)

         (check-equal? (try-sp "12.2=") "0")
         (check-equal? (try-sp "12.1=") "0")
         (check-equal? (try-sp "12.12.=") "0")

         (check-equal? (try-sp "34.") "(3 . 4)")
         (check-equal? (try-sp "34.[") "3")
         (check-equal? (try-sp "34.]") "4")
         (check-equal? (try-sp "43210....")
                       "(4 3 2 1 . 0)")
         (check-equal? (try-sp "436.210....")
                       "(4 (3 . 6) 2 1 . 0)")
         (check-equal? (try-sp "52x\\.11..")
                       "((5 . <lambda>) 1 . 1)")

         (check-equal? (try-sp "12340....1x2*+x\\m$$x[f$x]fm$$.0x0=?x\\f\\m#")
                       "(3 5 7 9 . 0)")

         ; 0,$ is 0-swap-call, which we use to call a "thunk"
         (check-equal? (try-sp "0s$0,$]0,$]0,$]0,$[nn1+s$.x\\n\\s#")
                       (number->string
                         (letrec ([succ (lambda (n)
                                          (thunk
                                            (cons n (succ (+ n 1)))))])
                           (car ((cdr ((cdr ((cdr ((succ 0))))))))))))
)

