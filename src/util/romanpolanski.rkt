#lang racket

(require racket/engine)

(provide do-rp
         try-rp
         write-rp)

#|
types: int bool null
<letter> var-name
^ true
_ false
' nil
+ add
- subtract
* multiply
/ divide
% modulo
= equality
! not
< less-than
> greater-than
& and
| or
? if
, dup (or swap)
~ swap
. drop
# top
[ start-thunk
] end-thunk
$ do-thunk
: store-var
; load-var
|#

#|

This is romanpolanski, a stack based language that pete recognizes and can evaluate

|#

(define TIMEOUT 2000)
(define MAX-LEN 400)

(struct var (v val))
(define (var-get var)
  (cdr (var-val var)))

(struct closure (exp))

;; Scanner for romanpolanski
 ; matches all input
(define scanner-rp
  string->list)

;; Parser and executor for romanpolanski
 ; returns #f on parse/eval failure
(define (parse-rp cs)
  (define (parse-rp* cs stack env)
    (define (resolve x)
      (if (var? x)
        (resolve (var-get x))
        x))
    (define (thunk-do thunk stack env)
      (parse-rp* (closure-exp thunk) stack env))
    (define-syntax-rule (vpop x stack body)
      (let ([x (car stack)]
            [stack (cdr stack)])
        body))
    (define-syntax-rule (epop x stack body)
      (vpop e stack
            (let ([x (resolve e)])
              body)))
    (define-syntax-rule (pop x stack env body)
      (letrec ([grab
                 (lambda (stack)
                   (match-let ([(cons val stack) stack])
                     (cond
                       [(var? val)
                        (grab (cons (var-get val) stack))]
                       [(closure? val)
                        (grab (thunk-do val stack env))]
                       [else (values val stack)])))])
        (let-values ([(x stack) (grab stack)])
          body)))
    (match cs
      ['() stack]
      [(cons c cs)
       (let ([push (lambda (x stack)
                     (parse-rp* cs (cons x stack) env))])
         (cond
           [(equal? #\+ c) (pop y stack env
                           (pop x stack env
                                (push (+ x y) stack)))]
           [(equal? #\- c) (pop y stack env
                           (pop x stack env
                                (push (- x y) stack)))]
           [(equal? #\* c) (pop y stack env
                           (pop x stack env
                                (push (* x y) stack)))]
           [(equal? #\/ c) (pop y stack env
                           (pop x stack env
                                (push (/ x y) stack)))]
           [(equal? #\% c) (pop y stack env
                           (pop x stack env
                                (push (modulo x y) stack)))]
           [(equal? #\= c) (pop y stack env
                           (pop x stack env
                                (push (eq? x y) stack)))]
           [(equal? #\! c) (pop x stack env
                                (if (boolean? x)
                                  (push (not x) stack)
                                  #f))]
           [(equal? #\< c) (pop y stack env
                           (pop x stack env
                                (push (< x y) stack)))]
           [(equal? #\> c) (pop y stack env
                           (pop x stack env
                                (push (> x y) stack)))]
           [(equal? #\& c) (pop y stack env
                           (pop x stack env
                                (push (and x y) stack)))]
           [(equal? #\| c) (pop y stack env
                           (pop x stack env
                                (push (or x y) stack)))]
           [(equal? #\, c) (let ([x (car stack)])
                             (push x stack))]
           [(equal? #\~ c) (vpop y stack
                           (vpop x stack
                                 (parse-rp* cs (cons x (cons y stack)) env)))]
           [(equal? #\. c) (vpop x stack
                                 (parse-rp* cs stack env))]
           [(equal? #\# c) (vpop x stack
                                 (parse-rp* cs (list x) env))]
           [(equal? #\: c) (vpop v stack
                           (epop x stack
                                 (parse-rp* cs stack (cons (cons (var-v v) x) env))))]
           [(equal? #\; c) (vpop v stack
                                 (push (var-get v) stack))]
           [(equal? #\[ c)
            (letrec ([capture
                       (lambda (ls cs level)
                         (match-let ([(cons c cs) cs])
                           (cond [(and (equal? #\] c) (= level 0))
                                  (values (reverse ls) cs)]
                                 [(equal? #\[ c)
                                  (capture (cons c ls) cs (+ level 1))]
                                 [(equal? #\] c)
                                  (capture (cons c ls) cs (- level 1))]
                                 [else (capture (cons c ls) cs level)])))])
              (let-values ([(thunk cs) (capture '() cs 0)])
                (parse-rp* cs (cons (closure thunk) stack) env)))]
           [(equal? #\$ c) (epop t stack
                                 (match-let ([(closure exp) t])
                                   (parse-rp* cs (parse-rp* exp stack env) env)))]
           [(equal? #\? c) (epop q stack
                           (epop t stack
                           (epop f stack
                           (let ([stack (cons q stack)])
                           (pop q stack env
                                (let ([v (if q t f)])
                                  (parse-rp* cs
                                             (if (closure? v)
                                               (thunk-do v stack env)
                                               (cons v stack))
                                             env)))))))]
           [(equal? #\^ c) (push true stack)]
           [(equal? #\_ c) (push false stack)]
           [(equal? #\' c) (push null stack)]
           [(char? c)
            (cond
              [(char-numeric? c) (push (string->number (string c)) stack)]
              [else (push (var c (assoc c env)) stack)]
              )]
           [else (cons c stack)]
           ))]
      [_ (error cs)]
      ))
  (parse-rp* cs '() '()))

;; typechecker?
 ; static typing is too hard.

;; res struct to string
(define (r->s r)
  (cond [(number? r) (number->string r)]
        [(boolean? r) (if r "^" "_")]
        [(null? r) "'"]
        [(var? r) (if (var-val r)
                    (r->s (var-get r))
                    (string (var-v r)))]
        [(closure? r) (string-append "[" (list->string (closure-exp r)) "]")]
        [else (error "unrecognized type")]))

(define (rs->s rs)
  (string-join (map r->s (reverse rs))))

(define do-rp
  (compose rs->s parse-rp scanner-rp))

;; parses romanpolanski and attempts to run it
(define (try-rp s)
  (let* ([cs (scanner-rp s)]
         [eng (engine (lambda (b)
                        (with-handlers
                          ([exn:fail:contract:divide-by-zero?
                             (lambda (exn) "divide by zero")]
                           [exn:fail?
                             (lambda (exn) #f)])
                          (parse-rp cs))))]
         [stack (if (engine-run TIMEOUT eng) (engine-result eng) "timeout")])
    (cond
      [(string? stack) stack]
      [stack
        (with-handlers
          ([exn:fail? (lambda (exn) #f)])
          (rs->s stack))]
      [else #f])))

(define (write-rp s fn)
  (cond
    [(try-rp s) => fn]
    [else #f]))


(module+ test ;; no spec so there can't be bugs.  thanks ernst!
         (require rackunit)

         ; basic parsing
         (check-equal? (try-rp "2") "2")
         (check-equal? (try-rp "23") "2 3")
         (check-equal? (try-rp "234+") "2 7")

         ; the space character is a valid variable name
         (check-equal? (try-rp "2 3") "2   3")
         (check-equal? (try-rp "2 3 +") #f)

         ; dup, squiggle, consume, smash
         (check-equal? (try-rp "2,") "2 2")
         (check-equal? (try-rp "23~") "3 2")
         (check-equal? (try-rp "23.") "2")
         (check-equal? (try-rp "2.") "")
         (check-equal? (try-rp "258215001934#") "4")
         (check-equal? (try-rp "23#") "3")
         (check-equal? (try-rp "2#") "2")
         (check-equal? (try-rp ",") #f)
         (check-equal? (try-rp "~") #f)
         (check-equal? (try-rp "2~") #f)
         (check-equal? (try-rp ".") #f)
         (check-equal? (try-rp "#") #f)

         ; basic arithmetic
         (check-equal? (try-rp "+") #f)
         (check-equal? (try-rp "2+") #f)
         (check-equal? (try-rp "23+") "5")
         (check-equal? (try-rp "234++") "9")
         (check-equal? (try-rp "23+45++")
                       (number->string (+ (+ 2 3) (+ 4 5))))
         (check-equal? (try-rp "23-") "-1")
         (check-equal? (try-rp "23~-") "1")
         (check-equal? (try-rp "432--") "3")
         (check-equal? (try-rp "32-45-+")
                       (number->string (+ (- 3 2) (- 4 5))))
         (check-equal? (try-rp "23*") "6")
         (check-equal? (try-rp "432**") "24")
         (check-equal? (try-rp "32*45-+")
                       (number->string (+ (* 3 2) (- 4 5))))
         (check-equal? (try-rp "52/6/7/")
                       (number->string (/ 5 2 6 7)))
         (check-equal? (try-rp "52+67*%")
                       (number->string (modulo (+ 5 2) (* 6 7))))

         ; basic booleans
         (check-equal? (try-rp "^") "^")
         (check-equal? (try-rp "_") "_")
         (check-equal? (try-rp "_2+") #f)
         (check-equal? (try-rp "^!") "_")
         (check-equal? (try-rp "_!") "^")
         (check-equal? (try-rp "4!") #f)
         (check-equal? (try-rp "0!") #f)

         ; comparison operators
         (check-equal? (try-rp "34<") "^")
         (check-equal? (try-rp "43<") "_")
         (check-equal? (try-rp "34=") "_")
         (check-equal? (try-rp "_2=") "_")
         (check-equal? (try-rp "44=") "^")
         (check-equal? (try-rp "__=") "^")
         (check-equal? (try-rp "^_=") "_")
         (check-equal? (try-rp "43>") "^")
         (check-equal? (try-rp "44>") "_")
         (check-equal? (try-rp "44>!") "^")

         ; boolean operators
         (check-equal? (try-rp "__&") "_")
         (check-equal? (try-rp "_^&") "_")
         (check-equal? (try-rp "^^&") "^")
         (check-equal? (try-rp "__|") "_")
         (check-equal? (try-rp "^_|") "^")
         (check-equal? (try-rp "^^|") "^")

         ; variable assignment
         (check-equal? (try-rp "5;") #f)
         (check-equal? (try-rp "x;") #f)
         (check-equal? (try-rp "5x:y;") #f)
         (check-equal? (try-rp "yx:x;") #f)
         (check-equal? (try-rp "x") "x")
         (check-equal? (try-rp "5x:x;") "5")
         (check-equal? (try-rp "5x:x") "5")

         ; chained variable assignment
         (check-equal? (try-rp "5x:xy:yz:z;") "5")
         (check-equal? (try-rp "5x:xy:yz:zz+") "10")
         (check-equal? (try-rp "5x:xy:yz:z,") "5 5")

         ; using variables
         (check-equal? (try-rp "6x:23+") "5")
         (check-equal? (try-rp "6x:x;3+") "9")
         (check-equal? (try-rp "6x:x3+") "9")
         (check-equal? (try-rp "5x:x;x;+") "10")
         (check-equal? (try-rp "5x:xx+") "10")
         (check-equal? (try-rp "5x:6y:xy*") "30")

         ; making thunks
         (check-equal? (try-rp "[5]") "[5]")
         (check-equal? (try-rp "[5]$") "5")
         (check-equal? (try-rp "[5],") "[5] [5]")
         (check-equal? (try-rp "[5],$~$") "5 5")

         ; using thunks
         (check-equal? (try-rp "[34]f:56f$") "5 6 3 4")
         (check-equal? (try-rp "[+]f:56f$") "11")

         ; implicitly using thunks
         (check-equal? (try-rp "[6]3+") "9")
         (check-equal? (try-rp "[5],+") "10")
         (check-equal? (try-rp "3[56]*") "3 30")
         (check-equal? (try-rp "[34][56]*") "[34] 30")
         (check-equal? (try-rp "[34][5]*") "3 20")
         (check-equal? (try-rp "[34][]*") "12")
         (check-equal? (try-rp "[56]*") "30")

         ; thunk/variable interaction
         (check-equal? (try-rp "[x]$") "x")
         (check-equal? (try-rp "5x:[x]$") "5")
         (check-equal? (try-rp "[x]5x:$") "5")
         (check-equal? (try-rp "5x:[x]4x:$") "4")
         (check-equal? (try-rp "5x:[3x:x]4x:$x") "3 4")
         (check-equal? (try-rp "5x:[x+]f:3x:xf$") "6")
         (check-equal? (try-rp "[x1+]f:3x:f$") "4")

         ; thunks with "parameters"
         (check-equal? (try-rp "[x:x1+]f:3f$") "4")
         (check-equal? (try-rp "[x:x1+]f:f$") #f)
         (check-equal? (try-rp "[x:x1+]f:345f$") "3 4 6")
         (check-equal? (try-rp "[x:x1+]f:5f$f$") "7")
         (check-equal? (try-rp "[y:x:xy-]f:34f$") "-1")
         (check-equal? (try-rp "[x:x1+]f:3f$x") "4 x")

         ; recursion
         (check-equal? (try-rp "[x$]x:x$") "timeout")
         (check-equal? (try-rp "[n:[nn1-s+][0]n0=?]s:4s$") "10")
         (check-equal? (try-rp "[[,1-s+][][,0=]?]s:4s$") "10")

         ; ternary operator
         (check-equal? (try-rp "23^?") "3")
         (check-equal? (try-rp "23_?") "2")
         (check-equal? (try-rp "2[3]^?") "3")
         (check-equal? (try-rp "23[_]?") "2")
         (check-equal? (try-rp "2[50/]34>?") "2")
         (check-equal? (try-rp "2[50/]34>!?") "divide by zero")
         (check-equal? (try-rp "7[50/]34>?1+") "8")

         ; nil
         (check-equal? (try-rp "'") "'")
         (check-equal? (try-rp "'5") "' 5")
         (check-equal? (try-rp "'!") #f)
         (check-equal? (try-rp "'2+") #f)
         (check-equal? (try-rp "0'=") "_")
         (check-equal? (try-rp "''=") "^")

         ; lists
         (check-equal? (try-rp "[[x:m$1x2*+][][,'=]?]m:'1234m$") "' 3 5 7 9")
         (check-equal? (try-rp "[[x:s$x+][.0][,'=]?]s:'3579s$") "24")

         ; streams
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$") "0 [n:n1+s$n]")
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$$") "1 [n:n1+s$n] 0")
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$$.$") "2 [n:n1+s$n] 1")
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$$.$.$") "3 [n:n1+s$n] 2")
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$$.$.$.$") "4 [n:n1+s$n] 3")
         (check-equal? (try-rp "[n:n[n:n1+s$n]]s:0s$$.$.$.$.$#") "4")
)
