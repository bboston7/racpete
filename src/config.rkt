#lang typed/racket

(provide (all-defined-out))

(define: HOST : String "irc.adelais.net")
(define: PORT : Positive-Integer 6667)
(define: NICK : String "racpete")
(define: IDENT : String "racpete")
(define: REALNAME : String "racpete")
(define: CHAN : String "#poopers")
(define RECONNECT_TIMER 30)

(define: GOOGLE_API_KEY : String "")
