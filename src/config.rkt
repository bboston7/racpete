#lang typed/racket

(provide (all-defined-out))

(define: HOST : String "example.server.com")
(define: PORT : Positive-Integer 6667)
(define SSL #f)
(define: NICK : String "racpete")
(define: IDENT : String "racpete")
(define: REALNAME : String "racpete")
(define: CHAN : String "#examplechannel")
(define RECONNECT_TIMER 30)
(define: BOT_LIST : (Listof String) (list NICK))

(define: GOOGLE_API_KEY : String "")
(define GOOGLE_SEARCH_CX #f)
