#lang racket

(provide (all-defined-out))

(define HOST "")
(define PORT 6667)
(define NICK "racpete")
(define IDENT "racpete")
(define REALNAME "racpete")
(define CHAN "")
(define OPS null) ; This is a terrible way of doing authentication
