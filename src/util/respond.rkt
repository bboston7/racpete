#lang racket

(require  "../config.rkt")

(provide msg-respond?)

#|

Common place for all one-to-one message-responses

|#

(define (msg-respond? msg)
  (match msg
    ["mux" "juhn"]
    ["derp" "meep"]
    ["YO" "YO"]
    ["has anyone done the ruzzler" "probably not"]
    [".boom" "BOOM GOES THE DYNAMITE!"]
    [".magic"
       "(ノﾟοﾟ)ノﾐ★゜・。。・゜゜・。。・゜☆゜・。。・゜゜・。。・゜゜・。。・゜☆゜・。。・゜゜・。。・゜"]
    [".plug"
       "help me out: http://www.github.com/bboston7/racpete"]
    [(regexp (string-append "(?i:i love you,? " NICK ")"))
       "Sorry, meatsack. You know I don't do that."]
    [_ #f]
    ))

