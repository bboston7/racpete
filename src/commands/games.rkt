#lang racket

(require
  "../config.rkt"
  "../util/list-utils.rkt"
  "../util/names-manager.rkt"
)
(provide
  swag-tag
  swag-reset
)

#|
Person who holds the swag for the swagtag game.
|#
(define current-swag-holder (make-parameter ""))

#|
Sets the swag holder to a random connected person.
|#
(define (swag-reset) (let ([nicks (current-nicks)])(begin
  (current-swag-holder (pick-random nicks));set to random
  (if (member (current-swag-holder) BOT_LIST)
    (swag-reset)
    (string-append "Swag has been entrusted to " (current-swag-holder) ".")
  )
)))

#|
swagtag game handler
|#
(define (swag-tag tagger taggee nicks NICK) (cond
  [(equal? taggee "reset")
    (if (and (member (current-swag-holder) nicks)
             (not (equal? (current-swag-holder) tagger)))
        "You can't reset while the swag master is present."
        (swag-reset)
     )
  ]
  [(equal? taggee "who?")
   (if (equal? (current-swag-holder) "")
     "Nobody has the swag. Type .swagtag reset to start the game."
     (string-append "All hail " (current-swag-holder) ", king of Swagnia."))]
  [(equal? taggee "help")
   "If you have the swag, type '.swagtag <name>' to share the swag. Type '.swagtag who?' to see who has the swag. Type '.swagtag reset' to reset the swag master."]
  [(member taggee BOT_LIST)
   (string-append taggee " has too much swag already. Any more and they might burst.")]
  [else (cond
    [(not (equal? (current-swag-holder) tagger)) "You don't have the swag you poser."]
    [(not (member taggee nicks)) "Those not present are not deserving of swag."]
    [else (begin ;change the swag holder
      (current-swag-holder taggee)
      (string-append tagger " swags " taggee)
    )]
  )]
))
