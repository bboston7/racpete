#!/usr/bin/racket
#lang racket

(require
  "commands/ycombinator.rkt"
  "commands/web-queries.rkt"
  "commands/kwanzaa.rkt"
  "commands/auth-commands.rkt"
  "commands/yesno.rkt"
  "commands/ballsohard.rkt"
  "commands/games.rkt"
  "commands/what-say.rkt"
  "commands/stimulate.rkt"
  "commands/learn-about.rkt"
  "config.rkt"
  "util/connection.rkt"
  "util/names-manager.rkt"
  "util/string-utils.rkt"
  "util/list-utils.rkt"
  "util/urltilities.rkt"
  "util/morse.rkt"
  "util/derek.rkt"
)

#|
Builds a pair of lists.  The car is quotes from the current channel log, the cdr
is urls from the current channel log
|#
(define (build-quotes-and-urls)
  (letrec ([file-name (string-append "logs/" CHAN ".log")]
           [log-file
             (if (file-exists? file-name)
               (open-input-file file-name #:mode 'text)
               #f)]
           ; Tail recursive because log files get large!
           ; Unfortunately, that means the list is backwards, this doesn't
           ; matter now, and since we don't store date information in our logs,
           ; maybe it never will
           [tail-fn
             (lambda (quotes-acc url-acc)
               (let ([line (read-line log-file)])
                 (if (eof-object? line)
                   (values quotes-acc url-acc)
                   (let ([url-match (regexp-match urlregex line)])
                     (if url-match
                       (tail-fn (cons line quotes-acc) (cons (car url-match) url-acc))
                       (tail-fn (cons line quotes-acc) url-acc))))))])
    (if log-file
      (begin0
        (tail-fn null null)
        (close-input-port log-file))
      (values null null))))

(define-values (quotes links) (build-quotes-and-urls))

#|
Log a line from the chat
|#
(define (log nick message)
  (let ([line (string-append "<" nick "> " message)])
    (begin
      (set! quotes (cons line quotes))
      (let ([url-match (regexp-match urlregex line)])
        (when url-match
          (set! links (cons (car url-match) links))))
      (display-to-file (string-append line "\n")
                       (string-append "logs/" CHAN ".log") #:exists 'append))))

#|
Handles incoming user irc commands
|#
(define (command-handler nick msg)
  (ping-stimulator)
  (let ([urlres (regexp-match urlregex msg)])
    (cond
      [(string-starts-with? msg ".swagtag ") (write-to-channel (swag-tag nick (substring msg 9) (current-nicks) NICK))]
      [(equal? ".q" msg) (write-to-channel (pick-random quotes))]
      [(equal? "mux" msg) (write-to-channel "juhn")]
      [(equal? "derp" msg) (write-to-channel "meep")]
      [(equal? "has anyone done the ruzzler" msg) (write-to-channel "probably not")]
      [(equal? ".boom" msg) (write-to-channel "BOOM GOES THE DYNAMITE!")]
      [(equal? ".kwanzaa" msg) (write-to-channel (compute-kwanzaa-str))]
      [(equal? ".link me" msg) (handle-link-me)]
      [(string-starts-with? msg "tell me about ") (write-to-channel (learn-about msg quotes))]
      [(string-starts-with? msg ".rx ") (egrep msg quotes write-to-channel)]
      [(regexp-match #rx"what has (.*) said\\?" msg)
         => (lambda (x) (has-said (cadr x) quotes write-to-channel))]
      [(regexp-match #rx"what would (.*) say\\?" msg)
         => (lambda (x) (what-would-say (cadr x) quotes write-to-channel))]
      [(regexp-match-exact? (pregexp (string-append NICK "\\b.*\\?")) msg) (write-to-channel (yesno))]
      [(equal? ".ycombinator" msg) (ycombo write-to-channel)]
      [(equal? ".plug" msg) (write-to-channel "help me out: http://www.github.com/bboston7/racpete")]
      [(string-starts-with? msg ".morse ") (write-to-channel
                                            (string->morse (substring msg 6)))]
      [(contains-morse? msg) (begin
                               (write-to-channel (convert-morse (parse-morse msg)))
                               (log nick msg))]
      [urlres (handle-url-match urlres nick msg)]
      [(equal? ".test" msg) (write-to-channel "caught .test")]
      [(string-starts-with? msg ".w ") (query-wikipedia-async (substring msg 3)
                                                             write-to-channel)]
      [(equal? ".btc" msg) (btc->usd-string-async write-to-channel)]
      [(string-starts-with? msg ".yt ") (handle-youtube-search msg)]
      [(string-starts-with? msg ".kick ") (act-to-channel
                                            (string-append "kicks "
                                                           (substring msg 6)))]
      [(equal? ".bash" msg) (rand-bash write-to-channel)]
      [(equal? ".roulette" msg) (act-to-channel
                                  (string-append "kicks " (pick-random (current-nicks))))]
      [(equal? ".ballsohard" msg) (write-to-channel (ball-so-hard))]
      [(string-starts-with? msg ".g") (handle-google-search msg)]
      [(try-eval msg) => write-to-channel]
      [else (log nick msg)])))

#|
Handles incoming user irc commands in private messages.
|#
(define (priv-command-handler nick msg)
  (define (channelize name)
    (if (string-starts-with? name "#")
      name
      (string-append "#" name)))
  (cond
    [(string-starts-with? msg ".swagtag reset ") (verify (substring msg 15) swag-reset)]
    [(string-starts-with? msg ".die ") (verify (substring msg 5) die)]
    [(string-starts-with? msg ".update ") (verify (substring msg 8) update)]
    [(string-starts-with? msg ".say ") (let ([tokens (string-split (substring msg 5))])
                                           (write-to-thing (string-join (cdr tokens))
                                                           (channelize (car tokens))))]
    [(string-starts-with? msg ".do ") (let ([tokens (string-split (substring msg 4))])
                                           (act-to-thing (string-join (cdr tokens))
                                                         (channelize (car tokens))))]
    [(equal? ".test" msg) (write-to-user "caught .test" nick)]
    [(equal? ".names" msg) (begin (names-from-channel)
                                  (write-to-user
                                    (string-append
                                      "requested names on "
                                      CHAN)
                                    nick))]
    [(equal? ".ycombinator" msg) (ycombo write-to-user nick)]
    [(try-eval msg) => write-to-channel]))

#|
Handles a link me request
|#
(define (handle-link-me)
  (let ([url (pick-random links)])
    (when url
      (write-to-channel url)
      (get-website-title-async url write-to-channel))))

#|
Handles a url match in a chat line
|#
(define (handle-url-match urlres nick msg)
  (get-website-title-async (car urlres)
                           (lambda (x) (write-to-channel x) (log nick msg))))

#|
Handles searching google
|#
(define (handle-google-search msg)
  (thread (lambda () (query-google (substring msg 3) write-to-channel))))

#|
Handles searching youtube
|#
(define (handle-youtube-search msg)
  (thread (lambda () (query-youtube (substring msg 4) write-to-channel))))

(start-stimulator
  (list handle-link-me
        (λ () (what-would-say NICK
                              quotes
                              (λ (x) (write-to-channel (chop-token x)))))
        (λ () (write-to-channel (chop-token (pick-random quotes))))))
(start-pete command-handler priv-command-handler)
