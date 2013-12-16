#lang racket

(require html
         json
         net/uri-codec
         net/url
         xml
         xml/path
         "../config.rkt"
         "../util/string-utils.rkt")

(provide (contract-out
           [btc->usd-string (-> (or/c boolean? usd-string?))]
           [btc->usd-string-async (-> (-> (or/c boolean? string?) any)
                                      thread?)]
           [query-wikipedia (-> string? (or/c boolean? pair?))]
           [query-wikipedia-async (-> string? (-> string? any) thread?)]
           [query-youtube (-> string? (-> (or/c boolean? string?) any) any)]
           [rand-bash (-> (-> string? any) thread?)]))

(permissive-xexprs #t)

(define BASH_BASE "http://bash.org/?")
(define BASH_RAND "http://bash.org/?random")
(define YOUTUBE_SEARCH_BASE
  (if GOOGLE_API_KEY
    (string-append
      "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=1&safeSearch=none&key="
      GOOGLE_API_KEY
      "&q=")
    #f))
(define MT_GOX_TICKER "http://data.mtgox.com/api/2/BTCUSD/money/ticker_fast")
(define wikipedia-api-base "http://en.wikipedia.org/w/api.php?action=query&list=search&format=xml&srlimit=1&srsearch=")
(define wikipedia-page-base "http://en.wikipedia.org/wiki/")

(define (usd-string? str)
  (regexp-match? #px"^\\$[0-9]+\\.[0-9]{2}$" str))

#|
Asynchronously grabs a random link from bash.org and calls out with it
|#
(define (rand-bash out)
  (define (fn top)
    (cond
      [(b? top) (substring (pcdata-string (car (html-full-content top))) 1)]
      [(pcdata? top) #f]
      [(head? top) #f]
      [(form? top) #f]
      [(html-full? top) (fn (html-full-content top))]
      [(list? top) (ormap fn top)]
      [(pair? top) (or (fn (car top)) (fn (cdr top)))]
      [else #f]))
  (thread (lambda ()
            (out (string-append BASH_BASE (fn (scrape-html BASH_RAND)))))))


#|
Finds the current cost of a bitcoin according to the MtGox ticker and returns
the value as a usd-string? or #f if the API call fails.
|#
(define (btc->usd-string)
  (let ([res (query-json-service MT_GOX_TICKER)])
    (if (equal? (hash-ref res 'result) "success")
      (from-nested-hash res (list 'data 'last 'display))
      #f)))

#|
Calls btc->usd-string in a new thread, then passes the result to fn

Returns immediately
|#
(define (btc->usd-string-async fn)
  (thread (lambda () (fn (btc->usd-string)))))

#|
Given a query string, returns a cons cell with a snippet description and link
to wikipedia page:
(cons snippet link)

On error, returns #f
|#
(define (query-wikipedia query)
  (if (equal? query "")
    #f
    (let ([res (query-xml-service (string-append wikipedia-api-base (uri-encode
                                                                  query)))])
      (if (or (eq? (caaddr res) 'error)
              (equal? (cadr (assoc 'totalhits (cadr (se-path* '(query) res)))) 
                      "0"))
        #f
        (let ([data (cddar (cdaddr (cadddr (cadddr res))))])
          (cons
            (string-normalize-spaces
              (strip-tags (cadr (assoc 'snippet data))))
            (string-append wikipedia-page-base
                           (uri-encode (cadr (assoc 'title data))))))))))

#|
Calls query-wikipedia in a new thread with query, then passes the result to fn

Returns immediately
|#
(define (query-wikipedia-async query fn)
  (thread (lambda ()
            (let ([res (query-wikipedia query)])
              (if res
                (begin (fn (car res)) (fn (cdr res)))
                (fn "No match"))))))

(define (query-xml-service url)
  (xml->xexpr (document-element (read-xml (get-pure-port
                                            (string->url url))))))

#|
Searches youtube for query and calls fn on the result.

Requires
    GOOGLE_API_KEY is valid

Parameters
    query - The item to search on youtube
    fn - Function to call on string "title - url" response or #f
|#
(define (query-youtube query fn)
  (let ([res (query-json-service (string-append YOUTUBE_SEARCH_BASE query))])
    (if (= 0 (from-nested-hash res (list 'pageInfo 'totalResults)))
      (fn (string-append "no results for " query))
      (let ([item (car (hash-ref res 'items))])
        (fn (string-append
              (from-nested-hash item (list 'snippet 'title))
              " - https://www.youtube.com/watch?v="
              (from-nested-hash item (list 'id 'videoId))))))))


#|
Get a jsexpr? from querying a json API
|#
(define (query-json-service url) (read-json (get-pure-port (string->url url))))

(define (from-nested-hash hash keys)
  (letrec ([fn (lambda (keys acc)
                 (if (null? keys)
                   acc
                   (fn (cdr keys) (hash-ref acc (car keys)))))])
  (fn keys hash)))

(define (scrape-html url)
  (read-html (get-pure-port (string->url url))))
