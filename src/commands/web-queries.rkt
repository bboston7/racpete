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
           [btc->usd-string (-> exchange? (or/c boolean? usd-string?))]
           [btc->usd-string-async (-> (-> (or/c boolean? string?) any)
                                      thread?)]
           [query-google (-> string? (-> (or/c boolean? string?) any) any)]
           [query-image (-> string? (-> (or/c boolean? string?) any) any)]
           [query-wikipedia (-> string? (or/c boolean? pair?))]
           [query-wikipedia-async (-> string? (-> string? any) thread?)]
           [query-youtube (-> string? (-> (or/c boolean? string?) any) any)]
           [rand-bash (-> (-> string? any) thread?)]
           [query-coin-cap (-> string? (-> string? any) any)]))

(permissive-xexprs #t)

(define BASH_BASE "http://bash.org/?")
(define BASH_RAND "http://bash.org/?random")
(define BITSTAMP_TICKER "https://www.bitstamp.net/api/ticker/")
(define IMAGE_SEARCH_BASE "http://ajax.googleapis.com/ajax/services/search/images?v=1.0&q=")
(define GOOGLE_SEARCH_BASE
  (if (and GOOGLE_API_KEY GOOGLE_SEARCH_CX)
    (string-append
      "https://www.googleapis.com/customsearch/v1?num=1&safe=off&key="
      GOOGLE_API_KEY
      "&cx="
      GOOGLE_SEARCH_CX
      "&q=")
    #f))
(define YOUTUBE_SEARCH_BASE
  (if GOOGLE_API_KEY
    (string-append
      "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=1&safeSearch=none&key="
      GOOGLE_API_KEY
      "&q=")
    #f))
(define wikipedia-api-base "https://en.wikipedia.org/w/api.php?action=query&list=search&format=json&srlimit=1&srsearch=")
(define wikipedia-page-base "https://en.wikipedia.org/wiki/")
(define COINCAP_BASE "https://api.coinmarketcap.com/v1/ticker/")

(define (exchange? exchange)
  (memq exchange '('bitstamp 'gox)))

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
(define (btc->usd-string exchange)
  (match exchange
    ['bitstamp (let ([res (query-json-service BITSTAMP_TICKER)])
                 (string-append "$" (hash-ref res 'last)))]))

#|
Calls btc->usd-string in a new thread, then passes the result to fn

Returns immediately
|#
(define (btc->usd-string-async fn)
  (thread
    (lambda () (fn (string-append "bitstamp: " (btc->usd-string 'bitstamp))))))

#|
Given a query string, returns a cons cell with a snippet description and link
to wikipedia page:
(cons snippet link)

On error, returns #f
|#
(define (query-wikipedia query)
  (if (equal? query "")
    #f
    (let ([res (query-json-service (string-append wikipedia-api-base
                                                   (uri-encode query)))])
      (if (or (hash-has-key? res 'error)
              (zero? (from-nested-hash res '(query searchinfo totalhits))))
        #f
        (let ([data (car (from-nested-hash res '(query search)))])
          (cons (string-normalize-spaces
                  (strip-tags (hash-ref data 'snippet)))
                (string-append wikipedia-page-base
                               (uri-encode (hash-ref data 'title)))))))))

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
Searches google for query and calls fn on the result

Requires that GOOGLE_API_KEY and GOOGLE_SEARCH_CX are valid
|#
(define (query-google query fn)
  (let ([res (query-json-service (string-append GOOGLE_SEARCH_BASE query))])
    (if (equal? "0" (from-nested-hash res (list 'searchInformation 'totalResults)))
      (fn (string-append "no results for " query))
      (let ([item (car (hash-ref res 'items))])
        (fn (string-append (hash-ref item 'title) " - " (hash-ref item 'link)))
        (fn (hash-ref item 'snippet))))))

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
Do an image search!
|#
(define (query-image query fn)
  (let ([res (query-json-service (string-append IMAGE_SEARCH_BASE query))])
    (if (empty? (from-nested-hash res (list 'responseData 'results)))
      (fn (string-append "no results for " query))
      ; TODO: Make Petebot select a random image of the first few?
      (let ([item (car (from-nested-hash res (list 'responseData 'results)))])
        (fn (hash-ref item 'unescapedUrl))))))

#|
Search for the price in USD of a cryptocurrency
|#
(define (query-coin-cap query fn)
  (define res (query-json-service (string-append COINCAP_BASE query "/")))
  (if (hash? res)
    (fn (string-append "no results for " query))
    (fn (string-append (hash-ref (car res) 'name)
                       ": $"
                       (hash-ref (car res) 'price_usd)))))


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
