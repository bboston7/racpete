#lang racket

(require json
         net/uri-codec
         net/url
         xml
         xml/path
         "../util/string-utils.rkt")

(provide (contract-out
           [btc->usd-string (-> (or/c boolean? usd-string?))]
           [query-wikipedia (-> string? (or/c boolean? pair?))]))

(permissive-xexprs #t)

(define MT_GOX_TICKER "http://data.mtgox.com/api/2/BTCUSD/money/ticker_fast")
(define wikipedia-api-base "http://en.wikipedia.org/w/api.php?action=query&list=search&format=xml&srlimit=1&srsearch=")
(define wikipedia-page-base "http://en.wikipedia.org/wiki/")

(define (usd-string? str)
  (regexp-match? #px"^\\$[0-9]+\\.[0-9]{2}$" str))

#|
Finds the current cost of a bitcoin according to the MtGox ticker and returns
the value as a usd-string? or #f if the API call fails.
|#
(define (btc->usd-string)
  (let ([res (query-json-service MT_GOX_TICKER)])
    (if (equal? (hash-ref res 'result) "success")
      (hash-ref (hash-ref (hash-ref res 'data) 'last) 'display)
      #f)))



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

(define (query-xml-service url)
  (xml->xexpr (document-element (read-xml (get-pure-port
                                            (string->url url))))))

#|
Get a jsexpr? from querying a json API
|#
(define (query-json-service url) (read-json (get-pure-port (string->url url))))
