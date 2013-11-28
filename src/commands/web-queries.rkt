#lang racket

(require net/uri-codec
         net/url
         xml
         xml/path
         "../util/string-utils.rkt")

(provide (contract-out
           [query-wikipedia (-> string? (or/c boolean? pair?))]))

(permissive-xexprs #t)

(define wikipedia-api-base "http://en.wikipedia.org/w/api.php?action=query&list=search&format=xml&srlimit=1&srsearch=")
(define wikipedia-page-base "http://en.wikipedia.org/wiki/")


#|
Given a query string, returns a cons cell with a snippet description and link
to wikipedia page:
(cons snippet link)

On error, returns #f
|#
(define (query-wikipedia query)
  (if (equal? query "")
    #f
    (let ([res (query-service (string-append wikipedia-api-base (uri-encode
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

(define (query-service url)
  (xml->xexpr (document-element (read-xml (get-pure-port
                                            (string->url url))))))
