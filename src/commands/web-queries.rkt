#lang racket

(require net/uri-codec
         net/url
         xml
         xml/path
         "../util/string-utils.rkt")

(provide (contract-out
           [query-wikipedia (-> string? (or/c boolean? string?))]))

(permissive-xexprs #t)

(define wikipedia-api-base "http://en.wikipedia.org/w/api.php?action=query&list=search&format=xml&srlimit=1&srsearch=")

(define (query-wikipedia query)
  (if (equal? query "")
    #f
    (let ([res (query-service (string-append wikipedia-api-base (uri-encode
                                                                  query)))])
      (if (or (eq? (caaddr res) 'error)
              (equal? (cadr (assoc 'totalhits (cadr (se-path* '(query) res)))) 
                      "0"))
        #f
        (string-normalize-spaces
          (strip-tags (cadr (caddar (cdaddr (cadddr (cadddr res))))))))))) ; What have I done

(define (query-service url)
  (xml->xexpr (document-element (read-xml (get-pure-port
                                            (string->url url))))))
