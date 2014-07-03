#lang racket

(require racket/serialize)

(provide define-state
         sstruct)

(define-syntax-rule (sstruct id ...) (serializable-struct id ...))

#|
Utility for managing state.
|#

(struct blank ())

;; We need to use (hash-copy) because (read) returns an immutable hash table,
 ; and we need an immutable one.
(define (load-state)
  (with-handlers ([exn:fail? (lambda (x) (make-hash))])
                 (deserialize (with-input-from-file "state" read))))

(define (save-state state)
  (with-handlers ([exn:fail:filesystem? (lambda (x) (void))])
                 (parameterize ([print-unreadable #f])
                   (with-output-to-file "state" (lambda () (write (serialize state))) #:exists 'replace))))

(define (make-state state key val)
  (lambda ([newval (blank)])
    (if (blank? newval)
      (begin (when (null? state) (set! state (load-state))) (hash-ref state key (lambda () val)))
      (begin (hash-set! state key newval) (save-state state)))))

(define state null)

(define-syntax-rule (define-state key default)
  (define key (make-state state 'key default)))

