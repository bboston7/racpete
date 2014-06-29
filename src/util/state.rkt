#lang racket

(provide define-state)

#|
Utility for managing state.
|#

(struct blank ())

;; We need to use (hash-copy) because (read) returns an immutable hash table,
 ; and we need an immutable one.
(define (load-state)
  (with-handlers ([exn:fail:filesystem? (lambda (x) (make-hash))])
                 (hash-copy (with-input-from-file "state" read))))

(define (save-state state)
  (with-handlers ([exn:fail:filesystem? (lambda (x) (void))])
                 (with-output-to-file "state" (lambda () (write state)) #:exists 'replace)))

(define (make-state state key val)
  (hash-ref state key (lambda ()
                        (hash-set! state key val)
                        (save-state state)))
  (lambda ([newval (blank)])
    (if (blank? newval)
      (hash-ref state key (lambda () val))
      (begin (hash-set! state key newval) (save-state state)))))

(define state (load-state))

(define-syntax-rule (define-state key default)
  (define key (make-state state 'key default)))

