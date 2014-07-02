#lang racket

(provide
    convert-base
)

#|
Convert (car args-list) from base (cadr args-list) to base (caddr args-list)
|#
(define (convert-base args-list)
    (with-handlers ([exn:fail? (lambda (v) "No.")])
    (if (and (member (string->number (caddr args-list)) (range 2 17 1))
             (member (string->number (cadr args-list)) (range 2 17 1)))
        (convert-to-string (string->number (car args-list) (string->number (cadr args-list)))
                           (string->number (caddr args-list)))
      "No.")))

#|
Converts num to a string with radix to-rad
|#
(define (convert-to-string num to-rad)
    (define (do-thing num to-rad so-far)
        (if (< num to-rad)
            (string-append (number->string num 16) so-far)
            (do-thing (floor (/ num to-rad)) to-rad
                            (string-append (number->string (modulo num to-rad) 16) so-far))))
    
    (do-thing num to-rad ""))


; Tests
#|
Test that we can properly convert to and from various bases
|#
(module+ test
        (require rackunit)
        (check-equal? "1010" (convert-base (list "10" "10" "2")))
        (check-equal? "10" (convert-base (list "1010" "2" "10")))
        (check-equal? "2e6" (convert-base (list "10432" "5" "16")))
        (check-equal? "10432" (convert-base (list "2e6" "16" "5")))
        (check-equal? "11001001000111" (convert-base (list "52345" "7" "2")))
        (check-equal? "52345" (convert-base (list "11001001000111" "2" "7")))
        (check-equal? "300121021" (convert-base (list "52345" "14" "4")))
        (check-equal? "52345" (convert-base (list "300121021" "4" "14")))
        (check-equal? "11111" (convert-base (list "11111" "5" "5")))
        (check-equal? "2dcea44a2" (convert-base (list "34657544345" "9" "16")))
        (check-equal? "34657544345" (convert-base (list "2dcea44a2" "16" "9")))

        ; Edge cases because if there's a way to break it, max WILL break it
        (check-equal? "No." (convert-base (list 5 "9" "4")))
        (check-equal? "No." (convert-base (list "5" '() "4")))
        (check-equal? "No." (convert-base (list "5" "9" #f)))
        (check-equal? "No." (convert-base (list 5 9 "4")))
        (check-equal? "No." (convert-base (list 5 "9" 4)))
        (check-equal? "No." (convert-base (list "5" 9 4)))
        (check-equal? "No." (convert-base (list 5 9 4)))

        (check-equal? "No." (convert-base (list "aoeu" "14" "14")))
        (check-equal? "No." (convert-base (list "10" "2f" "14")))
        (check-equal? "No." (convert-base (list "500" "14" "n")))
        (check-equal? "No." (convert-base (list "5" "4" "14")))
        (check-equal? "No." (convert-base (list "344" "2" "14")))
        (check-equal? "No." (convert-base (list "100" "17" "14")))
        (check-equal? "No." (convert-base (list "100" "14" "17")))
        (check-equal? "No." (convert-base (list "100" "1" "14")))
        (check-equal? "No." (convert-base (list "100" "14" "1")))

        (check-equal? "No." (convert-base (list "7")))
        (check-equal? "No." (convert-base (list "7" "10"))))
