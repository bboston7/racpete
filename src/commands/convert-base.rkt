#lang racket

(provide
	convert-base
)

#|
Convert num from base from-rad to base to-rad
|#
(define (convert-base num from-rad to-rad)
  	(with-handlers ([exn:fail? (lambda (v) "No.")])
      (if (and (member (string->number to-rad) (range 2 17 1))
               (member (string->number from-rad) (range 2 17 1)))
        (convert-to-string (string->number num (string->number from-rad))
                           (string->number to-rad))
      "No.")))

#|
Converts num to a string with radix to-rad
|#
(define (convert-to-string num to-rad)
	(define (do-thing num to-rad so-far)
		(cond
			[(< num to-rad) (string-append (number->string num 16) so-far)]
			[else (do-thing (floor (/ num to-rad)) to-rad
							(string-append (number->string (modulo num to-rad) 16) so-far))]))
	
	(do-thing num to-rad ""))


; Tests
#|
Test that we can properly convert to and from various bases
|#
(module+ test
        (require rackunit)
        (check-equal? "1010" (convert-base "10" "10" "2"))
        (check-equal? "10" (convert-base "1010" "2" "10"))
        (check-equal? "2e6" (convert-base "10432" "5" "16"))
        (check-equal? "10432" (convert-base "2e6" "16" "5"))
        (check-equal? "11001001000111" (convert-base "52345" "7" "2"))
        (check-equal? "52345" (convert-base "11001001000111" "2" "7"))
        (check-equal? "300121021" (convert-base "52345" "14" "4"))
        (check-equal? "52345" (convert-base "300121021" "4" "14"))
        (check-equal? "11111" (convert-base "11111" "5" "5"))
        (check-equal? "2dcea44a2" (convert-base "34657544345" "9" "16"))
        (check-equal? "34657544345" (convert-base "2dcea44a2" "16" "9"))

        ; Edge cases because if there's a way to break it, max WILL break it
        (check-equal? "No." (convert-base 5 "9" "4"))
        (check-equal? "No." (convert-base "5" '() "4"))
        (check-equal? "No." (convert-base "5" "9" #f))
        (check-equal? "No." (convert-base 5 9 "4"))
        (check-equal? "No." (convert-base 5 "9" 4))
        (check-equal? "No." (convert-base "5" 9 4))
        (check-equal? "No." (convert-base 5 9 4))

        (check-equal? "No." (convert-base "aoeu" "14" "14"))
        (check-equal? "No." (convert-base "10" "2f" "14"))
        (check-equal? "No." (convert-base "500" "14" "n"))
        (check-equal? "No." (convert-base "5" "4" "14"))
        (check-equal? "No." (convert-base "344" "2" "14"))
        (check-equal? "No." (convert-base "100" "17" "14"))
        (check-equal? "No." (convert-base "100" "14" "17"))
        (check-equal? "No." (convert-base "100" "1" "14"))
        (check-equal? "No." (convert-base "100" "14" "1")))