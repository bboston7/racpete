#lang racket

(require rackunit
	"../src/commands/convert-base.rkt")

#|
Test that we can properly convert to and from various bases
|#
(define/provide-test-suite
	test-convert-base
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
	(check-equal? #f (convert-base 5 "9" "4"))
	(check-equal? #f (convert-base "5" 9 "4"))
	(check-equal? #f (convert-base "5" "9" 4))
	(check-equal? #f (convert-base 5 9 "4"))
	(check-equal? #f (convert-base 5 "9" 4))
	(check-equal? #f (convert-base "5" 9 4))
	(check-equal? #f (convert-base 5 9 4)))

	; (check-equal? #f (convert-base "aoeu" "14" "14")))