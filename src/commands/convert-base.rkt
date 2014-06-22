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