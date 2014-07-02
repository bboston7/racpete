#!/usr/bin/racket
#lang racket

(require rackunit/text-ui
         "test-string-utils.rkt"
         "test-convert-base.rkt")

#|
This list of test suites to run.  Please keep this alphabetical so I don't
lose my mind --Brett

TODO: Can we populate this list automatically?
|#
(define suites (list
                 test-string-contains?
                 test-string-starts-with?
                 test-convert-base))

(define (all-pass? suite)
  (= 0 (run-tests suite)))

(define passed (foldl
                 (lambda (v l) (and v l))
                 #t
                 (map all-pass? suites)))

(if passed
  (begin
    (display "\e[1;32mPassed!\n")
    (exit))
  (begin
    (display "\e[1;31mFailed\n")
    (exit 1)))
