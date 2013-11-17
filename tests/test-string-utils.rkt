#lang racket

(require rackunit
         "../src/util/string-utils.rkt")

#|
Test the string-contains? function
|#
(define/provide-test-suite
  test-string-contains?
  (check-true (string-contains? "abcd" "b") "Basic string contains")
  (check-true (string-contains? "abcd" "ab") "Substring at front")
  (check-true (string-contains? "abcd" "cd") "Substring at end")
  (check-true (string-contains? "abcd" "abcd") "str == token")
  (check-true (string-contains? "abcd" "AB") "Unmatched case")
  (check-true (string-contains? "abcd" "") "Empty string token")
  (check-true (string-contains? "" "") "str == token == empty string")
  (check-false (string-contains? "" "abcd") "Empty string str")
  (check-false (string-contains? "abcd" "ef") "token not in str")
  (check-false (string-contains? "abcd" "abcde") "str substring of token")
  (check-false (string-contains? "abcd" "abe") "partial match"))

#|
Test the string-starts-with? function
|#
(define/provide-test-suite
  test-string-starts-with?
  (check-true (string-starts-with? "abcd" "ab") "Basic string starts with")
  (check-true (string-starts-with? "abcd" "abcd") "str == token")
  (check-true (string-starts-with? "abcd" "") "token empty string")
  (check-true (string-starts-with? "" "") "str == token == empty string")
  (check-true (string-starts-with? "abcd" "AB") "Unmatched case")
  (check-false (string-starts-with? "abcd" "ef") "token not in str")
  (check-false (string-starts-with? "abcd" "abe") "partial match")
  (check-false (string-starts-with? "" "abe") "empty str"))
