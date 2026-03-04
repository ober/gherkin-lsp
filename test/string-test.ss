;;; -*- Gerbil -*-
;;; Tests for lsp/util/string
(import :std/test
        :lsp/lsp/util/string)

(export string-test-suite)

(def string-test-suite
  (test-suite "lsp/util/string"

    ;; --- string-split-lines ---
    (test-case "string-split-lines: empty string"
      (check-equal? (string-split-lines "") '("")))

    (test-case "string-split-lines: no newlines"
      (check-equal? (string-split-lines "hello") '("hello")))

    (test-case "string-split-lines: trailing newline"
      (check-equal? (string-split-lines "hello\n") '("hello" "")))

    (test-case "string-split-lines: multiple lines"
      (check-equal? (string-split-lines "a\nb\nc") '("a" "b" "c")))

    (test-case "string-split-lines: multiple trailing newlines"
      (check-equal? (string-split-lines "a\n\n") '("a" "" "")))

    ;; --- string-contains ---
    (test-case "string-contains: match"
      (check (string-contains "hello world" "world") => #t))

    (test-case "string-contains: no match"
      (check (string-contains "hello world" "xyz") => #f))

    (test-case "string-contains: empty needle"
      (check (string-contains "hello" "") => #t))

    (test-case "string-contains: empty haystack"
      (check (string-contains "" "x") => #f))

    (test-case "string-contains: needle longer than haystack"
      (check (string-contains "hi" "hello") => #f))

    (test-case "string-contains: at start"
      (check (string-contains "hello" "hel") => #t))

    (test-case "string-contains: at end"
      (check (string-contains "hello" "llo") => #t))

    ;; --- string-contains-ci ---
    (test-case "string-contains-ci: case folding"
      (check (string-contains-ci "Hello World" "hello") => #t))

    (test-case "string-contains-ci: no match"
      (check (string-contains-ci "Hello" "xyz") => #f))

    (test-case "string-contains-ci: empty strings"
      (check (string-contains-ci "" "") => #t))

    (test-case "string-contains-ci: empty needle"
      (check (string-contains-ci "Hello" "") => #t))

    ;; --- string-suffix? ---
    (test-case "string-suffix?: match"
      (check (string-suffix? ".ss" "test.ss") => #t))

    (test-case "string-suffix?: no match"
      (check (string-suffix? ".ss" "test.scm") => #f))

    (test-case "string-suffix?: empty suffix"
      (check (string-suffix? "" "hello") => #t))

    (test-case "string-suffix?: suffix longer than string"
      (check (string-suffix? "toolong" "hi") => #f))

    (test-case "string-suffix?: equal strings"
      (check (string-suffix? "hello" "hello") => #t))

    ;; --- string-join ---
    (test-case "string-join: empty list"
      (check-equal? (string-join '() ", ") ""))

    (test-case "string-join: single element"
      (check-equal? (string-join '("a") ", ") "a"))

    (test-case "string-join: multiple elements"
      (check-equal? (string-join '("a" "b" "c") ", ") "a, b, c"))

    (test-case "string-join: empty separator"
      (check-equal? (string-join '("a" "b") "") "ab"))

    ;; --- string-join-newline ---
    (test-case "string-join-newline: empty list"
      (check-equal? (string-join-newline '()) ""))

    (test-case "string-join-newline: single element"
      (check-equal? (string-join-newline '("hello")) "hello"))

    (test-case "string-join-newline: multiple elements"
      (check-equal? (string-join-newline '("a" "b" "c")) "a\nb\nc"))

    ;; --- take-at-most ---
    (test-case "take-at-most: n=0"
      (check-equal? (take-at-most '(1 2 3) 0) '()))

    (test-case "take-at-most: n > length"
      (check-equal? (take-at-most '(1 2) 5) '(1 2)))

    (test-case "take-at-most: n < length"
      (check-equal? (take-at-most '(1 2 3 4 5) 3) '(1 2 3)))

    (test-case "take-at-most: empty list"
      (check-equal? (take-at-most '() 5) '()))

    (test-case "take-at-most: negative n"
      (check-equal? (take-at-most '(1 2 3) -1) '()))

    ;; --- classify-text-regions ---
    (test-case "classify-text-regions: pure code"
      (let ((r (classify-text-regions "(def x 1)")))
        (check (u8vector-ref r 0) => 0)  ;; ( is code
        (check (u8vector-ref r 4) => 0)  ;; x is code
        (check (u8vector-length r) => 9)))

    (test-case "classify-text-regions: string literal"
      (let ((r (classify-text-regions "(def x \"hello\")")))
        ;; The 'h' inside the string should be 1
        (check (u8vector-ref r 7) => 1)   ;; " opening
        (check (u8vector-ref r 8) => 1)   ;; h
        (check (u8vector-ref r 13) => 1)  ;; " closing
        ;; The ( should be code
        (check (u8vector-ref r 0) => 0)))

    (test-case "classify-text-regions: line comment"
      (let ((r (classify-text-regions "x ; comment")))
        (check (u8vector-ref r 0) => 0)   ;; x is code
        (check (u8vector-ref r 2) => 2)   ;; ; is comment
        (check (u8vector-ref r 4) => 2)   ;; c is comment
        (check (u8vector-ref r 10) => 2))) ;; t is comment

    (test-case "classify-text-regions: block comment"
      (let ((r (classify-text-regions "a #| block |# b")))
        (check (u8vector-ref r 0) => 0)    ;; a is code
        (check (u8vector-ref r 2) => 2)    ;; # is comment
        (check (u8vector-ref r 5) => 2)    ;; b inside comment
        (check (u8vector-ref r 12) => 2)   ;; # closing
        (check (u8vector-ref r 14) => 0))) ;; b after is code

    (test-case "classify-text-regions: escape in string"
      (let ((r (classify-text-regions "\"a\\\"b\"")))
        ;; The entire "a\"b" should be string (1)
        (check (u8vector-ref r 0) => 1)   ;; opening "
        (check (u8vector-ref r 2) => 1)   ;; backslash
        (check (u8vector-ref r 3) => 1)   ;; escaped "
        (check (u8vector-ref r 5) => 1))) ;; closing "

    (test-case "classify-text-regions: empty string"
      (let ((r (classify-text-regions "")))
        (check (u8vector-length r) => 0)))

    ;; --- in-string-or-comment? ---
    (test-case "in-string-or-comment?: code position"
      (let ((r (classify-text-regions "(def x 1)")))
        (check (in-string-or-comment? r 0) => #f)))

    (test-case "in-string-or-comment?: string position"
      (let ((r (classify-text-regions "(def x \"hi\")")))
        (check (in-string-or-comment? r 8) => #t)))

    (test-case "in-string-or-comment?: comment position"
      (let ((r (classify-text-regions "x ; y")))
        (check (in-string-or-comment? r 3) => #t)))

    (test-case "in-string-or-comment?: out of bounds"
      (let ((r (classify-text-regions "x")))
        (check (in-string-or-comment? r 100) => #f)))
  ))

(def main
  (lambda ()
    (run-tests! string-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
