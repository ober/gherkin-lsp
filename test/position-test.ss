;;; -*- Gerbil -*-
;;; Tests for lsp/util/position
(import :std/test
        :lsp/lsp/util/position)

(export position-test-suite)

(def position-test-suite
  (test-suite "lsp/util/position"

    ;; --- offset->line-col ---
    (test-case "offset->line-col: offset=0"
      (let-values (((line col) (offset->line-col "hello" 0)))
        (check line => 0)
        (check col => 0)))

    (test-case "offset->line-col: mid-line"
      (let-values (((line col) (offset->line-col "hello" 3)))
        (check line => 0)
        (check col => 3)))

    (test-case "offset->line-col: after newline"
      (let-values (((line col) (offset->line-col "ab\ncd" 4)))
        (check line => 1)
        (check col => 1)))

    (test-case "offset->line-col: at newline"
      (let-values (((line col) (offset->line-col "ab\ncd" 2)))
        (check line => 0)
        (check col => 2)))

    (test-case "offset->line-col: past end"
      (let-values (((line col) (offset->line-col "ab" 10)))
        (check line => 0)
        (check col => 2)))

    ;; --- line-col->offset ---
    (test-case "line-col->offset: 0,0"
      (check (line-col->offset "hello" 0 0) => 0))

    (test-case "line-col->offset: mid-first-line"
      (check (line-col->offset "hello" 0 3) => 3))

    (test-case "line-col->offset: second line start"
      (check (line-col->offset "ab\ncd" 1 0) => 3))

    (test-case "line-col->offset: second line mid"
      (check (line-col->offset "ab\ncd" 1 1) => 4))

    (test-case "line-col->offset: round-trip"
      (let* ((text "foo\nbar\nbaz")
             (offset (line-col->offset text 2 1)))
        (let-values (((line col) (offset->line-col text offset)))
          (check line => 2)
          (check col => 1))))

    ;; --- text-line-at ---
    (test-case "text-line-at: first line"
      (check-equal? (text-line-at "abc\ndef\nghi" 0) "abc"))

    (test-case "text-line-at: middle line"
      (check-equal? (text-line-at "abc\ndef\nghi" 1) "def"))

    (test-case "text-line-at: last line"
      (check-equal? (text-line-at "abc\ndef\nghi" 2) "ghi"))

    (test-case "text-line-at: past end"
      (check-equal? (text-line-at "abc\ndef" 5) ""))

    (test-case "text-line-at: empty text"
      (check-equal? (text-line-at "" 0) ""))

    ;; --- symbol-at-position ---
    (test-case "symbol-at-position: word at start"
      (let-values (((sym start end) (symbol-at-position "hello world" 0 0)))
        (check-equal? sym "hello")
        (check start => 0)
        (check end => 5)))

    (test-case "symbol-at-position: word at middle of word"
      (let-values (((sym start end) (symbol-at-position "hello world" 0 2)))
        (check-equal? sym "hello")
        (check start => 0)
        (check end => 5)))

    (test-case "symbol-at-position: second word"
      (let-values (((sym start end) (symbol-at-position "hello world" 0 7)))
        (check-equal? sym "world")
        (check start => 6)
        (check end => 11)))

    (test-case "symbol-at-position: on whitespace"
      (let-values (((sym start end) (symbol-at-position "hello world" 0 5)))
        (check sym => #f)
        (check start => #f)
        (check end => #f)))

    (test-case "symbol-at-position: past bounds"
      (let-values (((sym start end) (symbol-at-position "hello" 0 20)))
        (check sym => #f)))

    (test-case "symbol-at-position: empty text"
      (let-values (((sym start end) (symbol-at-position "" 0 0)))
        (check sym => #f)))

    (test-case "symbol-at-position: multiline"
      (let-values (((sym start end) (symbol-at-position "foo\nbar-baz" 1 2)))
        (check-equal? sym "bar-baz")))

    ;; --- symbol-char? ---
    (test-case "symbol-char?: alpha"
      (check (symbol-char? #\a) => #t)
      (check (symbol-char? #\Z) => #t))

    (test-case "symbol-char?: numeric"
      (check (symbol-char? #\0) => #t)
      (check (symbol-char? #\9) => #t))

    (test-case "symbol-char?: special chars"
      (check (and (symbol-char? #\-) #t) => #t)
      (check (and (symbol-char? #\_) #t) => #t)
      (check (and (symbol-char? #\!) #t) => #t)
      (check (and (symbol-char? #\?) #t) => #t)
      (check (and (symbol-char? #\*) #t) => #t)
      (check (and (symbol-char? #\+) #t) => #t)
      (check (and (symbol-char? #\/) #t) => #t)
      (check (and (symbol-char? #\<) #t) => #t)
      (check (and (symbol-char? #\>) #t) => #t)
      (check (and (symbol-char? #\=) #t) => #t)
      (check (and (symbol-char? #\.) #t) => #t)
      (check (and (symbol-char? #\:) #t) => #t)
      (check (and (symbol-char? #\#) #t) => #t))

    (test-case "symbol-char?: non-symbol chars"
      (check (symbol-char? #\space) => #f)
      (check (symbol-char? #\() => #f)
      (check (symbol-char? #\)) => #f)
      (check (symbol-char? #\newline) => #f))

    ;; --- make-lsp-position ---
    (test-case "make-lsp-position: structure"
      (let ((pos (make-lsp-position 5 10)))
        (check (hash-ref pos "line") => 5)
        (check (hash-ref pos "character") => 10)))

    ;; --- make-lsp-range ---
    (test-case "make-lsp-range: structure"
      (let ((range (make-lsp-range 1 2 3 4)))
        (let ((start (hash-ref range "start"))
              (end (hash-ref range "end")))
          (check (hash-ref start "line") => 1)
          (check (hash-ref start "character") => 2)
          (check (hash-ref end "line") => 3)
          (check (hash-ref end "character") => 4))))

    ;; --- make-lsp-location ---
    (test-case "make-lsp-location: structure"
      (let ((loc (make-lsp-location "file:///test.ss"
                                     (make-lsp-range 0 0 0 5))))
        (check-equal? (hash-ref loc "uri") "file:///test.ss")
        (check (hash-ref (hash-ref (hash-ref loc "range") "start") "line") => 0)))
  ))

(def main
  (lambda ()
    (run-tests! position-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
