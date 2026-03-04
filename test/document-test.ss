;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/document
(import :std/test
        :lsp/lsp/analysis/document)

(export document-test-suite)

(def document-test-suite
  (test-suite "lsp/analysis/document"

    ;; --- make-document-from-open ---
    (test-case "make-document-from-open: full params"
      (let ((doc (make-document-from-open
                   (hash ("textDocument"
                          (hash ("uri" "file:///test.ss")
                                ("version" 1)
                                ("text" "(def x 42)")
                                ("languageId" "gerbil")))))))
        (check-equal? (document-uri doc) "file:///test.ss")
        (check (document-version doc) => 1)
        (check-equal? (document-text doc) "(def x 42)")
        (check-equal? (document-language-id doc) "gerbil")))

    (test-case "make-document-from-open: missing keys use defaults"
      (let ((doc (make-document-from-open (hash))))
        (check-equal? (document-uri doc) "")
        (check (document-version doc) => 0)
        (check-equal? (document-text doc) "")
        (check-equal? (document-language-id doc) "gerbil")))

    ;; --- document-apply-full-change ---
    (test-case "document-apply-full-change: updates text and version"
      (let* ((doc (make-document "file:///t.ss" 1 "old text" "gerbil"))
             (updated (document-apply-full-change doc "new text" 2)))
        (check-equal? (document-uri updated) "file:///t.ss")
        (check-equal? (document-language-id updated) "gerbil")
        (check-equal? (document-text updated) "new text")
        (check (document-version updated) => 2)))

    ;; --- document-line-count ---
    (test-case "document-line-count: no newlines"
      (let ((doc (make-document "" 0 "hello" "")))
        (check (document-line-count doc) => 1)))

    (test-case "document-line-count: one newline"
      (let ((doc (make-document "" 0 "a\nb" "")))
        (check (document-line-count doc) => 2)))

    (test-case "document-line-count: multiple newlines"
      (let ((doc (make-document "" 0 "a\nb\nc\nd" "")))
        (check (document-line-count doc) => 4)))

    (test-case "document-line-count: trailing newline"
      (let ((doc (make-document "" 0 "a\nb\n" "")))
        (check (document-line-count doc) => 3)))

    ;; --- document-line-at ---
    (test-case "document-line-at: first line"
      (let ((doc (make-document "" 0 "abc\ndef\nghi" "")))
        (check-equal? (document-line-at doc 0) "abc")))

    (test-case "document-line-at: middle line"
      (let ((doc (make-document "" 0 "abc\ndef\nghi" "")))
        (check-equal? (document-line-at doc 1) "def")))

    (test-case "document-line-at: last line"
      (let ((doc (make-document "" 0 "abc\ndef\nghi" "")))
        (check-equal? (document-line-at doc 2) "ghi")))

    (test-case "document-line-at: past end"
      (let ((doc (make-document "" 0 "abc\ndef" "")))
        (check-equal? (document-line-at doc 10) "")))

    ;; --- document-lines ---
    (test-case "document-lines: empty text"
      (let ((doc (make-document "" 0 "" "")))
        (check-equal? (document-lines doc) '(""))))

    (test-case "document-lines: single line"
      (let ((doc (make-document "" 0 "hello" "")))
        (check-equal? (document-lines doc) '("hello"))))

    (test-case "document-lines: multiple lines"
      (let ((doc (make-document "" 0 "a\nb\nc" "")))
        (check-equal? (document-lines doc) '("a" "b" "c"))))

    (test-case "document-lines: trailing newline"
      (let ((doc (make-document "" 0 "a\nb\n" "")))
        (check-equal? (document-lines doc) '("a" "b" ""))))
  ))

(def main
  (lambda ()
    (run-tests! document-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
