;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/formatting
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/validation
        :lsp/lsp/handlers/formatting)

(export formatting-test-suite)

(def formatting-test-suite
  (test-suite "lsp/handlers/formatting"

    ;; --- comment-line? ---
    (test-case "comment-line?: leading semicolon"
      (check (comment-line? "; comment") => #t))

    (test-case "comment-line?: indented semicolon"
      (check (comment-line? "  ; comment") => #t))

    (test-case "comment-line?: triple semicolon"
      (check (comment-line? ";;; comment") => #t))

    (test-case "comment-line?: code line"
      (check (comment-line? "(def x 1)") => #f))

    (test-case "comment-line?: empty"
      (check (comment-line? "") => #f))

    (test-case "comment-line?: whitespace only"
      (check (comment-line? "   ") => #f))

    ;; --- blank-line? ---
    (test-case "blank-line?: empty string"
      (check (blank-line? "") => #t))

    (test-case "blank-line?: spaces"
      (check (blank-line? "   ") => #t))

    (test-case "blank-line?: tabs"
      (check (blank-line? "\t\t") => #t))

    (test-case "blank-line?: mixed whitespace"
      (check (blank-line? " \t ") => #t))

    (test-case "blank-line?: has char"
      (check (blank-line? "  x  ") => #f))

    ;; --- inline-comment-line? ---
    (test-case "inline-comment-line?: code with semicolon comment"
      (check (inline-comment-line? "(def x 1) ; a variable") => #t))

    (test-case "inline-comment-line?: just a comment"
      (check (inline-comment-line? "; just a comment") => #f))

    (test-case "inline-comment-line?: no semicolon"
      (check (inline-comment-line? "(def x 1)") => #f))

    (test-case "inline-comment-line?: semicolon inside string"
      ;; Semicolons inside strings should not count
      (check (inline-comment-line? "(def x \";not a comment\")") => #f))

    (test-case "inline-comment-line?: empty"
      (check (inline-comment-line? "") => #f))

    ;; --- count-lines ---
    (test-case "count-lines: no newlines"
      (check (count-lines "hello") => 0))

    (test-case "count-lines: one newline"
      (check (count-lines "a\nb") => 1))

    (test-case "count-lines: multiple newlines"
      (check (count-lines "a\nb\nc\nd") => 3))

    (test-case "count-lines: empty"
      (check (count-lines "") => 0))

    ;; --- last-line-length ---
    (test-case "last-line-length: no newline"
      (check (last-line-length "hello") => 5))

    (test-case "last-line-length: after newline"
      (check (last-line-length "abc\nde") => 2))

    (test-case "last-line-length: empty"
      (check (last-line-length "") => 0))

    (test-case "last-line-length: trailing newline"
      (check (last-line-length "abc\n") => 0))

    ;; --- format-gerbil-source ---
    (test-case "format-gerbil-source: preserves comments"
      (let ((result (format-gerbil-source "; a comment\n(def x 1)")))
        (check (string? result) => #t)
        ;; Comment should be preserved
        (check (and (string-contains-format result "; a comment") #t) => #t)))

    (test-case "format-gerbil-source: preserves blank lines"
      (let ((result (format-gerbil-source "(def x 1)\n\n(def y 2)")))
        (check (string? result) => #t)
        ;; The blank line between forms should be preserved
        (check (and (string-contains-format result "\n\n") #t) => #t)))

    (test-case "format-gerbil-source: formats code"
      (let ((result (format-gerbil-source "(def   x   1)")))
        (check (string? result) => #t)
        ;; Extra whitespace should be removed
        (check (not (string-contains-format result "   x   ")) => #t)))

    (test-case "format-gerbil-source: inline comments preserved"
      (let ((result (format-gerbil-source "(def x 1) ; inline\n(def y 2)")))
        (check (string? result) => #t)
        (check (and (string-contains-format result "; inline") #t) => #t)))

    ;; --- handle-formatting: integration ---
    (test-case "handle-formatting: returns text edits for open document"
      (let* ((uri "file:///test-format.ss")
             (text "(def   x   1)\n(def y 2)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("options" (hash ("tabSize" 2)
                                              ("insertSpaces" #t)))))
               (result (handle-formatting params)))
          (check (vector? result) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/formatting" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-formatting: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("options" (hash ("tabSize" 2)
                                            ("insertSpaces" #t)))))
             (result (handle-formatting params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)
                   (void? result)) => #t)))
  ))

;; Local helper for string-contains
(def (string-contains-format str needle)
  (let ((hlen (string-length str))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring str i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

(def main
  (lambda ()
    (run-tests! formatting-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
