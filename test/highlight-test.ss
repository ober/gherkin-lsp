;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/highlight
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/highlight)

(export highlight-test-suite)

(def highlight-test-suite
  (test-suite "lsp/handlers/highlight"

    ;; --- find-highlights ---
    (test-case "find-highlights: single occurrence"
      (let ((hl (find-highlights "def foo bar" "foo")))
        (check (length hl) => 1)
        (let* ((h (car hl))
               (range (hash-ref h "range"))
               (start (hash-ref range "start")))
          (check (hash-ref start "line") => 0)
          (check (hash-ref start "character") => 4))))

    (test-case "find-highlights: multiple occurrences"
      (let ((hl (find-highlights "foo bar foo" "foo")))
        (check (length hl) => 2)))

    (test-case "find-highlights: multi-line"
      (let ((hl (find-highlights "foo\nbar\nfoo" "foo")))
        (check (length hl) => 2)
        ;; First on line 0, second on line 2
        (let ((lines (map (lambda (h)
                            (hash-ref (hash-ref (hash-ref h "range") "start") "line"))
                          hl)))
          (check (and (member 0 lines) #t) => #t)
          (check (and (member 2 lines) #t) => #t))))

    (test-case "find-highlights: no match"
      (let ((hl (find-highlights "hello world" "xyz")))
        (check (length hl) => 0)))

    (test-case "find-highlights: word boundary - no match inside longer symbol"
      (let ((hl (find-highlights "foobar foo-baz" "foo")))
        ;; "foo" inside "foobar" should NOT match (not at word boundary)
        ;; "foo" inside "foo-baz" SHOULD match because '-' is a symbol char
        ;; Actually, symbol-char? includes '-', so "foo-baz" is one symbol
        ;; and "foo" is NOT at a word boundary there either
        (check (length hl) => 0)))

    (test-case "find-highlights: exact word match"
      (let ((hl (find-highlights "(foo) bar" "foo")))
        ;; foo is bounded by ( and ), which are not symbol chars
        (check (length hl) => 1)))

    ;; --- find-symbol-in-line ---
    (test-case "find-symbol-in-line: collects positions"
      (let ((positions '()))
        (find-symbol-in-line "abc def abc" "abc" 3 0
          (lambda (col) (set! positions (cons col positions))))
        ;; Should find at col 0 and col 8
        (check (length positions) => 2)
        (check (and (member 0 positions) #t) => #t)
        (check (and (member 8 positions) #t) => #t)))

    (test-case "find-symbol-in-line: no match"
      (let ((positions '()))
        (find-symbol-in-line "hello world" "xyz" 3 0
          (lambda (col) (set! positions (cons col positions))))
        (check (length positions) => 0)))

    (test-case "find-symbol-in-line: word boundary enforcement"
      (let ((positions '()))
        (find-symbol-in-line "foobar" "foo" 3 0
          (lambda (col) (set! positions (cons col positions))))
        ;; "foo" inside "foobar" — the char after foo is 'b' which is a symbol char
        ;; so this should NOT match
        (check (length positions) => 0)))

    (test-case "find-symbol-in-line: match after non-symbol char"
      (let ((positions '()))
        (find-symbol-in-line "(foo)" "foo" 3 0
          (lambda (col) (set! positions (cons col positions))))
        (check (length positions) => 1)
        (check (car positions) => 1)))

    ;; --- handle-document-highlight: integration ---
    (test-case "handle-document-highlight: finds highlights"
      (let* ((uri "file:///test-hl.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))))
               (result (handle-document-highlight params)))
          (check (vector? result) => #t)
          ;; "add" appears at definition and call
          (check (>= (vector-length result) 1) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/documentHighlight" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-document-highlight: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-document-highlight params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)
                   (void? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! highlight-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
