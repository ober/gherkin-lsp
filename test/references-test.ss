;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/references
(import :std/test
        :lsp/lsp/util/position
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/references)

(export references-test-suite)

(def references-test-suite
  (test-suite "lsp/handlers/references"

    ;; --- scan-line-for-symbol ---
    (test-case "scan-line-for-symbol: finds symbol at start"
      (let ((found '()))
        (scan-line-for-symbol "foo" 3 "foo bar baz" 0 11 0
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 1)
        (check-equal? (car found) '(0 0 3))))

    (test-case "scan-line-for-symbol: finds symbol in middle"
      (let ((found '()))
        (scan-line-for-symbol "bar" 3 "(foo bar baz)" 0 13 0
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 1)
        (check-equal? (car found) '(0 5 8))))

    (test-case "scan-line-for-symbol: finds multiple occurrences"
      (let ((found '()))
        (scan-line-for-symbol "x" 1 "(+ x x)" 0 7 5
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 2)))

    (test-case "scan-line-for-symbol: respects word boundaries"
      (let ((found '()))
        (scan-line-for-symbol "foo" 3 "foobar foo foo-bar" 0 18 0
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        ;; Only the standalone "foo" should match, not "foobar" or "foo-bar"
        (check (length found) => 1)
        (check-equal? (car found) '(0 7 10))))

    (test-case "scan-line-for-symbol: no match in empty line"
      (let ((found '()))
        (scan-line-for-symbol "foo" 3 "" 0 0 0
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 0)))

    ;; --- find-symbol-in-text ---
    (test-case "find-symbol-in-text: finds across multiple lines"
      (let ((found '()))
        (find-symbol-in-text "x" "(def x 1)\n(+ x 2)" "file:///test.ss"
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        ;; Should find "x" on both lines
        (check (>= (length found) 2) => #t)))

    (test-case "find-symbol-in-text: single line text"
      (let ((found '()))
        (find-symbol-in-text "add" "(add 1 2)" "file:///test.ss"
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 1)))

    (test-case "find-symbol-in-text: no matches"
      (let ((found '()))
        (find-symbol-in-text "xyz" "(def x 1)" "file:///test.ss"
          (lambda (line col end-col)
            (set! found (cons (list line col end-col) found))))
        (check (length found) => 0)))

    ;; --- handle-references: integration ---
    (test-case "handle-references: finds references in open document"
      (let* ((uri "file:///test-refs.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))
                             ("context" (hash ("includeDeclaration" #t)))))
               (result (handle-references params)))
          (check (vector? result) => #t)
          ;; "add" appears at definition and call site
          (check (>= (vector-length result) 1) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/references" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-references: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))
                           ("context" (hash ("includeDeclaration" #t)))))
             (result (handle-references params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)
                   (void? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! references-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
