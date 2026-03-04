;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/code-action
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/code-action)

(export code-action-test-suite)

(def code-action-test-suite
  (test-suite "lsp/handlers/code-action"

    ;; --- sort-import-specs ---
    (test-case "sort-import-specs: sorts alphabetically"
      (let ((sorted (sort-import-specs '(:std/sugar :std/iter :std/format))))
        (check-equal? (car sorted) ':std/format)
        (check-equal? (cadr sorted) ':std/iter)
        (check-equal? (caddr sorted) ':std/sugar)))

    (test-case "sort-import-specs: empty list"
      (check-equal? (sort-import-specs '()) '()))

    (test-case "sort-import-specs: single item"
      (let ((sorted (sort-import-specs '(:std/sugar))))
        (check (length sorted) => 1)))

    ;; --- extract-unbound-symbol ---
    (test-case "extract-unbound-symbol: from unbound identifier message"
      (let ((result (extract-unbound-symbol "unbound identifier: read-json")))
        (check-equal? result "read-json")))

    (test-case "extract-unbound-symbol: from Unbound variable message"
      (let ((result (extract-unbound-symbol "Unbound variable: hash-ref")))
        (check-equal? result "hash-ref")))

    (test-case "extract-unbound-symbol: returns #f for other messages"
      (check (extract-unbound-symbol "some other error") => #f))

    ;; --- suggest-import-for ---
    (test-case "suggest-import-for: known stdlib symbol"
      (let ((result (suggest-import-for "read-json")))
        (check (pair? result) => #t)))

    (test-case "suggest-import-for: unknown symbol"
      (let ((result (suggest-import-for "totally-unknown-xyz-42")))
        (check (null? result) => #t)))

    ;; --- format-organized-imports ---
    (test-case "format-organized-imports: formats specs"
      (let ((result (format-organized-imports '(:std/format :std/sugar))))
        (check (string? result) => #t)
        (check (> (string-length result) 0) => #t)))

    (test-case "format-organized-imports: empty"
      (check-equal? (format-organized-imports '()) ""))

    ;; --- handle-code-action: integration ---
    (test-case "handle-code-action: returns actions for document with imports"
      (let* ((uri "file:///test-ca.ss")
             (text "(import :std/sugar\n        :std/iter)\n(def x 1)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("range" (hash ("start" (hash ("line" 0)
                                                           ("character" 0)))
                                            ("end" (hash ("line" 2)
                                                         ("character" 0)))))
                             ("context" (hash ("diagnostics" (vector))))))
               (result (handle-code-action params)))
          ;; Should return a vector (possibly with organize imports action)
          (check (or (vector? result) (list? result)) => #t))
        (remove-document! uri)))

    (test-case "handle-code-action: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("range" (hash ("start" (hash ("line" 0) ("character" 0)))
                                          ("end" (hash ("line" 0) ("character" 0)))))
                           ("context" (hash ("diagnostics" (vector))))))
             (result (handle-code-action params)))
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! code-action-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
