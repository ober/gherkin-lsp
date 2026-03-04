;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/pull-diagnostics
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/validation
        :lsp/lsp/handlers/pull-diagnostics)

(export pull-diagnostics-test-suite)

(def pull-diagnostics-test-suite
  (test-suite "lsp/handlers/pull-diagnostics"

    ;; --- handle-document-diagnostic ---
    (test-case "handle-document-diagnostic: returns full report for open document"
      (let* ((uri "file:///test-pull-diag.ss")
             (text "(def x 1)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-document-diagnostic params)))
          (check (hash-table? result) => #t)
          (check-equal? (hash-ref result "kind") "full")
          (check (vector? (hash-ref result "items")) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/diagnostic" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-document-diagnostic: returns empty items for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))))
             (result (handle-document-diagnostic params)))
        (check (hash-table? result) => #t)
        (check-equal? (hash-ref result "kind") "full")
        (check (vector? (hash-ref result "items")) => #t)
        (check (vector-length (hash-ref result "items")) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! pull-diagnostics-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
