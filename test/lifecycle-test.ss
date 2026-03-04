;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/lifecycle
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/validation
        :lsp/lsp/handlers/lifecycle)

(export lifecycle-test-suite)

(def lifecycle-test-suite
  (test-suite "lsp/handlers/lifecycle"

    ;; --- handle-initialize ---
    (test-case "handle-initialize: returns capabilities and serverInfo"
      (let* ((params (hash ("rootUri" "file:///tmp/test-project")
                           ("capabilities" (hash))))
             (result (handle-initialize params)))
        (check (hash-table? result) => #t)
        (check (hash-table? (hash-ref result "capabilities" #f)) => #t)
        (check (hash-table? (hash-ref result "serverInfo" #f)) => #t)
        (check-equal? (hash-ref (hash-ref result "serverInfo") "name") "gerbil-lsp")
        ;; Validate against LSP schema
        (let ((violations (validate-response "initialize" result)))
          (check (null? violations) => #t))))

    (test-case "handle-initialize: handles missing rootUri"
      (let* ((params (hash ("capabilities" (hash))))
             (result (handle-initialize params)))
        (check (hash-table? result) => #t)
        (check (hash-table? (hash-ref result "capabilities" #f)) => #t)))

    (test-case "handle-initialize: stores workspace root"
      (let* ((params (hash ("rootUri" "file:///tmp/test-ws")
                           ("capabilities" (hash))))
             (result (handle-initialize params)))
        ;; workspace-root should be set
        (check (string? (workspace-root)) => #t)))

    ;; --- handle-shutdown ---
    (test-case "handle-shutdown: sets shutdown flag"
      ;; Reset state
      (set-shutdown-requested! #f)
      (check (shutdown-requested?) => #f)
      (handle-shutdown (hash))
      (check (shutdown-requested?) => #t)
      ;; Reset
      (set-shutdown-requested! #f))
  ))

(def main
  (lambda ()
    (run-tests! lifecycle-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
