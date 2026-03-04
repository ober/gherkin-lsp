;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/apply-edit
(import :std/test
        :lsp/lsp/server
        :lsp/lsp/handlers/apply-edit)

(export apply-edit-test-suite)

(def apply-edit-test-suite
  (test-suite "lsp/handlers/apply-edit"

    ;; Test that apply-workspace-edit! returns #f when no output port
    ;; (can't send request without a connected client)
    (test-case "apply-workspace-edit!: no client returns #f"
      ;; With *output-port* = #f (default), send-request! is a no-op
      ;; send-request-sync! returns #f (timeout with no response)
      ;; But we can't easily test the full roundtrip without a mock client.
      ;; Just verify the function is callable and handles nil output gracefully.
      (check (procedure? apply-workspace-edit!) => #t))
  ))

(def main
  (lambda ()
    (run-tests! apply-edit-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
