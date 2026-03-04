;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/execute-command
(import :std/test
        :lsp/lsp/handlers/execute-command)

(export execute-command-test-suite)

(def execute-command-test-suite
  (test-suite "lsp/handlers/execute-command"

    ;; --- handle-execute-command ---
    (test-case "handle-execute-command: unknown command returns void"
      (let* ((params (hash ("command" "unknown-command")
                           ("arguments" (vector))))
             (result (handle-execute-command params)))
        (check (void? result) => #t)))

    (test-case "handle-execute-command: empty command returns void"
      (let* ((params (hash ("command" "")
                           ("arguments" (vector))))
             (result (handle-execute-command params)))
        (check (void? result) => #t)))

    ;; Skip testing runTest and showReferences as they spawn processes
    ;; and send notifications requiring a running server
  ))

(def main
  (lambda ()
    (run-tests! execute-command-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
