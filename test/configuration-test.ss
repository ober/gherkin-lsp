;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/configuration
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/handlers/configuration)

(export configuration-test-suite)

(def configuration-test-suite
  (test-suite "lsp/handlers/configuration"

    ;; --- handle-did-change-configuration ---
    (test-case "handle-did-change-configuration: merges gerbil-lsp settings"
      (let ((params (hash ("settings"
                           (hash ("gerbil-lsp"
                                  (hash ("maxCompletions" 50))))))))
        (handle-did-change-configuration params)
        (check (get-config "maxCompletions") => 50)))

    (test-case "handle-did-change-configuration: handles gerbilLsp key"
      (let ((params (hash ("settings"
                           (hash ("gerbilLsp"
                                  (hash ("enableDiagnostics" #t))))))))
        (handle-did-change-configuration params)
        (check (get-config "enableDiagnostics") => #t)))

    (test-case "handle-did-change-configuration: handles missing settings"
      ;; Should not crash
      (handle-did-change-configuration (hash))
      (check #t => #t))

    (test-case "handle-did-change-configuration: handles missing gerbil-lsp key"
      ;; Falls back to using settings directly
      (let ((params (hash ("settings" (hash ("someKey" "someValue"))))))
        (handle-did-change-configuration params)
        (check (get-config "someKey") => "someValue")))
  ))

(def main
  (lambda ()
    (run-tests! configuration-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
