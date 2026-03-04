;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/sync (just the `last` function)
(import :std/test
        :lsp/lsp/handlers/sync)

(export sync-test-suite)

(def sync-test-suite
  (test-suite "lsp/handlers/sync"

    (test-case "last: multi-element"
      (check (last '(1 2 3)) => 3))

    (test-case "last: single element"
      (check (last '(42)) => 42))

    (test-case "last: empty list"
      (check (last '()) => #f))
  ))

(def main
  (lambda ()
    (run-tests! sync-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
