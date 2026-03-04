#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Run all test suites sequentially.
;;; Each test file is run as a subprocess via gxi.
(import :std/misc/process
        :std/format)

;; Ensure subprocesses find the local project build
(let ((gp (getenv "GERBIL_PATH" #f)))
  (unless gp
    (let ((local-gerbil (path-expand ".gerbil" (current-directory))))
      (when (file-exists? local-gerbil)
        (setenv "GERBIL_PATH" local-gerbil)))))

(def *test-files*
  '("test/string-test.ss"
    "test/position-test.ss"
    "test/types-test.ss"
    "test/document-test.ss"
    "test/jsonrpc-test.ss"
    "test/transport-test.ss"
    "test/parser-test.ss"
    "test/symbols-test.ss"
    "test/module-test.ss"
    "test/completion-data-test.ss"
    "test/diagnostics-test.ss"
    "test/formatting-test.ss"
    "test/highlight-test.ss"
    "test/capabilities-test.ss"
    "test/state-test.ss"
    "test/sync-test.ss"
    "test/hover-test.ss"
    "test/definition-test.ss"
    "test/references-test.ss"
    "test/rename-test.ss"
    "test/completion-test.ss"
    "test/signature-test.ss"
    "test/folding-test.ss"
    "test/selection-test.ss"
    "test/links-test.ss"
    "test/index-test.ss"
    "test/semantic-tokens-test.ss"
    "test/inlay-hints-test.ss"
    ;; Infrastructure tests
    "test/validation-test.ss"
    "test/cache-test.ss"
    "test/project-config-test.ss"
    ;; Handler tests (Phase 4)
    "test/lifecycle-test.ss"
    "test/configuration-test.ss"
    "test/pull-diagnostics-test.ss"
    "test/code-action-test.ss"
    "test/call-hierarchy-test.ss"
    "test/type-hierarchy-test.ss"
    "test/type-definition-test.ss"
    "test/implementation-test.ss"
    "test/code-lens-test.ss"
    "test/on-type-formatting-test.ss"
    "test/execute-command-test.ss"
    "test/will-rename-test.ss"
    "test/apply-edit-test.ss"))

(def (run-all-tests)
  (let ((failed '())
        (passed 0))
    (for-each
      (lambda (f)
        (fprintf (current-error-port) "Running ~a...~%" f)
        (with-catch
          (lambda (e)
            (fprintf (current-error-port) "FAILED: ~a~%" f)
            (set! failed (cons f failed)))
          (lambda ()
            (run-process (list "gxi" f)
              stdout-redirection: #f
              stderr-redirection: #f)
            (set! passed (+ passed 1)))))
      *test-files*)
    (newline (current-error-port))
    (fprintf (current-error-port) "Results: ~a passed, ~a failed out of ~a~%"
             passed (length failed) (length *test-files*))
    (when (pair? failed)
      (fprintf (current-error-port) "Failed:~%")
      (for-each (lambda (f) (fprintf (current-error-port) "  ~a~%" f)) (reverse failed)))
    (exit (if (null? failed) 0 1))))

(run-all-tests)
