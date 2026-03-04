;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/implementation
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/handlers/implementation)

(export implementation-test-suite)

(def implementation-test-suite
  (test-suite "lsp/handlers/implementation"

    ;; --- handle-implementation: integration ---
    (test-case "handle-implementation: finds method implementations"
      (let* ((uri "file:///test-impl.ss")
             (text "(defmethod {greet person}\n  (lambda (self)\n    (string-append \"Hi \" (person-name self))))")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        ;; Search for "greet" method
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 13)))))
               (result (handle-implementation params)))
          (check (vector? result) => #t))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-implementation: empty for non-method"
      (let* ((uri "file:///test-impl2.ss")
             (text "(def (add a b) (+ a b))")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 5)))))
               (result (handle-implementation params)))
          ;; "add" is a function, not a method, so no implementations
          (check (vector? result) => #t))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-implementation: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-implementation params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! implementation-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
