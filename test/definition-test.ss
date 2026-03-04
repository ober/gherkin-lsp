;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/definition
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/analysis/completion-data
        :lsp/lsp/validation
        :lsp/lsp/handlers/definition)

(export definition-test-suite)

(def definition-test-suite
  (test-suite "lsp/handlers/definition"

    ;; --- find-definition-location: local symbols ---
    (test-case "find-definition-location: finds local def"
      ;; Set up a document with a symbol
      (let* ((uri "file:///test-def.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-file-symbols! uri syms)
        (let ((result (find-definition-location "add" uri text)))
          (check (not (void? result)) => #t)
          (check (hash-table? result) => #t)
          (check-equal? (hash-ref result "uri") uri))
        ;; Cleanup
        (remove-file-symbols! uri)))

    (test-case "find-definition-location: returns void for unknown symbol"
      (let* ((uri "file:///test-def2.ss")
             (text "(def x 1)"))
        (set-file-symbols! uri '())
        (let ((result (find-definition-location "nonexistent" uri text)))
          (check (void? result) => #t))
        (remove-file-symbols! uri)))

    ;; --- handle-definition: integration test ---
    (test-case "handle-definition: returns void when no document"
      (let ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                          ("position" (hash ("line" 0) ("character" 0))))))
        (check (void? (handle-definition params)) => #t)))

    (test-case "handle-definition: finds symbol in open document"
      (let* ((uri "file:///test-def3.ss")
             (text "(def (foo x) x)\n(foo 42)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))))
               (result (handle-definition params)))
          (check (not (void? result)) => #t)
          (check (hash-table? result) => #t)
          (check-equal? (hash-ref result "uri") uri)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/definition" result)))
            (check (null? violations) => #t)))
        ;; Cleanup
        (remove-document! uri)
        (remove-file-symbols! uri)))

    ;; --- find-stdlib-module-for ---
    (test-case "find-stdlib-module-for: known symbol"
      (check-equal? (find-stdlib-module-for "read-json") ":std/text/json"))

    (test-case "find-stdlib-module-for: unknown symbol"
      (check (find-stdlib-module-for "nonexistent-xyz") => #f))

    (test-case "find-stdlib-module-for: sugar symbol"
      (check-equal? (find-stdlib-module-for "if-let") ":std/sugar"))

    ;; --- find-definition-in-stdlib ---
    (test-case "find-definition-in-stdlib: unknown returns void"
      (let ((result (find-definition-in-stdlib "totally-unknown-symbol-xyz")))
        (check (void? result) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! definition-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
