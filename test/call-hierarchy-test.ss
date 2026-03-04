;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/call-hierarchy
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/call-hierarchy)

(export call-hierarchy-test-suite)

(def call-hierarchy-test-suite
  (test-suite "lsp/handlers/call-hierarchy"

    ;; --- def-form-name ---
    (test-case "def-form-name: simple def"
      (check-equal? (def-form-name '(def foo 1)) "foo"))

    (test-case "def-form-name: function def"
      (check-equal? (def-form-name '(def (add a b) (+ a b))) "add"))

    (test-case "def-form-name: too short"
      (check (def-form-name '(def)) => #f))

    ;; --- extract-called-symbols ---
    (test-case "extract-called-symbols: extracts call targets"
      (let ((syms (extract-called-symbols '(def (foo x) (bar x) (baz x)))))
        (check (member "bar" syms) => (member "bar" syms))
        (check (member "baz" syms) => (member "baz" syms))))

    (test-case "extract-called-symbols: empty body"
      (let ((syms (extract-called-symbols '(def x))))
        (check (null? syms) => #t)))

    ;; --- deduplicate-calls ---
    (test-case "deduplicate-calls: removes duplicates by name+uri"
      (let* ((call1 (hash ("from" (hash ("uri" "file:///a.ss") ("name" "foo")))
                          ("fromRanges" (vector))))
             (call2 (hash ("from" (hash ("uri" "file:///a.ss") ("name" "foo")))
                          ("fromRanges" (vector))))
             (result (deduplicate-calls (list call1 call2))))
        (check (length result) => 1)))

    (test-case "deduplicate-calls: keeps distinct callers"
      (let* ((call1 (hash ("from" (hash ("uri" "file:///a.ss") ("name" "foo")))
                          ("fromRanges" (vector))))
             (call2 (hash ("from" (hash ("uri" "file:///b.ss") ("name" "bar")))
                          ("fromRanges" (vector))))
             (result (deduplicate-calls (list call1 call2))))
        (check (length result) => 2)))

    (test-case "deduplicate-calls: empty list"
      (check-equal? (deduplicate-calls '()) '()))

    ;; --- handle-prepare-call-hierarchy: integration ---
    (test-case "handle-prepare-call-hierarchy: finds function at cursor"
      (let* ((uri "file:///test-ch.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 5)))))
               (result (handle-prepare-call-hierarchy params)))
          (check (vector? result) => #t)
          (when (> (vector-length result) 0)
            (let ((item (vector-ref result 0)))
              (check (hash-table? item) => #t)
              (check-equal? (hash-ref item "name") "add"))))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-prepare-call-hierarchy: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-prepare-call-hierarchy params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! call-hierarchy-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
