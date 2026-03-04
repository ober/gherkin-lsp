;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/type-hierarchy
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/handlers/type-hierarchy)

(export type-hierarchy-test-suite)

(def type-hierarchy-test-suite
  (test-suite "lsp/handlers/type-hierarchy"

    ;; --- extract-parent-from-detail ---
    (test-case "extract-parent-from-detail: struct with parent"
      (check-equal? (extract-parent-from-detail "struct point < base-point")
                    "base-point"))

    (test-case "extract-parent-from-detail: class with parent"
      (check-equal? (extract-parent-from-detail "class dog < animal")
                    "animal"))

    (test-case "extract-parent-from-detail: no parent"
      (check (extract-parent-from-detail "struct point") => #f))

    (test-case "extract-parent-from-detail: empty string"
      (check (extract-parent-from-detail "") => #f))

    ;; --- string-contains ---
    (test-case "string-contains: finds substring"
      (check (number? (string-contains "hello world" "world")) => #t))

    (test-case "string-contains: returns index"
      (check (string-contains "hello world" "world") => 6))

    (test-case "string-contains: not found"
      (check (string-contains "hello" "xyz") => #f))

    (test-case "string-contains: empty needle"
      (check (string-contains "hello" "") => 0))

    ;; --- handle-prepare-type-hierarchy: integration ---
    (test-case "handle-prepare-type-hierarchy: finds struct at cursor"
      (let* ((uri "file:///test-th.ss")
             (text "(defstruct point (x y))")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 11)))))
               (result (handle-prepare-type-hierarchy params)))
          (check (vector? result) => #t)
          (when (> (vector-length result) 0)
            (let ((item (vector-ref result 0)))
              (check (hash-table? item) => #t)
              (check-equal? (hash-ref item "name") "point"))))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-prepare-type-hierarchy: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-prepare-type-hierarchy params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! type-hierarchy-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
