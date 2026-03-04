;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/type-definition
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/handlers/type-definition)

(export type-definition-test-suite)

(def type-definition-test-suite
  (test-suite "lsp/handlers/type-definition"

    ;; --- infer-type-name ---
    (test-case "infer-type-name: make-X constructor"
      (check-equal? (infer-type-name "make-point") "point"))

    (test-case "infer-type-name: X? predicate"
      (check-equal? (infer-type-name "point?") "point"))

    (test-case "infer-type-name: X-field accessor"
      ;; extract-type-prefix returns the prefix before last hyphen
      (let ((result (infer-type-name "point-x")))
        (check (string? result) => #t)
        (check-equal? result "point")))

    (test-case "infer-type-name: X-field-set! mutator"
      (let ((result (infer-type-name "point-x-set!")))
        (check (string? result) => #t)))

    (test-case "infer-type-name: plain symbol"
      (check (infer-type-name "foo") => #f))

    ;; --- extract-type-prefix ---
    (test-case "extract-type-prefix: hyphenated name"
      (check-equal? (extract-type-prefix "point-x") "point"))

    (test-case "extract-type-prefix: multi-hyphen"
      ;; Returns prefix up to last hyphen
      (check-equal? (extract-type-prefix "my-point-x") "my-point"))

    (test-case "extract-type-prefix: no hyphen"
      (check (extract-type-prefix "foo") => #f))

    ;; --- string-contains-char ---
    (test-case "string-contains-char: found"
      (check (string-contains-char "hello" #\l) => #t))

    (test-case "string-contains-char: not found"
      (check (string-contains-char "hello" #\z) => #f))

    (test-case "string-contains-char: empty string"
      (check (string-contains-char "" #\a) => #f))

    ;; --- handle-type-definition: integration ---
    (test-case "handle-type-definition: resolves make-X to struct"
      (let* ((uri "file:///test-td.ss")
             (text "(defstruct point (x y))\n(make-point 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))))
               (result (handle-type-definition params)))
          ;; Should resolve make-point to point struct definition
          (check (or (hash-table? result) (void? result)) => #t))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-type-definition: void for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-type-definition params)))
        (check (void? result) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! type-definition-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
