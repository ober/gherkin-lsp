;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/rename
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/util/string
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/rename)

(export rename-test-suite)

(def rename-test-suite
  (test-suite "lsp/handlers/rename"

    ;; --- find-edits-in-line ---
    (test-case "find-edits-in-line: single occurrence"
      (let* ((text "(def foo 1)")
             (regions (classify-text-regions text))
             (edits (find-edits-in-line text 0 11 0
                                        "foo" 3 "bar" regions)))
        (check (length edits) => 1)
        (let ((edit (car edits)))
          (check-equal? (hash-ref edit "newText") "bar"))))

    (test-case "find-edits-in-line: multiple occurrences"
      (let* ((text "(+ x x)")
             (regions (classify-text-regions text))
             (edits (find-edits-in-line text 0 7 0
                                        "x" 1 "y" regions)))
        (check (length edits) => 2)))

    (test-case "find-edits-in-line: no match"
      (let* ((text "(def foo 1)")
             (regions (classify-text-regions text))
             (edits (find-edits-in-line text 0 11 0
                                        "bar" 3 "baz" regions)))
        (check (length edits) => 0)))

    (test-case "find-edits-in-line: respects word boundaries"
      (let* ((text "foobar foo")
             (regions (classify-text-regions text))
             (edits (find-edits-in-line text 0 10 0
                                        "foo" 3 "baz" regions)))
        ;; Only standalone "foo" should match
        (check (length edits) => 1)))

    (test-case "find-edits-in-line: empty line"
      (let* ((text "")
             (regions (classify-text-regions text))
             (edits (find-edits-in-line text 0 0 0
                                        "foo" 3 "bar" regions)))
        (check (length edits) => 0)))

    ;; --- find-rename-edits-in-text ---
    (test-case "find-rename-edits-in-text: multi-line"
      (let ((edits (find-rename-edits-in-text
                     "(def (add a b) (+ a b))\n(add 1 2)"
                     "add" "plus")))
        ;; "add" appears twice: definition and call
        (check (>= (length edits) 2) => #t)
        (for-each
          (lambda (edit)
            (check-equal? (hash-ref edit "newText") "plus"))
          edits)))

    (test-case "find-rename-edits-in-text: no matches"
      (let ((edits (find-rename-edits-in-text
                     "(def x 1)"
                     "nonexistent" "new-name")))
        (check (length edits) => 0)))

    (test-case "find-rename-edits-in-text: single line"
      (let ((edits (find-rename-edits-in-text
                     "(set! x (+ x 1))"
                     "x" "y")))
        ;; "x" appears twice
        (check (length edits) => 2)))

    ;; --- string/comment filtering ---
    (test-case "find-rename-edits-in-text: skips matches in strings"
      (let ((edits (find-rename-edits-in-text
                     "(def x 1)\n(displayln \"x is great\")"
                     "x" "y")))
        ;; Only the code "x", not the "x" inside the string
        (check (length edits) => 1)))

    (test-case "find-rename-edits-in-text: skips matches in comments"
      (let ((edits (find-rename-edits-in-text
                     "(def x 1) ; x is a var"
                     "x" "y")))
        ;; Only the code "x", not the one in comment
        (check (length edits) => 1)))

    ;; --- handle-prepare-rename: integration ---
    (test-case "handle-prepare-rename: returns range for symbol"
      (let* ((uri "file:///test-rename.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))))
               (result (handle-prepare-rename params)))
          (when (and result (not (void? result)))
            (check (hash-table? result) => #t)
            (check (hash-table? (hash-ref result "range" #f)) => #t)
            (check-equal? (hash-ref result "placeholder") "add")
            ;; Validate against LSP schema
            (let ((violations (validate-response "textDocument/prepareRename" result)))
              (check (null? violations) => #t))))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    ;; --- handle-rename: integration ---
    (test-case "handle-rename: returns workspace edit"
      (let* ((uri "file:///test-rename2.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))
                             ("newName" "plus")))
               (result (handle-rename params)))
          (when (and result (not (void? result)))
            (check (hash-table? result) => #t)
            ;; Validate against LSP schema
            (let ((violations (validate-response "textDocument/rename" result)))
              (check (null? violations) => #t))))
        (remove-document! uri)
        (remove-file-symbols! uri)))
  ))

(def main
  (lambda ()
    (run-tests! rename-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
