;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/code-lens
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/code-lens)

(export code-lens-test-suite)

(def code-lens-test-suite
  (test-suite "lsp/handlers/code-lens"

    ;; --- top-level-definition? ---
    (test-case "top-level-definition?: function is top-level"
      (let ((s (make-sym-info "add" SymbolKind.Function 0 0 0 10 "(add a b)")))
        (check (top-level-definition? s) => #t)))

    (test-case "top-level-definition?: parameter is not top-level"
      (let ((s (make-sym-info "x" SymbolKind.Variable 0 0 0 1 "parameter")))
        (check (top-level-definition? s) => #f)))

    (test-case "top-level-definition?: local is not top-level"
      (let ((s (make-sym-info "y" SymbolKind.Variable 0 0 0 1 "local")))
        (check (top-level-definition? s) => #f)))

    (test-case "top-level-definition?: variable without detail is top-level"
      (let ((s (make-sym-info "z" SymbolKind.Variable 0 0 0 1 #f)))
        (check (top-level-definition? s) => #t)))

    ;; --- string-suffix? ---
    (test-case "string-suffix?: matches"
      (check (string-suffix? "-test" "my-test") => #t))

    (test-case "string-suffix?: no match"
      (check (string-suffix? "-test" "my-func") => #f))

    (test-case "string-suffix?: string too short"
      (check (string-suffix? "longlong" "ab") => #f))

    (test-case "string-suffix?: exact match"
      (check (string-suffix? "test" "test") => #t))

    ;; --- string-contains-detail ---
    (test-case "string-contains-detail: found"
      (check (string-contains-detail "test-suite definition" "test-suite") => #t))

    (test-case "string-contains-detail: not found"
      (check (string-contains-detail "hello world" "xyz") => #f))

    ;; --- make-code-lens ---
    (test-case "make-code-lens: creates correct structure"
      (let ((lens (make-code-lens 5 0 "3 references" "gerbil-lsp.showReferences" '("file:///a.ss" 5 0))))
        (check (hash-table? lens) => #t)
        (check (hash-table? (hash-ref lens "range" #f)) => #t)
        (let ((cmd (hash-ref lens "command" #f)))
          (check (hash-table? cmd) => #t)
          (check-equal? (hash-ref cmd "title") "3 references")
          (check-equal? (hash-ref cmd "command") "gerbil-lsp.showReferences"))))

    ;; --- handle-code-lens: integration ---
    (test-case "handle-code-lens: returns lenses for document"
      (let* ((uri "file:///test-cl.ss")
             (text "(def (add a b) (+ a b))")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-code-lens params)))
          (check (vector? result) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/codeLens" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-code-lens: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))))
             (result (handle-code-lens params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! code-lens-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
