;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/inlay-hints
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/inlay-hints)

(export inlay-hints-test-suite)

(def inlay-hints-test-suite
  (test-suite "lsp/handlers/inlay-hints"

    ;; --- InlayHintKind constants ---
    (test-case "InlayHintKind constants"
      (check InlayHintKind.Type => 1)
      (check InlayHintKind.Parameter => 2))

    ;; --- param-list->names ---
    (test-case "param-list->names: simple params"
      (let ((names (param-list->names '(a b c))))
        (check (length names) => 3)
        (check-equal? (car names) "a")
        (check-equal? (cadr names) "b")
        (check-equal? (caddr names) "c")))

    (test-case "param-list->names: rest param"
      (let ((names (param-list->names 'rest)))
        (check (length names) => 1)
        (check-equal? (car names) "rest...")))

    (test-case "param-list->names: empty"
      (check-equal? (param-list->names '()) '()))

    (test-case "param-list->names: dotted pair"
      (let ((names (param-list->names '(a b . rest))))
        (check (>= (length names) 3) => #t)))

    ;; --- extract-param-names-from-detail ---
    (test-case "extract-param-names-from-detail: function sig"
      (let* ((info (make-sym-info "add" SymbolKind.Function 0 0 0 10
                                   "(add a b)"))
             (params (extract-param-names-from-detail info)))
        (check (pair? params) => #t)
        (check (length params) => 2)
        (check-equal? (car params) "a")
        (check-equal? (cadr params) "b")))

    (test-case "extract-param-names-from-detail: no args"
      (let* ((info (make-sym-info "foo" SymbolKind.Function 0 0 0 10
                                   "(foo)"))
             (params (extract-param-names-from-detail info)))
        (check (or (not params) (null? params)) => #t)))

    (test-case "extract-param-names-from-detail: no detail"
      (let* ((info (make-sym-info "bar" SymbolKind.Variable 0 0 0 5 #f))
             (params (extract-param-names-from-detail info)))
        (check (not params) => #t)))

    ;; --- lookup-param-names ---
    (test-case "lookup-param-names: finds in indexed symbols"
      (let ((uri "file:///test-hints.ss"))
        (set-file-symbols! uri
          (list (make-sym-info "my-add" SymbolKind.Function 0 0 0 10
                                "(my-add x y)")))
        (let ((params (lookup-param-names "my-add" uri)))
          (check (pair? params) => #t)
          (check (length params) => 2))
        ;; Cleanup
        (remove-file-symbols! uri)))

    (test-case "lookup-param-names: not found"
      (check (lookup-param-names "nonexistent-func" "file:///test.ss") => #f))

    ;; --- find-arg-positions ---
    (test-case "find-arg-positions: simple call"
      (let ((positions (find-arg-positions "(my-func arg1 arg2 arg3)")))
        (check (length positions) => 3)
        (check (car positions) => 9)
        (check (cadr positions) => 14)
        (check (caddr positions) => 19)))

    (test-case "find-arg-positions: nested parens"
      (let ((positions (find-arg-positions "(my-func (+ 1 2) arg2)")))
        (check (length positions) => 2)
        (check (car positions) => 9)
        (check (cadr positions) => 17)))

    (test-case "find-arg-positions: empty call"
      (let ((positions (find-arg-positions "(my-func)")))
        (check (length positions) => 0)))

    (test-case "find-arg-positions: single arg"
      (let ((positions (find-arg-positions "(my-func arg1)")))
        (check (length positions) => 1)
        (check (car positions) => 9)))

    (test-case "find-arg-positions: empty string"
      (check-equal? (find-arg-positions "") '()))

    ;; --- make-inlay-hint ---
    (test-case "make-inlay-hint: creates correct structure"
      (let ((hint (make-inlay-hint 5 10 "param" InlayHintKind.Parameter)))
        (check (hash-table? hint) => #t)
        (check-equal? (hash-ref hint "label") "param:")
        (check (hash-ref hint "kind") => InlayHintKind.Parameter)
        (check (hash-ref hint "paddingRight") => #t)
        (let ((pos (hash-ref hint "position")))
          (check (hash-ref pos "line") => 5)
          (check (hash-ref pos "character") => 10))))

    ;; --- generate-param-hints ---
    (test-case "generate-param-hints: skips common functions"
      (let ((hints (generate-param-hints "cons" '(1 2) '("car" "cdr")
                                          "" 0)))
        (check (length hints) => 0)))

    (test-case "generate-param-hints: skips single-param functions"
      (let ((hints (generate-param-hints "my-func" '(1) '("x")
                                          "" 0)))
        (check (length hints) => 0)))

    (test-case "generate-param-hints: generates for multi-param"
      (let ((hints (generate-param-hints "my-add" '(1 2) '("x" "y")
                                          "(my-add 1 2)" 0)))
        (check (length hints) => 2)))

    ;; --- handle-inlay-hint: integration ---
    (test-case "handle-inlay-hint: returns hints for known function"
      (let* ((uri "file:///test-ih.ss")
             (text "(def (my-add x y) (+ x y))\n(my-add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("range" (hash ("start" (hash ("line" 0)
                                                           ("character" 0)))
                                            ("end" (hash ("line" 2)
                                                         ("character" 0)))))))
               (result (handle-inlay-hint params)))
          (check (vector? result) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/inlayHint" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-inlay-hint: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("range" (hash ("start" (hash ("line" 0)
                                                         ("character" 0)))
                                          ("end" (hash ("line" 10)
                                                       ("character" 0)))))))
             (result (handle-inlay-hint params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! inlay-hints-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
