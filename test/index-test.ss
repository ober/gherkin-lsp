;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/index
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/state
        :lsp/lsp/analysis/symbols
        :lsp/lsp/analysis/index)

(export index-test-suite)

(def index-test-suite
  (test-suite "lsp/analysis/index"

    ;; --- find-definitions-by-name ---
    (test-case "find-definitions-by-name: finds indexed symbol"
      ;; Manually index a symbol
      (let ((uri "file:///test-idx.ss")
            (syms (list (make-sym-info "my-func" SymbolKind.Function
                                        0 0 0 10 "(my-func x)"))))
        (set-file-symbols! uri syms)
        (let ((result (find-definitions-by-name "my-func")))
          (check (pair? result) => #t)
          (check-equal? (car (car result)) uri)
          (check-equal? (sym-info-name (cdr (car result))) "my-func"))
        ;; Cleanup
        (remove-file-symbols! uri)))

    (test-case "find-definitions-by-name: not found"
      (let ((result (find-definitions-by-name "totally-nonexistent-symbol")))
        (check (null? result) => #t)))

    (test-case "find-definitions-by-name: multiple files"
      (let ((uri1 "file:///test-idx1.ss")
            (uri2 "file:///test-idx2.ss"))
        (set-file-symbols! uri1
          (list (make-sym-info "shared-name" SymbolKind.Function 0 0 0 5 #f)))
        (set-file-symbols! uri2
          (list (make-sym-info "shared-name" SymbolKind.Variable 1 0 1 5 #f)))
        (let ((result (find-definitions-by-name "shared-name")))
          (check (>= (length result) 2) => #t))
        ;; Cleanup
        (remove-file-symbols! uri1)
        (remove-file-symbols! uri2)))

    ;; --- find-in-line ---
    (test-case "find-in-line: finds at start"
      (let ((found '()))
        (find-in-line "foo bar" "foo" 3 "file:///t.ss" 0
          (lambda (col) (set! found (cons col found))))
        (check (length found) => 1)
        (check (car found) => 0)))

    (test-case "find-in-line: finds in middle"
      (let ((found '()))
        (find-in-line "(+ foo 1)" "foo" 3 "file:///t.ss" 0
          (lambda (col) (set! found (cons col found))))
        (check (length found) => 1)
        (check (car found) => 3)))

    (test-case "find-in-line: respects word boundaries"
      (let ((found '()))
        (find-in-line "foobar foo" "foo" 3 "file:///t.ss" 0
          (lambda (col) (set! found (cons col found))))
        ;; Only standalone "foo" at position 7
        (check (length found) => 1)
        (check (car found) => 7)))

    (test-case "find-in-line: no match"
      (let ((found '()))
        (find-in-line "bar baz" "foo" 3 "file:///t.ss" 0
          (lambda (col) (set! found (cons col found))))
        (check (length found) => 0)))

    (test-case "find-in-line: multiple matches"
      (let ((found '()))
        (find-in-line "(+ x x)" "x" 1 "file:///t.ss" 0
          (lambda (col) (set! found (cons col found))))
        (check (length found) => 2)))

    ;; --- find-references-by-name ---
    (test-case "find-references-by-name: searches file text cache"
      (let ((uri "file:///test-ref.ss"))
        (set-file-symbols! uri '())
        (set-file-text! uri "(def x 1)\n(+ x 2)")
        (let ((result (find-references-by-name "x")))
          ;; Should find "x" in the cached text
          (check (>= (length result) 1) => #t))
        ;; Cleanup
        (remove-file-symbols! uri)
        (remove-file-text! uri)))
  ))

(def main
  (lambda ()
    (run-tests! index-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
