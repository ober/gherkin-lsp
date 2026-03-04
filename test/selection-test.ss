;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/selection
(import :std/test
        :lsp/lsp/util/position
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/validation
        :lsp/lsp/handlers/selection)

(export selection-test-suite)

(def selection-test-suite
  (test-suite "lsp/handlers/selection"

    ;; --- form-contains? ---
    (test-case "form-contains?: position inside"
      (let ((lf (make-located-form '(def x 1) 0 0 0 10)))
        (check (form-contains? lf 0 5) => #t)))

    (test-case "form-contains?: position at start"
      (let ((lf (make-located-form '(def x 1) 0 0 0 10)))
        (check (form-contains? lf 0 0) => #t)))

    (test-case "form-contains?: position at end"
      (let ((lf (make-located-form '(def x 1) 0 0 0 10)))
        (check (form-contains? lf 0 10) => #t)))

    (test-case "form-contains?: position before"
      (let ((lf (make-located-form '(def x 1) 1 0 1 10)))
        (check (form-contains? lf 0 5) => #f)))

    (test-case "form-contains?: position after"
      (let ((lf (make-located-form '(def x 1) 0 0 0 10)))
        (check (form-contains? lf 0 11) => #f)))

    (test-case "form-contains?: multiline form"
      (let ((lf (make-located-form '(def (foo x) x) 0 0 1 5)))
        (check (form-contains? lf 0 3) => #t)
        (check (form-contains? lf 1 2) => #t)
        (check (form-contains? lf 2 0) => #f)))

    ;; --- find-enclosing-forms ---
    (test-case "find-enclosing-forms: finds containing forms"
      (let* ((lf1 (make-located-form '(def (foo x) x) 0 0 1 5))
             (lf2 (make-located-form '(def y 2) 3 0 3 10))
             (forms (list lf1 lf2)))
        (let ((result (find-enclosing-forms forms 0 3)))
          (check (length result) => 1))))

    (test-case "find-enclosing-forms: no match"
      (let* ((lf1 (make-located-form '(def x 1) 0 0 0 10))
             (forms (list lf1)))
        (let ((result (find-enclosing-forms forms 5 0)))
          (check (length result) => 0))))

    (test-case "find-enclosing-forms: empty forms"
      (check-equal? (find-enclosing-forms '() 0 0) '()))

    ;; --- build-chain ---
    (test-case "build-chain: single form"
      (let* ((lf (make-located-form '(def x 1) 0 0 0 10))
             (chain (build-chain (list lf))))
        (check (hash-table? chain) => #t)
        (check (hash-table? (hash-ref chain "range" #f)) => #t)
        ;; Single form has no parent
        (check (hash-ref chain "parent" #f) => #f)))

    (test-case "build-chain: multiple forms"
      (let* ((outer (make-located-form '(def (foo x) x) 0 0 1 5))
             (inner (make-located-form '(foo x) 0 5 0 10))
             (chain (build-chain (list outer inner))))
        (check (hash-table? chain) => #t)
        ;; Innermost should have a parent
        (check (hash-table? (hash-ref chain "parent" #f)) => #t)))

    (test-case "build-chain: empty list"
      (check (build-chain '()) => #f))

    ;; --- build-selection-range ---
    (test-case "build-selection-range: basic"
      (let* ((text "(def x 1)")
             (forms (parse-source text))
             (result (build-selection-range text forms 0 5)))
        (check (hash-table? result) => #t)
        (let ((range (hash-ref result "range" #f)))
          (check (hash-table? range) => #t)
          ;; Verify the range start position is at line 0
          (let ((start (hash-ref range "start" #f)))
            (check (hash-table? start) => #t)
            (check (hash-ref start "line") => 0)))))

    ;; --- handle-selection-range: integration ---
    (test-case "handle-selection-range: returns nested ranges"
      (let* ((uri "file:///test-sel.ss")
             (text "(def (foo x) x)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("positions" (vector (hash ("line" 0)
                                                        ("character" 5))))))
               (result (handle-selection-range params)))
          (check (vector? result) => #t)
          (check (> (vector-length result) 0) => #t)
          (let ((first-range (vector-ref result 0)))
            (check (hash-table? first-range) => #t)
            (check (hash-table? (hash-ref first-range "range" #f)) => #t))
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/selectionRange" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-selection-range: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("positions" (vector (hash ("line" 0)
                                                      ("character" 0))))))
             (result (handle-selection-range params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (vector? result) (list? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! selection-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
