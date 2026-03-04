;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/folding
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/validation
        :lsp/lsp/handlers/folding)

(export folding-test-suite)

(def folding-test-suite
  (test-suite "lsp/handlers/folding"

    ;; --- form-folding-kind ---
    (test-case "form-folding-kind: import form"
      (check-equal? (form-folding-kind '(import :std/sugar :std/iter))
                    FoldingRangeKind.Imports))

    (test-case "form-folding-kind: non-import form"
      (check (form-folding-kind '(def x 1)) => #f))

    (test-case "form-folding-kind: non-list"
      (check (form-folding-kind 'x) => #f))

    ;; --- make-folding-range ---
    (test-case "make-folding-range: with kind"
      (let ((range (make-folding-range 0 5 FoldingRangeKind.Imports)))
        (check (hash-ref range "startLine") => 0)
        (check (hash-ref range "endLine") => 5)
        (check-equal? (hash-ref range "kind") "imports")))

    (test-case "make-folding-range: without kind"
      (let ((range (make-folding-range 1 3 #f)))
        (check (hash-ref range "startLine") => 1)
        (check (hash-ref range "endLine") => 3)
        (check (hash-ref range "kind" #f) => #f)))

    ;; --- collect-folding-ranges ---
    (test-case "collect-folding-ranges: multiline forms"
      (let* ((text "(def (foo x)\n  (+ x 1))\n(def y 42)")
             (forms (parse-source text))
             (ranges (collect-folding-ranges forms)))
        ;; First form spans 2 lines, second is single line
        (check (>= (length ranges) 1) => #t)
        (let ((r (car ranges)))
          (check (hash-ref r "startLine") => 0)
          (check (hash-ref r "endLine") => 1))))

    (test-case "collect-folding-ranges: single line form"
      (let* ((text "(def x 1)")
             (forms (parse-source text))
             (ranges (collect-folding-ranges forms)))
        ;; Single line form produces no folding range
        (check (length ranges) => 0)))

    (test-case "collect-folding-ranges: import form"
      (let* ((text "(import :std/sugar\n        :std/iter)")
             (forms (parse-source text))
             (ranges (collect-folding-ranges forms)))
        (check (>= (length ranges) 1) => #t)
        (when (pair? ranges)
          (check-equal? (hash-ref (car ranges) "kind") "imports"))))

    (test-case "collect-folding-ranges: empty"
      (check-equal? (collect-folding-ranges '()) '()))

    ;; --- handle-folding-range: integration ---
    (test-case "handle-folding-range: returns ranges for multiline document"
      (let* ((uri "file:///test-fold.ss")
             (text "(def (foo x)\n  (+ x 1))\n(def y 42)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-folding-range params)))
          (check (vector? result) => #t)
          (check (>= (vector-length result) 1) => #t)
          ;; Verify first range has correct start/end lines
          (let ((first-range (vector-ref result 0)))
            (check (hash-ref first-range "startLine") => 0)
            (check (hash-ref first-range "endLine") => 1))
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/foldingRange" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-folding-range: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))))
             (result (handle-folding-range params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)
                   (void? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! folding-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
