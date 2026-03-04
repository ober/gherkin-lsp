;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/links
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/validation
        :lsp/lsp/handlers/links)

(export links-test-suite)

(def links-test-suite
  (test-suite "lsp/handlers/links"

    ;; --- format-import-spec ---
    (test-case "format-import-spec: symbol"
      (check-equal? (format-import-spec ':std/sugar) ":std/sugar"))

    (test-case "format-import-spec: pair with symbol head"
      (check-equal? (format-import-spec '(only-in :std/sugar when-let))
                    "only-in"))

    (test-case "format-import-spec: non-symbol non-pair"
      (check-equal? (format-import-spec 42) ""))

    ;; --- collect-document-links ---
    (test-case "collect-document-links: no imports"
      (let* ((text "(def x 1)")
             (forms (parse-source text)))
        (check-equal? (collect-document-links forms "/tmp/test.ss") '())))

    (test-case "collect-document-links: with imports"
      ;; This test checks structure, resolution depends on filesystem
      (let* ((text "(import :std/sugar)")
             (forms (parse-source text))
             (links (collect-document-links forms "/tmp/test.ss")))
        ;; Links may be empty if the module can't be resolved,
        ;; but the function should not error
        (check (list? links) => #t)))

    ;; --- make-import-link ---
    (test-case "make-import-link: unresolvable returns #f"
      ;; A bogus import spec that can't be resolved
      (check (make-import-link ':nonexistent/module "/tmp/test.ss" 1) => #f))

    (test-case "make-import-link: relative import unresolvable"
      ;; Relative import to non-existing file
      (check (make-import-link './nonexistent "/tmp/test.ss" 1) => #f))

    ;; --- handle-document-link: integration ---
    (test-case "handle-document-link: returns vector for open document"
      (let* ((uri "file:///test-links.ss")
             (text "(def x 1)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-document-link params)))
          (check (vector? result) => #t)
          ;; No imports → empty vector
          (check (vector-length result) => 0)
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/documentLink" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-document-link: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))))
             (result (handle-document-link params)))
        ;; Handler may return empty vector or list for missing doc
        (check (or (and (vector? result) (= (vector-length result) 0))
                   (null? result)
                   (void? result)) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! links-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
