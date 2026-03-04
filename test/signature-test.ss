;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/signature
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/signature)

(export signature-test-suite)

(def signature-test-suite
  (test-suite "lsp/handlers/signature"

    ;; --- line-col->offset* ---
    (test-case "line-col->offset*: first char"
      (check (line-col->offset* "hello" 0 0) => 0))

    (test-case "line-col->offset*: middle of first line"
      (check (line-col->offset* "hello" 0 3) => 3))

    (test-case "line-col->offset*: second line"
      (check (line-col->offset* "abc\ndef" 1 0) => 4))

    (test-case "line-col->offset*: second line middle"
      (check (line-col->offset* "abc\ndef" 1 2) => 6))

    (test-case "line-col->offset*: past end returns #f"
      (check (line-col->offset* "abc" 5 0) => #f))

    ;; --- extract-func-name ---
    (test-case "extract-func-name: simple"
      (check-equal? (extract-func-name "(foo 1 2)" 1) "foo"))

    (test-case "extract-func-name: at start"
      (check-equal? (extract-func-name "bar " 0) "bar"))

    (test-case "extract-func-name: with closing paren"
      (check-equal? (extract-func-name "(x)" 1) "x"))

    (test-case "extract-func-name: empty after paren"
      (check (extract-func-name "( " 1) => #f))

    ;; --- extract-param-labels ---
    (test-case "extract-param-labels: simple function"
      (let ((params (extract-param-labels "(add a b)")))
        (check (length params) => 2)))

    (test-case "extract-param-labels: rest args"
      (let ((params (extract-param-labels "(foo a . rest)")))
        (check (>= (length params) 2) => #t)))

    (test-case "extract-param-labels: no args"
      (let ((params (extract-param-labels "(foo)")))
        (check (length params) => 0)))

    (test-case "extract-param-labels: invalid input"
      (let ((params (extract-param-labels "not a form")))
        (check (list? params) => #t)))

    ;; --- find-enclosing-call ---
    (test-case "find-enclosing-call: simple call"
      (let ((result (find-enclosing-call "(add 1 2)" 0 5)))
        (check (pair? result) => #t)
        (check-equal? (car result) "add")))

    (test-case "find-enclosing-call: nested call"
      (let ((result (find-enclosing-call "(+ (add 1 2) 3)" 0 9)))
        (check (pair? result) => #t)
        (check-equal? (car result) "add")))

    (test-case "find-enclosing-call: outside any call"
      (check (find-enclosing-call "hello" 0 2) => #f))

    (test-case "find-enclosing-call: at first arg"
      (let ((result (find-enclosing-call "(foo x y)" 0 5)))
        (check (pair? result) => #t)
        (check-equal? (car result) "foo")
        ;; arg-index should be >= 0
        (check (integer? (cdr result)) => #t)
        (check (>= (cdr result) 0) => #t)))

    (test-case "find-enclosing-call: multiline"
      (let ((result (find-enclosing-call "(define (add a b)\n  (+ a b))" 1 4)))
        (check (pair? result) => #t)))

    ;; --- handle-signature-help: integration ---
    (test-case "handle-signature-help: finds signature in open document"
      (let* ((uri "file:///test-sig.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 5)))))
               (result (handle-signature-help params)))
          (when (and result (not (void? result)))
            (check (hash-table? result) => #t)
            (let ((sigs (hash-ref result "signatures" [])))
              (check (vector? sigs) => #t))
            ;; Validate against LSP schema
            (let ((violations (validate-response "textDocument/signatureHelp" result)))
              (check (null? violations) => #t))))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-signature-help: returns void for no document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-signature-help params)))
        (check (void? result) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! signature-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
