;;; -*- Gerbil -*-
;;; Tests for lsp/types
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position)

(export types-test-suite)

(def types-test-suite
  (test-suite "lsp/types"

    ;; --- SymbolKind constants ---
    (test-case "SymbolKind constants"
      (check SymbolKind.File => 1)
      (check SymbolKind.Module => 2)
      (check SymbolKind.Namespace => 3)
      (check SymbolKind.Class => 5)
      (check SymbolKind.Function => 12)
      (check SymbolKind.Variable => 13)
      (check SymbolKind.Constant => 14)
      (check SymbolKind.Struct => 23))

    ;; --- CompletionItemKind constants ---
    (test-case "CompletionItemKind constants"
      (check CompletionItemKind.Text => 1)
      (check CompletionItemKind.Method => 2)
      (check CompletionItemKind.Function => 3)
      (check CompletionItemKind.Variable => 6)
      (check CompletionItemKind.Class => 7)
      (check CompletionItemKind.Module => 9)
      (check CompletionItemKind.Keyword => 14)
      (check CompletionItemKind.Struct => 22)
      ;; Macro aliases to Snippet
      (check CompletionItemKind.Macro => CompletionItemKind.Snippet))

    ;; --- DiagnosticSeverity constants ---
    (test-case "DiagnosticSeverity constants"
      (check DiagnosticSeverity.Error => 1)
      (check DiagnosticSeverity.Warning => 2)
      (check DiagnosticSeverity.Information => 3)
      (check DiagnosticSeverity.Hint => 4))

    ;; --- TextDocumentSyncKind constants ---
    (test-case "TextDocumentSyncKind constants"
      (check TextDocumentSyncKind.None => 0)
      (check TextDocumentSyncKind.Full => 1)
      (check TextDocumentSyncKind.Incremental => 2))

    ;; --- MarkupKind constants ---
    (test-case "MarkupKind constants"
      (check-equal? MarkupKind.PlainText "plaintext")
      (check-equal? MarkupKind.Markdown "markdown"))

    ;; --- make-text-edit ---
    (test-case "make-text-edit: structure"
      (let* ((range (make-lsp-range 0 0 0 5))
             (edit (make-text-edit range "hello")))
        (check (hash-ref edit "range") => range)
        (check-equal? (hash-ref edit "newText") "hello")))

    ;; --- make-diagnostic ---
    (test-case "make-diagnostic: defaults"
      (let* ((range (make-lsp-range 0 0 0 1))
             (diag (make-diagnostic range "error msg")))
        (check-equal? (hash-ref diag "message") "error msg")
        (check (hash-ref diag "severity") => DiagnosticSeverity.Error)
        (check-equal? (hash-ref diag "source") "gerbil-lsp")))

    (test-case "make-diagnostic: custom severity and source"
      (let* ((range (make-lsp-range 0 0 0 1))
             (diag (make-diagnostic range "warn msg"
                     severity: DiagnosticSeverity.Warning
                     source: "custom")))
        (check (hash-ref diag "severity") => DiagnosticSeverity.Warning)
        (check-equal? (hash-ref diag "source") "custom")))

    (test-case "make-diagnostic: with code"
      (let* ((range (make-lsp-range 0 0 0 1))
             (diag (make-diagnostic range "msg" code: "E001")))
        (check-equal? (hash-ref diag "code") "E001")))

    ;; --- make-completion-item ---
    (test-case "make-completion-item: minimal"
      (let ((item (make-completion-item "foo")))
        (check-equal? (hash-ref item "label") "foo")
        (check (hash-ref item "kind") => CompletionItemKind.Text)))

    (test-case "make-completion-item: with all optional fields"
      (let ((item (make-completion-item "bar"
                    kind: CompletionItemKind.Function
                    detail: "A function"
                    documentation: "Does stuff"
                    insert-text: "bar()")))
        (check-equal? (hash-ref item "label") "bar")
        (check (hash-ref item "kind") => CompletionItemKind.Function)
        (check-equal? (hash-ref item "detail") "A function")
        (check-equal? (hash-ref item "insertText") "bar()")
        ;; documentation is wrapped in markup
        (let ((doc (hash-ref item "documentation")))
          (check-equal? (hash-ref doc "kind") "markdown")
          (check-equal? (hash-ref doc "value") "Does stuff"))))

    ;; --- make-hover ---
    (test-case "make-hover: without range"
      (let ((h (make-hover "some info")))
        (let ((contents (hash-ref h "contents")))
          (check-equal? (hash-ref contents "kind") "markdown")
          (check-equal? (hash-ref contents "value") "some info"))
        (check (hash-key? h "range") => #f)))

    (test-case "make-hover: with range"
      (let* ((range (make-lsp-range 1 0 1 5))
             (h (make-hover "info" range)))
        (check (hash-key? h "range") => #t)
        (check (hash-ref h "range") => range)))

    ;; --- make-document-symbol ---
    (test-case "make-document-symbol: without children"
      (let* ((range (make-lsp-range 0 0 5 0))
             (sel (make-lsp-range 0 0 0 10))
             (sym (make-document-symbol "my-func" SymbolKind.Function range sel)))
        (check-equal? (hash-ref sym "name") "my-func")
        (check (hash-ref sym "kind") => SymbolKind.Function)
        (check (hash-key? sym "children") => #f)))

    (test-case "make-document-symbol: with children"
      (let* ((range (make-lsp-range 0 0 5 0))
             (sel (make-lsp-range 0 0 0 10))
             (sym (make-document-symbol "parent" SymbolKind.Class range sel
                    children: '())))
        (check (hash-key? sym "children") => #t)))

    ;; --- make-symbol-information ---
    (test-case "make-symbol-information: structure"
      (let* ((loc (make-lsp-location "file:///t.ss" (make-lsp-range 0 0 0 5)))
             (si (make-symbol-information "foo" SymbolKind.Function loc)))
        (check-equal? (hash-ref si "name") "foo")
        (check (hash-ref si "kind") => SymbolKind.Function)
        (check (hash-ref si "location") => loc)))
  ))

(def main
  (lambda ()
    (run-tests! types-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
