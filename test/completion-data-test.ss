;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/completion-data
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/analysis/completion-data)

(export completion-data-test-suite)

(def completion-data-test-suite
  (test-suite "lsp/analysis/completion-data"

    ;; --- get-completion-prefix ---
    (test-case "get-completion-prefix: mid-word"
      ;; col 8 -> scan backward from index 7 (which is '-')
      ;; prefix is "hello-" (indices 2..7 inclusive)
      (check-equal? (get-completion-prefix "  hello-world" 0 8) "hello-"))

    (test-case "get-completion-prefix: full word"
      (check-equal? (get-completion-prefix "define" 0 6) "define"))

    (test-case "get-completion-prefix: col=0"
      (check (get-completion-prefix "hello" 0 0) => #f))

    (test-case "get-completion-prefix: after paren"
      (check (get-completion-prefix "(def " 0 1) => #f))

    (test-case "get-completion-prefix: multiline"
      (check-equal? (get-completion-prefix "line0\nfoo-bar" 1 7) "foo-bar"))

    ;; --- completion-char? ---
    (test-case "completion-char?: alpha"
      (check (completion-char? #\a) => #t)
      (check (completion-char? #\Z) => #t))

    (test-case "completion-char?: special chars"
      (check (and (completion-char? #\-) #t) => #t)
      (check (and (completion-char? #\!) #t) => #t)
      (check (and (completion-char? #\?) #t) => #t))

    (test-case "completion-char?: non-symbol"
      (check (completion-char? #\space) => #f)
      (check (completion-char? #\() => #f)
      (check (completion-char? #\)) => #f))

    ;; --- sym-kind->completion-kind ---
    (test-case "sym-kind->completion-kind: Function"
      (check (sym-kind->completion-kind SymbolKind.Function)
             => CompletionItemKind.Function))

    (test-case "sym-kind->completion-kind: Variable"
      (check (sym-kind->completion-kind SymbolKind.Variable)
             => CompletionItemKind.Variable))

    (test-case "sym-kind->completion-kind: Constant"
      (check (sym-kind->completion-kind SymbolKind.Constant)
             => CompletionItemKind.Variable))

    (test-case "sym-kind->completion-kind: Struct"
      (check (sym-kind->completion-kind SymbolKind.Struct)
             => CompletionItemKind.Struct))

    (test-case "sym-kind->completion-kind: Class"
      (check (sym-kind->completion-kind SymbolKind.Class)
             => CompletionItemKind.Class))

    (test-case "sym-kind->completion-kind: Method"
      (check (sym-kind->completion-kind SymbolKind.Method)
             => CompletionItemKind.Method))

    (test-case "sym-kind->completion-kind: unknown fallback"
      (check (sym-kind->completion-kind SymbolKind.Namespace)
             => CompletionItemKind.Text))

    ;; --- get-line-text ---
    (test-case "get-line-text: first line"
      (check-equal? (get-line-text "abc\ndef\nghi" 0) "abc"))

    (test-case "get-line-text: last line"
      (check-equal? (get-line-text "abc\ndef\nghi" 2) "ghi"))

    (test-case "get-line-text: past end"
      (check-equal? (get-line-text "abc\ndef" 5) ""))
  ))

(def main
  (lambda ()
    (run-tests! completion-data-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
