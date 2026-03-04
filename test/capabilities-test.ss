;;; -*- Gerbil -*-
;;; Tests for lsp/capabilities
(import :std/test
        :lsp/lsp/capabilities)

(export capabilities-test-suite)

(def capabilities-test-suite
  (test-suite "lsp/capabilities"

    (test-case "server-capabilities: has textDocumentSync"
      (let ((caps (server-capabilities)))
        (check (hash-key? caps "textDocumentSync") => #t)))

    (test-case "server-capabilities: has completionProvider"
      (check (hash-key? (server-capabilities) "completionProvider") => #t))

    (test-case "server-capabilities: has hoverProvider"
      (check (hash-ref (server-capabilities) "hoverProvider") => #t))

    (test-case "server-capabilities: has definitionProvider"
      (check (hash-ref (server-capabilities) "definitionProvider") => #t))

    (test-case "server-capabilities: has referencesProvider"
      (check (hash-ref (server-capabilities) "referencesProvider") => #t))

    (test-case "server-capabilities: has documentSymbolProvider"
      (check (hash-ref (server-capabilities) "documentSymbolProvider") => #t))

    (test-case "server-capabilities: has workspaceSymbolProvider"
      (check (hash-ref (server-capabilities) "workspaceSymbolProvider") => #t))

    (test-case "server-capabilities: has renameProvider"
      (check (hash-key? (server-capabilities) "renameProvider") => #t))

    (test-case "server-capabilities: has documentFormattingProvider"
      (check (hash-ref (server-capabilities) "documentFormattingProvider") => #t))

    (test-case "server-capabilities: has signatureHelpProvider"
      (check (hash-key? (server-capabilities) "signatureHelpProvider") => #t))

    (test-case "server-capabilities: has codeActionProvider"
      (check (hash-key? (server-capabilities) "codeActionProvider") => #t))

    (test-case "server-capabilities: has documentHighlightProvider"
      (check (hash-ref (server-capabilities) "documentHighlightProvider") => #t))

    (test-case "server-capabilities: has foldingRangeProvider"
      (check (hash-ref (server-capabilities) "foldingRangeProvider") => #t))

    (test-case "server-capabilities: has selectionRangeProvider"
      (check (hash-ref (server-capabilities) "selectionRangeProvider") => #t))

    (test-case "server-capabilities: has documentLinkProvider"
      (check (hash-key? (server-capabilities) "documentLinkProvider") => #t))

    (test-case "server-capabilities: has executeCommandProvider"
      (let ((caps (server-capabilities)))
        (check (hash-key? caps "executeCommandProvider") => #t)
        (let* ((ecp (hash-ref caps "executeCommandProvider"))
               (cmds (hash-ref ecp "commands")))
          ;; Should have our two commands
          (check (list? cmds) => #t)
          (check (>= (length cmds) 2) => #t))))
  ))

(def main
  (lambda ()
    (run-tests! capabilities-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
