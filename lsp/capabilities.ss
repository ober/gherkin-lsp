;;; -*- Gerbil -*-
;;; LSP server capabilities declaration
(import ./types)

(export #t)


;;; Semantic token legend (must match indices in semantic-tokens.ss)
(def *capability-token-types*
  ["keyword"
   "function"
   "variable"
   "parameter"
   "type"
   "macro"
   "comment"
   "string"
   "number"
   "operator"
   "property"])

(def *capability-token-modifiers* ["definition" "readonly"])


;; Returns the ServerCapabilities object for the initialize response
(def (server-capabilities)
  (hash
   ;; Incremental document sync — client sends range edits
   ("textDocumentSync"
    (hash ("openClose" #t)
          ("change" TextDocumentSyncKind.Incremental)
          ("save" (hash ("includeText" #t)))))

   ;; Completion
   ("completionProvider"
    (hash ("triggerCharacters" ["(" ":" "/" "."]) ("resolveProvider" #t)))

   ;; Hover
   ("hoverProvider" #t)

   ;; Go to definition
   ("definitionProvider" #t)

   ;; Go to declaration (same as definition in Gerbil)
   ("declarationProvider" #t)

   ;; Find references
   ("referencesProvider" #t)

   ;; Document symbols (outline)
   ("documentSymbolProvider" #t)

   ;; Workspace symbols (search)
   ("workspaceSymbolProvider" #t)

   ;; Rename
   ("renameProvider" (hash ("prepareProvider" #t)))

   ;; Formatting
   ("documentFormattingProvider" #t)

   ;; Range formatting
   ("documentRangeFormattingProvider" #t)

   ;; Signature help
   ("signatureHelpProvider" (hash ("triggerCharacters" [" " "("])))

   ;; Code actions
   ("codeActionProvider"
    (hash ("codeActionKinds" ["quickfix" "source.organizeImports"
                              "refactor.extract" "refactor.rewrite"])
          ("resolveProvider" #t)))

   ;; Document highlight
   ("documentHighlightProvider" #t)

   ;; Folding ranges
   ("foldingRangeProvider" #t)

   ;; Selection ranges
   ("selectionRangeProvider" #t)

   ;; Document links
   ("documentLinkProvider" (hash ("resolveProvider" #f)))

   ;; Semantic tokens
   ("semanticTokensProvider"
    (hash ("legend"
           (hash ("tokenTypes" *capability-token-types*)
                 ("tokenModifiers" *capability-token-modifiers*)))
          ("full" (hash ("delta" #t)))
          ("range" #t)))

   ;; Inlay hints
   ("inlayHintProvider" (hash ("resolveProvider" #t)))

   ;; Call hierarchy
   ("callHierarchyProvider" #t)

   ;; Go to type definition
   ("typeDefinitionProvider" #t)

   ;; Go to implementation
   ("implementationProvider" #t)

   ;; Type hierarchy
   ("typeHierarchyProvider" #t)

   ;; Code lenses
   ("codeLensProvider" (hash ("resolveProvider" #f)))

   ;; On-type formatting
   ("documentOnTypeFormattingProvider"
    (hash ("firstTriggerCharacter" ")") ("moreTriggerCharacter" ["]" "\n"])))

   ;; Pull diagnostics
   ("diagnosticProvider"
    (hash ("interFileDependencies" #t) ("workspaceDiagnostics" #f)))

   ;; Execute command
   ("executeCommandProvider"
    (hash ("commands" ["gerbil-lsp.runTest" "gerbil-lsp.showReferences"
                       "gerbil-lsp.compileFile" "gerbil-lsp.buildProject"])))

   ;; Workspace capabilities
   ("workspace"
    (hash ("workDoneProgress" #t)
          ("workspaceFolders"
           (hash ("supported" #t) ("changeNotifications" #t)))
          ("fileOperations"
           (hash ("willRename"
                  (hash ("filters"
                         (vector
                           (hash ("scheme" "file")
                                 ("pattern"
                                  (hash ("glob" "**/*.ss"))))))))))))
   ))
