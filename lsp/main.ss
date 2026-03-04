;;; -*- Gerbil -*-
;;; gerbil-lsp entry point
(import ./compat/compat
        ./util/log
        ./server
        ./validation
        ./handlers/lifecycle
        ./handlers/sync
        ./handlers/diagnostics
        ./handlers/completion
        ./handlers/hover
        ./handlers/definition
        ./handlers/references
        ./handlers/symbols
        ./handlers/rename
        ./handlers/formatting
        ./handlers/signature
        ./handlers/code-action
        ./handlers/configuration
        ./handlers/highlight
        ./handlers/folding
        ./handlers/selection
        ./handlers/links
        ./handlers/semantic-tokens
        ./handlers/inlay-hints
        ./handlers/call-hierarchy
        ./handlers/type-definition
        ./handlers/implementation
        ./handlers/type-hierarchy
        ./handlers/code-lens
        ./handlers/on-type-formatting
        ./handlers/pull-diagnostics
        ./handlers/execute-command
        ./handlers/will-rename)
(export main)

;;; Note: Gambit SIGVTALRM spin-detection wrapper removed — not needed on Chez Scheme.

(def (main . args)
  (call-with-getopt gerbil-lsp-main args
    program: "gerbil-lsp"
    help: "Language Server Protocol server for Gerbil Scheme"
    (flag 'stdio "--stdio"
      help: "Use stdio transport (default)")
    (option 'log-level "--log-level"
      help: "Log level: debug, info, warn, error"
      default: "info")
    (flag 'version "--version"
      help: "Print version and exit")
    (flag 'validate "--validate"
      help: "Enable LSP response validation (debug mode)")))

(def (gerbil-lsp-main opt)
  (when (hash-ref opt 'version #f)
    (displayln (string-append "gerbil-lsp 0.1.0 (Gerbil " (gerbil-version-string) ")"))
    (exit 0))
  ;; Set log level
  (set-log-level! (log-level-from-string (hash-ref opt 'log-level "info")))
  ;; Log Gerbil version at startup
  (lsp-info (string-append "gerbil-lsp starting (Gerbil " (gerbil-version-string) ")"))
  ;; Enable validation if requested
  (when (hash-ref opt 'validate #f)
    (enable-validation!))
  ;; Register all handlers
  (register-all-handlers!)
  ;; Start the server on stdio
  (start-server))

;;; Register all LSP method handlers
(def (register-all-handlers!)
  ;; Lifecycle
  (register-request-handler! "initialize" handle-initialize)
  (register-notification-handler! "initialized" handle-initialized)
  (register-request-handler! "shutdown" handle-shutdown)
  (register-notification-handler! "exit" handle-exit)
  ;; Protocol
  (register-notification-handler! "$/cancelRequest"
    (lambda (params) (lsp-debug "cancelRequest ignored (single-threaded)")))
  ;; Document sync
  (register-notification-handler! "textDocument/didOpen" handle-did-open)
  (register-notification-handler! "textDocument/didChange" handle-did-change)
  (register-notification-handler! "textDocument/didClose" handle-did-close)
  (register-notification-handler! "textDocument/didSave" handle-did-save)
  ;; Workspace
  (register-notification-handler! "workspace/didChangeConfiguration"
    handle-did-change-configuration)
  (register-notification-handler! "workspace/didChangeWatchedFiles"
    handle-did-change-watched-files)
  (register-notification-handler! "workspace/didChangeWorkspaceFolders"
    handle-did-change-workspace-folders)
  ;; Language features
  (register-request-handler! "textDocument/completion" handle-completion)
  (register-request-handler! "completionItem/resolve" handle-completion-resolve)
  (register-request-handler! "textDocument/hover" handle-hover)
  (register-request-handler! "textDocument/definition" handle-definition)
  (register-request-handler! "textDocument/declaration" handle-definition)
  (register-request-handler! "textDocument/references" handle-references)
  (register-request-handler! "textDocument/documentSymbol" handle-document-symbol)
  (register-request-handler! "workspace/symbol" handle-workspace-symbol)
  (register-request-handler! "textDocument/prepareRename" handle-prepare-rename)
  (register-request-handler! "textDocument/rename" handle-rename)
  (register-request-handler! "textDocument/formatting" handle-formatting)
  (register-request-handler! "textDocument/signatureHelp" handle-signature-help)
  (register-request-handler! "textDocument/codeAction" handle-code-action)
  (register-request-handler! "codeAction/resolve" handle-code-action-resolve)
  (register-request-handler! "textDocument/documentHighlight" handle-document-highlight)
  (register-request-handler! "textDocument/foldingRange" handle-folding-range)
  (register-request-handler! "textDocument/selectionRange" handle-selection-range)
  (register-request-handler! "textDocument/documentLink" handle-document-link)
  ;; Semantic tokens
  (register-request-handler! "textDocument/semanticTokens/full"
    handle-semantic-tokens-full)
  (register-request-handler! "textDocument/semanticTokens/range"
    handle-semantic-tokens-range)
  (register-request-handler! "textDocument/semanticTokens/full/delta"
    handle-semantic-tokens-delta)
  ;; Range formatting
  (register-request-handler! "textDocument/rangeFormatting" handle-range-formatting)
  ;; Inlay hints
  (register-request-handler! "textDocument/inlayHint" handle-inlay-hint)
  (register-request-handler! "inlayHint/resolve" handle-inlay-hint-resolve)
  ;; Call hierarchy
  (register-request-handler! "textDocument/prepareCallHierarchy"
    handle-prepare-call-hierarchy)
  (register-request-handler! "callHierarchy/incomingCalls" handle-incoming-calls)
  (register-request-handler! "callHierarchy/outgoingCalls" handle-outgoing-calls)
  ;; Go to type definition
  (register-request-handler! "textDocument/typeDefinition" handle-type-definition)
  ;; Go to implementation
  (register-request-handler! "textDocument/implementation" handle-implementation)
  ;; Type hierarchy
  (register-request-handler! "textDocument/prepareTypeHierarchy"
    handle-prepare-type-hierarchy)
  (register-request-handler! "typeHierarchy/supertypes" handle-supertypes)
  (register-request-handler! "typeHierarchy/subtypes" handle-subtypes)
  ;; Code lenses
  (register-request-handler! "textDocument/codeLens" handle-code-lens)
  ;; On-type formatting
  (register-request-handler! "textDocument/onTypeFormatting" handle-on-type-formatting)
  ;; Pull diagnostics
  (register-request-handler! "textDocument/diagnostic" handle-document-diagnostic)
  ;; Execute command
  (register-request-handler! "workspace/executeCommand" handle-execute-command)
  ;; File operations
  (register-request-handler! "workspace/willRenameFiles" handle-will-rename-files)
  (lsp-info "all handlers registered"))
