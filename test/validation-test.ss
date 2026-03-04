;;; -*- Gerbil -*-
;;; Tests for lsp/validation — schema validation of LSP responses
(import :std/test
        :std/format
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/validation)

(export validation-test-suite)

(def validation-test-suite
  (test-suite "lsp/validation"

    ;; ---- Toggle tests ----

    (test-case "validation toggle: default disabled"
      (disable-validation!)
      (check (validation-enabled?) => #f))

    (test-case "validation toggle: enable/disable"
      (enable-validation!)
      (check (validation-enabled?) => #t)
      (disable-validation!)
      (check (validation-enabled?) => #f))

    ;; ---- Schema registry tests ----

    (test-case "schema registry: all request methods have schemas"
      ;; Every request method that returns a response should have a schema
      (let ((request-methods
              '("initialize"
                "shutdown"
                "textDocument/completion"
                "completionItem/resolve"
                "textDocument/hover"
                "textDocument/definition"
                "textDocument/declaration"
                "textDocument/references"
                "textDocument/documentSymbol"
                "workspace/symbol"
                "textDocument/prepareRename"
                "textDocument/rename"
                "textDocument/formatting"
                "textDocument/rangeFormatting"
                "textDocument/signatureHelp"
                "textDocument/codeAction"
                "codeAction/resolve"
                "textDocument/documentHighlight"
                "textDocument/foldingRange"
                "textDocument/selectionRange"
                "textDocument/documentLink"
                "textDocument/semanticTokens/full"
                "textDocument/inlayHint"
                "inlayHint/resolve"
                "textDocument/prepareCallHierarchy"
                "callHierarchy/incomingCalls"
                "callHierarchy/outgoingCalls"
                "textDocument/typeDefinition"
                "textDocument/implementation"
                "textDocument/prepareTypeHierarchy"
                "typeHierarchy/supertypes"
                "typeHierarchy/subtypes"
                "textDocument/codeLens"
                "textDocument/onTypeFormatting"
                "textDocument/diagnostic"
                "workspace/executeCommand"
                "workspace/willRenameFiles")))
        (for-each
          (lambda (method)
            (check (not (not (get-method-schema method))) => #t))
          request-methods)))

    (test-case "schema registry: notification methods are marked"
      (let ((notifications
              '("initialized" "exit" "$/cancelRequest"
                "textDocument/didOpen" "textDocument/didChange"
                "textDocument/didClose" "textDocument/didSave"
                "workspace/didChangeConfiguration"
                "workspace/didChangeWatchedFiles"
                "workspace/didChangeWorkspaceFolders")))
        (for-each
          (lambda (method)
            (check (get-method-schema method) => 'notification))
          notifications)))

    (test-case "schema registry: unknown method returns #f"
      (check (get-method-schema "nonexistent/method") => #f))

    ;; ---- type-check? tests ----

    (test-case "type-check: string"
      (check (type-check? "hello" 'string) => #t)
      (check (type-check? 42 'string) => #f))

    (test-case "type-check: integer"
      (check (type-check? 42 'integer) => #t)
      (check (type-check? "nope" 'integer) => #f))

    (test-case "type-check: number"
      (check (type-check? 42 'number) => #t)
      (check (type-check? 3.14 'number) => #t)
      (check (type-check? "nope" 'number) => #f))

    (test-case "type-check: boolean"
      (check (type-check? #t 'boolean) => #t)
      (check (type-check? #f 'boolean) => #t)
      (check (type-check? 0 'boolean) => #f))

    (test-case "type-check: hash"
      (check (type-check? (hash) 'hash) => #t)
      (check (type-check? "nope" 'hash) => #f))

    (test-case "type-check: vector"
      (check (type-check? (vector 1 2) 'vector) => #t)
      (check (type-check? '(1 2) 'vector) => #t)
      (check (type-check? "nope" 'vector) => #f))

    (test-case "type-check: any"
      (check (type-check? "anything" 'any) => #t)
      (check (type-check? 42 'any) => #t)
      (check (type-check? #f 'any) => #t))

    (test-case "type-check: void-ok"
      (check (type-check? (void) 'void-ok) => #t)
      (check (type-check? #f 'void-ok) => #t)
      (check (type-check? "nope" 'void-ok) => #f))

    (test-case "type-check: vector-of"
      (check (type-check? (vector "a" "b") '(vector-of string)) => #t)
      (check (type-check? (vector 1 2 3) '(vector-of integer)) => #t)
      (check (type-check? (vector "a" 1) '(vector-of string)) => #f)
      (check (type-check? (vector) '(vector-of string)) => #t))

    (test-case "type-check: or union type"
      (check (type-check? "hi" '(or string integer)) => #t)
      (check (type-check? 42 '(or string integer)) => #t)
      (check (type-check? #t '(or string integer)) => #f))

    ;; ---- validate-hash tests ----

    (test-case "validate-hash: valid object passes"
      (let ((obj (hash ("line" 5) ("character" 10))))
        (check (validate-hash obj *schema:position* "") => '())))

    (test-case "validate-hash: missing required field"
      (let ((obj (hash ("line" 5))))
        (let ((violations (validate-hash obj *schema:position* "")))
          (check (length violations) => 1)
          (check (string-contains (car violations) "character") => #t)
          (check (string-contains (car violations) "required") => #t))))

    (test-case "validate-hash: wrong type for field"
      (let ((obj (hash ("line" "not-a-number") ("character" 10))))
        (let ((violations (validate-hash obj *schema:position* "")))
          (check (length violations) => 1)
          (check (string-contains (car violations) "line") => #t)
          (check (string-contains (car violations) "integer") => #t))))

    (test-case "validate-hash: optional field missing is OK"
      (let ((obj (hash ("range" (hash)) ("message" "err"))))
        ;; severity and source are optional in diagnostic schema
        (check (null? (validate-hash obj *schema:diagnostic* "")) => #t)))

    (test-case "validate-hash: optional field with wrong type"
      (let ((obj (hash ("range" (hash)) ("message" "err") ("severity" "bad"))))
        (let ((violations (validate-hash obj *schema:diagnostic* "")))
          (check (length violations) => 1)
          (check (string-contains (car violations) "severity") => #t))))

    (test-case "validate-hash: multiple errors"
      (let ((obj (hash)))
        ;; position requires both line and character
        (let ((violations (validate-hash obj *schema:position* "")))
          (check (length violations) => 2))))

    (test-case "validate-hash: extra fields are OK"
      ;; LSP objects may have additional fields not in the schema
      (let ((obj (hash ("line" 0) ("character" 0) ("extra" "stuff"))))
        (check (validate-hash obj *schema:position* "") => '())))

    (test-case "validate-hash: nested path in errors"
      (let ((obj (hash ("line" "bad"))))
        (let ((violations (validate-hash obj *schema:position* "result.start")))
          (check (string-contains (car violations) "result.start.line") => #t))))

    ;; ---- validate-response: void methods ----

    (test-case "validate-response: shutdown accepts void"
      (check (validate-response "shutdown" (void)) => '()))

    (test-case "validate-response: shutdown accepts #f"
      (check (validate-response "shutdown" #f) => '()))

    (test-case "validate-response: shutdown rejects non-void"
      (let ((violations (validate-response "shutdown" "unexpected")))
        (check (pair? violations) => #t)))

    ;; ---- validate-response: notification methods ----

    (test-case "validate-response: notifications always pass"
      (check (validate-response "textDocument/didOpen" (void)) => '())
      (check (validate-response "initialized" "anything") => '()))

    ;; ---- validate-response: unknown methods ----

    (test-case "validate-response: unknown method passes"
      (check (validate-response "custom/unknown" "anything") => '()))

    ;; ---- validate-response: void-or-object methods ----

    (test-case "validate-response: hover void is OK"
      (check (validate-response "textDocument/hover" (void)) => '())
      (check (validate-response "textDocument/hover" #f) => '()))

    (test-case "validate-response: hover with valid object"
      (let ((result (make-hover "test content"
                      (make-lsp-range 0 0 0 5))))
        (check (validate-response "textDocument/hover" result) => '())))

    (test-case "validate-response: hover missing required contents"
      (let ((result (hash ("range" (hash)))))
        (let ((violations (validate-response "textDocument/hover" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "contents") => #t))))

    ;; ---- validate-response: object schemas ----

    (test-case "validate-response: initialize valid result"
      (let ((result (hash ("capabilities" (hash ("hoverProvider" #t)))
                          ("serverInfo" (hash ("name" "test") ("version" "1.0"))))))
        (check (validate-response "initialize" result) => '())))

    (test-case "validate-response: initialize missing capabilities"
      (let ((result (hash ("serverInfo" (hash)))))
        (let ((violations (validate-response "initialize" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "capabilities") => #t))))

    (test-case "validate-response: completion list valid"
      (let ((result (hash ("isIncomplete" #f)
                          ("items" (vector)))))
        (check (validate-response "textDocument/completion" result) => '())))

    (test-case "validate-response: completion list missing isIncomplete"
      (let ((result (hash ("items" (vector)))))
        (let ((violations (validate-response "textDocument/completion" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "isIncomplete") => #t))))

    (test-case "validate-response: completion list wrong type for isIncomplete"
      (let ((result (hash ("isIncomplete" "yes") ("items" (vector)))))
        (let ((violations (validate-response "textDocument/completion" result)))
          (check (pair? violations) => #t))))

    ;; ---- validate-response: vector-of methods ----

    (test-case "validate-response: references empty array"
      (check (validate-response "textDocument/references" (vector)) => '()))

    (test-case "validate-response: references valid locations"
      (let ((locs (vector
                    (hash ("uri" "file:///a.ss")
                          ("range" (make-lsp-range 0 0 0 5)))
                    (hash ("uri" "file:///b.ss")
                          ("range" (make-lsp-range 1 0 1 3))))))
        (check (validate-response "textDocument/references" locs) => '())))

    (test-case "validate-response: references missing uri in location"
      (let ((locs (vector (hash ("range" (make-lsp-range 0 0 0 5))))))
        (let ((violations (validate-response "textDocument/references" locs)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "uri") => #t))))

    (test-case "validate-response: document symbols valid"
      (let ((syms (vector
                    (make-document-symbol "my-func" SymbolKind.Function
                      (make-lsp-range 0 0 5 0)
                      (make-lsp-range 0 5 0 12)))))
        (check (validate-response "textDocument/documentSymbol" syms) => '())))

    (test-case "validate-response: formatting valid text edits"
      (let ((edits (vector (make-text-edit (make-lsp-range 0 0 0 5) "hello"))))
        (check (validate-response "textDocument/formatting" edits) => '())))

    (test-case "validate-response: formatting text edit missing newText"
      (let ((edits (vector (hash ("range" (make-lsp-range 0 0 0 5))))))
        (let ((violations (validate-response "textDocument/formatting" edits)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "newText") => #t))))

    ;; ---- validate-response: semantic tokens ----

    (test-case "validate-response: semantic tokens valid"
      (let ((result (hash ("data" (vector 0 5 3 0 0)))))
        (check (validate-response "textDocument/semanticTokens/full" result) => '())))

    (test-case "validate-response: semantic tokens missing data"
      (let ((result (hash)))
        (let ((violations
                (validate-response "textDocument/semanticTokens/full" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "data") => #t))))

    ;; ---- validate-response: signature help ----

    (test-case "validate-response: signature help void OK"
      (check (validate-response "textDocument/signatureHelp" (void)) => '()))

    (test-case "validate-response: signature help valid"
      (let ((result (make-signature-help
                      (vector (make-signature-information "my-func (x y)"))
                      0 0)))
        (check (validate-response "textDocument/signatureHelp" result) => '())))

    (test-case "validate-response: signature help missing signatures"
      (let ((result (hash ("activeSignature" 0) ("activeParameter" 0))))
        (let ((violations
                (validate-response "textDocument/signatureHelp" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "signatures") => #t))))

    ;; ---- validate-response: workspace edit ----

    (test-case "validate-response: rename valid workspace edit"
      (let ((result (hash ("changes"
                            (hash ("file:///a.ss"
                                   (vector (make-text-edit
                                             (make-lsp-range 0 0 0 3)
                                             "new-name"))))))))
        (check (validate-response "textDocument/rename" result) => '())))

    ;; ---- validate-response: diagnostic report ----

    (test-case "validate-response: pull diagnostic valid"
      (let ((result (hash ("kind" "full") ("items" (vector)))))
        (check (validate-response "textDocument/diagnostic" result) => '())))

    (test-case "validate-response: pull diagnostic missing kind"
      (let ((result (hash ("items" (vector)))))
        (let ((violations
                (validate-response "textDocument/diagnostic" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "kind") => #t))))

    ;; ---- validate-response: definition void-or-location ----

    (test-case "validate-response: definition void OK"
      (check (validate-response "textDocument/definition" (void)) => '())
      (check (validate-response "textDocument/definition" #f) => '()))

    (test-case "validate-response: definition valid location"
      (let ((result (hash ("uri" "file:///test.ss")
                          ("range" (make-lsp-range 5 0 5 10)))))
        (check (validate-response "textDocument/definition" result) => '())))

    (test-case "validate-response: definition missing uri"
      (let ((result (hash ("range" (make-lsp-range 0 0 0 5)))))
        (let ((violations
                (validate-response "textDocument/definition" result)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "uri") => #t))))

    ;; ---- validate-response: call hierarchy ----

    (test-case "validate-response: prepare call hierarchy valid"
      (let ((items (vector
                     (hash ("name" "my-func")
                           ("kind" SymbolKind.Function)
                           ("uri" "file:///a.ss")
                           ("range" (make-lsp-range 0 0 5 0))
                           ("selectionRange" (make-lsp-range 0 5 0 12))))))
        (check (validate-response "textDocument/prepareCallHierarchy"
                 items)
               => '())))

    (test-case "validate-response: call hierarchy item missing name"
      (let ((items (vector
                     (hash ("kind" SymbolKind.Function)
                           ("uri" "file:///a.ss")
                           ("range" (hash))
                           ("selectionRange" (hash))))))
        (let ((violations
                (validate-response "textDocument/prepareCallHierarchy" items)))
          (check (pair? violations) => #t)
          (check (string-contains (car violations) "name") => #t))))

    ;; ---- validate-response: inlay hints ----

    (test-case "validate-response: inlay hints valid"
      (let ((hints (vector
                     (hash ("position" (hash ("line" 0) ("character" 5)))
                           ("label" "x:")
                           ("kind" 2)))))
        (check (validate-response "textDocument/inlayHint" hints) => '())))

    ;; ---- validate-response: code lenses ----

    (test-case "validate-response: code lens valid"
      (let ((lenses (vector
                      (hash ("range" (make-lsp-range 0 0 0 10))
                            ("command" (hash ("title" "Run test")
                                             ("command" "test")))))))
        (check (validate-response "textDocument/codeLens" lenses) => '())))

    ;; ---- validate-and-log! tests ----

    (test-case "validate-and-log!: returns result unchanged"
      (let ((result (hash ("isIncomplete" #f) ("items" (vector)))))
        (enable-validation!)
        (check (eq? (validate-and-log! "textDocument/completion" result)
                    result)
               => #t)
        (disable-validation!)))

    (test-case "validate-and-log!: skips when disabled"
      (disable-validation!)
      ;; Should not throw even with invalid data
      (let ((result "invalid"))
        (check (equal? (validate-and-log! "textDocument/completion" result)
                       "invalid")
               => #t)))

    ;; ---- all-validated-methods test ----

    (test-case "all-validated-methods: returns non-empty list"
      (let ((methods (all-validated-methods)))
        (check (pair? methods) => #t)
        ;; Should have at least 30+ methods
        (check (> (length methods) 30) => #t)))

    ;; ---- E2E: simulate full request/response validation ----

    (test-case "e2e: hover response round-trip"
      ;; Simulates what server.ss does: handler returns, validate-and-log! checks
      (enable-validation!)
      (let ((result (make-hover "```gerbil\n(def foo 42)\n```"
                      (make-lsp-range 0 5 0 8))))
        (check (validate-response "textDocument/hover" result) => '())
        (validate-and-log! "textDocument/hover" result))
      (disable-validation!))

    (test-case "e2e: completion response round-trip"
      (enable-validation!)
      (let ((result (hash ("isIncomplete" #f)
                          ("items" (vector
                                    (make-completion-item "define"
                                      kind: CompletionItemKind.Keyword)
                                    (make-completion-item "my-func"
                                      kind: CompletionItemKind.Function
                                      detail: "(my-func x y)"))))))
        (check (validate-response "textDocument/completion" result) => '())
        (validate-and-log! "textDocument/completion" result))
      (disable-validation!))

    (test-case "e2e: definition response round-trip"
      (enable-validation!)
      (let ((result (hash ("uri" "file:///project/src/lib.ss")
                          ("range" (make-lsp-range 10 0 10 15)))))
        (check (validate-response "textDocument/definition" result) => '())
        (validate-and-log! "textDocument/definition" result))
      (disable-validation!))

    (test-case "e2e: references response round-trip"
      (enable-validation!)
      (let ((result (vector
                      (hash ("uri" "file:///a.ss")
                            ("range" (make-lsp-range 0 0 0 3)))
                      (hash ("uri" "file:///b.ss")
                            ("range" (make-lsp-range 5 2 5 5))))))
        (check (validate-response "textDocument/references" result) => '())
        (validate-and-log! "textDocument/references" result))
      (disable-validation!))

    (test-case "e2e: document symbols response round-trip"
      (enable-validation!)
      (let ((result (vector
                      (make-document-symbol "my-struct" SymbolKind.Struct
                        (make-lsp-range 0 0 3 0)
                        (make-lsp-range 0 12 0 21))
                      (make-document-symbol "my-func" SymbolKind.Function
                        (make-lsp-range 5 0 8 0)
                        (make-lsp-range 5 5 5 12)))))
        (check (validate-response "textDocument/documentSymbol" result) => '())
        (validate-and-log! "textDocument/documentSymbol" result))
      (disable-validation!))

    (test-case "e2e: signature help response round-trip"
      (enable-validation!)
      (let ((result (make-signature-help
                      (vector
                        (make-signature-information "(my-func x y z)"
                          parameters: (list
                            (make-parameter-information "x")
                            (make-parameter-information "y")
                            (make-parameter-information "z"))))
                      0 1)))
        (check (validate-response "textDocument/signatureHelp" result) => '())
        (validate-and-log! "textDocument/signatureHelp" result))
      (disable-validation!))

    (test-case "e2e: formatting response round-trip"
      (enable-validation!)
      (let ((result (vector
                      (make-text-edit (make-lsp-range 0 0 10 0)
                                     "(def (my-func x)\n  (+ x 1))\n"))))
        (check (validate-response "textDocument/formatting" result) => '())
        (validate-and-log! "textDocument/formatting" result))
      (disable-validation!))

    (test-case "e2e: rename response round-trip"
      (enable-validation!)
      (let ((result (hash ("changes"
                            (hash ("file:///src/main.ss"
                                   (vector
                                     (make-text-edit (make-lsp-range 5 5 5 8) "bar")
                                     (make-text-edit (make-lsp-range 10 2 10 5) "bar"))))))))
        (check (validate-response "textDocument/rename" result) => '())
        (validate-and-log! "textDocument/rename" result))
      (disable-validation!))

    (test-case "e2e: diagnostic report round-trip"
      (enable-validation!)
      (let ((result (hash ("kind" "full")
                          ("items" (vector
                                    (make-diagnostic
                                      (make-lsp-range 3 0 3 20)
                                      "unbound identifier: foo"
                                      severity: DiagnosticSeverity.Error))))))
        (check (validate-response "textDocument/diagnostic" result) => '())
        (validate-and-log! "textDocument/diagnostic" result))
      (disable-validation!))

    (test-case "e2e: code action response round-trip"
      (enable-validation!)
      (let ((result (vector
                      (hash ("title" "Organize imports")
                            ("kind" "source.organizeImports")))))
        (check (validate-response "textDocument/codeAction" result) => '())
        (validate-and-log! "textDocument/codeAction" result))
      (disable-validation!))

    (test-case "e2e: folding ranges round-trip"
      (enable-validation!)
      (let ((result (vector
                      (hash ("startLine" 0) ("endLine" 5)
                            ("kind" "imports"))
                      (hash ("startLine" 7) ("endLine" 20)
                            ("kind" "region")))))
        (check (validate-response "textDocument/foldingRange" result) => '())
        (validate-and-log! "textDocument/foldingRange" result))
      (disable-validation!))

  ))

;;; Helper: check if a string contains a substring
(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (if (> nlen hlen) #f
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? (substring haystack i (+ i nlen)) needle) #t)
          (else (loop (+ i 1))))))))

(def main
  (lambda ()
    (run-tests! validation-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
