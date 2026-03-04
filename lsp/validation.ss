;;; -*- Gerbil -*-
;;; LSP protocol response validation
;;; Validates outgoing messages against LSP spec schemas in debug mode.
(import ./compat/compat
        ./util/log
        ./types)
(export #t)

;;; ---- Validation toggle ----

(def *validation-enabled* #f)

(def (validation-enabled?)
  *validation-enabled*)

(def (enable-validation!)
  (set! *validation-enabled* #t)
  (lsp-info "LSP response validation enabled"))

(def (disable-validation!)
  (set! *validation-enabled* #f))

;;; ---- Type predicates for schema validation ----

;;; Check if a value matches a type descriptor.
;;; Type descriptors:
;;;   'string   - string?
;;;   'integer  - integer?
;;;   'number   - number?
;;;   'boolean  - boolean?
;;;   'hash     - hash-table?
;;;   'vector   - vector? or list? (JSON arrays)
;;;   'any      - always valid
;;;   'void-ok  - void? is acceptable (null result)
;;;   (vector-of <type>) - vector/list where each element matches <type>
;;;   (or <type1> <type2> ...) - union type
;;;   ((field-name . spec) ...) - nested object schema (alist)
(def (type-check? value type-desc)
  (cond
    ((eq? type-desc 'string) (string? value))
    ((eq? type-desc 'integer) (integer? value))
    ((eq? type-desc 'number) (number? value))
    ((eq? type-desc 'boolean) (boolean? value))
    ((eq? type-desc 'hash) (hash-table? value))
    ((eq? type-desc 'vector)
     (or (vector? value) (list? value)))
    ((eq? type-desc 'any) #t)
    ((eq? type-desc 'void-ok)
     (or (void? value) (not value)))
    ;; (vector-of sub-type) — array where elements match sub-type
    ((and (pair? type-desc) (eq? (car type-desc) 'vector-of))
     (let ((elem-type (cadr type-desc)))
       (and (or (vector? value) (list? value))
            (let ((elems (if (vector? value) (vector->list value) value)))
              (andmap (lambda (e) (type-check? e elem-type)) elems)))))
    ;; (or type1 type2 ...) — union type
    ((and (pair? type-desc) (eq? (car type-desc) 'or))
     (ormap (lambda (t) (type-check? value t)) (cdr type-desc)))
    ;; Nested schema: alist of (field-name required? type)
    ((and (pair? type-desc) (pair? (car type-desc)))
     (and (hash-table? value)
          (null? (validate-hash value type-desc ""))))
    (else #t)))

;;; ---- Common sub-schemas (LSP spec structures) ----

;;; Each schema is an alist of (field-name required? type-descriptor)

(def *schema:position*
  '(("line" #t integer)
    ("character" #t integer)))

(def *schema:range*
  '(("start" #t hash)
    ("end" #t hash)))

(def *schema:location*
  '(("uri" #t string)
    ("range" #t hash)))

(def *schema:text-edit*
  '(("range" #t hash)
    ("newText" #t string)))

(def *schema:markup-content*
  '(("kind" #t string)
    ("value" #t string)))

(def *schema:diagnostic*
  '(("range" #t hash)
    ("message" #t string)
    ("severity" #f integer)
    ("source" #f string)
    ("code" #f any)
    ("tags" #f vector)
    ("relatedInformation" #f vector)))

(def *schema:completion-item*
  '(("label" #t string)
    ("kind" #f integer)
    ("detail" #f string)
    ("documentation" #f any)
    ("insertText" #f string)
    ("insertTextFormat" #f integer)
    ("textEdit" #f hash)
    ("additionalTextEdits" #f vector)
    ("data" #f any)
    ("sortText" #f string)
    ("filterText" #f string)))

(def *schema:completion-list*
  '(("isIncomplete" #t boolean)
    ("items" #t vector)))

(def *schema:hover*
  '(("contents" #t hash)
    ("range" #f hash)))

(def *schema:signature-help*
  '(("signatures" #t vector)
    ("activeSignature" #t integer)
    ("activeParameter" #t integer)))

(def *schema:signature-info*
  '(("label" #t string)
    ("documentation" #f any)
    ("parameters" #f vector)))

(def *schema:document-symbol*
  '(("name" #t string)
    ("kind" #t integer)
    ("range" #t hash)
    ("selectionRange" #t hash)
    ("children" #f vector)))

(def *schema:symbol-information*
  '(("name" #t string)
    ("kind" #t integer)
    ("location" #t hash)))

(def *schema:code-action*
  '(("title" #t string)
    ("kind" #f string)
    ("diagnostics" #f vector)
    ("edit" #f hash)
    ("command" #f hash)
    ("data" #f any)))

(def *schema:document-highlight*
  '(("range" #t hash)
    ("kind" #f integer)))

(def *schema:folding-range*
  '(("startLine" #t integer)
    ("endLine" #t integer)
    ("startCharacter" #f integer)
    ("endCharacter" #f integer)
    ("kind" #f string)))

(def *schema:document-link*
  '(("range" #t hash)
    ("target" #f string)
    ("data" #f any)))

(def *schema:inlay-hint*
  '(("position" #t hash)
    ("label" #t any)
    ("kind" #f integer)
    ("paddingLeft" #f boolean)
    ("paddingRight" #f boolean)))

(def *schema:call-hierarchy-item*
  '(("name" #t string)
    ("kind" #t integer)
    ("uri" #t string)
    ("range" #t hash)
    ("selectionRange" #t hash)
    ("detail" #f string)
    ("data" #f any)))

(def *schema:incoming-call*
  '(("from" #t hash)
    ("fromRanges" #t vector)))

(def *schema:outgoing-call*
  '(("to" #t hash)
    ("fromRanges" #t vector)))

(def *schema:type-hierarchy-item*
  '(("name" #t string)
    ("kind" #t integer)
    ("uri" #t string)
    ("range" #t hash)
    ("selectionRange" #t hash)
    ("detail" #f string)
    ("data" #f any)))

(def *schema:code-lens*
  '(("range" #t hash)
    ("command" #f hash)
    ("data" #f any)))

(def *schema:workspace-edit*
  '(("changes" #f hash)
    ("documentChanges" #f vector)))

(def *schema:text-document-edit*
  '(("textDocument" #t hash)
    ("edits" #t vector)))

(def *schema:diagnostic-report*
  '(("kind" #t string)
    ("items" #t vector)))

(def *schema:prepare-rename*
  '(("range" #t hash)
    ("placeholder" #t string)))

(def *schema:selection-range*
  '(("range" #t hash)
    ("parent" #f any)))

(def *schema:semantic-tokens*
  '(("data" #t vector)))

(def *schema:initialize-result*
  '(("capabilities" #t hash)
    ("serverInfo" #f hash)))

;;; ---- Method → response schema mapping ----

;;; Maps LSP method names to their expected response schema.
;;; Schema values:
;;;   'void      — result should be void/#f/null
;;;   'notification — no response expected (notification)
;;;   schema-alist  — validate against field specs
;;;   (vector-of schema) — result is array of items matching schema
;;;   (or 'void-ok schema) — result can be void/null or match schema
(def *method-schemas* (make-hash-table))

(def (init-method-schemas!)
  ;; Lifecycle
  (hash-put! *method-schemas* "initialize" *schema:initialize-result*)
  (hash-put! *method-schemas* "initialized" 'notification)
  (hash-put! *method-schemas* "shutdown" 'void)
  (hash-put! *method-schemas* "exit" 'notification)
  ;; Protocol
  (hash-put! *method-schemas* "$/cancelRequest" 'notification)
  ;; Document sync (all notifications)
  (hash-put! *method-schemas* "textDocument/didOpen" 'notification)
  (hash-put! *method-schemas* "textDocument/didChange" 'notification)
  (hash-put! *method-schemas* "textDocument/didClose" 'notification)
  (hash-put! *method-schemas* "textDocument/didSave" 'notification)
  (hash-put! *method-schemas* "workspace/didChangeConfiguration" 'notification)
  (hash-put! *method-schemas* "workspace/didChangeWatchedFiles" 'notification)
  (hash-put! *method-schemas* "workspace/didChangeWorkspaceFolders" 'notification)
  ;; Completion
  (hash-put! *method-schemas* "textDocument/completion" *schema:completion-list*)
  (hash-put! *method-schemas* "completionItem/resolve" *schema:completion-item*)
  ;; Hover
  (hash-put! *method-schemas* "textDocument/hover"
    `(or void-ok ,*schema:hover*))
  ;; Definition / Declaration
  (hash-put! *method-schemas* "textDocument/definition"
    `(or void-ok ,*schema:location*))
  (hash-put! *method-schemas* "textDocument/declaration"
    `(or void-ok ,*schema:location*))
  ;; References
  (hash-put! *method-schemas* "textDocument/references"
    `(vector-of ,*schema:location*))
  ;; Symbols
  (hash-put! *method-schemas* "textDocument/documentSymbol"
    `(vector-of ,*schema:document-symbol*))
  (hash-put! *method-schemas* "workspace/symbol"
    `(vector-of ,*schema:symbol-information*))
  ;; Rename
  (hash-put! *method-schemas* "textDocument/prepareRename"
    `(or void-ok ,*schema:prepare-rename*))
  (hash-put! *method-schemas* "textDocument/rename" *schema:workspace-edit*)
  ;; Formatting
  (hash-put! *method-schemas* "textDocument/formatting"
    `(vector-of ,*schema:text-edit*))
  (hash-put! *method-schemas* "textDocument/rangeFormatting"
    `(vector-of ,*schema:text-edit*))
  ;; Signature help
  (hash-put! *method-schemas* "textDocument/signatureHelp"
    `(or void-ok ,*schema:signature-help*))
  ;; Code actions
  (hash-put! *method-schemas* "textDocument/codeAction"
    `(vector-of ,*schema:code-action*))
  (hash-put! *method-schemas* "codeAction/resolve" *schema:code-action*)
  ;; Highlight
  (hash-put! *method-schemas* "textDocument/documentHighlight"
    `(vector-of ,*schema:document-highlight*))
  ;; Folding
  (hash-put! *method-schemas* "textDocument/foldingRange"
    `(vector-of ,*schema:folding-range*))
  ;; Selection
  (hash-put! *method-schemas* "textDocument/selectionRange"
    `(vector-of ,*schema:selection-range*))
  ;; Document links
  (hash-put! *method-schemas* "textDocument/documentLink"
    `(vector-of ,*schema:document-link*))
  ;; Semantic tokens
  (hash-put! *method-schemas* "textDocument/semanticTokens/full"
    *schema:semantic-tokens*)
  ;; Inlay hints
  (hash-put! *method-schemas* "textDocument/inlayHint"
    `(vector-of ,*schema:inlay-hint*))
  (hash-put! *method-schemas* "inlayHint/resolve" *schema:inlay-hint*)
  ;; Call hierarchy
  (hash-put! *method-schemas* "textDocument/prepareCallHierarchy"
    `(vector-of ,*schema:call-hierarchy-item*))
  (hash-put! *method-schemas* "callHierarchy/incomingCalls"
    `(vector-of ,*schema:incoming-call*))
  (hash-put! *method-schemas* "callHierarchy/outgoingCalls"
    `(vector-of ,*schema:outgoing-call*))
  ;; Type definition
  (hash-put! *method-schemas* "textDocument/typeDefinition"
    `(or void-ok ,*schema:location*))
  ;; Implementation
  (hash-put! *method-schemas* "textDocument/implementation"
    `(vector-of ,*schema:location*))
  ;; Type hierarchy
  (hash-put! *method-schemas* "textDocument/prepareTypeHierarchy"
    `(vector-of ,*schema:type-hierarchy-item*))
  (hash-put! *method-schemas* "typeHierarchy/supertypes"
    `(vector-of ,*schema:type-hierarchy-item*))
  (hash-put! *method-schemas* "typeHierarchy/subtypes"
    `(vector-of ,*schema:type-hierarchy-item*))
  ;; Code lenses
  (hash-put! *method-schemas* "textDocument/codeLens"
    `(vector-of ,*schema:code-lens*))
  ;; On-type formatting
  (hash-put! *method-schemas* "textDocument/onTypeFormatting"
    `(vector-of ,*schema:text-edit*))
  ;; Pull diagnostics
  (hash-put! *method-schemas* "textDocument/diagnostic"
    *schema:diagnostic-report*)
  ;; Execute command
  (hash-put! *method-schemas* "workspace/executeCommand" 'void)
  ;; File operations
  (hash-put! *method-schemas* "workspace/willRenameFiles"
    `(or void-ok ,*schema:workspace-edit*)))

;; Initialize on module load
(init-method-schemas!)

;;; ---- Validation engine ----

;;; Validate a hash table against a schema (alist of field specs).
;;; Returns a list of violation strings. Empty list = valid.
;;; path: string prefix for nested error reporting (e.g. "result.range")
(def (validate-hash value schema path)
  (let ((violations []))
    (for-each
      (lambda (spec)
        (let ((field-name (car spec))
              (required? (cadr spec))
              (field-type (caddr spec)))
          (let ((field-path (if (string=? path "")
                              field-name
                              (string-append path "." field-name))))
            (if (hash-key? value field-name)
              ;; Field present — check type
              (let ((field-val (hash-ref value field-name)))
                (unless (type-check? field-val field-type)
                  (set! violations
                    (cons (format "~a: expected ~a, got ~a"
                            field-path field-type
                            (type-name field-val))
                          violations))))
              ;; Field missing — error only if required
              (when required?
                (set! violations
                  (cons (format "~a: required field missing" field-path)
                        violations)))))))
      schema)
    (reverse violations)))

;;; Get a human-readable type name for a value
(def (type-name value)
  (cond
    ((string? value) "string")
    ((integer? value) "integer")
    ((number? value) "number")
    ((boolean? value) "boolean")
    ((hash-table? value) "object")
    ((vector? value) "array")
    ((list? value) "list")
    ((void? value) "void")
    ((not value) "null")
    (else (format "~a" (type-of value)))))

;;; ---- Public API ----

;;; Validate a response result for a given LSP method.
;;; Returns a list of violation strings. Empty = valid.
(def (validate-response method result)
  (let ((schema (hash-get *method-schemas* method)))
    (cond
      ;; No schema registered — unknown method, skip
      ((not schema) [])
      ;; Notification — should never have a response
      ((eq? schema 'notification) [])
      ;; Void — result should be void or #f
      ((eq? schema 'void)
       (if (or (void? result) (not result) (equal? result (hash)))
         []
         [(format "~a: expected void/null result, got ~a" method (type-name result))]))
      ;; (or void-ok <schema>) — result can be void or match schema
      ((and (pair? schema) (eq? (car schema) 'or)
            (eq? (cadr schema) 'void-ok))
       (if (or (void? result) (not result))
         []
         (let ((inner-schema (caddr schema)))
           (validate-value result inner-schema method))))
      ;; (vector-of <schema>) — result is array of items
      ((and (pair? schema) (eq? (car schema) 'vector-of))
       (validate-vector-result result (cadr schema) method))
      ;; Alist schema — validate as object
      ((and (pair? schema) (pair? (car schema)))
       (if (hash-table? result)
         (validate-hash result schema "result")
         [(format "~a: expected object, got ~a" method (type-name result))]))
      (else []))))

;;; Validate a vector/list result where each element matches elem-schema
(def (validate-vector-result result elem-schema method)
  (cond
    ((or (void? result) (not result))
     ;; Some handlers return void instead of empty array — warn
     [(format "~a: expected array, got ~a (use empty array instead)"
        method (type-name result))])
    ((or (vector? result) (list? result))
     (let* ((elems (if (vector? result) (vector->list result) result))
            (violations []))
       (let loop ((items elems) (i 0))
         (if (pair? items)
           (let ((item (car items)))
             (when (and (pair? elem-schema) (pair? (car elem-schema)))
               ;; It's an object schema — validate each element
               (when (hash-table? item)
                 (let ((item-violations
                         (validate-hash item elem-schema
                           (format "result[~a]" i))))
                   (set! violations (append violations item-violations)))))
             (loop (cdr items) (+ i 1)))
           violations))))
    (else
     [(format "~a: expected array, got ~a" method (type-name result))])))

;;; Validate a single value against a type descriptor
(def (validate-value value type-desc context)
  (cond
    ;; Alist schema — validate as object
    ((and (pair? type-desc) (pair? (car type-desc)))
     (if (hash-table? value)
       (validate-hash value type-desc "result")
       [(format "~a: expected object, got ~a" context (type-name value))]))
    ;; vector-of
    ((and (pair? type-desc) (eq? (car type-desc) 'vector-of))
     (validate-vector-result value (cadr type-desc) context))
    ;; Simple type
    (else
     (if (type-check? value type-desc)
       []
       [(format "~a: expected ~a, got ~a" context type-desc (type-name value))]))))

;;; Validate and log — the main entry point called from server.ss
;;; Returns the result unchanged (validation is observational only)
(def (validate-and-log! method result)
  (when (validation-enabled?)
    (let ((violations (validate-response method result)))
      (unless (null? violations)
        (lsp-warn "validation violations for ~a:" method)
        (for-each (lambda (v) (lsp-warn "  ~a" v)) violations))))
  result)

;;; Get the schema for a method (for testing/inspection)
(def (get-method-schema method)
  (hash-get *method-schemas* method))

;;; List all methods that have schemas registered
(def (all-validated-methods)
  (hash-keys *method-schemas*))
