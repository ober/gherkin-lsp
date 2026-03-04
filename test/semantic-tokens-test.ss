;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/semantic-tokens
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/validation
        :lsp/lsp/handlers/semantic-tokens)

(export semantic-tokens-test-suite)

(def semantic-tokens-test-suite
  (test-suite "lsp/handlers/semantic-tokens"

    ;; --- classify-symbol-token ---
    (test-case "classify-symbol-token: keyword"
      (let ((tok (classify-symbol-token "def" 0 1 #f)))
        (check (cadddr tok) => SemanticTokenType.keyword)))

    (test-case "classify-symbol-token: if keyword"
      (let ((tok (classify-symbol-token "if" 0 1 #f)))
        (check (cadddr tok) => SemanticTokenType.keyword)))

    (test-case "classify-symbol-token: import keyword"
      (let ((tok (classify-symbol-token "import" 0 1 #f)))
        (check (cadddr tok) => SemanticTokenType.keyword)))

    (test-case "classify-symbol-token: lambda keyword"
      (let ((tok (classify-symbol-token "lambda" 0 1 #f)))
        (check (cadddr tok) => SemanticTokenType.keyword)))

    (test-case "classify-symbol-token: regular variable"
      (let ((tok (classify-symbol-token "my-var" 0 0 #f)))
        (check (cadddr tok) => SemanticTokenType.variable)))

    (test-case "classify-symbol-token: make- constructor"
      (let ((tok (classify-symbol-token "make-point" 0 0 #f)))
        (check (cadddr tok) => SemanticTokenType.function)))

    (test-case "classify-symbol-token: type with ::t"
      (let ((tok (classify-symbol-token "point::t" 0 0 #f)))
        (check (cadddr tok) => SemanticTokenType.type)))

    (test-case "classify-symbol-token: keyword parameter ending with :"
      (let ((tok (classify-symbol-token "name:" 0 0 #f)))
        (check (cadddr tok) => SemanticTokenType.parameter)))

    (test-case "classify-symbol-token: UPPERCASE constant"
      (let ((tok (classify-symbol-token "MAX_SIZE" 0 0 #f)))
        (check (cadddr tok) => SemanticTokenType.variable)
        (check (car (cddddr tok)) => SemanticTokenModifier.readonly)))

    (test-case "classify-symbol-token: def-ctx override"
      ;; When def-ctx is set (e.g. after 'defstruct'), name gets type+mods
      (let* ((ctx (cons SemanticTokenType.type SemanticTokenModifier.definition))
             (tok (classify-symbol-token "point" 0 0 ctx)))
        (check (cadddr tok) => SemanticTokenType.type)
        (check (car (cddddr tok)) => SemanticTokenModifier.definition)))

    ;; --- all-uppercase? ---
    (test-case "all-uppercase?: YES"
      (check (all-uppercase? "HELLO") => #t))

    (test-case "all-uppercase?: with underscore"
      (check (all-uppercase? "MAX_SIZE") => #t))

    (test-case "all-uppercase?: with numbers"
      (check (all-uppercase? "HTTP2") => #t))

    (test-case "all-uppercase?: lowercase"
      (check (all-uppercase? "hello") => #f))

    (test-case "all-uppercase?: mixed case"
      (check (all-uppercase? "Hello") => #f))

    (test-case "all-uppercase?: single char"
      (check (all-uppercase? "x") => #f))

    ;; --- dot-accessor-split ---
    (test-case "dot-accessor-split: obj.method"
      (let ((result (dot-accessor-split "obj.method")))
        (check (pair? result) => #t)
        (check (car result) => "obj")
        (check (cdr result) => "method")))

    (test-case "dot-accessor-split: no dot"
      (check (dot-accessor-split "hello") => #f))

    (test-case "dot-accessor-split: leading dot"
      (check (dot-accessor-split ".foo") => #f))

    (test-case "dot-accessor-split: trailing dot"
      (check (dot-accessor-split "foo.") => #f))

    ;; --- tokenize-source ---
    (test-case "tokenize-source: empty string"
      (check-equal? (tokenize-source "") '()))

    (test-case "tokenize-source: simple def"
      (let ((tokens (tokenize-source "(def x 1)")))
        ;; Should have exactly 3 tokens: def (keyword), x (function+def), 1 (number)
        (check (= (length tokens) 3) => #t)
        ;; Verify token types
        (check (cadddr (car tokens)) => SemanticTokenType.keyword)
        ;; x after def gets function+definition context
        (check (cadddr (cadr tokens)) => SemanticTokenType.function)
        (check (car (cddddr (cadr tokens))) => SemanticTokenModifier.definition)
        (check (cadddr (caddr tokens)) => SemanticTokenType.number)))

    (test-case "tokenize-source: defstruct name"
      (let ((tokens (tokenize-source "(defstruct point x y)")))
        ;; defstruct, point (type+def), x (variable), y (variable)
        (check (>= (length tokens) 2) => #t)
        (let ((name-tok (cadr tokens)))
          (check (cadddr name-tok) => SemanticTokenType.type)
          (check (car (cddddr name-tok)) => SemanticTokenModifier.definition))))

    (test-case "tokenize-source: dot accessor"
      (let ((tokens (tokenize-source "obj.method")))
        ;; Should produce two tokens: obj (variable) and method (property)
        (check (= (length tokens) 2) => #t)
        (check (cadddr (car tokens)) => SemanticTokenType.variable)
        (check (cadddr (cadr tokens)) => SemanticTokenType.property)))

    (test-case "tokenize-source: comment"
      (let ((tokens (tokenize-source "; a comment")))
        (check (>= (length tokens) 1) => #t)
        (let ((tok (car tokens)))
          (check (cadddr tok) => SemanticTokenType.comment))))

    (test-case "tokenize-source: string literal"
      (let ((tokens (tokenize-source "\"hello\"")))
        (check (>= (length tokens) 1) => #t)
        (let ((tok (car tokens)))
          (check (cadddr tok) => SemanticTokenType.string))))

    (test-case "tokenize-source: number literal"
      (let ((tokens (tokenize-source "42")))
        (check (>= (length tokens) 1) => #t)
        (let ((tok (car tokens)))
          (check (cadddr tok) => SemanticTokenType.number))))

    (test-case "tokenize-source: multiline"
      (let ((tokens (tokenize-source "(def x 1)\n(def y 2)")))
        ;; Should have 6 tokens: 3 per line
        (check (= (length tokens) 6) => #t)))

    ;; --- encode-semantic-tokens ---
    (test-case "encode-semantic-tokens: empty"
      (let ((result (encode-semantic-tokens '())))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))

    (test-case "encode-semantic-tokens: single token"
      (let ((result (encode-semantic-tokens '((0 5 3 0 0)))))
        (check (vector? result) => #t)
        (check (vector-length result) => 5)
        ;; deltaLine=0, deltaStartChar=5, length=3, type=0, modifiers=0
        (check (vector-ref result 0) => 0)
        (check (vector-ref result 1) => 5)
        (check (vector-ref result 2) => 3)
        (check (vector-ref result 3) => 0)
        (check (vector-ref result 4) => 0)))

    (test-case "encode-semantic-tokens: delta encoding"
      ;; Two tokens: line 0 col 1, line 0 col 5
      (let ((result (encode-semantic-tokens '((0 1 3 0 0) (0 5 3 1 0)))))
        (check (vector-length result) => 10)
        ;; First token: deltaLine=0, deltaCol=1
        (check (vector-ref result 0) => 0)
        (check (vector-ref result 1) => 1)
        ;; Second token: deltaLine=0, deltaCol=4 (5-1=4)
        (check (vector-ref result 5) => 0)
        (check (vector-ref result 6) => 4)))

    (test-case "encode-semantic-tokens: different lines"
      ;; Two tokens: line 0 col 1, line 2 col 3
      (let ((result (encode-semantic-tokens '((0 1 3 0 0) (2 3 3 1 0)))))
        (check (vector-length result) => 10)
        ;; Second token: deltaLine=2, deltaCol=3 (new line, absolute col)
        (check (vector-ref result 5) => 2)
        (check (vector-ref result 6) => 3)))

    ;; --- sort-tokens ---
    (test-case "sort-tokens: already sorted"
      (let ((result (sort-tokens '((0 0 3 0 0) (0 5 3 0 0) (1 0 3 0 0)))))
        (check (car (car result)) => 0)
        (check (cadr (car result)) => 0)))

    (test-case "sort-tokens: reverse order"
      (let ((result (sort-tokens '((1 0 3 0 0) (0 5 3 0 0) (0 0 3 0 0)))))
        (check (car (car result)) => 0)
        (check (cadr (car result)) => 0)
        (check (car (caddr result)) => 1)))

    ;; --- token-delimiter? ---
    (test-case "token-delimiter?: space"
      (check (token-delimiter? #\space) => #t))

    (test-case "token-delimiter?: paren"
      (check (token-delimiter? #\() => #t)
      (check (token-delimiter? #\)) => #t))

    (test-case "token-delimiter?: letter"
      (check (token-delimiter? #\a) => #f))

    ;; --- symbol-start-char? ---
    (test-case "symbol-start-char?: letter"
      (check (symbol-start-char? #\a) => #t))

    (test-case "symbol-start-char?: special"
      (check (not (not (symbol-start-char? #\+))) => #t)
      (check (not (not (symbol-start-char? #\-))) => #t)
      (check (not (not (symbol-start-char? #\!))) => #t))

    (test-case "symbol-start-char?: digit"
      (check (symbol-start-char? #\0) => #f))

    ;; --- handle-semantic-tokens-full: integration ---
    (test-case "handle-semantic-tokens-full: returns encoded tokens"
      (let* ((uri "file:///test-tokens.ss")
             (text "(def x 1)\n(def y 2)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-semantic-tokens-full params)))
          (check (hash-table? result) => #t)
          (let ((data (hash-ref result "data" #f)))
            (check (vector? data) => #t)
            ;; Length must be divisible by 5 (each token = 5 ints)
            (check (= (modulo (vector-length data) 5) 0) => #t)
            (check (> (vector-length data) 0) => #t))
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/semanticTokens/full" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)))

    (test-case "handle-semantic-tokens-full: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))))
             (result (handle-semantic-tokens-full params)))
        ;; Should return a hash with empty data
        (check (hash-table? result) => #t)
        (let ((data (hash-ref result "data" [])))
          (check (or (and (vector? data) (= (vector-length data) 0))
                     (null? data)) => #t))))
  ))

(def main
  (lambda ()
    (run-tests! semantic-tokens-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
