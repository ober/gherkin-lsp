;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/symbols
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/symbols)

(export symbols-test-suite)

(def symbols-test-suite
  (test-suite "lsp/analysis/symbols"

    ;; --- extract-symbols: def variable ---
    (test-case "extract-symbols: def variable"
      (let* ((forms (parse-source "(def x 42)"))
             (syms (extract-symbols forms)))
        (check (>= (length syms) 1) => #t)
        (let ((s (car syms)))
          (check-equal? (sym-info-name s) "x")
          (check (sym-info-kind s) => SymbolKind.Variable))))

    ;; --- extract-symbols: def function ---
    (test-case "extract-symbols: def function"
      (let* ((forms (parse-source "(def (add a b) (+ a b))"))
             (syms (extract-symbols forms)))
        (check (>= (length syms) 1) => #t)
        (let ((s (find-sym-by-name "add" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Function))))

    ;; --- extract-symbols: defstruct ---
    (test-case "extract-symbols: defstruct"
      (let* ((forms (parse-source "(defstruct point (x y) transparent: #t)"))
             (syms (extract-symbols forms)))
        (check (>= (length syms) 1) => #t)
        (let ((s (find-sym-by-name "point" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Struct))))

    ;; --- extract-symbols: defstruct with super ---
    (test-case "extract-symbols: defstruct with super"
      (let* ((forms (parse-source "(defstruct (point3d point) (z))"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "point3d" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Struct))))

    ;; --- extract-symbols: defclass ---
    (test-case "extract-symbols: defclass"
      (let* ((forms (parse-source "(defclass widget (name))"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "widget" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Class))))

    ;; --- extract-symbols: defmethod ---
    (test-case "extract-symbols: defmethod"
      (let* ((forms (parse-source "(defmethod (draw w) (display w))"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "draw" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Method))))

    ;; --- extract-symbols: defrule with symbol name ---
    (test-case "extract-symbols: defrule"
      ;; defrule with symbol as second element (not pattern form)
      (let* ((forms (parse-source "(defrule my-mac (syntax-rules () ((_ x) x)))"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "my-mac" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Function))))

    ;; --- extract-symbols: defrule with pattern form returns nothing ---
    (test-case "extract-symbols: defrule pattern form"
      ;; Pattern-style defrule doesn't extract (cadr is a list, not symbol)
      (let* ((forms (parse-source "(defrule (my-when test body ...) (if test (begin body ...) (void)))"))
             (syms (extract-symbols forms)))
        (check (find-sym-by-name "my-when" syms) => #f)))

    ;; --- extract-symbols: defsyntax ---
    (test-case "extract-symbols: defsyntax"
      (let* ((forms (parse-source "(defsyntax my-mac (syntax-rules () ((_ x) x)))"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "my-mac" syms)))
          (check (not (eq? s #f)) => #t))))

    ;; --- extract-symbols: defvalues ---
    (test-case "extract-symbols: defvalues"
      (let* ((forms (parse-source "(defvalues (a b c) (values 1 2 3))"))
             (syms (extract-symbols forms)))
        (check (not (eq? (find-sym-by-name "a" syms) #f)) => #t)
        (check (not (eq? (find-sym-by-name "b" syms) #f)) => #t)
        (check (not (eq? (find-sym-by-name "c" syms) #f)) => #t)))

    ;; --- extract-symbols: defconst ---
    (test-case "extract-symbols: defconst"
      (let* ((forms (parse-source "(defconst PI 3.14)"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "PI" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Constant))))

    ;; --- extract-symbols: deferror-class ---
    (test-case "extract-symbols: deferror-class"
      (let* ((forms (parse-source "(deferror-class MyError () message: \"err\")"))
             (syms (extract-symbols forms)))
        (let ((s (find-sym-by-name "MyError" syms)))
          (check (not (eq? s #f)) => #t)
          (check (sym-info-kind s) => SymbolKind.Class))))

    ;; --- extract-symbols: empty ---
    (test-case "extract-symbols: empty input"
      (check (length (extract-symbols '())) => 0))

    ;; --- extract-symbols: non-def forms ---
    (test-case "extract-symbols: non-def forms ignored"
      (let* ((forms (parse-source "(if x y z)\n(display \"hi\")"))
             (syms (extract-symbols forms)))
        (check (length syms) => 0)))

    ;; --- extract-imports ---
    (test-case "extract-imports: import form"
      (let* ((forms (parse-source "(import :std/text/json :std/sugar)"))
             (imports (extract-imports forms)))
        (check (length imports) => 2)
        (check-equal? (car imports) ':std/text/json)))

    (test-case "extract-imports: no imports"
      (let* ((forms (parse-source "(def x 1)"))
             (imports (extract-imports forms)))
        (check (length imports) => 0)))

    ;; --- extract-exports ---
    (test-case "extract-exports: export #t"
      (let* ((forms (parse-source "(export #t)"))
             (exports (extract-exports forms)))
        (check (member #t exports) => '(#t))))

    (test-case "extract-exports: export names"
      (let* ((forms (parse-source "(export foo bar)"))
             (exports (extract-exports forms)))
        (check (length exports) => 2)
        (check (and (member 'foo exports) #t) => #t)
        (check (and (member 'bar exports) #t) => #t)))

    (test-case "extract-exports: empty"
      (let* ((forms (parse-source "(def x 1)"))
             (exports (extract-exports forms)))
        (check (length exports) => 0)))

    ;; --- find-sym-by-name ---
    (test-case "find-sym-by-name: found"
      (let ((syms (list (make-sym-info "foo" SymbolKind.Function 0 0 0 0 #f)
                        (make-sym-info "bar" SymbolKind.Variable 0 0 0 0 #f))))
        (let ((s (find-sym-by-name "bar" syms)))
          (check (not (eq? s #f)) => #t)
          (check-equal? (sym-info-name s) "bar"))))

    (test-case "find-sym-by-name: not found"
      (let ((syms (list (make-sym-info "foo" SymbolKind.Function 0 0 0 0 #f))))
        (check (find-sym-by-name "xyz" syms) => #f)))

    (test-case "find-sym-by-name: empty list"
      (check (find-sym-by-name "foo" '()) => #f))

    ;; --- format-def-signature ---
    (test-case "format-def-signature: function sig"
      (check-equal? (format-def-signature '(foo a b)) "(foo a b)"))

    (test-case "format-def-signature: no args"
      (check-equal? (format-def-signature '(foo)) "(foo)"))

    (test-case "format-def-signature: rest args"
      (let ((sig (format-def-signature '(foo . rest))))
        (check-equal? sig "(foo . rest)")))

    (test-case "format-def-signature: non-pair"
      (check (format-def-signature 'x) => #f))

    ;; --- extract-param-names ---
    (test-case "extract-param-names: simple list"
      (check-equal? (extract-param-names '(a b c)) '(a b c)))

    (test-case "extract-param-names: rest param"
      (check-equal? (extract-param-names 'rest) '(rest)))

    (test-case "extract-param-names: empty"
      (check-equal? (extract-param-names '()) '()))

    ;; --- extract-local-bindings ---
    (test-case "extract-local-bindings: let"
      (let ((bindings (extract-local-bindings
                        '((let ((x 1) (y 2)) (+ x y))))))
        (check (and (member 'x bindings) #t) => #t)
        (check (and (member 'y bindings) #t) => #t)))

    (test-case "extract-local-bindings: named let"
      (let ((bindings (extract-local-bindings
                        '((let loop ((i 0)) (loop (+ i 1)))))))
        (check (and (member 'loop bindings) #t) => #t)
        (check (and (member 'i bindings) #t) => #t)))

    (test-case "extract-local-bindings: nested let"
      (let ((bindings (extract-local-bindings
                        '((let ((x 1))
                            (let ((y 2))
                              (+ x y)))))))
        (check (and (member 'x bindings) #t) => #t)
        (check (and (member 'y bindings) #t) => #t)))

    (test-case "extract-local-bindings: empty body"
      (check-equal? (extract-local-bindings '()) '()))

    ;; --- handle-document-symbol: integration ---
    (test-case "handle-document-symbol: returns symbols for open document"
      (let* ((uri "file:///test-dsym.ss")
             (text "(def (add a b) (+ a b))\n(def x 42)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))))
               (result (handle-document-symbol params)))
          (check (vector? result) => #t)
          (check (>= (vector-length result) 2) => #t)
          ;; Each symbol should have name and kind
          (let ((first-sym (vector-ref result 0)))
            (check (string? (hash-ref first-sym "name")) => #t)
            (check (integer? (hash-ref first-sym "kind")) => #t))
          ;; Validate against LSP schema
          (let ((violations (validate-response "textDocument/documentSymbol" result)))
            (check (null? violations) => #t)))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    ;; --- handle-workspace-symbol: integration ---
    (test-case "handle-workspace-symbol: filters by query"
      (let* ((uri "file:///test-wsym.ss")
             (syms (list (make-sym-info "add" SymbolKind.Function 0 0 0 10
                                         "(add a b)")
                         (make-sym-info "x" SymbolKind.Variable 1 0 1 5 #f))))
        (set-file-symbols! uri syms)
        (let* ((params (hash ("query" "ad")))
               (result (handle-workspace-symbol params)))
          (check (vector? result) => #t)
          ;; Should find "add" matching "ad" query
          (check (>= (vector-length result) 1) => #t)
          ;; Validate against LSP schema
          (let ((violations (validate-response "workspace/symbol" result)))
            (check (null? violations) => #t)))
        (remove-file-symbols! uri)))
  ))

(def main
  (lambda ()
    (run-tests! symbols-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
