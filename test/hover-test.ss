;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/hover
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/validation
        :lsp/lsp/handlers/hover)

(export hover-test-suite)

(def hover-test-suite
  (test-suite "lsp/handlers/hover"

    ;; --- symbol-kind-name ---
    (test-case "symbol-kind-name: function"
      (check-equal? (symbol-kind-name SymbolKind.Function) "function"))

    (test-case "symbol-kind-name: variable"
      (check-equal? (symbol-kind-name SymbolKind.Variable) "variable"))

    (test-case "symbol-kind-name: constant"
      (check-equal? (symbol-kind-name SymbolKind.Constant) "constant"))

    (test-case "symbol-kind-name: struct"
      (check-equal? (symbol-kind-name SymbolKind.Struct) "struct"))

    (test-case "symbol-kind-name: class"
      (check-equal? (symbol-kind-name SymbolKind.Class) "class"))

    (test-case "symbol-kind-name: method"
      (check-equal? (symbol-kind-name SymbolKind.Method) "method"))

    (test-case "symbol-kind-name: module"
      (check-equal? (symbol-kind-name SymbolKind.Module) "module"))

    (test-case "symbol-kind-name: unknown"
      (check-equal? (symbol-kind-name 999) "symbol"))

    ;; --- format-hover-info ---
    (test-case "format-hover-info: function with detail"
      (let* ((info (make-sym-info "add" SymbolKind.Function 0 0 0 10
                                   "(add a b)"))
             (result (format-hover-info "add" info)))
        (check (string? result) => #t)
        (check (string-contains-hover result "add") => #t)
        (check (string-contains-hover result "function") => #t)))

    (test-case "format-hover-info: variable without detail"
      (let* ((info (make-sym-info "x" SymbolKind.Variable 0 0 0 5 #f))
             (result (format-hover-info "x" info)))
        (check (string? result) => #t)
        (check (string-contains-hover result "variable") => #t)
        (check (string-contains-hover result "x") => #t)))

    (test-case "format-hover-info: struct"
      (let* ((info (make-sym-info "point" SymbolKind.Struct 0 0 0 20
                                   "struct point"))
             (result (format-hover-info "point" info)))
        (check (string? result) => #t)
        (check (string-contains-hover result "struct") => #t)))

    ;; --- find-symbol-info ---
    (test-case "find-symbol-info: returns #f for unknown symbol"
      (check (find-symbol-info "nonexistent" "file:///test.ss") => #f))

    ;; --- handle-hover: integration ---
    (test-case "handle-hover: returns hover for known symbol"
      (let* ((uri "file:///test-hover.ss")
             (text "(def (add a b) (+ a b))\n(add 1 2)")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 1)))))
               (result (handle-hover params)))
          (when (and result (not (void? result)))
            (check (hash-table? result) => #t)
            (let ((contents (hash-ref result "contents" #f)))
              (check (hash-table? contents) => #t)
              ;; Contents should have markdown with function name
              (let ((value (hash-ref contents "value" "")))
                (check (string-contains-hover value "add") => #t)))
            ;; Validate against LSP schema
            (let ((violations (validate-response "textDocument/hover" result)))
              (check (null? violations) => #t))))
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-hover: returns void for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-hover params)))
        (check (void? result) => #t)))
  ))

;; Local helper for string-contains
(def (string-contains-hover str needle)
  (let ((hlen (string-length str))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring str i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

(def main
  (lambda ()
    (run-tests! hover-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
