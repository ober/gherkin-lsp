;;; -*- Gerbil -*-
;;; Hover handler — show symbol info on hover
(import ../compat/compat
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/hover
(def (handle-hover params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name start-col end-col)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (let ((info (find-symbol-info sym-name uri)))
            (if info
              (make-hover (format-hover-info sym-name info)
                          (make-lsp-range line start-col line end-col))
              ;; No definition found, show just the symbol name
              (make-hover (format "```gerbil\n~a\n```" sym-name)
                          (make-lsp-range line start-col line end-col))))
          (void)))
      (void))))

;;; Find info about a symbol for hover display
(def (find-symbol-info name uri)
  ;; First check local file symbols
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-sym-by-name name local-syms)))
      (if found found
        ;; Check workspace symbols
        (let ((defs (find-definitions-by-name name)))
          (if (pair? defs)
            (cdr (car defs))  ; return the sym-info
            #f))))))

;;; Format hover information as markdown
(def (format-hover-info name info)
  (let ((kind (symbol-kind-name (sym-info-kind info)))
        (detail (sym-info-detail info)))
    (string-append
      "```gerbil\n"
      (if detail
        (format "~a  ; ~a ~a" detail kind name)
        (format "(~a ~a)" kind name))
      "\n```")))

;;; Human-readable name for a SymbolKind
(def (symbol-kind-name kind)
  (cond
    ((= kind SymbolKind.Function) "function")
    ((= kind SymbolKind.Variable) "variable")
    ((= kind SymbolKind.Constant) "constant")
    ((= kind SymbolKind.Struct)   "struct")
    ((= kind SymbolKind.Class)    "class")
    ((= kind SymbolKind.Method)   "method")
    ((= kind SymbolKind.Module)   "module")
    (else "symbol")))
