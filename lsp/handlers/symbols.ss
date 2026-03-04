;;; -*- Gerbil -*-
;;; Document symbol and workspace symbol handlers
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/documentSymbol
;;; Returns DocumentSymbol[] (hierarchical)
(def (handle-document-symbol params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (let ((syms (get-file-symbols uri)))
      (list->vector
        (map sym-info->document-symbol syms)))))

;;; Handle workspace/symbol
;;; Returns SymbolInformation[] (flat, filtered by query)
(def (handle-workspace-symbol params)
  (let ((query (hash-ref params "query" "")))
    (let ((result '()))
      (hash-for-each
        (lambda (uri syms)
          (for-each
            (lambda (s)
              (when (or (string=? query "")
                        (string-contains-ci (sym-info-name s) query))
                (set! result
                  (cons (sym-info->symbol-information s uri) result))))
            syms))
        *symbol-index*)
      (list->vector (take-at-most result 100)))))

;;; Convert sym-info to DocumentSymbol
(def (sym-info->document-symbol s)
  (let ((range (make-lsp-range (sym-info-line s) (sym-info-col s)
                               (sym-info-end-line s) (sym-info-end-col s)))
        (sel-range (make-lsp-range (sym-info-line s) (sym-info-col s)
                                    (sym-info-line s)
                                    (+ (sym-info-col s)
                                       (string-length (sym-info-name s))))))
    (make-document-symbol (sym-info-name s) (sym-info-kind s)
                          range sel-range)))

;;; Convert sym-info to SymbolInformation (for workspace/symbol)
(def (sym-info->symbol-information s uri)
  (make-symbol-information
    (sym-info-name s)
    (sym-info-kind s)
    (make-lsp-location uri
      (make-lsp-range (sym-info-line s) (sym-info-col s)
                      (sym-info-end-line s) (sym-info-end-col s)))))

