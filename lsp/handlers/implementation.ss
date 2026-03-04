;;; -*- Gerbil -*-
;;; Go to implementation handler — textDocument/implementation
;;; When cursor is on a method name, finds all defmethod entries
(import ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/module)
(export #t)

;;; Handle textDocument/implementation
;;; Finds all implementations (defmethod entries) matching the symbol at cursor
(def (handle-implementation params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (let-values (((sym start-col end-col) (symbol-at-position text line col)))
          (if sym
            (let ((result '()))
              ;; Search all indexed symbols for methods matching this name
              (hash-for-each
                (lambda (sym-uri syms)
                  (for-each
                    (lambda (s)
                      (when (and (= (sym-info-kind s) SymbolKind.Method)
                                 (string=? (sym-info-name s) sym))
                        (set! result
                          (cons (make-lsp-location
                                  sym-uri
                                  (make-lsp-range
                                    (sym-info-line s) (sym-info-col s)
                                    (sym-info-end-line s) (sym-info-end-col s)))
                                result))))
                    syms))
                *symbol-index*)
              (list->vector result))
            (vector))))
      (vector))))
