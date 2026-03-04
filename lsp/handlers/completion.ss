;;; -*- Gerbil -*-
;;; Completion handler
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index
        ../analysis/completion-data
        ../analysis/parser
        ../analysis/module)
(export #t)

;;; Handle textDocument/completion
(def (handle-completion params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    ;; Cache URI for completionItem/resolve (which doesn't receive it)
    (set-last-completion-uri! uri)
    (if doc
      (let ((items (completion-candidates uri (document-text doc) line col)))
        (hash ("isIncomplete" #f)
              ("items" (list->vector items))))
      (hash ("isIncomplete" #f)
            ("items" (vector))))))

;;; Handle completionItem/resolve
;;; Enrich a completion item with documentation and auto-import
(def (handle-completion-resolve params)
  (let* ((label (hash-ref params "label" ""))
         (data (hash-ref params "data" #f))
         (result (hash-copy params)))
    ;; Add documentation if we can find the symbol
    (when data
      (let ((module-name (hash-ref data "module" #f))
            (sym-name (hash-ref data "name" label)))
        ;; Try to find documentation from indexed symbols
        (let ((defs (find-definitions-by-name sym-name)))
          (when (pair? defs)
            (let* ((def-entry (car defs))
                   (sym (cdr def-entry)))
              (let ((detail (sym-info-detail sym)))
                (when detail
                  (hash-put! result "documentation"
                    (hash ("kind" MarkupKind.Markdown)
                          ("value" (format "```scheme\n~a\n```\n\nDefined in `~a`"
                                     detail (car def-entry))))))))))
        ;; Auto-import: if this symbol comes from a known module and
        ;; that module isn't already imported, add an additionalTextEdit
        (when module-name
          (let ((uri (last-completion-uri)))
            (when uri
              (let ((doc (get-document uri)))
                (when doc
                  (let ((text (document-text doc)))
                    (unless (module-already-imported? text module-name)
                      (let ((insert-line (find-auto-import-position text)))
                        (hash-put! result "additionalTextEdits"
                          (vector
                            (make-text-edit
                              (make-lsp-range insert-line 0 insert-line 0)
                              (compute-import-text module-name))))))))))))))
    result))

;;; Check if a module is already imported in the document text
(def (module-already-imported? text module-name)
  (string-contains text module-name))

;;; Find the line position to insert an auto-import
;;; Inserts after the last existing import form, or at line 0
(def (find-auto-import-position text)
  (with-catch
    (lambda (e) 0)
    (lambda ()
      (let* ((forms (parse-source text))
             (last-import-end 0))
        (for-each
          (lambda (lf)
            (let ((form (located-form-form lf)))
              (when (and (pair? form) (eq? (car form) 'import))
                (let ((end (+ (located-form-end-line lf) 1)))
                  (when (> end last-import-end)
                    (set! last-import-end end))))))
          forms)
        last-import-end))))

;;; Compute the text to insert for an auto-import
(def (compute-import-text module-name)
  (string-append "(import " module-name ")\n"))
