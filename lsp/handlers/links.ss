;;; -*- Gerbil -*-
;;; Document link handler — textDocument/documentLink
(import ../util/log
        ../util/position
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/module)
(export #t)

;;; Handle textDocument/documentLink
;;; Parses import forms and resolves each spec to a file path
(def (handle-document-link params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (file-path (uri->file-path uri))
             (forms (parse-source text))
             (links (collect-document-links forms file-path)))
        (list->vector links))
      [])))

;;; Collect document links from import forms
(def (collect-document-links forms file-path)
  (let ((result '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (let ((line (located-form-line lf)))
              ;; Each import spec is on its own line typically
              ;; We process each import spec in the form
              (let loop ((specs (cdr form)) (spec-line (+ line 1)))
                (unless (null? specs)
                  (let ((spec (car specs)))
                    (let ((link (make-import-link spec file-path spec-line)))
                      (when link
                        (set! result (cons link result))))
                    (loop (cdr specs) (+ spec-line 1)))))))))
      forms)
    (reverse result)))

;;; Create a DocumentLink for an import spec if it resolves to a file
(def (make-import-link spec file-path line)
  (let ((resolved (resolve-import-spec spec file-path)))
    (if resolved
      (let* ((spec-str (if (symbol? spec) (symbol->string spec)
                         (if (pair? spec) (format-import-spec spec) "")))
             (col 8) ; approximate column after "        " indentation
             (end-col (+ col (string-length spec-str))))
        (hash ("range" (make-lsp-range line col line end-col))
              ("target" (path->uri resolved))))
      #f)))

;;; Format an import spec to string for range calculation
(def (format-import-spec spec)
  (cond
    ((symbol? spec) (symbol->string spec))
    ((pair? spec)
     (if (and (pair? spec) (symbol? (car spec)))
       (symbol->string (car spec))
       ""))
    (else "")))
