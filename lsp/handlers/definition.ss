;;; -*- Gerbil -*-
;;; Go-to-definition handler
(import ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/module
        ../analysis/index
        ../analysis/completion-data)
(export #t)

;;; Handle textDocument/definition
(def (handle-definition params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name _start-col _end-col)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (find-definition-location sym-name uri (document-text doc))
          (void)))
      (void))))

;;; Find the definition location for a symbol
;;; Returns a Location or void if not found
(def (find-definition-location name uri text)
  ;; First search in current file
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-sym-by-name name local-syms)))
      (if found
        (make-lsp-location uri
          (make-lsp-range (sym-info-line found) (sym-info-col found)
                          (sym-info-end-line found) (sym-info-end-col found)))
        ;; Search workspace index
        (let ((defs (find-definitions-by-name name)))
          (if (pair? defs)
            (let* ((first-def (car defs))
                   (def-uri (car first-def))
                   (info (cdr first-def)))
              (make-lsp-location def-uri
                (make-lsp-range (sym-info-line info) (sym-info-col info)
                                (sym-info-end-line info)
                                (sym-info-end-col info))))
            ;; Try resolving through imports
            (let ((import-result (find-definition-in-imports name uri text)))
              (if (not (void? import-result))
                import-result
                ;; Fallback: try stdlib symbols
                (find-definition-in-stdlib name)))))))))

;;; Find a symbol definition in the standard library
;;; Uses *stdlib-symbols* to find the module, then resolves to source
(def (find-definition-in-stdlib name)
  (with-catch
    (lambda (e)
      (lsp-debug "stdlib definition lookup failed: ~a" e)
      (void))
    (lambda ()
      (let ((module (find-stdlib-module-for name)))
        (if module
          (let ((path (resolve-std-module module)))
            (if path
              ;; Try to find exact position via exports analysis
              (let ((exports (analyze-file-exports path)))
                (let ((found (find-sym-by-name name exports)))
                  (if found
                    (make-lsp-location (path->uri path)
                      (make-lsp-range (sym-info-line found) (sym-info-col found)
                                      (sym-info-end-line found) (sym-info-end-col found)))
                    ;; Fall back to beginning of file
                    (make-lsp-location (path->uri path)
                      (make-lsp-range 0 0 0 0)))))
              (void)))
          (void))))))

;;; Look up which stdlib module exports a given symbol name
(def (find-stdlib-module-for name)
  (let loop ((entries *stdlib-symbols*))
    (if (null? entries) #f
      (let ((entry (car entries)))
        (if (string=? name (car entry))
          (cadr entry)
          (loop (cdr entries)))))))

;;; Try to find a symbol definition by resolving the file's imports
;;; Handles rename-in / prefix-in aliases through resolve-aliased-name
(def (find-definition-in-imports name uri text)
  (with-catch
    (lambda (e)
      (lsp-debug "import definition lookup failed: ~a" e)
      (void))
    (lambda ()
      (let* ((file-path (uri->file-path uri))
             (forms (parse-source text))
             (imports (extract-imports forms)))
        (let loop ((specs imports))
          (if (null? specs)
            (void)
            (let* ((spec (car specs))
                   (path (resolve-import-spec spec file-path)))
              (if path
                (let ((exports (analyze-file-exports path)))
                  ;; First try direct name lookup
                  (let ((found (find-sym-by-name name exports)))
                    (if found
                      (make-lsp-location (path->uri path)
                        (make-lsp-range (sym-info-line found)
                                        (sym-info-col found)
                                        (sym-info-end-line found)
                                        (sym-info-end-col found)))
                      ;; Try resolving through rename-in / prefix-in aliases
                      (let* ((original-name (resolve-aliased-name spec name))
                             (alias-found (and (not (string=? original-name name))
                                               (find-sym-by-name original-name exports))))
                        (if alias-found
                          (make-lsp-location (path->uri path)
                            (make-lsp-range (sym-info-line alias-found)
                                            (sym-info-col alias-found)
                                            (sym-info-end-line alias-found)
                                            (sym-info-end-col alias-found)))
                          (loop (cdr specs)))))))
                (loop (cdr specs))))))))))
