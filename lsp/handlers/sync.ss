;;; -*- Gerbil -*-
;;; Document synchronization handlers: didOpen, didChange, didClose, didSave, watchedFiles
(import ../util/log
        ../state
        ../server
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index
        ../analysis/module
        ./diagnostics)
(export #t)

;;; Handle textDocument/didOpen
(def (handle-did-open params)
  (let* ((doc (make-document-from-open params))
         (uri (document-uri doc)))
    (lsp-info "didOpen: ~a" uri)
    (set-document! uri doc)
    ;; Analyze the document
    (analyze-document! uri doc)
    ;; Publish diagnostics
    (publish-diagnostics-for uri)))

;;; Handle textDocument/didChange
(def (handle-did-change params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (version (hash-ref td "version" 0))
         (changes (hash-ref params "contentChanges" '())))
    (let ((doc (get-document uri)))
      (when doc
        ;; Apply each content change sequentially
        (let ((updated
               (let loop ((d doc) (cs (if (vector? changes)
                                        (vector->list changes)
                                        changes)))
                 (if (null? cs) d
                   (let ((change (car cs)))
                     (if (hash-key? change "range")
                       ;; Incremental change — apply range edit
                       (loop (document-apply-incremental-change d change version)
                             (cdr cs))
                       ;; Full change — replace entire text
                       (loop (document-apply-full-change
                               d (hash-ref change "text" (document-text d)) version)
                             (cdr cs))))))))
          (set-document! uri updated)
          ;; Re-analyze symbols
          (analyze-document! uri updated)
          ;; Publish parse-level diagnostics (fast, no gxc)
          (publish-parse-diagnostics uri (document-text updated))
          ;; Schedule debounced gxc diagnostics after quiet period
          (schedule-debounced-diagnostics! uri))))))

;;; Handle textDocument/didClose
(def (handle-did-close params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (lsp-info "didClose: ~a" uri)
    (remove-document! uri)
    (remove-file-symbols! uri)
    ;; Clear diagnostics
    (send-notification! "textDocument/publishDiagnostics"
      (hash ("uri" uri) ("diagnostics" [])))))

;;; Handle textDocument/didSave
(def (handle-did-save params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (lsp-debug "didSave: ~a" uri)
    ;; Update text if included
    (let ((text (hash-ref params "text" #f)))
      (when text
        (let ((doc (get-document uri)))
          (when doc
            (let ((updated (document-apply-full-change doc text
                             (document-version doc))))
              (set-document! uri updated)
              (analyze-document! uri updated))))))
    ;; Cancel any pending debounced diagnostics
    (cancel-debounce-thread!)
    ;; Invalidate module cache for this file
    (invalidate-module-cache-for-uri! uri)
    ;; Run diagnostics on save
    (publish-diagnostics-for uri)))

;;; Handle workspace/didChangeWatchedFiles notification
;;; Responds to file create/change/delete events for .ss files
(def (handle-did-change-watched-files params)
  (let ((changes (hash-ref params "changes" [])))
    (for-each
      (lambda (change)
        (let* ((change-obj (if (vector? change) (vector->list change) change))
               (uri (hash-ref change-obj "uri" ""))
               (type (hash-ref change-obj "type" 0)))
          (lsp-debug "watchedFile ~a type=~a" uri type)
          (cond
            ;; FileChangeType.Created = 1
            ((= type 1)
             (let ((path (uri->file-path uri)))
               (when (and (string? path) (file-exists? path))
                 (with-catch
                   (lambda (e) (lsp-debug "failed to index new file ~a: ~a" path e))
                   (lambda () (index-file-by-path! path))))))
            ;; FileChangeType.Changed = 2
            ((= type 2)
             (let ((path (uri->file-path uri)))
               ;; Re-index the changed file
               (when (and (string? path) (file-exists? path))
                 (with-catch
                   (lambda (e) (lsp-debug "failed to re-index ~a: ~a" path e))
                   (lambda () (index-file-by-path! path))))
               ;; Invalidate module cache
               (invalidate-module-cache-for-uri! uri)))
            ;; FileChangeType.Deleted = 3
            ((= type 3)
             (remove-file-symbols! uri)
             (remove-file-text! uri)
             (clear-gxc-diagnostics! uri)
             (invalidate-module-cache-for-uri! uri)))))
      (if (vector? changes) (vector->list changes) changes))))

;;; Invalidate module cache for a URI
(def (invalidate-module-cache-for-uri! uri)
  (let ((path (uri->file-path uri)))
    (when path
      ;; Clear any module cache entry that resolves to this path
      ;; Since we can't easily reverse-lookup, clear the whole cache
      ;; for the file's module path
      (clear-module-cache-for! path))))

;;; Analyze a document: parse and extract symbols
(def (analyze-document! uri doc)
  (let* ((text (document-text doc))
         (forms (parse-source text))
         (syms (extract-symbols forms)))
    (set-file-symbols! uri syms)
    (lsp-debug "analyzed ~a: ~a symbols" uri (length syms))))

;;; Get the last element of a list (returns #f for empty list)
(def (last lst)
  (cond
    ((null? lst) #f)
    ((null? (cdr lst)) (car lst))
    (else (last (cdr lst)))))
