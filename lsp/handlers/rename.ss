;;; -*- Gerbil -*-
;;; Rename handler — rename symbols across the workspace
;;; Supports scope-aware rename (local variables stay within their form)
;;; and skips matches inside strings and comments.
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols)
(export #t)

;;; Handle textDocument/prepareRename
;;; Returns the range of the symbol that will be renamed, or error
(def (handle-prepare-rename params)
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
          (hash ("range" (make-lsp-range line start-col line end-col))
                ("placeholder" sym-name))
          (void)))
      (void))))

;;; Handle textDocument/rename
;;; Returns a WorkspaceEdit with text edits across documents
(def (handle-rename params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (new-name (hash-ref params "newName" ""))
         (doc (get-document uri)))
    (if (and doc (> (string-length new-name) 0))
      (let-values (((sym-name _start _end)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (let ((scope (determine-rename-scope uri sym-name line)))
            (if (eq? scope 'local)
              ;; Local rename: restrict to enclosing form
              (let ((edits (compute-local-rename-edits
                             uri (document-text doc) sym-name new-name line)))
                (make-workspace-edit edits))
              ;; Global rename: workspace-wide
              (let ((edits (compute-rename-edits sym-name new-name)))
                (make-workspace-edit edits))))
          (make-workspace-edit (hash))))
      (make-workspace-edit (hash)))))

;;; Determine whether a symbol should be renamed locally or globally
;;; Returns 'local for parameters and local bindings, 'global otherwise
(def (determine-rename-scope uri sym-name line)
  (let ((syms (get-file-symbols uri)))
    (let ((found (find-sym-by-name sym-name syms)))
      (if found
        (let ((detail (or (sym-info-detail found) "")))
          (if (or (string=? detail "parameter")
                  (string=? detail "local"))
            'local
            'global))
        'global))))

;;; Compute rename edits restricted to the enclosing form (local scope)
(def (compute-local-rename-edits uri text old-name new-name cursor-line)
  (let ((changes (make-hash-table)))
    (with-catch
      (lambda (e)
        (lsp-debug "local rename failed: ~a" e)
        changes)
      (lambda ()
        (let* ((forms (parse-source text))
               (enclosing (form-at-line forms cursor-line)))
          (if enclosing
            (let* ((start-line (located-form-line enclosing))
                   (end-line (located-form-end-line enclosing))
                   (edits (find-rename-edits-in-range
                            text old-name new-name start-line end-line)))
              (when (pair? edits)
                (hash-put! changes uri (list->vector edits)))
              changes)
            ;; Fallback: treat as single-file global
            (let ((edits (find-rename-edits-in-text text old-name new-name)))
              (when (pair? edits)
                (hash-put! changes uri (list->vector edits)))
              changes)))))))

;;; Compute rename edits across the entire workspace
;;; Searches open documents and indexed files
;;; Returns a hash of uri → TextEdit[]
(def (compute-rename-edits old-name new-name)
  (let ((changes (make-hash-table))
        (searched (make-hash-table)))
    ;; Search open documents first (they have the latest text)
    (for-each
      (lambda (uri)
        (hash-put! searched uri #t)
        (let ((doc (get-document uri)))
          (when doc
            (let ((edits (find-rename-edits-in-text
                           (document-text doc) old-name new-name)))
              (when (pair? edits)
                (hash-put! changes uri (list->vector edits)))))))
      (all-document-uris))
    ;; Search indexed files that aren't open
    (for-each
      (lambda (uri)
        (unless (hash-key? searched uri)
          (let ((text (get-file-text uri)))
            (when text
              (let ((edits (find-rename-edits-in-text text old-name new-name)))
                (when (pair? edits)
                  (hash-put! changes uri (list->vector edits))))))))
      (all-indexed-uris))
    changes))

;;; Find all positions to rename in a text, skipping strings and comments
(def (find-rename-edits-in-text text old-name new-name)
  (let ((regions (classify-text-regions text))
        (old-len (string-length old-name))
        (edits '()))
    (let line-loop ((i 0) (line-num 0) (line-start 0))
      (cond
        ((>= i (string-length text))
         ;; Process last line
         (append edits
           (find-edits-in-line text line-start i line-num
                               old-name old-len new-name regions)))
        ((char=? (string-ref text i) #\newline)
         (set! edits
           (append edits
             (find-edits-in-line text line-start i line-num
                                 old-name old-len new-name regions)))
         (line-loop (+ i 1) (+ line-num 1) (+ i 1)))
        (else
         (line-loop (+ i 1) line-num line-start))))))

;;; Find rename edits within a specific line range, skipping strings/comments
(def (find-rename-edits-in-range text old-name new-name start-line end-line)
  (let ((regions (classify-text-regions text))
        (old-len (string-length old-name))
        (edits '()))
    (let line-loop ((i 0) (line-num 0) (line-start 0))
      (cond
        ((>= i (string-length text))
         ;; Process last line if in range
         (if (and (>= line-num start-line) (<= line-num end-line))
           (append edits
             (find-edits-in-line text line-start i line-num
                                 old-name old-len new-name regions))
           edits))
        ((char=? (string-ref text i) #\newline)
         (when (and (>= line-num start-line) (<= line-num end-line))
           (set! edits
             (append edits
               (find-edits-in-line text line-start i line-num
                                   old-name old-len new-name regions))))
         (line-loop (+ i 1) (+ line-num 1) (+ i 1)))
        (else
         (line-loop (+ i 1) line-num line-start))))))

;;; Find rename edits in a single line, skipping matches in strings/comments
(def (find-edits-in-line text line-start line-end line-num
                          old-name old-len new-name regions)
  (let ((line-text (substring text line-start line-end))
        (edits '()))
    (let loop ((col 0))
      (if (> (+ col old-len) (string-length line-text))
        (reverse edits)
        (begin
          (when (and (string=? old-name
                       (substring line-text col (+ col old-len)))
                     ;; Word boundary check
                     (or (= col 0)
                         (not (symbol-char? (string-ref line-text (- col 1)))))
                     (or (= (+ col old-len) (string-length line-text))
                         (not (symbol-char?
                                (string-ref line-text (+ col old-len)))))
                     ;; Skip if in string or comment
                     (not (in-string-or-comment? regions (+ line-start col))))
            (set! edits
              (cons (make-text-edit
                      (make-lsp-range line-num col line-num (+ col old-len))
                      new-name)
                    edits)))
          (loop (+ col 1)))))))
