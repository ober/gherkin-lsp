;;; -*- Gerbil -*-
;;; Will-rename handler — workspace/willRenameFiles
;;; Updates import paths when .ss files are renamed
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/module)
(export #t)

;;; Handle workspace/willRenameFiles
;;; Returns a WorkspaceEdit with import path changes, or null
(def (handle-will-rename-files params)
  (let ((files (hash-ref params "files" [])))
    (if (or (not files) (and (vector? files) (= (vector-length files) 0)))
      (void)  ;; null response
      (let ((edits (collect-rename-edits
                     (if (vector? files) (vector->list files) files))))
        (if (null? edits)
          (void)
          (hash ("documentChanges" (list->vector edits))))))))

;;; Collect text document edits for all file renames
(def (collect-rename-edits files)
  (let ((all-edits '()))
    (for-each
      (lambda (file)
        (let* ((old-uri (hash-ref file "oldUri" ""))
               (new-uri (hash-ref file "newUri" ""))
               (old-path (uri->file-path old-uri))
               (new-path (uri->file-path new-uri)))
          ;; Only process .ss files
          (when (and (string-suffix? ".ss" old-path)
                     (string-suffix? ".ss" new-path))
            (let ((file-edits (compute-import-edits old-path new-path)))
              (set! all-edits (append all-edits file-edits))))))
      files)
    all-edits))

;;; Compute text document edits for a single file rename
;;; Scans all open documents and indexed files for imports referencing old-path
(def (compute-import-edits old-path new-path)
  (let ((edits '())
        (old-module (path->module-name old-path))
        (new-module (path->module-name new-path)))
    (when (and old-module new-module)
      ;; Scan open documents
      (for-each
        (lambda (uri)
          (let ((doc (get-document uri)))
            (when doc
              (let ((doc-edits (find-import-edits-in-text
                                 (document-text doc) uri
                                 old-path new-path old-module new-module)))
                (when (pair? doc-edits)
                  (set! edits (cons (make-text-document-edit uri doc-edits)
                                    edits)))))))
        (all-document-uris))
      ;; Scan indexed files not currently open
      (hash-for-each
        (lambda (uri _syms)
          (unless (get-document uri)  ;; skip already-scanned open docs
            (let ((text (get-file-text uri)))
              (when text
                (let ((doc-edits (find-import-edits-in-text
                                   text uri
                                   old-path new-path old-module new-module)))
                  (when (pair? doc-edits)
                    (set! edits (cons (make-text-document-edit uri doc-edits)
                                      edits))))))))
        *symbol-index*))
    edits))

;;; Find import edits needed in a single file's text
(def (find-import-edits-in-text text uri old-path new-path old-module new-module)
  (with-catch
    (lambda (e)
      (lsp-debug "will-rename scan failed for ~a: ~a" uri e)
      '())
    (lambda ()
      (let* ((forms (parse-source text))
             (importing-file-path (uri->file-path uri))
             (edits '()))
        (for-each
          (lambda (lf)
            (let ((form (located-form-form lf)))
              (when (and (pair? form) (eq? (car form) 'import))
                ;; Check each import spec
                (for-each
                  (lambda (spec)
                    (let ((edit (check-import-spec-for-rename
                                  spec lf text importing-file-path
                                  old-path new-path old-module new-module)))
                      (when edit
                        (set! edits (cons edit edits)))))
                  (cdr form)))))
          forms)
        (reverse edits)))))

;;; Check a single import spec to see if it references the renamed file
;;; Returns a TextEdit or #f
(def (check-import-spec-for-rename spec lf text importing-file-path
                                    old-path new-path old-module new-module)
  (let ((spec-str (if (symbol? spec)
                    (symbol->string spec)
                    (if (pair? spec)
                      ;; Handle (only-in :mod ...) etc — check the module part
                      (let ((inner (find-module-in-complex-spec spec)))
                        (if inner (symbol->string inner) #f))
                      #f))))
    (when spec-str
      (cond
        ;; Relative import: ./foo or ../foo
        ((or (string-prefix? "./" spec-str)
             (string-prefix? "../" spec-str))
         (let ((resolved (resolve-relative-import-path spec-str importing-file-path)))
           (when (and resolved (string=? resolved old-path))
             ;; Compute new relative path
             (let ((new-rel (compute-relative-import importing-file-path new-path)))
               (when new-rel
                 (make-spec-replacement-edit lf text spec-str new-rel))))))
        ;; Absolute module import: :pkg/module
        ((string-prefix? ":" spec-str)
         (when (string=? spec-str old-module)
           (make-spec-replacement-edit lf text spec-str new-module)))
        (else #f)))))

;;; Find the module path in a complex import spec
;;; e.g. (only-in ./foo bar) → ./foo
(def (find-module-in-complex-spec spec)
  (if (and (pair? spec)
           (memq (car spec) '(only-in except-in rename-in prefix-in))
           (> (length spec) 1))
    (let ((mod (cadr spec)))
      (if (symbol? mod) mod #f))
    #f))

;;; Resolve a relative import path to an absolute path
(def (resolve-relative-import-path rel-str importing-file-path)
  (let* ((dir (path-directory importing-file-path))
         (candidate (string-append dir "/" rel-str ".ss"))
         (resolved (path-normalize candidate)))
    (if (file-exists? resolved)
      resolved
      (let ((alt (path-normalize (string-append dir "/" rel-str))))
        (if (file-exists? alt) alt #f)))))

;;; Compute a relative import path from source-file to target-file
;;; Returns something like "./foo" or "../bar/baz"
(def (compute-relative-import source-file target-file)
  (let* ((source-dir (path-directory source-file))
         (target-no-ext (strip-ss-extension target-file))
         (rel (compute-relative-path source-dir target-no-ext)))
    (if rel
      (if (string-prefix? "./" rel)
        rel
        (string-append "./" rel))
      #f)))

;;; Strip .ss extension from a path
(def (strip-ss-extension path)
  (if (string-suffix? ".ss" path)
    (substring path 0 (- (string-length path) 3))
    path))

;;; Compute relative path from a directory to a target path
(def (compute-relative-path from-dir to-path)
  (let* ((from-parts (split-path from-dir))
         (to-parts (split-path to-path))
         ;; Find common prefix length
         (common-len (common-prefix-length from-parts to-parts))
         (ups (- (length from-parts) common-len))
         (downs (list-tail to-parts common-len)))
    (let ((prefix (let loop ((n ups) (acc '()))
                    (if (<= n 0)
                      acc
                      (loop (- n 1) (cons ".." acc))))))
      (let ((parts (append prefix downs)))
        (if (null? parts)
          "."
          (string-join "/" parts))))))

;;; Split a path into components
(def (split-path path)
  (let loop ((i 0) (start 0) (parts '()))
    (cond
      ((>= i (string-length path))
       (let ((last-part (substring path start i)))
         (reverse (if (string=? last-part "")
                    parts
                    (cons last-part parts)))))
      ((char=? (string-ref path i) #\/)
       (let ((part (substring path start i)))
         (loop (+ i 1) (+ i 1)
               (if (string=? part "")
                 parts
                 (cons part parts)))))
      (else (loop (+ i 1) start parts)))))

;;; Find common prefix length between two lists of strings
(def (common-prefix-length a b)
  (let loop ((aa a) (bb b) (n 0))
    (if (or (null? aa) (null? bb))
      n
      (if (string=? (car aa) (car bb))
        (loop (cdr aa) (cdr bb) (+ n 1))
        n))))

;;; Join strings with a separator
(def (string-join sep parts)
  (if (null? parts)
    ""
    (let loop ((rest (cdr parts)) (acc (car parts)))
      (if (null? rest)
        acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; Convert a file path to a module name (e.g. :pkg/module)
;;; Returns #f if the path doesn't map to a known module
(def (path->module-name path)
  ;; Try to derive from workspace root
  (let ((root (workspace-root)))
    (if (and root (string-prefix? root path))
      (let* ((rel (substring path (+ (string-length root) 1)
                              (string-length path)))
             (no-ext (strip-ss-extension rel)))
        (string-append ":" no-ext))
      #f)))

;;; Create a TextEdit that replaces an import spec in the source
(def (make-spec-replacement-edit lf text old-spec new-spec)
  ;; Search for the old-spec string within the import form's line range
  (let* ((start-line (located-form-line lf))
         (end-line (located-form-end-line lf)))
    (let line-loop ((line start-line))
      (if (> line end-line)
        #f
        (let ((line-text (text-line-at text line)))
          (let ((pos (string-contains line-text old-spec)))
            (if pos
              (make-text-edit
                (make-lsp-range line pos line (+ pos (string-length old-spec)))
                new-spec)
              (line-loop (+ line 1)))))))))

;;; Create a TextDocumentEdit
(def (make-text-document-edit uri edits)
  (hash ("textDocument" (hash ("uri" uri) ("version" #!void)))
        ("edits" (list->vector edits))))

