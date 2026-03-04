;;; -*- Gerbil -*-
;;; Workspace-wide symbol index
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../state
        ../server
        ./document
        ./parser
        ./symbols
        ./module
        ./cache)
(export #t)

;;; Index all .ss files in the workspace
;;; First tries to load from cache, then indexes remaining files
(def (index-workspace! root)
  (when root
    (lsp-info "indexing workspace: ~a" root)
    ;; Try to load from cache first
    (let ((cached-count (load-index-cache! root)))
      (load-deps-cache! root)
      (let ((files (find-gerbil-files root))
            (indexed 0))
        (lsp-info "found ~a Gerbil files, ~a from cache" (length files) cached-count)
        (for-each
          (lambda (path)
            (let ((uri (path->uri path)))
              ;; Only index files not in cache
              (when (null? (get-file-symbols uri))
                (with-catch
                  (lambda (e)
                    (lsp-debug "failed to index ~a: ~a" path e))
                  (lambda ()
                    (index-file-by-path! path)
                    (set! indexed (+ indexed 1)))))))
          files)
        (lsp-info "indexed ~a new files" indexed)
        ;; Save updated cache
        (save-index-cache! root)
        (save-deps-cache! root)))))

;;; Index workspace with progress reporting
;;; Index workspace with progress reporting
;;; Uses cache for unchanged files
(def (index-workspace-with-progress! root progress-token)
  (when root
    (lsp-info "indexing workspace: ~a" root)
    ;; Load cache first
    (let ((cached-count (load-index-cache! root)))
      (load-deps-cache! root)
      (let ((files (find-gerbil-files root)))
        (let ((total (length files))
              (done 0)
              (indexed 0))
          (lsp-info "found ~a Gerbil files, ~a from cache" total cached-count)
          (for-each
            (lambda (path)
              (let ((uri (path->uri path)))
                ;; Only index files not in cache
                (if (null? (get-file-symbols uri))
                  (with-catch
                    (lambda (e)
                      (lsp-debug "failed to index ~a: ~a" path e))
                    (lambda ()
                      (index-file-by-path! path)
                      (set! indexed (+ indexed 1))))
                  (void)))
              (set! done (+ done 1))
              (when (> total 0)
                (let ((pct (min 100 (quotient (* done 100) total))))
                  (send-progress! progress-token "report"
                    message: (format "~a/~a files (~a indexed)" done total indexed)
                    percentage: pct))))
            files)
          (lsp-info "indexed ~a new files" indexed)
          ;; Save updated cache
          (save-index-cache! root)
          (save-deps-cache! root))))))

;;; Index a single file by its filesystem path
;;; Extracts symbols and tracks import dependencies
(def (index-file-by-path! path)
  (let* ((uri (path->uri path))
         (text (read-file-string path))
         (forms (parse-source text))
         (syms (extract-symbols forms))
         (imports (extract-imports forms)))
    (set-file-symbols! uri syms)
    ;; Cache text for workspace-wide references/rename
    (set-file-text! uri text)
    ;; Track import dependencies
    (let ((import-uris (resolve-import-uris imports path)))
      (set-file-imports! uri import-uris))
    (lsp-debug "indexed ~a: ~a symbols" path (length syms))))

;;; Resolve import specs to URIs for dependency tracking
(def (resolve-import-uris import-specs file-path)
  (let ((result '()))
    (for-each
      (lambda (spec)
        (with-catch
          (lambda (e) (void))
          (lambda ()
            (let ((resolved (resolve-import-spec spec file-path)))
              (when resolved
                (set! result (cons (path->uri resolved) result)))))))
      import-specs)
    result))

;;; Find all .ss files under a directory recursively
(def (find-gerbil-files root)
  (let ((result '()))
    (scan-directory root
      (lambda (path)
        (when (and (string-suffix? ".ss" path)
                   (not (string-contains path "/.gerbil/"))
                   (not (string-contains path "/build-deps/")))
          (set! result (cons path result)))))
    (reverse result)))

;;; Recursively scan a directory, calling proc on each file path
(def (scan-directory dir proc)
  (with-catch
    (lambda (e)
      (lsp-debug "scan-directory error ~a: ~a" dir e))
    (lambda ()
      (let ((entries (directory-files dir)))
        (for-each
          (lambda (entry)
            (let ((full-path (string-append dir "/" entry)))
              (if (file-directory? full-path)
                (unless (string-prefix? "." entry)
                  (scan-directory full-path proc))
                (proc full-path))))
          entries)))))

;;; Check if a path is a directory
(def (file-directory? path)
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (eq? (file-info-type (file-info path)) 'directory))))

;;; Find all definitions matching a name across the workspace
(def (find-definitions-by-name name)
  (let ((result '()))
    (hash-for-each
      (lambda (uri syms)
        (for-each
          (lambda (s)
            (when (string=? (sym-info-name s) name)
              (set! result (cons (cons uri s) result))))
          syms))
      *symbol-index*)
    result))

;;; Find all references to a symbol name across indexed files
;;; This does text-based search with word boundary detection
;;; Searches open documents and the file text cache
(def (find-references-by-name name)
  (let ((result '())
        (searched (make-hash-table)))
    ;; Search open documents first (latest text)
    (for-each
      (lambda (uri)
        (hash-put! searched uri #t)
        (let ((doc (get-document uri)))
          (when doc
            (search-text-for-refs (document-text doc) name uri
              (lambda (entry) (set! result (cons entry result)))))))
      (all-document-uris))
    ;; Search indexed files not currently open
    (hash-for-each
      (lambda (uri _syms)
        (unless (hash-key? searched uri)
          (let ((text (get-file-text uri)))
            (when text
              (search-text-for-refs text name uri
                (lambda (entry) (set! result (cons entry result))))))))
      *symbol-index*)
    result))

;;; Search text for all references to a name, calling callback with (uri line col end-col)
(def (search-text-for-refs text name uri callback)
  (let ((name-len (string-length name))
        (lines (string-split-lines text)))
    (let line-loop ((ls lines) (line-num 0))
      (unless (null? ls)
        (let ((line-text (car ls)))
          (find-in-line line-text name name-len uri line-num
            (lambda (col)
              (callback (list uri line-num col (+ col name-len)))))
          (line-loop (cdr ls) (+ line-num 1)))))))

;;; Find occurrences of name in a line with word boundary checking
(def (find-in-line text name name-len uri line-num callback)
  (let ((text-len (string-length text)))
    (let loop ((i 0))
      (when (< (+ i name-len) (+ text-len 1))
        (when (and (string=? name (substring text i (+ i name-len)))
                   (or (= i 0) (not (symbol-char? (string-ref text (- i 1)))))
                   (or (= (+ i name-len) text-len)
                       (not (symbol-char? (string-ref text (+ i name-len))))))
          (callback i))
        (loop (+ i 1))))))

