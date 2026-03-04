;;; -*- Gerbil -*-
;;; Persistent symbol index cache
;;; Stores symbol index to disk for fast startup and incremental updates
(import :std/text/json
        ../compat/compat
        ../util/log
        ../state
        ./symbols)
(export #t)

;;; Cache file version - increment when format changes
(def *cache-version* 1)

;;; Get the cache directory for a workspace
(def (cache-dir root)
  (path-expand ".gerbil-lsp-cache" root))

;;; Get the index cache file path
(def (index-cache-path root)
  (path-expand "index.json" (cache-dir root)))

;;; Get the dependency graph cache file path
(def (deps-cache-path root)
  (path-expand "deps.json" (cache-dir root)))

;;; Ensure cache directory exists
(def (ensure-cache-dir! root)
  (let ((dir (cache-dir root)))
    (unless (file-exists? dir)
      (with-catch
        (lambda (e) (lsp-debug "failed to create cache dir: ~a" e))
        (lambda () (create-directory* dir))))))

;;; Save the symbol index to disk
(def (save-index-cache! root)
  (with-catch
    (lambda (e) (lsp-warn "failed to save index cache: ~a" e))
    (lambda ()
      (ensure-cache-dir! root)
      (let ((cache-data (make-hash-table))
            (files-data (make-hash-table)))
        ;; Collect symbol data with file modification times
        (hash-for-each
          (lambda (uri syms)
            (let ((path (uri->file-path* uri)))
              (when (and path (file-exists? path))
                (hash-put! files-data uri
                  (hash ("mtime" (file-mtime path))
                        ("symbols" (map sym-info->hash syms)))))))
          *symbol-index*)
        (hash-put! cache-data "version" *cache-version*)
        (hash-put! cache-data "files" files-data)
        (hash-put! cache-data "timestamp" (current-second))
        ;; Write atomically via temp file
        (let ((cache-path (index-cache-path root))
              (temp-path (string-append (index-cache-path root) ".tmp")))
          (call-with-output-file temp-path
            (lambda (port)
              (write-json cache-data port)))
          (rename-file temp-path cache-path)
          (lsp-debug "saved index cache with ~a files" (hash-length files-data)))))))

;;; Load the symbol index from disk
;;; Returns the number of files loaded, or 0 if cache is invalid/missing
(def (load-index-cache! root)
  (with-catch
    (lambda (e)
      (lsp-debug "failed to load index cache: ~a" e)
      0)
    (lambda ()
      (let ((cache-path (index-cache-path root)))
        (if (file-exists? cache-path)
          (let ((cache-data (call-with-input-file cache-path read-json)))
            ;; Check version
            (if (and (hash-table? cache-data)
                     (= (hash-ref cache-data "version" 0) *cache-version*))
              (let ((files-data (hash-ref cache-data "files" (hash)))
                    (loaded 0)
                    (stale 0))
                (hash-for-each
                  (lambda (uri file-data)
                    (let ((path (uri->file-path* uri))
                          (cached-mtime (hash-ref file-data "mtime" 0))
                          (syms-data (hash-ref file-data "symbols" [])))
                      (if (and path (file-exists? path))
                        (let ((current-mtime (file-mtime path)))
                          (if (= cached-mtime current-mtime)
                            ;; File unchanged - use cached symbols
                            (begin
                              (set-file-symbols! uri (map hash->sym-info
                                                       (if (vector? syms-data)
                                                         (vector->list syms-data)
                                                         syms-data)))
                              (set! loaded (+ loaded 1)))
                            ;; File changed - mark as stale
                            (set! stale (+ stale 1))))
                        ;; File deleted - skip
                        (void))))
                  files-data)
                (lsp-info "loaded ~a cached files, ~a stale" loaded stale)
                loaded)
              (begin
                (lsp-info "index cache version mismatch, rebuilding")
                0)))
          0)))))

;;; Convert sym-info to a serializable hash
(def (sym-info->hash sym)
  (hash ("name" (sym-info-name sym))
        ("kind" (sym-info-kind sym))
        ("line" (sym-info-line sym))
        ("col" (sym-info-col sym))
        ("end-line" (sym-info-end-line sym))
        ("end-col" (sym-info-end-col sym))
        ("detail" (or (sym-info-detail sym) ""))))

;;; Convert a hash back to sym-info
(def (hash->sym-info h)
  (make-sym-info
    (hash-ref h "name" "")
    (hash-ref h "kind" "")
    (hash-ref h "line" 0)
    (hash-ref h "col" 0)
    (hash-ref h "end-line" 0)
    (hash-ref h "end-col" 0)
    (let ((d (hash-ref h "detail" "")))
      (if (string=? d "") #f d))))

;;; Get file modification time as seconds since epoch
(def (file-mtime path)
  (with-catch
    (lambda (e) 0)
    (lambda ()
      (time->seconds (file-info-last-modification-time (file-info path))))))

;;; URI to file path helper (avoiding circular import)
(def (uri->file-path* uri)
  (if (string-prefix? "file://" uri)
    (let ((path (substring uri 7 (string-length uri))))
      ;; Handle URL encoding
      (uri-decode* path))
    #f))

;;; Simple URL decode
(def (uri-decode* str)
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length str))
        (get-output-string out)
        (let ((c (string-ref str i)))
          (if (and (char=? c #\%)
                   (< (+ i 2) (string-length str)))
            (let ((hex (substring str (+ i 1) (+ i 3))))
              (with-catch
                (lambda (e) (write-char c out) (loop (+ i 1)))
                (lambda ()
                  (write-char (integer->char (string->number hex 16)) out)
                  (loop (+ i 3)))))
            (begin
              (write-char c out)
              (loop (+ i 1)))))))))

;;; ============================================================
;;; Dependency Graph for Inter-file Dependencies
;;; ============================================================

;;; Import dependency graph: uri -> list of imported uris
(def *import-deps* (make-hash-table))

;;; Reverse dependency graph: uri -> list of uris that import it
(def *reverse-deps* (make-hash-table))

;;; Set the imports for a file
(def (set-file-imports! uri imports)
  ;; Remove old reverse deps
  (let ((old-imports (hash-ref *import-deps* uri '())))
    (for-each
      (lambda (imp-uri)
        (hash-update! *reverse-deps* imp-uri
          (lambda (deps) (filter (lambda (d) (not (string=? d uri))) deps))
          '()))
      old-imports))
  ;; Add new deps
  (hash-put! *import-deps* uri imports)
  ;; Add reverse deps
  (for-each
    (lambda (imp-uri)
      (hash-update! *reverse-deps* imp-uri
        (lambda (deps)
          (if (member uri deps) deps (cons uri deps)))
        '()))
    imports))

;;; Get files that depend on a given file (reverse deps)
(def (get-dependents uri)
  (hash-ref *reverse-deps* uri '()))

;;; Get all transitive dependents of a file
(def (get-transitive-dependents uri)
  (let ((visited (make-hash-table))
        (result '()))
    (let loop ((uris (list uri)))
      (unless (null? uris)
        (let ((current (car uris)))
          (unless (hash-key? visited current)
            (hash-put! visited current #t)
            (unless (string=? current uri)
              (set! result (cons current result)))
            (loop (append (cdr uris) (get-dependents current)))))))
    result))

;;; Clear dependency info for a file
(def (clear-file-deps! uri)
  (let ((old-imports (hash-ref *import-deps* uri '())))
    (for-each
      (lambda (imp-uri)
        (hash-update! *reverse-deps* imp-uri
          (lambda (deps) (filter (lambda (d) (not (string=? d uri))) deps))
          '()))
      old-imports))
  (hash-remove! *import-deps* uri))

;;; Save dependency graph to disk
(def (save-deps-cache! root)
  (with-catch
    (lambda (e) (lsp-debug "failed to save deps cache: ~a" e))
    (lambda ()
      (ensure-cache-dir! root)
      (let ((deps-data (hash ("version" *cache-version*)
                              ("imports" *import-deps*)
                              ("reverse" *reverse-deps*))))
        (let ((cache-path (deps-cache-path root))
              (temp-path (string-append (deps-cache-path root) ".tmp")))
          (call-with-output-file temp-path
            (lambda (port) (write-json deps-data port)))
          (rename-file temp-path cache-path))))))

;;; Load dependency graph from disk
(def (load-deps-cache! root)
  (with-catch
    (lambda (e)
      (lsp-debug "failed to load deps cache: ~a" e)
      #f)
    (lambda ()
      (let ((cache-path (deps-cache-path root)))
        (if (file-exists? cache-path)
          (let ((deps-data (call-with-input-file cache-path read-json)))
            (when (and (hash-table? deps-data)
                       (= (hash-ref deps-data "version" 0) *cache-version*))
              (let ((imports (hash-ref deps-data "imports" (hash)))
                    (reverse (hash-ref deps-data "reverse" (hash))))
                ;; Restore hash tables
                (hash-for-each (lambda (k v) (hash-put! *import-deps* k v)) imports)
                (hash-for-each (lambda (k v) (hash-put! *reverse-deps* k v)) reverse)
                #t)))
          #f)))))
