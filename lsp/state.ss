;;; -*- Gerbil -*-
;;; Global LSP server state
(export #t)

;;; Server lifecycle state
(def *initialized* #f)
(def *shutdown-requested* #f)
(def *workspace-root* #f)
(def *workspace-folders* '())  ;; List of workspace folder paths (multi-root)
(def *client-capabilities* #f)

;;; Document store: uri (string) → document record (hash table)
(def *documents* (make-hash-table))

;;; Symbol index: populated by analysis
(def *symbol-index* (make-hash-table))

;;; Module cache: module-path → export list
(def *module-cache* (make-hash-table))

;;; File text cache: uri → text string (for indexed but not open files)
(def *file-text-cache* (make-hash-table))

;;; Last gxc diagnostics cache: uri → diagnostics list
;;; Preserved during editing so parse diagnostics don't replace them
(def *gxc-diagnostics-cache* (make-hash-table))

;;; --- Document operations ---

(def (get-document uri)
  (hash-ref *documents* uri #f))

(def (set-document! uri doc)
  (hash-put! *documents* uri doc))

(def (remove-document! uri)
  (hash-remove! *documents* uri))

(def (all-document-uris)
  (hash-keys *documents*))

;;; --- State accessors ---

(def (server-initialized?)
  *initialized*)

(def (set-initialized! v)
  (set! *initialized* v))

(def (shutdown-requested?)
  *shutdown-requested*)

(def (set-shutdown-requested! v)
  (set! *shutdown-requested* v))

(def (workspace-root)
  *workspace-root*)

(def (set-workspace-root! root)
  (set! *workspace-root* root))

(def (workspace-folders)
  *workspace-folders*)

(def (set-workspace-folders! folders)
  (set! *workspace-folders* folders))

(def (add-workspace-folder! folder)
  (unless (member folder *workspace-folders*)
    (set! *workspace-folders* (cons folder *workspace-folders*))))

(def (remove-workspace-folder! folder)
  (set! *workspace-folders* (filter (lambda (f) (not (string=? f folder))) *workspace-folders*)))

(def (all-workspace-roots)
  "Returns all workspace roots including the primary and multi-root folders"
  (let ((roots (if *workspace-root* (list *workspace-root*) '())))
    (append roots (filter (lambda (f) (not (member f roots))) *workspace-folders*))))

(def (client-capabilities)
  *client-capabilities*)

(def (set-client-capabilities! caps)
  (set! *client-capabilities* caps))

;;; --- Symbol index operations ---

(def (get-file-symbols uri)
  (hash-ref *symbol-index* uri '()))

(def (set-file-symbols! uri symbols)
  (hash-put! *symbol-index* uri symbols))

(def (remove-file-symbols! uri)
  (hash-remove! *symbol-index* uri))

(def (all-indexed-symbols)
  (let ((result '()))
    (hash-for-each
      (lambda (uri syms)
        (for-each (lambda (s) (set! result (cons (cons uri s) result))) syms))
      *symbol-index*)
    result))

;;; --- File text cache operations ---
;;; Used for workspace-wide references/rename on files not currently open

(def (get-file-text uri)
  (hash-ref *file-text-cache* uri #f))

(def (set-file-text! uri text)
  (hash-put! *file-text-cache* uri text))

(def (remove-file-text! uri)
  (hash-remove! *file-text-cache* uri))

;;; Get all URIs that have indexed symbols
(def (all-indexed-uris)
  (hash-keys *symbol-index*))

;;; --- GXC diagnostics cache operations ---

(def (get-gxc-diagnostics uri)
  (hash-ref *gxc-diagnostics-cache* uri '()))

(def (set-gxc-diagnostics! uri diags)
  (hash-put! *gxc-diagnostics-cache* uri diags))

(def (clear-gxc-diagnostics! uri)
  (hash-remove! *gxc-diagnostics-cache* uri))

;;; --- Module cache operations ---

(def (get-module-exports module-path)
  (hash-ref *module-cache* module-path #f))

(def (set-module-exports! module-path exports)
  (hash-put! *module-cache* module-path exports))

;;; --- Diagnostics thread state ---
(def *diagnostics-thread* #f)
(def *diagnostics-mutex* (make-mutex 'diagnostics))

(def (diagnostics-thread) *diagnostics-thread*)
(def (set-diagnostics-thread! t) (set! *diagnostics-thread* t))
(def (diagnostics-mutex) *diagnostics-mutex*)

;;; --- Debounce thread state ---
;;; Used for debounced diagnostics on didChange
(def *debounce-thread* #f)
(def *debounce-mutex* (make-mutex 'debounce))

(def (debounce-thread) *debounce-thread*)
(def (set-debounce-thread! t) (set! *debounce-thread* t))
(def (debounce-mutex) *debounce-mutex*)

;;; --- Last completion URI ---
;;; completionItem/resolve doesn't receive the document URI,
;;; so we cache it from the last textDocument/completion request.
(def *last-completion-uri* #f)

(def (last-completion-uri) *last-completion-uri*)
(def (set-last-completion-uri! uri) (set! *last-completion-uri* uri))

;;; --- Configuration ---
(def *server-config*
  (hash ("gxc-path" "gxc")
        ("diagnostics-on-save" #t)
        ("diagnostics-delay" 1500)
        ("log-level" "info")
        ("format-line-width" 80)))

(def (get-config key)
  (hash-ref *server-config* key #f))

(def (set-config! key value)
  (hash-put! *server-config* key value))

(def (merge-config! settings)
  (when (hash-table? settings)
    (hash-for-each
      (lambda (k v)
        (hash-put! *server-config* k v))
      settings)))

;;; --- Module cache timestamps ---
(def *module-cache-timestamps* (make-hash-table))

(def (get-module-cache-timestamp module-path)
  (hash-ref *module-cache-timestamps* module-path #f))

(def (set-module-cache-timestamp! module-path ts)
  (hash-put! *module-cache-timestamps* module-path ts))

(def (clear-module-cache-for! module-path)
  (hash-remove! *module-cache* module-path)
  (hash-remove! *module-cache-timestamps* module-path))
