;;; -*- Gerbil -*-
;;; Diagnostics handler — compile Gerbil files and report errors
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../server
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/module
        ../analysis/cache
        ../analysis/project-config)
(export #t)

;;; Publish diagnostics for a document (full: parse + compile)
;;; Called on save — spawns a background thread for gxc
;;; Also triggers re-diagnosis of dependent files
(def (publish-diagnostics-for uri)
  (let ((doc (get-document uri)))
    (when doc
      (let* ((text (document-text doc))
             (file-path (uri->file-path uri))
             (parse-diags (parse-diagnostics text)))
        ;; Publish parse diagnostics immediately with cached gxc
        (let ((cached-gxc (get-gxc-diagnostics uri)))
          (send-notification! "textDocument/publishDiagnostics"
            (hash ("uri" uri)
                  ("diagnostics"
                   (list->vector (append parse-diags cached-gxc))))))
        ;; Spawn background thread for gxc diagnostics
        (spawn-gxc-diagnostics-thread! uri file-path text parse-diags)
        ;; Schedule diagnostics for dependent files
        (schedule-dependent-diagnostics! uri)))))

;;; Schedule diagnostics for files that depend on the changed file
(def (schedule-dependent-diagnostics! uri)
  (let ((dependents (get-dependents uri)))
    (when (pair? dependents)
      (lsp-debug "scheduling diagnostics for ~a dependents" (length dependents))
      (spawn
        (lambda ()
          ;; Small delay to let the main file's diagnostics complete first
          (thread-sleep! 0.5)
          (for-each
            (lambda (dep-uri)
              (let ((dep-doc (get-document dep-uri)))
                (when dep-doc
                  (let* ((dep-path (uri->file-path dep-uri))
                         (dep-text (document-text dep-doc)))
                    (when (and dep-path (file-exists? dep-path))
                      (with-catch
                        (lambda (e) (lsp-debug "dependent diagnostics failed: ~a" e))
                        (lambda ()
                          (let ((gxc-diags (compile-diagnostics dep-path dep-text)))
                            (set-gxc-diagnostics! dep-uri gxc-diags)
                            (send-notification! "textDocument/publishDiagnostics"
                              (hash ("uri" dep-uri)
                                    ("diagnostics" (list->vector gxc-diags))))))))))))
            dependents))))))

;;; Spawn a background thread to run gxc diagnostics with progress reporting
(def (spawn-gxc-diagnostics-thread! uri file-path text parse-diags)
  (mutex-lock! (diagnostics-mutex))
  ;; Cancel previous diagnostics thread if running
  (let ((prev (diagnostics-thread)))
    (when prev
      (with-catch (lambda (e) (void))
        (lambda () (thread-terminate! prev)))))
  (let ((t (spawn
              (lambda ()
                (with-catch
                  (lambda (e)
                    (lsp-debug "async gxc diagnostics failed: ~a" e))
                  (lambda ()
                    ;; Report progress for compilation
                    (let ((progress-token (string-append "gxc-" uri)))
                      (with-catch (lambda (e) (void))
                        (lambda ()
                          (send-request! "window/workDoneProgress/create"
                            (hash ("token" progress-token)))
                          (send-progress! progress-token "begin"
                            title: "Checking"
                            message: (if (string? file-path)
                                      (path-strip-directory file-path)
                                      "buffer"))))
                      (let ((gxc-diags (compile-diagnostics file-path text))
                            (unused-diags (detect-unused-imports uri text)))
                        ;; End progress
                        (with-catch (lambda (e) (void))
                          (lambda ()
                            (send-progress! progress-token "end"
                              message: "Done")))
                        ;; Cache and publish combined results
                        (set-gxc-diagnostics! uri gxc-diags)
                        (send-notification! "textDocument/publishDiagnostics"
                          (hash ("uri" uri)
                                ("diagnostics"
                                 (list->vector
                                   (append parse-diags gxc-diags unused-diags)))))))))))))
    (set-diagnostics-thread! t)
    (mutex-unlock! (diagnostics-mutex))))

;;; Publish parse-only diagnostics for a document (fast, no gxc)
;;; Used on didChange for immediate feedback while typing.
;;; Merges parse diagnostics with cached gxc diagnostics so compiler
;;; errors don't disappear while editing.
(def (publish-parse-diagnostics uri text)
  (with-catch
    (lambda (e)
      (lsp-debug "parse diagnostics failed: ~a" e))
    (lambda ()
      (let ((parse-diags (parse-diagnostics text))
            (cached-gxc (get-gxc-diagnostics uri)))
        (send-notification! "textDocument/publishDiagnostics"
          (hash ("uri" uri)
                ("diagnostics"
                 (list->vector (append parse-diags cached-gxc)))))))))

;;; Collect diagnostics for a document
;;; Combines parse-level and compile-level diagnostics.
;;; Caches the gxc diagnostics for use during editing.
(def (collect-diagnostics uri doc)
  (let* ((text (document-text doc))
         (file-path (uri->file-path uri))
         (parse-diags (parse-diagnostics text))
         (gxc-diags (compile-diagnostics file-path text)))
    ;; Cache gxc diagnostics for merging during didChange
    (set-gxc-diagnostics! uri gxc-diags)
    (append parse-diags gxc-diags)))

;;; Quick parse diagnostics — try to read the file as S-expressions
(def (parse-diagnostics text)
  (let ((port (open-input-string text)))
    (let loop ((diags '()))
      (with-catch
        (lambda (e)
          (let ((msg (error-message e)))
            (let-values (((line col) (error-line-col e)))
              (let ((l (or line 0))
                    (c (or col 0)))
                ;; Don't try to continue after parse error
                (cons (make-diagnostic
                        (make-lsp-range l c l (+ c 1))
                        (or msg (format "~a" e))
                        severity: DiagnosticSeverity.Error
                        source: "gerbil-lsp/parse")
                      diags)))))
        (lambda ()
          (let ((form (read port)))
            (if (eof-object? form)
              diags
              (loop diags))))))))

;;; Extract error message from an exception
(def (error-message e)
  (with-catch
    (lambda (_) (format "~a" e))
    (lambda ()
      (cond
        ((datum-parsing-exception? e)
         (format "parse error: ~a" (datum-parsing-exception-kind e)))
        ((error-exception? e)
         (error-exception-message e))
        (else (format "~a" e))))))

;;; Extract line and column from a parse exception
;;; Returns (values line col) with 0-based values, or (values #f #f)
;;; Gambit's readenv filepos encodes: lower 16 bits = 0-indexed line,
;;; upper bits = 1-indexed column at error point
(def (error-line-col e)
  (with-catch
    (lambda (_) (values #f #f))
    (lambda ()
      (if (datum-parsing-exception? e)
        (let* ((re (datum-parsing-exception-readenv e))
               (filepos (##readenv-current-filepos re)))
          (if (fixnum? filepos)
            (values (bitwise-and filepos #xFFFF)
                    (max 0 (- (arithmetic-shift filepos -16) 1)))
            (values #f #f)))
        (values #f #f)))))

;;; Compile diagnostics — run gxc on the file
;;; For unsaved files, writes to a temp file first
(def (compile-diagnostics file-path text)
  (cond
    ;; For actual files that exist, run gxc on them directly
    ((and (string? file-path) (file-exists? file-path))
     (run-gxc-diagnostics file-path))
    ;; For unsaved content with a known path, write to temp and compile
    ((and (string? file-path) (string? text) (> (string-length text) 0))
     (run-gxc-on-temp-file file-path text))
    (else '())))

;;; Run gxc on buffer content via a temporary file
;;; Preserves the original filename for proper module resolution
(def (run-gxc-on-temp-file original-path text)
  (with-catch
    (lambda (e)
      (lsp-debug "temp file diagnostics failed: ~a" e)
      '())
    (lambda ()
      (let* ((dir (path-directory original-path))
             (filename (path-strip-directory original-path))
             (temp-dir (path-expand ".gerbil-lsp-temp" dir))
             (temp-path (path-expand filename temp-dir)))
        ;; Ensure temp directory exists
        (unless (file-exists? temp-dir)
          (create-directory* temp-dir))
        ;; Write buffer content to temp file
        (call-with-output-file temp-path
          (lambda (port) (display text port)))
        ;; Run gxc on temp file
        (let ((diags (run-gxc-diagnostics temp-path)))
          ;; Clean up temp file
          (with-catch (lambda (e) (void))
            (lambda () (delete-file temp-path)))
          diags)))))

;;; Run gxc -S on a file and parse the error output.
;;; Uses open-process because run-process throws on non-zero exit,
;;; but gxc exits non-zero when there are compilation errors —
;;; exactly when we need to read its output.
;;; Uses project-specific gxc path, flags, and loadpath if configured.
(def (run-gxc-diagnostics file-path)
  (with-catch
    (lambda (e)
      (lsp-debug "gxc diagnostics failed: ~a" e)
      '())
    (lambda ()
      (let* ((root (workspace-root))
             (gxc-path (if root
                         (project-gxc-path root)
                         (or (get-config "gxc-path") "gxc")))
             (extra-flags (if root (project-gxc-flags root) '()))
             (loadpath (if root (project-loadpath root) '()))
             ;; Build arguments: -S file-path plus extra flags
             (args (append (list "-S") extra-flags (list file-path)))
             ;; Build environment with GERBIL_LOADPATH if needed
             (env (if (pair? loadpath)
                    (list (string-append "GERBIL_LOADPATH="
                            (string-join loadpath ":")))
                    #f))
             (proc (open-process
                     (list path: gxc-path
                           arguments: args
                           environment: env
                           stderr-redirection: #t
                           stdout-redirection: #t)))
             (output (read-all-as-string proc))
             (status (process-status proc)))
        ;; process-status returns raw waitpid status;
        ;; normal exit code N gives N*256, signals give lower byte.
        ;; Any non-zero status indicates failure.
        (if (and (not (= status 0))
                 (string? output)
                 (> (string-length output) 0))
          (parse-gxc-output output file-path)
          '())))))

;;; Parse gxc error output into diagnostics
(def (parse-gxc-output output file-path)
  (let ((lines (string-split-lines output))
        (diags '()))
    (for-each
      (lambda (line)
        (when (> (string-length line) 0)
          (let ((diag (parse-gxc-error-line line)))
            (when diag
              (set! diags (cons diag diags))))))
      lines)
    ;; If we couldn't parse structured errors but there is output,
    ;; report it as a generic error
    (if (and (null? diags) (> (string-length output) 0))
      (list (make-diagnostic
              (make-lsp-range 0 0 0 1)
              (string-trim-eol output)
              severity: DiagnosticSeverity.Error
              source: "gxc"))
      diags)))

;;; Parse a single gxc error line
;;; Format varies but common patterns:
;;;   file.ss:line:col: error message
;;;   *** ERROR IN procedure -- message
(def (parse-gxc-error-line line)
  (with-catch
    (lambda (e) #f)
    (lambda ()
      ;; Try pattern: path:line:col: message
      (let ((parts (string-split-colon line)))
        (if (and (>= (length parts) 4)
                 (string->number (cadr parts)))
          (let ((err-line (- (string->number (cadr parts)) 1)) ; 0-based
                (err-col (or (string->number (caddr parts)) 0))
                (msg (string-join (cdddr parts) ":")))
            (make-diagnostic
              (make-lsp-range err-line err-col err-line (+ err-col 1))
              (string-trim-eol msg)
              severity: DiagnosticSeverity.Error
              source: "gxc"))
          ;; Try pattern: *** ERROR ...
          (if (string-prefix? "***" line)
            (make-diagnostic
              (make-lsp-range 0 0 0 1)
              line
              severity: DiagnosticSeverity.Error
              source: "gxc")
            #f))))))

;;; Split a string on colons (simple)
(def (string-split-colon str)
  (let loop ((i 0) (start 0) (parts '()))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) parts)))
      ((char=? (string-ref str i) #\:)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
      (else (loop (+ i 1) start parts)))))

;;; Detect unused imports in a file
;;; Returns Warning-severity diagnostics for each unused import symbol
(def (detect-unused-imports uri text)
  (with-catch
    (lambda (e)
      (lsp-debug "detect-unused-imports failed: ~a" e)
      '())
    (lambda ()
      (let* ((forms (parse-source-resilient text))
             (import-specs (extract-imports forms))
             (lines (string-split-lines text))
             ;; Get the text after imports for usage checking
             (non-import-text (get-non-import-text forms text))
             (diags '()))
        ;; Check each simple symbol import
        (for-each
          (lambda (spec)
            (when (symbol? spec)
              (let ((spec-str (symbol->string spec)))
                ;; Skip relative imports and complex specs
                (unless (or (string-prefix? "./" spec-str)
                            (string-prefix? "../" spec-str))
                  ;; Find the line where this import appears
                  (let ((import-line (find-import-line-for-spec spec text)))
                    (when import-line
                      ;; Check if any symbol from this module is used
                      (let ((module-used? (module-symbols-used? spec non-import-text uri)))
                        (unless module-used?
                          (set! diags
                            (cons (make-diagnostic
                                    (make-lsp-range import-line 0 import-line 1)
                                    (format "Unused import: ~a" spec)
                                    severity: DiagnosticSeverity.Warning
                                    source: "gerbil-lsp"
                                    code: "unused-import"
                                    tags: (list DiagnosticTag.Unnecessary))
                                  diags))))))))))
          import-specs)
        diags))))

;;; Get the text of the file excluding import forms
(def (get-non-import-text forms text)
  (let ((lines (string-split-lines text))
        (import-lines (make-hash-table)))
    ;; Mark all lines that are part of import forms
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (let loop ((l (located-form-line lf)))
              (when (<= l (located-form-end-line lf))
                (hash-put! import-lines l #t)
                (loop (+ l 1)))))))
      forms)
    ;; Collect non-import lines
    (let ((out (open-output-string)))
      (let loop ((i 0) (ls lines))
        (unless (null? ls)
          (unless (hash-key? import-lines i)
            (display (car ls) out)
            (newline out))
          (loop (+ i 1) (cdr ls))))
      (get-output-string out))))

;;; Check if any symbol from a module is used in the non-import text
(def (module-symbols-used? module-spec non-import-text uri)
  (with-catch
    (lambda (e) #t)  ;; On error, assume it's used
    (lambda ()
      (let ((file-path (uri->file-path uri)))
        (let ((exports (get-or-resolve-module-exports module-spec file-path)))
          (if (null? exports)
            #t  ;; Can't resolve — assume used
            (let loop ((syms exports))
              (if (null? syms) #f
                (let ((name (sym-info-name (car syms))))
                  (if (string-contains non-import-text name)
                    #t
                    (loop (cdr syms))))))))))))

;;; Schedule debounced gxc diagnostics for a URI
;;; Cancels any previous debounce timer and starts a new one.
;;; After the delay (ms), runs full diagnostics.
(def (schedule-debounced-diagnostics! uri)
  (mutex-lock! (debounce-mutex))
  ;; Cancel previous debounce thread
  (let ((prev (debounce-thread)))
    (when prev
      (with-catch (lambda (e) (void))
        (lambda () (thread-terminate! prev)))))
  (let* ((delay-ms (or (get-config "diagnostics-delay") 1500))
         (delay-secs (/ delay-ms 1000.0))
         (t (spawn
              (lambda ()
                (with-catch
                  (lambda (e)
                    (lsp-debug "debounced diagnostics cancelled: ~a" e))
                  (lambda ()
                    (thread-sleep! delay-secs)
                    (publish-diagnostics-for uri)))))))
    (set-debounce-thread! t))
  (mutex-unlock! (debounce-mutex)))

;;; Cancel the debounce thread if running
(def (cancel-debounce-thread!)
  (mutex-lock! (debounce-mutex))
  (let ((prev (debounce-thread)))
    (when prev
      (with-catch (lambda (e) (void))
        (lambda () (thread-terminate! prev)))
      (set-debounce-thread! #f)))
  (mutex-unlock! (debounce-mutex)))

;;; Find the line number where a given import spec appears
(def (find-import-line-for-spec spec text)
  (let ((spec-str (format "~a" spec))
        (lines (string-split-lines text)))
    (let loop ((ls lines) (n 0))
      (if (null? ls) #f
        (if (string-contains (car ls) spec-str)
          n
          (loop (cdr ls) (+ n 1)))))))

