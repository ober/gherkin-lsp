;;; -*- Gerbil -*-
;;; workspace/executeCommand handler
;;; Dispatches code lens commands: run test, show references
(import ../compat/compat
        ../util/log
        ../util/position
        ../types
        ../state
        ../server
        ../analysis/document
        ../analysis/symbols
        ../analysis/index
        ../analysis/module)
(export #t)

;;; Handle workspace/executeCommand
(def (handle-execute-command params)
  (let* ((command (hash-ref params "command" ""))
         (args (let ((a (hash-ref params "arguments" (vector))))
                 (if (vector? a) (vector->list a) a))))
    (lsp-debug "executeCommand: ~a args=~a" command args)
    (cond
      ((string=? command "gerbil-lsp.runTest")
       (execute-run-test args))
      ((string=? command "gerbil-lsp.showReferences")
       (execute-show-references args))
      ((string=? command "gerbil-lsp.compileFile")
       (execute-compile-file args))
      ((string=? command "gerbil-lsp.buildProject")
       (execute-build-project args))
      (else
       (lsp-warn "unknown command: ~a" command)
       (void)))))

;;; Execute "Run test" command
;;; args: [test-name]
(def (execute-run-test args)
  (if (and (pair? args) (string? (car args)))
    (let* ((test-name (car args))
           (defs (find-definitions-by-name test-name)))
      (if (pair? defs)
        (let* ((def-entry (car defs))
               (def-uri (car def-entry))
               (file-path (uri->file-path def-uri)))
          (if (and file-path (file-exists? file-path))
            ;; Run gerbil test on the file in a background thread
            (begin
              (send-notification! "window/showMessage"
                (hash ("type" MessageType.Info)
                      ("message" (format "Running test: ~a" test-name))))
              (spawn
                (lambda ()
                  (with-catch
                    (lambda (e)
                      (send-notification! "window/showMessage"
                        (hash ("type" MessageType.Error)
                              ("message" (format "Test failed: ~a" e)))))
                    (lambda ()
                      (let* ((proc (open-process
                                     (list path: "gerbil"
                                           arguments: (list "test" file-path)
                                           stderr-redirection: #t
                                           stdout-redirection: #t)))
                             (output (read-all-as-string proc))
                             (status (process-status proc)))
                        (if (= status 0)
                          (send-notification! "window/showMessage"
                            (hash ("type" MessageType.Info)
                                  ("message" (format "Test passed: ~a" test-name))))
                          (send-notification! "window/showMessage"
                            (hash ("type" MessageType.Warning)
                                  ("message" (format "Test ~a failed:\n~a"
                                               test-name
                                               (if (> (string-length output) 500)
                                                 (substring output 0 500)
                                                 output)))))))))))
              (void))
            (begin
              (send-notification! "window/showMessage"
                (hash ("type" MessageType.Warning)
                      ("message" (format "Test file not found for ~a" test-name))))
              (void))))
        (begin
          (send-notification! "window/showMessage"
            (hash ("type" MessageType.Warning)
                  ("message" (format "Definition not found for test: ~a" test-name))))
          (void))))
    (void)))

;;; Execute "Show references" command
;;; args: [uri line col]
(def (execute-show-references args)
  (if (and (pair? args) (>= (length args) 3))
    (let* ((uri (car args))
           (line (cadr args))
           (col (caddr args))
           (doc (get-document uri)))
      (if doc
        (let-values (((sym-name _start _end)
                      (symbol-at-position (document-text doc) line col)))
          (if sym-name
            (let* ((refs (find-references-by-name sym-name))
                   (count (length refs)))
              (send-notification! "window/showMessage"
                (hash ("type" MessageType.Info)
                      ("message" (format "~a: ~a reference~a"
                                   sym-name count
                                   (if (= count 1) "" "s")))))
              (void))
            (void)))
        (void)))
    (void)))

;;; Execute "Compile file" command
;;; args: [uri]
(def (execute-compile-file args)
  (if (and (pair? args) (string? (car args)))
    (let* ((uri (car args))
           (file-path (uri->file-path uri)))
      (if (and file-path (file-exists? file-path))
        (begin
          (send-notification! "window/showMessage"
            (hash ("type" MessageType.Info)
                  ("message" (format "Compiling: ~a" file-path))))
          (spawn
            (lambda ()
              (with-catch
                (lambda (e)
                  (send-notification! "window/showMessage"
                    (hash ("type" MessageType.Error)
                          ("message" (format "Compile error: ~a" e)))))
                (lambda ()
                  (let* ((proc (open-process
                                 (list path: "gxc"
                                       arguments: (list "-S" file-path)
                                       stderr-redirection: #t
                                       stdout-redirection: #t)))
                         (output (read-all-as-string proc))
                         (status (process-status proc)))
                    (if (= status 0)
                      (send-notification! "window/showMessage"
                        (hash ("type" MessageType.Info)
                              ("message" (format "Compiled OK: ~a" file-path))))
                      (send-notification! "window/showMessage"
                        (hash ("type" MessageType.Error)
                              ("message" (format "Compile failed:\n~a"
                                           (if (> (string-length output) 500)
                                             (substring output 0 500)
                                             output)))))))))))
          (void))
        (begin
          (send-notification! "window/showMessage"
            (hash ("type" MessageType.Warning)
                  ("message" (format "File not found: ~a" uri))))
          (void))))
    (void)))

;;; Walk up from file-path to find a directory containing build.ss or gerbil.pkg.
;;; Returns the directory path string, or #f if not found.
(def (find-project-root file-path)
  (let dir-loop ((dir (find-parent-dir file-path)))
    (if (not dir) #f
      (if (or (file-exists? (string-append dir "/build.ss"))
              (file-exists? (string-append dir "/gerbil.pkg")))
        dir
        (dir-loop (find-parent-dir dir))))))

;;; Return the parent directory of a path, or #f if at filesystem root.
(def (find-parent-dir path)
  ;; Strip trailing slash
  (let* ((p (let ((len (string-length path)))
               (if (and (> len 1) (char=? (string-ref path (- len 1)) #\/))
                 (substring path 0 (- len 1))
                 path)))
         (len (string-length p)))
    (if (<= len 1) #f
      (let loop ((i (- len 1)))
        (cond
          ((< i 0) #f)
          ((char=? (string-ref p i) #\/)
           (if (= i 0) "/" (substring p 0 i)))
          (else (loop (- i 1))))))))

;;; Execute "Build project" command
;;; args: [uri]  (any file in the project)
(def (execute-build-project args)
  (if (and (pair? args) (string? (car args)))
    (let* ((uri (car args))
           (file-path (uri->file-path uri)))
      (if file-path
        (let ((project-root (find-project-root file-path)))
          (if project-root
            (begin
              (send-notification! "window/showMessage"
                (hash ("type" MessageType.Info)
                      ("message" (format "Building project in: ~a" project-root))))
              (spawn
                (lambda ()
                  (with-catch
                    (lambda (e)
                      (send-notification! "window/showMessage"
                        (hash ("type" MessageType.Error)
                              ("message" (format "Build error: ~a" e)))))
                    (lambda ()
                      (let* ((proc (open-process
                                     (list path: "gerbil"
                                           arguments: '("build")
                                           directory: project-root
                                           stderr-redirection: #t
                                           stdout-redirection: #t)))
                             (output (read-all-as-string proc))
                             (status (process-status proc)))
                        (if (= status 0)
                          (send-notification! "window/showMessage"
                            (hash ("type" MessageType.Info)
                                  ("message" "Build succeeded")))
                          (send-notification! "window/showMessage"
                            (hash ("type" MessageType.Error)
                                  ("message" (format "Build failed:\n~a"
                                               (if (> (string-length output) 500)
                                                 (substring output 0 500)
                                                 output)))))))))))
              (void))
            (begin
              (send-notification! "window/showMessage"
                (hash ("type" MessageType.Warning)
                      ("message" "Could not find project root (no build.ss or gerbil.pkg)")))
              (void))))
        (begin
          (send-notification! "window/showMessage"
            (hash ("type" MessageType.Warning)
                  ("message" (format "Invalid URI: ~a" uri))))
          (void))))
    (void)))
