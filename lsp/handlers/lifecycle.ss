;;; -*- Gerbil -*-
;;; LSP lifecycle handlers: initialize, initialized, shutdown, exit
(import ../compat/compat
        ../util/log
        ../jsonrpc
        ../types
        ../capabilities
        ../state
        ../server
        ../analysis/index
        ../analysis/module)
(export #t)

;;; Handle "initialize" request
;;; Returns the InitializeResult with server capabilities
(def (handle-initialize params)
  (let ((root-uri (hash-ref params "rootUri" #f))
        (root-path (hash-ref params "rootPath" #f))
        (workspace-folders-param (hash-ref params "workspaceFolders" #f))
        (caps (hash-ref params "capabilities" (hash))))
    ;; Store primary workspace root
    (let ((root (or root-uri root-path)))
      (when root
        (set-workspace-root! (uri->file-path root))))
    ;; Store multi-root workspace folders
    (when workspace-folders-param
      (let ((folders (if (vector? workspace-folders-param)
                       (vector->list workspace-folders-param)
                       workspace-folders-param)))
        (for-each
          (lambda (folder)
            (let ((uri (hash-ref folder "uri" #f)))
              (when uri
                (add-workspace-folder! (uri->file-path uri)))))
          folders)))
    ;; Store client capabilities
    (set-client-capabilities! caps)
    (lsp-info "initialize: workspace roots = ~a" (all-workspace-roots))
    ;; Return InitializeResult
    (hash ("capabilities" (server-capabilities))
          ("serverInfo" (hash ("name" "gerbil-lsp")
                              ("version" "0.1.0"))))))

;;; Handle "initialized" notification
;;; Client confirms initialization is complete — index workspace
(def (handle-initialized params)
  (set-initialized! #t)
  (lsp-info "server initialized")
  ;; Register for file watchers
  (register-file-watchers!)
  ;; Index all .ss files in all workspace roots
  (let ((roots (all-workspace-roots)))
    (when (pair? roots)
      ;; Create progress token
      (let ((progress-token "indexing"))
        (send-request! "window/workDoneProgress/create"
          (hash ("token" progress-token)))
        (send-progress! progress-token "begin"
          title: "Indexing workspace"
          message: (format "Scanning ~a root(s)" (length roots))
          percentage: 0)
        (with-catch
          (lambda (e)
            (lsp-warn "workspace indexing failed: ~a" e)
            (send-progress! progress-token "end" message: "Indexing failed")
            (send-log-message! MessageType.Error
              (format "Workspace indexing failed: ~a" e)))
          (lambda ()
            ;; Index each workspace root
            (for-each
              (lambda (root)
                (lsp-info "indexing root: ~a" root)
                (index-workspace-with-progress! root progress-token))
              roots)
            (send-progress! progress-token "end" message: "Indexing complete")
            (send-log-message! MessageType.Info "Workspace indexing complete"))))))
  (void))

;;; Register for file watcher events (dynamic registration)
(def (register-file-watchers!)
  (with-catch
    (lambda (e) (lsp-debug "file watcher registration failed: ~a" e))
    (lambda ()
      (send-request! "client/registerCapability"
        (hash ("registrations"
               (vector
                 (hash ("id" "gerbil-file-watcher")
                       ("method" "workspace/didChangeWatchedFiles")
                       ("registerOptions"
                        (hash ("watchers"
                               (vector
                                 (hash ("globPattern" "**/*.ss")
                                       ("kind" 7)))))))))))))) ;; 7 = Create|Change|Delete

;;; Handle workspace/didChangeWorkspaceFolders notification
;;; Called when workspace folders are added or removed
(def (handle-did-change-workspace-folders params)
  (let ((event (hash-ref params "event" (hash))))
    (let ((added (hash-ref event "added" []))
          (removed (hash-ref event "removed" [])))
      ;; Remove old folders
      (for-each
        (lambda (folder)
          (let ((uri (hash-ref folder "uri" #f)))
            (when uri
              (let ((path (uri->file-path uri)))
                (lsp-info "removing workspace folder: ~a" path)
                (remove-workspace-folder! path)))))
        (if (vector? removed) (vector->list removed) removed))
      ;; Add new folders and index them
      (for-each
        (lambda (folder)
          (let ((uri (hash-ref folder "uri" #f)))
            (when uri
              (let ((path (uri->file-path uri)))
                (lsp-info "adding workspace folder: ~a" path)
                (add-workspace-folder! path)
                ;; Index the new folder
                (spawn
                  (lambda ()
                    (with-catch
                      (lambda (e) (lsp-warn "failed to index new folder: ~a" e))
                      (lambda () (index-workspace! path)))))))))
        (if (vector? added) (vector->list added) added)))))

;;; Handle "shutdown" request
;;; Prepare for exit, return null
(def (handle-shutdown params)
  (set-shutdown-requested! #t)
  (lsp-info "shutdown requested")
  (void))

;;; Handle "exit" notification
;;; Terminate the process
(def (handle-exit params)
  (lsp-info "exiting")
  (force-output (current-output-port))
  (exit (if (shutdown-requested?) 0 1)))

