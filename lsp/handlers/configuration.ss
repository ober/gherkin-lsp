;;; -*- Gerbil -*-
;;; Configuration handler — workspace/didChangeConfiguration
(import ../util/log
        ../state)
(export #t)

;;; Handle workspace/didChangeConfiguration notification
;;; Merges settings into the server config
(def (handle-did-change-configuration params)
  (lsp-debug "didChangeConfiguration: ~a" params)
  (let ((settings (hash-ref params "settings" #f)))
    (when settings
      ;; Look for gerbil-lsp specific settings
      (let ((lsp-settings (or (hash-ref settings "gerbil-lsp" #f)
                              (hash-ref settings "gerbilLsp" #f)
                              settings)))
        (when (hash-table? lsp-settings)
          (merge-config! lsp-settings)
          ;; Wire log-level changes
          (let ((log-level (get-config "log-level")))
            (when (string? log-level)
              (set-log-level! (log-level-from-string log-level))))
          (lsp-info "configuration updated"))))))
