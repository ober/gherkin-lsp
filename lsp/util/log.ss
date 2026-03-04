;;; -*- Gerbil -*-
;;; LSP server logging — all output goes to stderr
;;; stdout is reserved exclusively for LSP transport
(import ../compat/compat)
(export #t)

(def *log-level* 1) ; 0=debug 1=info 2=warn 3=error

(def (set-log-level! level)
  (set! *log-level* level))

(def (log-level-from-string str)
  (cond
    ((string=? str "debug") 0)
    ((string=? str "info") 1)
    ((string=? str "warn") 2)
    ((string=? str "error") 3)
    (else 1)))

(def (lsp-log level tag fmt . args)
  (when (>= level *log-level*)
    (let ((port (current-error-port)))
      (fprintf port "[gerbil-lsp ~a] " tag)
      (apply fprintf port fmt args)
      (newline port)
      (force-output port))))

(def (lsp-debug fmt . args)
  (apply lsp-log 0 "DEBUG" fmt args))

(def (lsp-info fmt . args)
  (apply lsp-log 1 "INFO" fmt args))

(def (lsp-warn fmt . args)
  (apply lsp-log 2 "WARN" fmt args))

(def (lsp-error fmt . args)
  (apply lsp-log 3 "ERROR" fmt args))
