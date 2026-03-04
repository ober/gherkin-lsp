;;; -*- Gerbil -*-
;;; Project configuration handling
;;; Reads settings from gerbil.pkg and .gerbil-lsp.json
(import :std/text/json
        ../compat/compat
        ../util/log
        ../state)
(export #t)

;;; Project-specific configuration cache
(def *project-config-cache* (make-hash-table))

;;; Get project configuration for a workspace root
(def (get-project-config root)
  (or (hash-ref *project-config-cache* root #f)
      (let ((config (load-project-config root)))
        (hash-put! *project-config-cache* root config)
        config)))

;;; Load project configuration from files
;;; Checks: .gerbil-lsp.json, then falls back to gerbil.pkg
(def (load-project-config root)
  (let ((config (make-hash-table)))
    ;; Try loading .gerbil-lsp.json first
    (let ((json-path (path-expand ".gerbil-lsp.json" root)))
      (when (file-exists? json-path)
        (with-catch
          (lambda (e) (lsp-debug "failed to load .gerbil-lsp.json: ~a" e))
          (lambda ()
            (let ((json-config (call-with-input-file json-path read-json)))
              (when (hash-table? json-config)
                (hash-for-each
                  (lambda (k v) (hash-put! config k v))
                  json-config)))))))
    ;; Also read gerbil.pkg for package info
    (let ((pkg-path (path-expand "gerbil.pkg" root)))
      (when (file-exists? pkg-path)
        (with-catch
          (lambda (e) (lsp-debug "failed to load gerbil.pkg: ~a" e))
          (lambda ()
            (let ((pkg-data (call-with-input-file pkg-path read)))
              (when (list? pkg-data)
                (for-each
                  (lambda (item)
                    (when (and (pair? item) (pair? (cdr item)))
                      (case (car item)
                        ((package:)
                         (hash-put! config "package" (cadr item)))
                        ((prelude:)
                         (hash-put! config "prelude" (cadr item)))
                        ((depend:)
                         (hash-put! config "dependencies"
                           (if (pair? (cdr item))
                             (cdr item)
                             (list (cdr item))))))))
                  pkg-data)))))))
    config))

;;; Get a config value with default
(def (project-config-ref root key default)
  (let ((config (get-project-config root)))
    (hash-ref config key default)))

;;; Get gxc path for a project (allows per-project override)
(def (project-gxc-path root)
  (or (project-config-ref root "gxc-path" #f)
      (get-config "gxc-path")
      "gxc"))

;;; Get load path for a project
(def (project-loadpath root)
  (let ((paths (project-config-ref root "loadpath" '())))
    (if (list? paths) paths
      (if (string? paths) (list paths) '()))))

;;; Get extra gxc flags for a project
(def (project-gxc-flags root)
  (let ((flags (project-config-ref root "gxc-flags" '())))
    (if (list? flags) flags
      (if (string? flags) (list flags) '()))))

;;; Get diagnostics delay for a project
(def (project-diagnostics-delay root)
  (project-config-ref root "diagnostics-delay"
    (get-config "diagnostics-delay")))

;;; Check if a feature is disabled for a project
(def (project-feature-disabled? root feature)
  (let ((disabled (project-config-ref root "disabled-features" '())))
    (and (list? disabled)
         (member feature disabled))))

;;; Invalidate cached config for a root
(def (invalidate-project-config! root)
  (hash-remove! *project-config-cache* root))

;;; Clear all project config caches
(def (clear-project-config-cache!)
  (set! *project-config-cache* (make-hash-table)))
