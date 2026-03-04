;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/project-config module
(import :std/test
        :std/misc/ports
        :lsp/lsp/state
        :lsp/lsp/analysis/project-config)

(export project-config-test-suite)

(def project-config-test-suite
  (test-suite "lsp/analysis/project-config"
    (test-case "get-project-config returns hash"
      (clear-project-config-cache!)
      (let ((config (get-project-config "/nonexistent/path")))
        (check (hash-table? config) => #t)))

    (test-case "project-config-ref with default"
      (clear-project-config-cache!)
      (check (project-config-ref "/nonexistent" "missing-key" "default-value")
             => "default-value"))

    (test-case "project-gxc-path fallback to global"
      (clear-project-config-cache!)
      (let ((path (project-gxc-path "/nonexistent")))
        ;; Should return global config or "gxc"
        (check (string? path) => #t)))

    (test-case "project-loadpath returns list"
      (clear-project-config-cache!)
      (let ((paths (project-loadpath "/nonexistent")))
        (check (list? paths) => #t)))

    (test-case "project-gxc-flags returns list"
      (clear-project-config-cache!)
      (let ((flags (project-gxc-flags "/nonexistent")))
        (check (list? flags) => #t)))

    (test-case "project-feature-disabled? with empty list"
      (clear-project-config-cache!)
      (check (project-feature-disabled? "/nonexistent" "some-feature") => #f))

    (test-case "invalidate-project-config! clears cache"
      (clear-project-config-cache!)
      ;; Load config to cache it
      (get-project-config "/test/path")
      (check (hash-key? *project-config-cache* "/test/path") => #t)
      ;; Invalidate
      (invalidate-project-config! "/test/path")
      (check (hash-key? *project-config-cache* "/test/path") => #f))

    (test-case "clear-project-config-cache! clears all"
      ;; Load multiple configs
      (get-project-config "/path1")
      (get-project-config "/path2")
      (check (> (hash-length *project-config-cache*) 0) => #t)
      ;; Clear all
      (clear-project-config-cache!)
      (check (hash-length *project-config-cache*) => 0))))

(def (main . args)
  (run-tests! project-config-test-suite))
