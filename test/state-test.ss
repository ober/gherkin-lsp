;;; -*- Gerbil -*-
;;; Tests for lsp/state
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document)

(export state-test-suite)

;; Reset all global state before running tests
(def (reset-state!)
  (set! *documents* (make-hash-table))
  (set! *symbol-index* (make-hash-table))
  (set! *module-cache* (make-hash-table))
  (set! *file-text-cache* (make-hash-table))
  (set! *gxc-diagnostics-cache* (make-hash-table))
  (set! *initialized* #f)
  (set! *shutdown-requested* #f)
  (set! *workspace-root* #f)
  (set! *client-capabilities* #f)
  (set! *last-completion-uri* #f)
  (set! *debounce-thread* #f))

(def state-test-suite
  (test-suite "lsp/state"

    ;; --- Document operations ---
    (test-case "documents: get/set/remove"
      (reset-state!)
      (check (get-document "file:///test.ss") => #f)
      (let ((doc (make-document "file:///test.ss" 1 "hello" "gerbil")))
        (set-document! "file:///test.ss" doc)
        (check (not (eq? (get-document "file:///test.ss") #f)) => #t)
        (check-equal? (document-text (get-document "file:///test.ss")) "hello")
        (remove-document! "file:///test.ss")
        (check (get-document "file:///test.ss") => #f)))

    (test-case "all-document-uris"
      (reset-state!)
      (set-document! "a" (make-document "a" 0 "" ""))
      (set-document! "b" (make-document "b" 0 "" ""))
      (let ((uris (all-document-uris)))
        (check (length uris) => 2)
        (check (and (member "a" uris) #t) => #t)
        (check (and (member "b" uris) #t) => #t)))

    ;; --- Symbol index operations ---
    (test-case "symbol index: get/set/remove"
      (reset-state!)
      (check-equal? (get-file-symbols "uri") '())
      (set-file-symbols! "uri" '(sym1 sym2))
      (check (length (get-file-symbols "uri")) => 2)
      (remove-file-symbols! "uri")
      (check-equal? (get-file-symbols "uri") '()))

    (test-case "all-indexed-symbols"
      (reset-state!)
      (set-file-symbols! "a" '(s1 s2))
      (set-file-symbols! "b" '(s3))
      (let ((all (all-indexed-symbols)))
        (check (length all) => 3)))

    ;; --- Lifecycle flags ---
    (test-case "initialized flag"
      (reset-state!)
      (check (server-initialized?) => #f)
      (set-initialized! #t)
      (check (server-initialized?) => #t)
      (set-initialized! #f))

    (test-case "shutdown-requested flag"
      (reset-state!)
      (check (shutdown-requested?) => #f)
      (set-shutdown-requested! #t)
      (check (shutdown-requested?) => #t)
      (set-shutdown-requested! #f))

    ;; --- Workspace root ---
    (test-case "workspace root"
      (reset-state!)
      (check (workspace-root) => #f)
      (set-workspace-root! "/home/user/project")
      (check-equal? (workspace-root) "/home/user/project")
      (set-workspace-root! #f))

    ;; --- Client capabilities ---
    (test-case "client capabilities"
      (reset-state!)
      (check (client-capabilities) => #f)
      (set-client-capabilities! (hash ("textDocument" (hash))))
      (check (hash-table? (client-capabilities)) => #t)
      (set-client-capabilities! #f))

    ;; --- GXC diagnostics cache ---
    (test-case "gxc diagnostics cache"
      (reset-state!)
      (check-equal? (get-gxc-diagnostics "uri") '())
      (set-gxc-diagnostics! "uri" '(diag1 diag2))
      (check (length (get-gxc-diagnostics "uri")) => 2)
      (clear-gxc-diagnostics! "uri")
      (check-equal? (get-gxc-diagnostics "uri") '()))

    ;; --- Module cache ---
    (test-case "module cache"
      (reset-state!)
      (check (get-module-exports ':std/test) => #f)
      (set-module-exports! ':std/test '(run-tests!))
      (check-equal? (get-module-exports ':std/test) '(run-tests!)))

    ;; --- File text cache ---
    (test-case "file text cache"
      (reset-state!)
      (check (get-file-text "uri") => #f)
      (set-file-text! "uri" "source code")
      (check-equal? (get-file-text "uri") "source code")
      (remove-file-text! "uri")
      (check (get-file-text "uri") => #f))

    ;; --- Last completion URI ---
    (test-case "last-completion-uri: get/set"
      (reset-state!)
      (check (last-completion-uri) => #f)
      (set-last-completion-uri! "file:///test.ss")
      (check-equal? (last-completion-uri) "file:///test.ss")
      (set-last-completion-uri! #f))

    ;; --- Debounce thread ---
    (test-case "debounce-thread: get/set"
      (reset-state!)
      (check (debounce-thread) => #f)
      (set-debounce-thread! 'dummy-thread)
      (check-equal? (debounce-thread) 'dummy-thread)
      (set-debounce-thread! #f))

    ;; --- Configuration defaults ---
    (test-case "config: diagnostics-delay default"
      (check (number? (get-config "diagnostics-delay")) => #t)
      (check (= (get-config "diagnostics-delay") 1500) => #t))
  ))

(def main
  (lambda ()
    (run-tests! state-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
