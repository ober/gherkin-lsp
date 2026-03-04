;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/cache module
(import :std/test
        :std/misc/ports
        :lsp/lsp/state
        :lsp/lsp/analysis/symbols
        :lsp/lsp/analysis/cache)

(export cache-test-suite)

;; Reset state between tests
(def (reset-state!)
  (set! *symbol-index* (make-hash-table))
  (set! *import-deps* (make-hash-table))
  (set! *reverse-deps* (make-hash-table)))

(def cache-test-suite
  (test-suite "lsp/analysis/cache"
    (test-case "sym-info->hash round-trip"
      (let* ((sym (make-sym-info "test-fn" "function" 10 5 10 15 "procedure"))
             (h (sym-info->hash sym))
             (sym2 (hash->sym-info h)))
        (check (sym-info-name sym2) => "test-fn")
        (check (sym-info-kind sym2) => "function")
        (check (sym-info-line sym2) => 10)
        (check (sym-info-col sym2) => 5)
        (check (sym-info-end-line sym2) => 10)
        (check (sym-info-end-col sym2) => 15)
        (check (sym-info-detail sym2) => "procedure")))

    (test-case "sym-info->hash with empty detail"
      (let* ((sym (make-sym-info "var" "variable" 1 0 1 5 #f))
             (h (sym-info->hash sym))
             (sym2 (hash->sym-info h)))
        (check (sym-info-detail sym2) => #f)))

    (test-case "set-file-imports! tracks dependencies"
      (reset-state!)
      (set-file-imports! "file:///a.ss" '("file:///b.ss" "file:///c.ss"))
      (check (hash-ref *import-deps* "file:///a.ss" '()) => '("file:///b.ss" "file:///c.ss")))

    (test-case "get-dependents returns reverse deps"
      (reset-state!)
      (set-file-imports! "file:///a.ss" '("file:///common.ss"))
      (set-file-imports! "file:///b.ss" '("file:///common.ss"))
      (let ((deps (get-dependents "file:///common.ss")))
        (check (pair? (member "file:///a.ss" deps)) => #t)
        (check (pair? (member "file:///b.ss" deps)) => #t)))

    (test-case "get-transitive-dependents"
      (reset-state!)
      ;; c depends on b, b depends on a
      (set-file-imports! "file:///b.ss" '("file:///a.ss"))
      (set-file-imports! "file:///c.ss" '("file:///b.ss"))
      (let ((deps (get-transitive-dependents "file:///a.ss")))
        ;; Both b and c should be transitive dependents of a
        (check (pair? (member "file:///b.ss" deps)) => #t)
        (check (pair? (member "file:///c.ss" deps)) => #t)))

    (test-case "clear-file-deps! removes dependencies"
      (reset-state!)
      (set-file-imports! "file:///a.ss" '("file:///b.ss"))
      (check (pair? (get-dependents "file:///b.ss")) => #t)
      (clear-file-deps! "file:///a.ss")
      (check (get-dependents "file:///b.ss") => '()))

    (test-case "uri->file-path* basic"
      (check (uri->file-path* "file:///home/user/test.ss") => "/home/user/test.ss"))

    (test-case "uri->file-path* with encoding"
      (check (uri->file-path* "file:///home/user/my%20file.ss") => "/home/user/my file.ss"))

    (test-case "uri->file-path* non-file uri"
      (check (uri->file-path* "untitled:Untitled-1") => #f))))

(def (main . args)
  (run-tests! cache-test-suite))
