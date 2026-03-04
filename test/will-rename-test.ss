;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/will-rename
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/handlers/will-rename)

(export will-rename-test-suite)

(def will-rename-test-suite
  (test-suite "lsp/handlers/will-rename"

    ;; --- strip-ss-extension ---
    (test-case "strip-ss-extension: removes .ss"
      (check-equal? (strip-ss-extension "/foo/bar.ss") "/foo/bar"))

    (test-case "strip-ss-extension: no .ss extension"
      (check-equal? (strip-ss-extension "/foo/bar.txt") "/foo/bar.txt"))

    (test-case "strip-ss-extension: only .ss"
      (check-equal? (strip-ss-extension ".ss") ""))

    ;; --- split-path ---
    (test-case "split-path: absolute path"
      (let ((parts (split-path "/foo/bar/baz")))
        (check (member "foo" parts) => (member "foo" parts))
        (check (member "bar" parts) => (member "bar" parts))
        (check (member "baz" parts) => (member "baz" parts))))

    (test-case "split-path: relative path"
      (let ((parts (split-path "foo/bar")))
        (check (length parts) => 2)
        (check-equal? (car parts) "foo")
        (check-equal? (cadr parts) "bar")))

    (test-case "split-path: single component"
      (let ((parts (split-path "foo")))
        (check (length parts) => 1)
        (check-equal? (car parts) "foo")))

    ;; --- common-prefix-length ---
    (test-case "common-prefix-length: full match"
      (check (common-prefix-length '("a" "b" "c") '("a" "b" "c")) => 3))

    (test-case "common-prefix-length: partial match"
      (check (common-prefix-length '("a" "b" "c") '("a" "b" "d")) => 2))

    (test-case "common-prefix-length: no match"
      (check (common-prefix-length '("x") '("y")) => 0))

    (test-case "common-prefix-length: empty lists"
      (check (common-prefix-length '() '()) => 0))

    ;; --- string-join ---
    (test-case "string-join: multiple parts"
      (check-equal? (string-join "/" '("a" "b" "c")) "a/b/c"))

    (test-case "string-join: single part"
      (check-equal? (string-join "/" '("a")) "a"))

    (test-case "string-join: empty"
      (check-equal? (string-join "/" '()) ""))

    ;; --- compute-relative-path ---
    (test-case "compute-relative-path: sibling"
      (let ((result (compute-relative-path "/foo/bar" "/foo/baz")))
        (check (string? result) => #t)))

    (test-case "compute-relative-path: parent"
      (let ((result (compute-relative-path "/foo/bar/baz" "/foo/qux")))
        (check (string? result) => #t)))

    ;; --- handle-will-rename-files: integration ---
    (test-case "handle-will-rename-files: void for empty files"
      (let* ((params (hash ("files" (vector))))
             (result (handle-will-rename-files params)))
        (check (void? result) => #t)))

    (test-case "handle-will-rename-files: void for missing files key"
      (let* ((params (hash))
             (result (handle-will-rename-files params)))
        ;; files defaults to [], should return void
        (check (void? result) => #t)))
  ))

(def main
  (lambda ()
    (run-tests! will-rename-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
