;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/module (pure URI functions)
(import :std/test
        :lsp/lsp/analysis/module)

(export module-test-suite)

(def module-test-suite
  (test-suite "lsp/analysis/module"

    ;; --- uri->file-path ---
    (test-case "uri->file-path: file:// prefix"
      (check-equal? (uri->file-path "file:///home/user/test.ss")
                     "/home/user/test.ss"))

    (test-case "uri->file-path: percent-encoded"
      (check-equal? (uri->file-path "file:///home/my%20dir/test.ss")
                     "/home/my dir/test.ss"))

    (test-case "uri->file-path: no prefix passthrough"
      (check-equal? (uri->file-path "/tmp/test.ss")
                     "/tmp/test.ss"))

    ;; --- path->uri ---
    (test-case "path->uri: simple path"
      (check-equal? (path->uri "/home/user/test.ss")
                     "file:///home/user/test.ss"))

    (test-case "path->uri: path with spaces"
      (check-equal? (path->uri "/home/my dir/test.ss")
                     "file:///home/my%20dir/test.ss"))

    ;; --- uri-encode-path ---
    (test-case "uri-encode-path: no encoding needed"
      (check-equal? (uri-encode-path "/foo/bar.ss") "/foo/bar.ss"))

    (test-case "uri-encode-path: space encoded"
      (check-equal? (uri-encode-path "/foo bar") "/foo%20bar"))

    (test-case "uri-encode-path: unreserved preserved"
      ;; Letters, digits, -, _, ., ~, / should not be encoded
      (check-equal? (uri-encode-path "/a-b_c.d~e/f") "/a-b_c.d~e/f"))

    ;; --- uri-decode ---
    (test-case "uri-decode: %20 -> space"
      (check-equal? (uri-decode "hello%20world") "hello world"))

    (test-case "uri-decode: %41%42%43 -> ABC"
      (check-equal? (uri-decode "%41%42%43") "ABC"))

    (test-case "uri-decode: no percent"
      (check-equal? (uri-decode "hello") "hello"))

    (test-case "uri-decode: empty"
      (check-equal? (uri-decode "") ""))

    (test-case "uri-decode: incomplete %2 at end"
      ;; Not enough chars after %, should pass through as-is
      (check-equal? (uri-decode "ab%2") "ab%2"))

    ;; --- uri round-trip ---
    (test-case "uri encode/decode round trip"
      (let* ((path "/home/my dir/test file.ss")
             (encoded (uri-encode-path path))
             (decoded (uri-decode encoded)))
        (check-equal? decoded path)))

    ;; --- resolve-import-spec ---
    (test-case "resolve-import-spec: non-symbol/non-pair returns #f"
      (check (resolve-import-spec 42 "/tmp/test.ss") => #f))

    (test-case "resolve-import-spec: keyword symbol returns #f for nonexistent"
      ;; A standard library module that doesn't exist on disk
      (check (resolve-import-spec ':nonexistent/module "/tmp/test.ss") => #f))
  ))

(def main
  (lambda ()
    (run-tests! module-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
