;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/diagnostics
(import :std/test
        :std/misc/string
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/handlers/diagnostics)

(export diagnostics-test-suite)

(def diagnostics-test-suite
  (test-suite "lsp/handlers/diagnostics"

    ;; --- string-split-colon ---
    (test-case "string-split-colon: multiple colons"
      (check-equal? (string-split-colon "a:b:c") '("a" "b" "c")))

    (test-case "string-split-colon: no colons"
      (check-equal? (string-split-colon "hello") '("hello")))

    (test-case "string-split-colon: empty string"
      (check-equal? (string-split-colon "") '("")))

    (test-case "string-split-colon: adjacent colons"
      (check-equal? (string-split-colon "a::b") '("a" "" "b")))

    (test-case "string-split-colon: leading colon"
      (check-equal? (string-split-colon ":a") '("" "a")))

    ;; --- parse-gxc-error-line ---
    (test-case "parse-gxc-error-line: file:line:col format"
      (let ((diag (parse-gxc-error-line "test.ss:10:5: unbound variable")))
        (check (not (eq? diag #f)) => #t)
        (check-equal? (hash-ref diag "message") " unbound variable")
        (check-equal? (hash-ref diag "source") "gxc")))

    (test-case "parse-gxc-error-line: *** ERROR format"
      (let ((diag (parse-gxc-error-line "*** ERROR IN foo -- bad syntax")))
        (check (not (eq? diag #f)) => #t)
        (check-equal? (hash-ref diag "source") "gxc")))

    (test-case "parse-gxc-error-line: non-match"
      (check (parse-gxc-error-line "just some text") => #f))

    (test-case "parse-gxc-error-line: empty"
      (check (parse-gxc-error-line "") => #f))

    ;; --- parse-gxc-output ---
    (test-case "parse-gxc-output: multi-line errors"
      (let ((diags (parse-gxc-output "test.ss:1:0: error one\ntest.ss:5:2: error two" "/tmp/test.ss")))
        (check (length diags) => 2)))

    (test-case "parse-gxc-output: unparseable gives generic error"
      (let ((diags (parse-gxc-output "something went wrong" "/tmp/test.ss")))
        (check (length diags) => 1)
        (check-equal? (hash-ref (car diags) "source") "gxc")))

    (test-case "parse-gxc-output: empty string"
      (let ((diags (parse-gxc-output "" "/tmp/test.ss")))
        (check (length diags) => 0)))

    ;; --- parse-diagnostics ---
    (test-case "parse-diagnostics: valid source has no diagnostics"
      (let ((diags (parse-diagnostics "(def x 42)\n(def y 10)")))
        (check (length diags) => 0)))

    (test-case "parse-diagnostics: malformed source has diagnostics"
      (let ((diags (parse-diagnostics "(def x")))
        (check (>= (length diags) 1) => #t)
        ;; Each diagnostic should have a non-empty message
        (let ((first-diag (car diags)))
          (check (hash-table? first-diag) => #t)
          (let ((msg (hash-ref first-diag "message" "")))
            (check (> (string-length msg) 0) => #t)))))

    ;; --- error-message ---
    (test-case "error-message: error returns string"
      ;; Gerbil's (error ...) creates an Error class, not Gambit error-exception
      ;; error-message falls through to format "~a" which includes the full repr
      (let ((msg (with-catch
                   (lambda (e) (error-message e))
                   (lambda () (error "test error")))))
        (check (string? msg) => #t)
        ;; The message should contain "test error" somewhere
        (check (and (string-contains-local msg "test error") #t) => #t)))

    (test-case "error-message: datum-parsing-exception"
      (let ((msg (with-catch
                   (lambda (e) (error-message e))
                   (lambda () (read (open-input-string "(unclosed"))))))
        (check (string? msg) => #t)))

    (test-case "error-message: other exception"
      (let ((msg (with-catch
                   (lambda (e) (error-message e))
                   (lambda () (/ 1 0)))))
        (check (string? msg) => #t)))
  ))

;; Local helper
(def (string-contains-local haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring haystack i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

(def main
  (lambda ()
    (run-tests! diagnostics-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
