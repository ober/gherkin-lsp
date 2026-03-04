;;; -*- Gerbil -*-
;;; Tests for lsp/jsonrpc
(import :std/test
        :std/text/json
        :lsp/lsp/jsonrpc)

(export jsonrpc-test-suite)

(def jsonrpc-test-suite
  (test-suite "lsp/jsonrpc"

    ;; --- Error code constants ---
    (test-case "error code constants"
      (check PARSE-ERROR => -32700)
      (check INVALID-REQUEST => -32600)
      (check METHOD-NOT-FOUND => -32601)
      (check INVALID-PARAMS => -32602)
      (check INTERNAL-ERROR => -32603)
      (check SERVER-NOT-INITIALIZED => -32002)
      (check REQUEST-CANCELLED => -32800))

    ;; --- parse-jsonrpc ---
    (test-case "parse-jsonrpc: valid request"
      (let ((msg (parse-jsonrpc
                   (json-object->string
                     (hash ("jsonrpc" "2.0") ("id" 1) ("method" "initialize")
                           ("params" (hash)))))))
        (check (hash-table? msg) => #t)
        (check (hash-ref msg "id") => 1)
        (check-equal? (hash-ref msg "method") "initialize")))

    (test-case "parse-jsonrpc: invalid JSON"
      (check (parse-jsonrpc "not json {{{") => #f))

    (test-case "parse-jsonrpc: empty string returns eof"
      ;; string->json-object returns #!eof for empty input (not an error)
      (check (eof-object? (parse-jsonrpc "")) => #t))

    ;; --- jsonrpc-request? ---
    (test-case "jsonrpc-request?: has id and method"
      (check (jsonrpc-request? (hash ("id" 1) ("method" "foo"))) => #t))

    (test-case "jsonrpc-request?: missing id"
      (check (jsonrpc-request? (hash ("method" "foo"))) => #f))

    (test-case "jsonrpc-request?: missing method"
      (check (jsonrpc-request? (hash ("id" 1))) => #f))

    (test-case "jsonrpc-request?: non-hash"
      (check (jsonrpc-request? "not a hash") => #f))

    ;; --- jsonrpc-notification? ---
    (test-case "jsonrpc-notification?: has method no id"
      (check (jsonrpc-notification? (hash ("method" "didOpen"))) => #t))

    (test-case "jsonrpc-notification?: has id"
      (check (jsonrpc-notification? (hash ("id" 1) ("method" "foo"))) => #f))

    (test-case "jsonrpc-notification?: non-hash"
      (check (jsonrpc-notification? 42) => #f))

    ;; --- jsonrpc-response? ---
    (test-case "jsonrpc-response?: has id no method"
      (check (jsonrpc-response? (hash ("id" 1) ("result" "ok"))) => #t))

    (test-case "jsonrpc-response?: has method"
      (check (jsonrpc-response? (hash ("id" 1) ("method" "foo"))) => #f))

    ;; --- jsonrpc-method/id/params ---
    (test-case "jsonrpc-method: present"
      (check-equal? (jsonrpc-method (hash ("method" "init"))) "init"))

    (test-case "jsonrpc-method: missing"
      (check (jsonrpc-method (hash)) => #f))

    (test-case "jsonrpc-id: present"
      (check (jsonrpc-id (hash ("id" 42))) => 42))

    (test-case "jsonrpc-id: missing"
      (check (jsonrpc-id (hash)) => #f))

    (test-case "jsonrpc-params: present"
      (let ((p (jsonrpc-params (hash ("params" (hash ("a" 1)))))))
        (check (hash-ref p "a") => 1)))

    (test-case "jsonrpc-params: missing returns empty hash"
      (let ((p (jsonrpc-params (hash))))
        (check (hash-table? p) => #t)))

    ;; --- make-response ---
    (test-case "make-response: round-trip"
      (let* ((json-str (make-response 1 (hash ("x" 42))))
             (parsed (string->json-object json-str)))
        (check-equal? (hash-ref parsed "jsonrpc") "2.0")
        (check (hash-ref parsed "id") => 1)
        (check (hash-ref (hash-ref parsed "result") "x") => 42)))

    ;; --- make-error-response ---
    (test-case "make-error-response: without data"
      (let* ((json-str (make-error-response 2 -32600 "bad request"))
             (parsed (string->json-object json-str)))
        (check (hash-ref parsed "id") => 2)
        (let ((err (hash-ref parsed "error")))
          (check (hash-ref err "code") => -32600)
          (check-equal? (hash-ref err "message") "bad request"))))

    (test-case "make-error-response: with data"
      (let* ((json-str (make-error-response 3 -32603 "internal" "extra"))
             (parsed (string->json-object json-str)))
        (let ((err (hash-ref parsed "error")))
          (check-equal? (hash-ref err "data") "extra"))))

    ;; --- make-notification ---
    (test-case "make-notification: structure"
      (let* ((json-str (make-notification "textDocument/publishDiagnostics"
                                           (hash ("uri" "file:///t.ss"))))
             (parsed (string->json-object json-str)))
        (check-equal? (hash-ref parsed "jsonrpc") "2.0")
        (check-equal? (hash-ref parsed "method") "textDocument/publishDiagnostics")
        (check-equal? (hash-ref (hash-ref parsed "params") "uri") "file:///t.ss")
        (check (hash-key? parsed "id") => #f)))
  ))

(def main
  (lambda ()
    (run-tests! jsonrpc-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
