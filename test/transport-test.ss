;;; -*- Gerbil -*-
;;; Tests for lsp/transport
(import :std/test
        :std/text/json
        :lsp/lsp/transport)

(export transport-test-suite)

(def +scratchpad+
  (let ((dir (string-append (or (getenv "TMPDIR" #f)
                                (getenv "TMP" #f)
                                "/tmp")
                            "/gerbil-lsp-test")))
    (when (not (file-exists? dir))
      (create-directory dir))
    dir))

;; Helper: write string to a temp file and return an input port open in binary mode
(def (make-binary-input-port-from-string str filename)
  (let ((path (string-append +scratchpad+ "/" filename)))
    (call-with-output-file path
      (lambda (p) (display str p)))
    ;; Reopen as binary
    (open-input-file (list path: path char-encoding: 'ISO-8859-1))))

;; Helper: get output as string from binary output port
(def (binary-output-port->string port)
  (bytes->string (get-output-u8vector port)))

;; Local string-contains helper
(def (string-contains str needle)
  (let ((hlen (string-length str))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring str i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

(def transport-test-suite
  (test-suite "lsp/transport"

    ;; --- read-headers ---
    ;; read-headers uses read-u8 (byte I/O), so we need byte input ports
    (test-case "read-headers: valid Content-Length"
      (let ((port (open-input-u8vector (string->bytes "Content-Length: 42\r\n\r\n"))))
        (check (read-headers port) => 42)))

    (test-case "read-headers: EOF"
      (let ((port (open-input-u8vector (u8vector))))
        (check (read-headers port) => 'eof)))

    (test-case "read-headers: multiple headers"
      (let ((port (open-input-u8vector
                    (string->bytes "Content-Type: application/json\r\nContent-Length: 10\r\n\r\n"))))
        (check (read-headers port) => 10)))

    (test-case "read-headers: missing Content-Length"
      (let ((port (open-input-u8vector
                    (string->bytes "Content-Type: text/plain\r\n\r\n"))))
        (check (read-headers port) => #f)))

    ;; --- read-message: error recovery ---
    (test-case "read-message: malformed headers returns error symbol"
      (let ((port (open-input-u8vector
                    (string->bytes "garbage\r\n\r\n"))))
        (check (read-message port) => 'error)))

    ;; --- write-message ---
    (test-case "write-message: contains header and body"
      (let* ((port (open-output-u8vector))
             (body "{\"result\":true}"))
        (write-message port body)
        (let ((output (binary-output-port->string port)))
          (check (and (string-contains output "Content-Length:") #t) => #t)
          (check (and (string-contains output body) #t) => #t))))

    (test-case "write-message: correct byte count"
      (let* ((port (open-output-u8vector))
             (body "{\"x\":1}")
             (body-len (u8vector-length (string->bytes body))))
        (write-message port body)
        (let ((output (binary-output-port->string port)))
          (check (and (string-contains output
                        (string-append "Content-Length: " (number->string body-len)))
                      #t)
                 => #t))))

    ;; --- round trip via file ---
    ;; write-message writes bytes; read-message uses read-u8/read-subu8vector.
    (test-case "write-message then read-message: round trip via file"
      (let* ((body (json-object->string (hash ("jsonrpc" "2.0") ("method" "test"))))
             (tmp-path (string-append +scratchpad+ "/transport-roundtrip.bin")))
        ;; Write
        (let ((out (open-output-file tmp-path)))
          (write-message out body)
          (close-port out))
        ;; Read back — both header and body reading use byte I/O now
        (let ((in (open-input-file tmp-path)))
          (let ((result (read-message in)))
            (close-port in)
            (check-equal? result body)))))

    ;; --- read-message: EOF ---
    (test-case "read-message: EOF"
      (let ((port (open-input-u8vector (u8vector))))
        (check (read-message port) => 'eof)))
  ))

(def main
  (lambda ()
    (run-tests! transport-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
