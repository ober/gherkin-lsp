;;; -*- Gerbil -*-
;;; LSP stdio transport — Content-Length framing over stdin/stdout
;;;
;;; All reading uses byte-level I/O (read-u8 / read-subu8vector) to avoid
;;; the need for `port-settings-set! '(buffering: #f)` which can trigger a
;;; Gambit scheduler busy-wait when the fd is O_NONBLOCK (set by editors).
(import :std/text/json
        ./compat/compat
        ./util/log)
(export #t)

;; Read one LSP message from port.
;; Returns the JSON string, 'eof on end-of-stream, or 'error on malformed input.
;; Both headers and body are read as raw bytes to avoid mixing
;; character I/O (read-line) with byte I/O (read-subu8vector).
(def (read-message port)
  (let ((content-length (read-headers port)))
    (cond
      ((eq? content-length 'eof) 'eof)
      ((not content-length)
       ;; Malformed headers (no Content-Length) — not EOF
       'error)
      (else
       (let* ((buf (make-u8vector content-length 0))
              (n (read-subu8vector buf 0 content-length port)))
         (if (= n content-length)
           (let ((body (bytes->string buf)))
             (lsp-debug "recv: ~a" body)
             body)
           (begin
             (lsp-error "incomplete read: expected ~a bytes, got ~a" content-length n)
             'eof)))))))

;; Read headers as bytes until blank line (\r\n\r\n), extract Content-Length.
;; Returns content-length as integer, 'eof on EOF, or #f if no Content-Length found.
;; Uses byte-at-a-time reading to avoid buffering issues with mixed I/O.
(def (read-headers port)
  (let loop ((content-length #f)
             (line-bytes '()))
    (let ((b (read-u8 port)))
      (cond
        ((eof-object? b) 'eof)
        ;; LF (0x0A) — end of header line
        ((= b 10)
         (let ((line (bytes->string
                       (list->u8vector (reverse line-bytes)))))
           ;; Strip trailing \r if present
           (let ((clean (if (and (> (string-length line) 0)
                                 (char=? (string-ref line
                                           (- (string-length line) 1))
                                         #\return))
                          (substring line 0 (- (string-length line) 1))
                          line)))
             (cond
               ;; Blank line — end of headers
               ((string=? clean "")
                (or content-length
                    (begin
                      (lsp-error "no Content-Length header found")
                      #f)))
               ;; Content-Length header
               ((and (>= (string-length clean) 16)
                     (string=? (substring clean 0 16) "Content-Length: "))
                (let ((len (string->number (substring clean 16
                                             (string-length clean)))))
                  (if len
                    (loop len '())
                    (begin
                      (lsp-error "invalid Content-Length: ~a" clean)
                      (loop content-length '())))))
               ;; Other header — skip
               (else (loop content-length '()))))))
        ;; Accumulate header byte
        (else (loop content-length (cons b line-bytes)))))))

;; Write one LSP message to port.
;; Content-Length is the byte count of the UTF-8 body.
;; We write raw bytes for both header and body to avoid
;; encoding mismatches from port settings.
(def (write-message port json-string)
  (let* ((body-bytes (string->bytes json-string))
         (content-length (u8vector-length body-bytes))
         (header (string-append "Content-Length: "
                                (number->string content-length)
                                "\r\n\r\n"))
         (header-bytes (string->bytes header)))
    (lsp-debug "send: ~a" json-string)
    (write-subu8vector header-bytes 0 (u8vector-length header-bytes) port)
    (write-subu8vector body-bytes 0 content-length port)
    (force-output port)))
