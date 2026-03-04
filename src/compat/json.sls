#!chezscheme
;;; json.sls -- JSON parser and serializer for Chez Scheme
;;; Compatible with Gerbil's :std/text/json interface.
;;; Representation: objects → hash tables (equal-hash), arrays → vectors,
;;; strings → strings, numbers → numbers, true/#t, false/#f, null → 'null

(library (compat json)
  (export read-json write-json json-object->string string->json-object)

  (import (except (chezscheme) make-hash-table hash-table?)
          (runtime hash))

  ;; --- JSON Parser ---

  ;; Skip whitespace characters
  (define (skip-ws port)
    (let loop ()
      (let ((c (peek-char port)))
        (when (and (not (eof-object? c))
                   (or (char=? c #\space) (char=? c #\tab)
                       (char=? c #\newline) (char=? c #\return)))
          (read-char port)
          (loop)))))

  ;; Expect a specific character
  (define (expect-char port expected)
    (let ((c (read-char port)))
      (unless (and (char? c) (char=? c expected))
        (error 'read-json
          (format "expected '~a' but got '~a'" expected c)))))

  ;; Read a JSON string (opening " already consumed or not)
  (define (read-json-string port)
    (expect-char port #\")
    (let ((out (open-output-string)))
      (let loop ()
        (let ((c (read-char port)))
          (cond
            ((eof-object? c)
             (error 'read-json "unterminated string"))
            ((char=? c #\")
             (get-output-string out))
            ((char=? c #\\)
             (let ((esc (read-char port)))
               (cond
                 ((char=? esc #\") (write-char #\" out))
                 ((char=? esc #\\) (write-char #\\ out))
                 ((char=? esc #\/) (write-char #\/ out))
                 ((char=? esc #\b) (write-char #\backspace out))
                 ((char=? esc #\f) (write-char #\page out))
                 ((char=? esc #\n) (write-char #\newline out))
                 ((char=? esc #\r) (write-char #\return out))
                 ((char=? esc #\t) (write-char #\tab out))
                 ((char=? esc #\u)
                  (let ((hex (read-hex4 port)))
                    (if (and (>= hex #xD800) (<= hex #xDBFF))
                      ;; High surrogate — expect \uXXXX low surrogate
                      (begin
                        (expect-char port #\\)
                        (expect-char port #\u)
                        (let ((low (read-hex4 port)))
                          (let ((cp (+ #x10000
                                       (bitwise-arithmetic-shift-left
                                         (- hex #xD800) 10)
                                       (- low #xDC00))))
                            (write-char (integer->char cp) out))))
                      (write-char (integer->char hex) out))))
                 (else (error 'read-json
                         (format "invalid escape: \\~a" esc)))))
               (loop))
            (else
             (write-char c out)
             (loop)))))))

  ;; Read 4 hex digits
  (define (read-hex4 port)
    (let ((s (make-string 4)))
      (do ((i 0 (+ i 1)))
          ((= i 4))
        (string-set! s i (read-char port)))
      (string->number s 16)))

  ;; Read a JSON number
  (define (read-json-number port)
    (let ((out (open-output-string)))
      (let loop ()
        (let ((c (peek-char port)))
          (if (and (char? c)
                   (or (char-numeric? c)
                       (char=? c #\-)
                       (char=? c #\+)
                       (char=? c #\.)
                       (char=? c #\e)
                       (char=? c #\E)))
            (begin (write-char (read-char port) out) (loop))
            (let ((s (get-output-string out)))
              (or (string->number s)
                  (error 'read-json (format "invalid number: ~a" s)))))))))

  ;; Read a JSON value
  (define (read-json-value port)
    (skip-ws port)
    (let ((c (peek-char port)))
      (cond
        ((eof-object? c) (error 'read-json "unexpected EOF"))
        ((char=? c #\") (read-json-string port))
        ((char=? c #\{) (read-json-object port))
        ((char=? c #\[) (read-json-array port))
        ((char=? c #\t) (read-literal port "true" #t))
        ((char=? c #\f) (read-literal port "false" #f))
        ((char=? c #\n) (read-literal port "null" 'null))
        ((or (char-numeric? c) (char=? c #\-))
         (read-json-number port))
        (else (error 'read-json (format "unexpected char: ~a" c))))))

  ;; Read a literal (true, false, null)
  (define (read-literal port expected value)
    (let ((len (string-length expected)))
      (do ((i 0 (+ i 1)))
          ((= i len) value)
        (let ((c (read-char port)))
          (unless (char=? c (string-ref expected i))
            (error 'read-json
              (format "expected ~a" expected)))))))

  ;; Read a JSON object → hash table
  (define (read-json-object port)
    (expect-char port #\{)
    (skip-ws port)
    (let ((ht (make-hash-table)))
      (if (char=? (peek-char port) #\})
        (begin (read-char port) ht)
        (let loop ()
          (skip-ws port)
          (let ((key (read-json-string port)))
            (skip-ws port)
            (expect-char port #\:)
            (let ((val (read-json-value port)))
              (hash-put! ht key val)
              (skip-ws port)
              (let ((c (read-char port)))
                (cond
                  ((char=? c #\}) ht)
                  ((char=? c #\,) (loop))
                  (else (error 'read-json
                          (format "expected ',' or '}' but got '~a'" c)))))))))))

  ;; Read a JSON array → vector
  (define (read-json-array port)
    (expect-char port #\[)
    (skip-ws port)
    (if (char=? (peek-char port) #\])
      (begin (read-char port) (vector))
      (let loop ((items '()))
        (let ((val (read-json-value port)))
          (skip-ws port)
          (let ((c (read-char port)))
            (cond
              ((char=? c #\])
               (list->vector (reverse (cons val items))))
              ((char=? c #\,)
               (loop (cons val items)))
              (else (error 'read-json
                      (format "expected ',' or ']' but got '~a'" c)))))))))

  ;; Main read-json: reads one JSON value from port
  (define (read-json . args)
    (let ((port (if (pair? args) (car args) (current-input-port))))
      (skip-ws port)
      (if (eof-object? (peek-char port))
        (eof-object)
        (read-json-value port))))

  ;; Parse JSON from string
  (define (string->json-object str)
    (call-with-port (open-input-string str) read-json))

  ;; --- JSON Serializer ---

  ;; Write a JSON string with proper escaping
  (define (write-json-string str port)
    (put-char port #\")
    (let ((len (string-length str)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (let ((c (string-ref str i)))
          (cond
            ((char=? c #\") (put-string port "\\\""))
            ((char=? c #\\) (put-string port "\\\\"))
            ((char=? c #\backspace) (put-string port "\\b"))
            ((char=? c #\page) (put-string port "\\f"))
            ((char=? c #\newline) (put-string port "\\n"))
            ((char=? c #\return) (put-string port "\\r"))
            ((char=? c #\tab) (put-string port "\\t"))
            ((< (char->integer c) #x20)
             (put-string port (format "\\u~4,'0x" (char->integer c))))
            (else (put-char port c))))))
    (put-char port #\"))

  ;; Write a JSON value
  (define (write-json-value val port)
    (cond
      ((string? val) (write-json-string val port))
      ((and (integer? val) (exact? val))
       (put-string port (number->string val)))
      ((number? val)
       (let ((s (number->string (inexact val))))
         ;; Ensure .0 for integer-valued floats
         (put-string port s)))
      ((eq? val #t) (put-string port "true"))
      ((eq? val #f) (put-string port "false"))
      ((eq? val 'null) (put-string port "null"))
      ((vector? val) (write-json-array val port))
      ((hash-table? val) (write-json-object val port))
      ((list? val) (write-json-array (list->vector val) port))
      ((null? val) (put-string port "null"))
      ;; Gerbil void → null
      ((eq? val (void)) (put-string port "null"))
      (else (error 'write-json (format "cannot serialize: ~a" val)))))

  ;; Write a JSON object from hash table
  (define (write-json-object ht port)
    (put-char port #\{)
    (let ((entries (hash->list ht)))
      (let loop ((rest entries) (first? #t))
        (unless (null? rest)
          (unless first? (put-char port #\,))
          (write-json-string (caar rest) port)
          (put-char port #\:)
          (write-json-value (cdar rest) port)
          (loop (cdr rest) #f))))
    (put-char port #\}))

  ;; Write a JSON array from vector
  (define (write-json-array vec port)
    (put-char port #\[)
    (let ((len (vector-length vec)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (when (> i 0) (put-char port #\,))
        (write-json-value (vector-ref vec i) port)))
    (put-char port #\]))

  ;; Main write-json: writes one JSON value to port
  (define (write-json val . args)
    (let ((port (if (pair? args) (car args) (current-output-port))))
      (write-json-value val port)))

  ;; Serialize to string
  (define (json-object->string val)
    (let ((p (open-output-string)))
      (write-json-value val p)
      (get-output-string p)))

  ) ;; end library
