#!chezscheme
;;; compat.sls -- Unified compatibility layer for gherkin-lsp
;;; Replaces the Gerbil v18/v19 compat selection with Chez native implementations.
;;; Re-exports format, sort, file I/O, string utils, and CLI parsing.

(library (lsp compat)
  (export
    ;; Format
    fprintf
    ;; Sort
    sort
    ;; File I/O
    read-file-string read-all-as-string
    ;; String
    string-trim-eol
    ;; CLI
    call-with-getopt flag option
    ;; Gambit exception compat (stubs for Chez)
    datum-parsing-exception? datum-parsing-exception-kind
    datum-parsing-exception-readenv
    error-exception? error-exception-message
    ;; Gambit thread compat
    thread-terminate!
    ;; Directory
    create-directory*
    ;; Version
    gerbil-version-string
    ;; JSON re-exports
    read-json write-json json-object->string string->json-object
    ;; Misc re-exports
    displayln display-exception
    ;; Port I/O for byte transport
    read-subu8vector
    ;; Re-export hash-key? helper
    hash-key?
    ;; path utils not in gambit compat
    path-strip-directory path-directory path-expand
    ;; Re-export process
    open-process run-process
    ;; Runtime compat
    type-of keyword?
    ;; Gambit internals (stubs)
    |##readenv-current-filepos| |##filepos-line| |##filepos-col|
    ;; Port I/O compat
    read-u8 input-port-line input-port-column
    ;; Time compat (Gambit → Chez)
    seconds->time time->seconds
    )

  (import
    (except (chezscheme) sort sort! fprintf printf iota path-extension)
    (compat format)
    (compat sort)
    (compat getopt)
    (compat json)
    (compat process)
    (compat misc))

  ;; --- File I/O ---

  (define (read-file-string path)
    (call-with-input-file path
      (lambda (p) (get-string-all p))))

  (define (read-all-as-string port)
    (let ((content (get-string-all port)))
      (if (eof-object? content) "" content)))

  ;; --- String ---

  (define (string-trim-eol str)
    (let ((len (string-length str)))
      (let loop ((i (- len 1)))
        (if (and (>= i 0)
                 (let ((c (string-ref str i)))
                   (or (char=? c #\newline)
                       (char=? c #\return))))
          (loop (- i 1))
          (substring str 0 (+ i 1))))))

  ;; --- displayln ---
  (define (displayln . args)
    (for-each display args)
    (newline))

  ;; --- Gambit exception compatibility stubs ---
  ;; Chez doesn't have Gambit's datum-parsing-exception type.
  ;; These stubs return #f so error handlers fall through gracefully.

  (define (datum-parsing-exception? e) #f)
  (define (datum-parsing-exception-kind e) "unknown")
  (define (datum-parsing-exception-readenv e) #f)

  ;; Gambit's error-exception? and error-exception-message
  ;; In Chez, errors are conditions. Map appropriately.
  (define (error-exception? e)
    (or (message-condition? e)
        (condition? e)))

  (define (error-exception-message e)
    (cond
      ((message-condition? e) (condition-message e))
      ((condition? e)
       (call-with-string-output-port
         (lambda (p) (display-condition e p))))
      (else (format "~a" e))))

  ;; --- Thread compatibility ---
  ;; Gambit thread-terminate! forcibly kills a thread.
  ;; Chez has no direct equivalent. Use a no-op (threads will
  ;; finish naturally or be replaced by new spawns).
  (define (thread-terminate! th) (void))

  ;; --- Directory creation ---
  (define (create-directory* path)
    (unless (file-exists? path)
      (mkdir path)))

  ;; --- Version ---
  (define (gerbil-version-string)
    "gherkin-lsp (Chez Scheme)")

  ;; --- display-exception for error formatting ---
  ;; Re-export Chez's display-condition as display-exception for Gambit compat
  (define (display-exception e . port-opt)
    (let ((port (if (pair? port-opt) (car port-opt) (current-error-port))))
      (if (condition? e)
        (display-condition e port)
        (display e port))))

  ;; --- read-subu8vector (Gambit → Chez) ---
  ;; Gambit: (read-subu8vector buf start end port) → count
  ;; Chez: use get-bytevector-n! on a binary port
  (define (read-subu8vector buf start count port)
    (if (binary-port? port)
      (let ((n (get-bytevector-n! port buf start count)))
        (if (eof-object? n) 0 n))
      ;; For textual ports, read byte-by-byte via char conversion
      (let loop ((i 0))
        (if (>= i count) i
          (let ((c (read-char port)))
            (if (eof-object? c) i
              (begin
                (bytevector-u8-set! buf (+ start i) (char->integer c))
                (loop (+ i 1)))))))))

  ;; --- type-of (Gerbil runtime) ---
  (define (type-of x)
    (cond
      ((boolean? x) 'boolean)
      ((number? x) 'number)
      ((string? x) 'string)
      ((symbol? x) 'symbol)
      ((pair? x) 'pair)
      ((null? x) 'null)
      ((vector? x) 'vector)
      ((hashtable? x) 'hash-table)
      ((procedure? x) 'procedure)
      ((char? x) 'char)
      ((bytevector? x) 'u8vector)
      ((port? x) 'port)
      ((eof-object? x) 'eof)
      (else 'unknown)))

  ;; --- read-u8 (Gambit → Chez) ---
  ;; Gambit's read-u8 works on any port. Chez's get-u8 requires binary.
  ;; Fall back to char reading for textual ports.
  (define (read-u8 port)
    (if (binary-port? port)
      (get-u8 port)
      (let ((c (read-char port)))
        (if (eof-object? c)
          (eof-object)
          (char->integer c)))))

  ;; --- input-port-line/column (Gambit → Chez) ---
  ;; Chez doesn't track line/column on ports. Return 1 (Gambit is 1-based).
  (define (input-port-line port) 1)
  (define (input-port-column port) 1)

  ;; --- seconds->time / time->seconds (Gambit → Chez) ---
  ;; Gambit: (seconds->time secs) creates a time, (time->seconds t) extracts
  ;; Chez: use make-time/time-second/time-nanosecond for time-utc
  (define (seconds->time secs)
    (let* ((s (exact (floor secs)))
           (ns (exact (round (* (- secs s) 1000000000)))))
      (make-time 'time-utc ns s)))

  (define (time->seconds t)
    (+ (time-second t)
       (/ (time-nanosecond t) 1000000000.0)))

  ;; --- Gambit internal stubs ---
  ;; These are Gambit ## internals for parsing error positions.
  ;; Never actually called since datum-parsing-exception? returns #f.
  (define (|##readenv-current-filepos| re) 0)
  (define (|##filepos-line| fp) 0)
  (define (|##filepos-col| fp) 0)

  ;; --- keyword? (Gerbil runtime) ---
  ;; In Gerbil, keywords are distinct objects. In Chez (via Gherkin),
  ;; they're symbols ending in ':'
  (define (keyword? x)
    (and (symbol? x)
         (let ((s (symbol->string x)))
           (and (> (string-length s) 1)
                (char=? (string-ref s (- (string-length s) 1)) #\:)))))

  ;; --- hash-key? (Gerbil hash table predicate) ---
  ;; Check if a key exists in a Gherkin runtime hash table
  ;; The Gherkin runtime hash uses hashtable-contains? under the hood
  (define (hash-key? ht key)
    (hashtable-contains? ht key))

  ;; --- open-process (Gambit-style keyword args) ---
  ;; Gambit: (open-process (list path: cmd arguments: args ...))
  ;; Returns a port that can be read for stdout
  (define (open-process settings)
    (let ((path #f) (args '()) (dir #f)
          (stderr-redir #f) (stdout-redir #f) (env #f))
      (let loop ((rest (if (list? settings) settings '())))
        (cond
          ((null? rest) (void))
          ((and (pair? (cdr rest)) (memq (car rest) '(path path:)))
           (set! path (cadr rest)) (loop (cddr rest)))
          ((and (pair? (cdr rest)) (memq (car rest) '(arguments arguments:)))
           (set! args (cadr rest)) (loop (cddr rest)))
          ((and (pair? (cdr rest)) (memq (car rest) '(directory directory:)))
           (set! dir (cadr rest)) (loop (cddr rest)))
          ((and (pair? (cdr rest)) (memq (car rest) '(stderr-redirection stderr-redirection:)))
           (set! stderr-redir (cadr rest)) (loop (cddr rest)))
          ((and (pair? (cdr rest)) (memq (car rest) '(stdout-redirection stdout-redirection:)))
           (set! stdout-redir (cadr rest)) (loop (cddr rest)))
          ((and (pair? (cdr rest)) (memq (car rest) '(environment environment:)))
           (set! env (cadr rest)) (loop (cddr rest)))
          (else (loop (cdr rest)))))
      (let* ((prog (or path "/bin/sh"))
             ;; Build shell command for Chez's (process ...)
             (cmd (if (null? args) prog
                    (let ((p (open-output-string)))
                      (display prog p)
                      (for-each (lambda (a)
                                  (display " " p)
                                  ;; Quote args with single quotes
                                  (display "'" p) (display a p) (display "'" p))
                                args)
                      (get-output-string p))))
             ;; Add stderr redirection if requested
             (full-cmd (if stderr-redir
                         (string-append cmd " 2>&1")
                         cmd))
             ;; Add cd prefix if directory specified
             (full-cmd (if dir
                         (string-append "cd '" dir "' && " full-cmd)
                         full-cmd))
             ;; Add env prefix if specified
             (full-cmd (if (and env (pair? env))
                         (string-append
                           (let ((p (open-output-string)))
                             (for-each (lambda (e)
                                         (display "export " p)
                                         (display e p)
                                         (display "; " p))
                                       env)
                             (get-output-string p))
                           full-cmd)
                         full-cmd)))
        (let-values (((from-stdout to-stdin pid)
                      (apply values (process full-cmd))))
          (close-port to-stdin)
          from-stdout))))

  ) ;; end library
