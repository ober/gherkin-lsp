;;; -*- Gerbil -*-
;;; Document formatting handler
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Handle textDocument/formatting
;;; Returns TextEdit[] that replaces the entire document with formatted text
(def (handle-formatting params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (let ((formatted (format-gerbil-source text)))
          (if (and formatted (not (string=? formatted text)))
            (let ((last-line (count-lines text))
                  (last-col (last-line-length text)))
              (vector
                (make-text-edit
                  (make-lsp-range 0 0 last-line last-col)
                  formatted)))
            [])))
      [])))

;;; Format Gerbil source preserving comments and blank lines.
;;; Strategy: identify regions that are comments/blank lines vs S-expressions.
;;; Only pretty-print the S-expression regions; pass comments through unchanged.
;;; Lines with inline comments (code followed by ;) are also passed through.
(def (format-gerbil-source text)
  (with-catch
    (lambda (e)
      (lsp-debug "formatting error: ~a" e)
      #f)
    (lambda ()
      (let ((lines (string-split-lines text))
            (out (open-output-string)))
        (let loop ((ls lines) (first? #t) (form-lines '()))
          (cond
            ;; End of input -- flush any accumulated form lines
            ((null? ls)
             (when (pair? form-lines)
               (unless first? (newline out))
               (format-form-lines (reverse form-lines) out))
             (get-output-string out))
            ;; Comment line or blank line -- flush form, emit as-is
            ((or (comment-line? (car ls))
                 (blank-line? (car ls)))
             ;; Flush any accumulated form
             (when (pair? form-lines)
               (unless first? (newline out))
               (format-form-lines (reverse form-lines) out)
               (set! form-lines '()))
             (unless (and first? (null? form-lines))
               (newline out))
             (display (car ls) out)
             (loop (cdr ls) #f '()))
            ;; Inline comment (code followed by ;) -- pass through unchanged
            ((inline-comment-line? (car ls))
             (when (pair? form-lines)
               (unless first? (newline out))
               (format-form-lines (reverse form-lines) out)
               (set! form-lines '()))
             (unless (and first? (null? form-lines))
               (newline out))
             (display (car ls) out)
             (loop (cdr ls) #f '()))
            ;; Code line -- accumulate for formatting
            (else
             (loop (cdr ls) first? (cons (car ls) form-lines)))))))))

;;; Format accumulated code lines by reading and pretty-printing forms
(def (format-form-lines lines out)
  (let ((text (string-join-newline lines)))
    (with-catch
      (lambda (e)
        ;; If formatting fails, emit original text unchanged
        (display text out))
      (lambda ()
        (let ((port (open-input-string text)))
          (let loop ((first? #t))
            (let ((form (read port)))
              (unless (eof-object? form)
                (unless first? (newline out))
                (pretty-print form out)
                (loop #f)))))))))

;;; Check if a line is a comment (starts with ; ignoring whitespace)
(def (comment-line? line)
  (let loop ((i 0))
    (cond
      ((>= i (string-length line)) #f)
      ((char=? (string-ref line i) #\;) #t)
      ((char-whitespace-simple? (string-ref line i)) (loop (+ i 1)))
      (else #f))))

;;; Check if a line is blank (only whitespace)
(def (blank-line? line)
  (let loop ((i 0))
    (cond
      ((>= i (string-length line)) #t)
      ((char-whitespace-simple? (string-ref line i)) (loop (+ i 1)))
      (else #f))))

;;; Check if a line has code followed by an inline comment
;;; Detects ; outside of string literals after non-whitespace content
(def (inline-comment-line? line)
  (let ((len (string-length line)))
    (let loop ((i 0) (in-string? #f) (seen-code? #f))
      (cond
        ((>= i len) #f)
        ;; Toggle string state on unescaped double-quote
        ((char=? (string-ref line i) #\")
         (loop (+ i 1) (not in-string?) #t))
        ;; Skip escaped characters inside strings
        ((and in-string? (char=? (string-ref line i) #\\) (< (+ i 1) len))
         (loop (+ i 2) in-string? seen-code?))
        ;; Semicolon outside string after code = inline comment
        ((and (not in-string?) seen-code? (char=? (string-ref line i) #\;))
         #t)
        ;; Track whether we've seen non-whitespace (code)
        ((and (not in-string?) (not (char-whitespace-simple? (string-ref line i))))
         (loop (+ i 1) in-string? #t))
        (else
         (loop (+ i 1) in-string? seen-code?))))))

(def (char-whitespace-simple? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\return)))

;;; Count the number of lines in text (0-based last line number)
(def (count-lines text)
  (let loop ((i 0) (lines 0))
    (cond
      ((>= i (string-length text)) lines)
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ lines 1)))
      (else (loop (+ i 1) lines)))))

;;; Get the length of the last line
(def (last-line-length text)
  (let loop ((i (- (string-length text) 1)) (len 0))
    (cond
      ((< i 0) len)
      ((char=? (string-ref text i) #\newline) len)
      (else (loop (- i 1) (+ len 1))))))

;;; Handle textDocument/rangeFormatting
;;; Formats only the lines within the specified range
(def (handle-range-formatting params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (start (hash-ref range "start" (hash)))
         (end (hash-ref range "end" (hash)))
         (start-line (hash-ref start "line" 0))
         (end-line (hash-ref end "line" 0))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (lines (string-split-lines text))
             (total-lines (length lines)))
        ;; Clamp range to valid lines
        (let* ((sl (max 0 (min start-line (- total-lines 1))))
               (el (max sl (min end-line (- total-lines 1))))
               ;; Extract range lines
               (range-lines (take-lines lines sl el))
               (range-text (string-join-newline range-lines))
               (formatted (format-gerbil-source range-text)))
          (if (and formatted (not (string=? formatted range-text)))
            ;; Compute end col of the last line in range
            (let ((end-line-text (list-ref lines el)))
              (vector
                (make-text-edit
                  (make-lsp-range sl 0 el (string-length end-line-text))
                  formatted)))
            [])))
      [])))

;;; Extract lines from start to end (inclusive), 0-based
(def (take-lines lines start end)
  (let loop ((ls lines) (i 0) (result '()))
    (cond
      ((null? ls) (reverse result))
      ((> i end) (reverse result))
      ((>= i start)
       (loop (cdr ls) (+ i 1) (cons (car ls) result)))
      (else
       (loop (cdr ls) (+ i 1) result)))))

;;; Safe list-ref that returns "" for out-of-bounds
(def (list-ref lst n)
  (let loop ((l lst) (i 0))
    (cond
      ((null? l) "")
      ((= i n) (car l))
      (else (loop (cdr l) (+ i 1))))))
